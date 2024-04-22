# we want it to be possible to write wrappers
# as shell functions, not just commands.
do_exec () {
#    exec "$@"
    "$@"
    status=$?
    exit $status
}

debug_print () {
    lvl="$1"
    shift
    if [[ "$DEBUG_CC" -ge $lvl ]]; then
        echo "$@" 1>&2
    fi
}

# nasty HACK: examine the parent process's command line to guess the driver
# This isn't *so* bad, because as a wrapper script we can be pretty sure that
# we are caller directly by the driver.
# HMM. Unless someone (like me) uses -### and then tries to run the commands
# directly. Then it guesses our driver is 'bash', with hilarious results.
# Why do we have to guess the driver? Because it's better than guessing just "cpp".
guess_driver () {
    driver="$( ps -p $PPID -o comm= )"
    debug_print 1 "driver binary might be $driver" 1>&2
    # What's a sanity check we can do on the parent process? For now just rule out the stupids...
    case "$driver" in
        (*sh)   
            echo "Bailing rather than believing driver is $driver" 1>&2
            exit 1
        ;;
        (*)
            true
        ;;
    esac
    echo "$driver"
}

# We want to allow a "drop-in cpp replacement" when in fact, when
# compiling C code, even with -no-integrated-cpp, cpp /per se/ is never run.
# Rather, a separate cc1 invocation is done... but the details of its command
# line are rather compiler-specific. We try to turn these into cpp options.
#
# In this and subsequent functions we take a full command, i.e. with "$1" being
# the command name, even though we only really care about the options. This is
# to allow us to write "while shift || break ...", which is handy since bash
# lacks a do--while loop. The alternative "while true" has the downside that a
# "continue" will make it loop infinitely.
write_cpp_options_from_cc1_command () {
    declare -a norm_cc1_command
    write_normalized_cc1_options_from_cc1_command "$@"
    # the normalized options lack the initial command, so add it back
    do_write_cpp_options_from_normalized_cc1_command "$1" "${norm_cc1_options[@]}"
}
# take a cc1 command with normalized options, write cpp options
do_write_cpp_options_from_normalized_cc1_command () {
    ctr=0
    while shift || break; do
        debug_print 2 "\$# is $#, \$1 is '$1'" 1>&2
        if [[ $# -eq 0 ]]; then break; fi
        case "$1" in
            # we are running our own cpp-alike -- filter out non-cpp args
            (-imultiarch|-iremap*) shift || break ;; # skip arg too
            (-quiet|-fhonour-copts) ;; # skip just it
            (*)
                debug_print 2 "Write-cpp: snarfing $1" 1>&2
                cpp_options[$ctr]="$1"
                ctr=$(( $ctr + 1 ))
            ;;
        esac
    done
    # Finally, if we were called with CPP="/path/to/cpp -some_arg -another_arg"
    # (say by an automake-generated Makefile)
    # we may want to deduplicate arguments between that cpp and us.
    # We assume that no argument added to CPP has duplicate-sensitive or
    # position-sensitive meaning.
    # PROBLEM: this doesn't work if normalization changed a word syntactically,
    # which it does with -D et al. Need to act earlier
    if [[ -n "$CPP" ]]; then
        declare -a norm_cpp_command
        word_is_present () {
            for w in "${cpp_options[@]}"; do
                debug_print 2 "Comparing $w against $1" 1>&2
                if [[ "$w" == "$1" ]]; then
                    return 0
                else
                    true
                fi
            done
            return 1
        }
        ctr=0
        for word in $CPP; do
            if word_is_present "$word"; then
                debug_print 1 "Dropping $w from CPP" 1>&2
            else
                norm_cpp_command[$ctr]="$word"
                ctr=$(( $ctr + 1))
            fi
        done
        CPP=""
        for w in "${norm_cpp_command[@]}"; do
            CPP="${CPP:+$CPP }$w"
        done
    fi
}

write_cc_options_from_cc1_command () {
    declare -a norm_cc1_options
    write_normalized_cc1_options_from_cc1_command "$@"
    # the normalized options lack the initial command, so add it back
    do_write_cc_options_from_normalized_cc1_command "$1" "${norm_cc1_options[@]}"
}
# given command, write options
do_write_cc_options_from_normalized_cc1_command () {
    debug_print 1 "writing cc options for $@" 1>&2
    ctr=0
    while shift || break; do
        debug_print 2 "\$# is $#, \$1 is '$1'" 1>&2
        if [[ $# -eq 0 ]]; then break; fi
        case "$1" in
            # we are running our own .i-to-.s pass -- filter out cpp-only args
            (-imultiarch|-iremap*) shift || break ;; # skip arg too
            (-quiet|-fhonour-copts) ;; # skip just it
            (-MD|-MMD) # NASTY: with cc1, -MD has an argument! it's the filename, like with -MF.
                # with cc or cpp, we have to use -MM?D -MF
                cc_options[$ctr]="$1"
                ctr=$(( $ctr + 1 ))
                shift # now $1 is the filename argument
                cc_options[$ctr]="-MF"
                ctr=$(( $ctr + 1 ))
                cc_options[$ctr]="$1"
                ctr=$(( $ctr + 1 ))
            ;;
            (*)
                debug_print 2 "Write-cc: snarfing $1" 1>&2
                cc_options[$ctr]="$1"
                ctr=$(( $ctr + 1 ))
            ;;
        esac
    done
}

declare -a infiles
# We run a normalizing pass on a cc1 command, to remove some awkward forms from it.
# NOTE: normalizing cc1 options does not remove the nasty "-MD <argument>" form.
#
# What does it do?
# It tries to enumerate input files, but is very imprecise at present.
# It identifies the output file, pulling out "-o" and then re-adding it (pointlessly).
#
# The main purpose is to rewrite cases where a single cc1 invocation works, but
# splitting into separate preprocessing and compilation invocations doesn't.
# The main thing currently is to futz with options to avoid a breaking interaction
# between -Werror, -D_FORTIFY_SOURCE and -Os.
write_normalized_cc1_options_from_cc1_command () {
    prev_arg=""
    inctr=0
    ctr=0
    outfile=""
    saw_E=""
    saw_Os=""
    saw_Werror=""
    saw_D_FORTIFY_SOURCE=""
    while shift || break; do
        if [[ $# -eq 0 ]]; then break; fi
        debug_print 2 "\$# is $#, \$1 is '$1'" 1>&2
        not_infile=""
        case "$1" in
            (-o)
                outfile="$2"
                shift
            ;; # don't keep matching; we've shifted
            (-E) saw_E=1;
                ;;& # then keep matching... want to hit "-*" below
            (-Os) saw_Os=1
                ;;& # then keep matching
            (-Werror) saw_Werror=1
                ;;& # then keep matching
            (-D_FORTIFY_SOURCE|-D_FORTIFY_SOURCE=*) saw_D_FORTIFY_SOURCE=1; pos_D_FORTIFY_SOURCE=$ctr
                ;;& # then keep matching
            (_FORTIFY_SOURCE|_FORTIFY_SOURCE=*)
                if [[ "$prev_arg" == "-D" ]]; then
                    saw_D_FORTIFY_SOURCE=1; pos_D_FORTIFY_SOURCE=$(( $ctr - 1 ))
                fi
                ;;& # then keep matching
            (-) # it denotes stdin, so is an infile
                # (the case of a stdout outfile was handled above)
                infiles[$inctr]="$1"
                inctr=$(( $inctr + 1 ))
            ;;& # then keep matching
            (-*) # looks like an option, so assume it's NOT an infile
                 # though FIXME: that could be wrong if we've seen '--' (does gcc accept this?)
            ;;&
            ([^-]*)
                # FIXME: this is super-imprecise.
                # A useful regex for scraping the gcc man page for opts-with-args:
                # -[-a-z0-9]+ [^-[:blank:]]
                case "$prev_arg" in
                    (-D|-U|-x|-aux-info|-dumpbase|-auxbase|-I|-i[a-z]*|-X*|-u|-T|--param|-G|-MF|-MD|-MMD)
                    ;; # it's the opt's arg
                    (*)
                        # it may be an input file
                        infiles[$inctr]="$1"
                        inctr=$(( $inctr + 1 ))
                    ;;
                esac
            ;;&
            (*)
                debug_print 2 "Normalize: snarfing $1" 1>&2
                norm_cc1_options[$ctr]="$1"
                ctr=$(( $ctr + 1))
            ;;
        esac
        prev_arg="$1"
    done
    # re-add -o
    if [[ -n "$outfile" ]]; then
        norm_cc1_options[$ctr]="-o"
        ctr=$(( $ctr + 1))
        norm_cc1_options[$ctr]="$outfile"
        ctr=$(( $ctr + 1))
    fi
    # if we saw -Os and -Werror and -D_FORTIFY_SOURCE, then we have a problem:
    # -Os adds a builtin #define of _FORTIFY_SOURCE which triggers a multiple
    # definition warning, hence error. The integrated cpp avoids this, presumably
    # by detecting the already-present -D_FORTIFY_SOURCE. Arguably the client
    # is at fault, although it's not documented that -Os implies -D_FORTIFY_SOURCE.
    # HACK: if we saw all three, change -D_FORTIFY_SOURCE to -U_...
    if [[ -n "$saw_Os" ]] && [[ -n "$saw_Werror" ]] & [[ -n "$saw_D_FORTIFY_SOURCE" ]]; then
        # another HACK: to handle separate "-D" and "_FORTIFY_SOURCE", we only assume that
        # the pos records where the "-D" is
        case "${norm_cc1_options[$pos_D_FORTIFY_SOURCE]}" in
            (-D) # rewrite the next one too
                norm_cc1_options[$pos_D_FORTIFY_SOURCE]="-U"
                norm_cc1_options[$(( $pos_D_FORTIFY_SOURCE + 1 ))]="$( echo "${norm_cc1_options[$(( $pos_D_FORTIFY_SOURCE + 1 ))]}" | sed 's/=.*//' )"
            ;;
            (-D_FORTIFY_SOURCE*)
                norm_cc1_options[$pos_D_FORTIFY_SOURCE]="$( echo "${norm_cc1_options[$pos_D_FORTIFY_SOURCE]}" | sed 's/-D/-U/' | sed 's/=.*//' )"
            ;;
        esac
    fi
}

# utility for as-wrappers
parse_as_command () {
    debug_print 1 "My as: $@" 1>&2
    as="$1"
    shift
    declare -ga as_args
    declare -ga as_infiles
    for a in `seq 1 $#`; do
        as_args[$ctr]="${!a}"
        ctr=$(( $ctr + 1 ))
    done
    declare -ga as_options
    ctr=0
    while expr match "$1" '^-.*' >/dev/null; do
        as_options[$ctr]="$1"
        ctr=$(( $ctr + 1 ))
        case "$1" in
            ('--')  # no more opts
                shift; break
            ;;
            # options taking a non-option-looking argument
            ('-o')
                as_outfile="$2"
            ;;&
            (--debug-prefix-map|-I|--MD|-o)
                as_options[$ctr]="$2"
                ctr=$(( $ctr + 1 ))
                shift
            ;;
            (-*)
                # some other option
            ;;
            (*) # error!
                debug_print 1 "Unrecognised 'as' option: $1" 1>&2
                false
            ;;
        esac
        shift || break
    done
    debug_print 1 "as args left over ($#): $@" 1>&2
    ctr=0
    for a in `seq 1 $#`; do
        as_infiles[$ctr]="${!a}"
        ctr=$(( $ctr + 1 ))
    done
    debug_print 1 "Doing: ${AS:-as} ${as_options[@]} -- ${as_infiles[@]}" 1>&2
    #"${as}" "${as_options[@]}" "$@"
}

# our "include guard": once WRAPPER is set, well-behaved client scripts should refrain from sourcing us again
WRAPPER="${WRAPPER:-$(dirname "$0")/../../wrapper/bin/wrapper}"
