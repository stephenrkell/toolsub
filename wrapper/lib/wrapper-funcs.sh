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
        echo "$@" 1>&2 # don't use 1>&2 -- callers do it (FIXME: why?)
    fi
}

run_with_replacement_outfile () {
    replacement="$1"
    declare -a args
    ctr=0
    while shift; do
        ctr="$(( $ctr + 1 ))"
        if [[ -z "$1" ]]; then continue; fi
        case "$1" in
         (-o) args[$ctr]="$1"; args[$(( $ctr + 1 ))]="$replacement"; shift; ctr="$(( $ctr + 1 ))" ;;
         (*)  args[$ctr]="$1" ;;
        esac
    done
    debug_print 1 "Running + replacement outfile: ${args[@]}" 1>&2
    "${args[@]}"
}

# FIXME: replace this with use of norm_cc1_options
run_with_replacement_infile () {
    # Scan a command line, identify the input file and replace it with $1
    # PROBLEM: how do we identify the input filename?
    # Doing this generally means understanding cpp's entire command-line syntax.
    # We instead use a HACK:
    # for gcc we only need understand the options -E -quiet -imultiarch x86_64-linux-gnu
    # ... which (1) come first, and (2) cpp *doesn't* understand.
    replacement="$1"
    debug_print 1 "Trying to replace input file given args: $@" 1>&2
    shift
    declare -a args
    args[1]="$1"
    ctr=2
    orig_infile=""
    must_read_arg=""
    handle_possible_infile () {
        if [[ -z "$orig_infile" ]]; then
            debug_print 1 "We think the input file is $1" 1>&2
            orig_infile="$1";
            args[$ctr]="$replacement"
        else
            args[$ctr]="$1"
        fi
    }
    while shift; do
        ctr="$(( $ctr + 1 ))"
        if [[ -n "$must_read_arg" ]]; then
            args[$ctr]="$1"
            must_read_arg=""
            continue
        fi
        if [[ -z "$1" ]]; then continue; fi
        case "$1" in
            (-E|-quiet) ;; # skip it!
            (-D|-U|-include) must_read_arg=1; args[$ctr]="$1" ;;
            (-imultiarch|-iremap) shift || break ;; # skip it and its arg!
            (--) no_more_options=1 ;;
            (-*) if [[ -z "$no_more_options" ]]; then args[$ctr]="$1"; else handle_possible_infile "$1"; fi ;;
            (*) handle_possible_infile "$1" ;;
        esac
    done
    debug_print 1 "Running + replacement infile ($replacement): ${args[@]}" 1>&2
    "${args[@]}"
}

guess_driver () {
    driver="$( ps -p $PPID -o comm= )"
    debug_print 1 "driver binary is probably $driver" 1>&2
    echo "$driver"
}

# Build the array of options to pass our preprocessing tool,
# given the options that the compiler driver wanted to pass to cc1
# -- which has its own built-in cpp! Some options won't be understood
# by the standalone cpp, so we have to filter these out.


# We want to allow a "drop-in cpp replacement" when in fact, when
# compiling C code, even with -no-integrated-cpp, cpp /per se/ is never run.
# Rather, a separate cc1 invocation is done... but the details of its command
# line are rather compiler-specific. It would be more robust simply to run
# that command to get our preprocessing done, and fit our extra processing
# as pre- and/or post-passes.
write_cpp_options_from_cc1_options () {
    declare -a norm_cc1_options
    normalize_cc1_options "$@"
    do_write_cpp_options "${norm_cc1_options[@]}"
}
do_write_cpp_options () {
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

declare -a infiles
normalize_cc1_options () {
    prev_arg=""
    inctr=0
    ctr=0
    saw_MD=""
    mf_opt=""
    mf_file=""
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
            (-MD|-MMD)
                # with cc1, -MD and -MMD always take an argument.
                # with cpp, they never do.
                # solve by removing -MD and translating to -MF
                mf_opt="$1"
                mf_file="$2"
                saw_md="$1"
                shift
            ;; # don't keep matching
            (-MF|-MMF)
                mf_opt="$1"
                mf_file="$2"
                shift
            ;; # don't keep matching
            (-o)
                outfile="$2"
                shift
            ;; # don't keep matching
            (-E) saw_E=1;
                ;;& # then keep matching
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
                    (-D|-U|-x|-aux-info|-dumpbase|-auxbase|-I|-i[a-z]*|-X*|-u|-T|--param|-G)
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
    # figure out -MD -MMD -MF -MMF
    if [[ -n "$mf_opt" ]]; then
        case "$saw_md" in
            (-MMD)
                norm_cc1_options[$ctr]="-MM"
                ctr=$(( $ctr + 1 ))
            ;;
            (-MD)
                norm_cc1_options[$ctr]="-M"
                ctr=$(( $ctr + 1 ))
            ;;
            (*) ;;
        esac
        norm_cc1_options[$ctr]="$( echo "$mf_opt" | sed 's/D$/F/' )"
        ctr=$(( $ctr + 1)) 
        norm_cc1_options[$ctr]="$mf_file"
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

# our "include guard": once WRAPPER is set, client scripts won't source us
WRAPPER="${WRAPPER:-$(dirname "$0")/../../wrapper/bin/wrapper}"
