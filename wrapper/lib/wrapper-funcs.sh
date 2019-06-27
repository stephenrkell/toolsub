debug_print () {
    lvl="$1"
    shift
    if [[ "$DEBUG_CC" -ge $lvl ]]; then
        echo "$@"
    fi
}

is_pp () {
    saw_pp=0
    outpos=0
    declare -a seen_args
    while shift; do
        ctr="$(( $ctr + 1 ))"
        seen_args[$ctr]="$1"
        if [[ -z "$1" ]]; then continue; fi
        case "$1" in
         (-o) outfile="$2"; outpos="$(( $ctr + 1 ))";;
         (-E) saw_pp=1 ;;
        esac
    done
    # if we're only preprocessing, or if we have no explicit output file, just run cc1
    # FIXME: why the "no explicit output file" rule? Seems like a hole in our coverage.
    if [ "$saw_pp" -eq 0 ] || [ $outpos -eq 0 ]; then
        debug_print 1 "Wrapping cc1, we don't seem to be doing preprocessing." 1>&2
        false
    else
        debug_print 1 "Wrapping cc1, we seem to be doing a preprocessing step; args ${seen_args[@]}" 1>&2
        true
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
    driver="$( readlink /proc/$PPID/exe )"
    debug_print 1 "driver binary is probably $driver" 1>&2
    echo "$driver"
}

# Build the array of options to pass our preprocessing tool,
# given the options that the compiler driver wanted to pass to cc1
# -- which has its own built-in cpp! Some options won't be understood
# by the standalone cpp, so we have to filter these out.


# This whole approach is hacky and fragile.
# We are forcing a "drop-in cpp replacement" when in fact, when
# compiling C code, even with -no-integrated-cpp, cpp /per se/ is never run.
# Rather, a separate cc1 invocation is done... but the details of its command
# line are rather compiler-specific. It would be more robust simply to run
# that command to get our preprocessing done, and fit our extra processing
# as pre- and/or post-passes.
write_cpp_options_from_cc1_options () {
    debug_print 1 "outpos is $outpos" 1>&2
    debug_print 1 "outfile is $outfile" 1>&2
    prev_arg=""
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
        debug_print 1 "\$# is $#, \$1 is '$1'" 1>&2
        if [[ $# -eq 0 ]]; then break; fi
        case "$1" in
            # we are running our own cpp-alike -- filter out non-cpp args
            (-imultiarch|-iremap*) shift || break ;; # skip arg too
            (-quiet|-fhonour-copts) ;; # skip just it
            (-MD|-MMD)
                # with cc1, -MD and -MMD always take an argument.
                # with cpp, they never do.
                # solve by removing -MD and translating to -MF
                mf_opt="$1"
                mf_file="$2"
                saw_md="$1"
                shift
            ;;
            (-MF|-MMF)
                mf_opt="$1"
                mf_file="$2"
                shift
            ;;
            (-o)
                outfile="$2"
                shift
            ;;
            (-E) saw_E=1
                ;;& # then fall through
            (-Os) saw_Os=1
                ;;& # then fall through
            (-Werror) saw_Werror=1
                ;;& # then fall through
            (-D_FORTIFY_SOURCE|-D_FORTIFY_SOURCE=*) saw_D_FORTIFY_SOURCE=1; pos_D_FORTIFY_SOURCE=$ctr
                ;;& # then fall through
            (_FORTIFY_SOURCE|_FORTIFY_SOURCE=*)
                if [[ "$prev_arg" == "-D" ]]; then
                    saw_D_FORTIFY_SOURCE=1; pos_D_FORTIFY_SOURCE=$(( $ctr - 1 ))
                fi
                ;;& # then fall through
            (*)
                debug_print 1 "Snarfing $1" 1>&2
                cpp_options[$ctr]="$1"
                ctr=$(( $ctr + 1))
            ;;
        esac
        prev_arg="$1"
    done
    # re-add -o
    if [[ -n "$outfile" ]]; then
        cpp_options[$ctr]="-o"
        ctr=$(( $ctr + 1))
        cpp_options[$ctr]="$outfile"
        ctr=$(( $ctr + 1))
    fi
    # figure out -MD -MMD -MF -MMF
    if [[ -n "$mf_opt" ]]; then
        case "$saw_md" in
            (-MMD)
                cpp_options[$ctr]="-MM"
                ctr=$(( $ctr + 1 ))
            ;;
            (-MD)
                cpp_options[$ctr]="-M"
                ctr=$(( $ctr + 1 ))
            ;;
            (*) ;;
        esac
        cpp_options[$ctr]="$( echo "$mf_opt" | sed 's/D$/F/' )"
        ctr=$(( $ctr + 1)) 
        cpp_options[$ctr]="$mf_file"
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
        case "${cpp_options[$pos_D_FORTIFY_SOURCE]}" in
            (-D) # rewrite the next one too
                cpp_options[$pos_D_FORTIFY_SOURCE]="-U"
                cpp_options[$(( $pos_D_FORTIFY_SOURCE + 1 ))]="$( echo "${cpp_options[$(( $pos_D_FORTIFY_SOURCE + 1 ))]}" | sed 's/=.*//' )"
            ;;
            (-D_FORTIFY_SOURCE*)
                cpp_options[$pos_D_FORTIFY_SOURCE]="$( echo "${cpp_options[$pos_D_FORTIFY_SOURCE]}" | sed 's/-D/-U/' | sed 's/=.*//' )"
            ;;
        esac
    fi
}

# our "include guard": once WRAPPER is set, client scripts won't source us
WRAPPER="${WRAPPER:-$(dirname "$0")/../../wrapper/bin/wrapper}"
