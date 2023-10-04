(* compiler_args.ml: -- a simple bit of compiler wrapping, intended
 * for custom preprocessors (cilpp, cccppp). *)


open Printf
open Unix
module D = Dynlink
external mkstemp: string -> Unix.file_descr * string = "caml_mkstemp"
external mkstemps: string -> int -> Unix.file_descr * string = "caml_mkstemps"

(* Use polymorphic variants because clients like cilpp will extend this *)
type basic_extra_arg = [
    `ArgNamingOutputFile
  | `ArgNamingDriver
  | `ArgNamingLang
  | `ArgNamingDependencyOutputFile of bool (* nameIsExplicitlyRequested *)
  ]

(* Test whether a string matches a prefix... but instead of returning a
 * boolean, return None if it doesn't and Some(suffix) if it does, i.e.
 * returning the part of the string that follows the prefix. *)
let matchesPrefix (prefix: string) (s: string) : string option =
    if (String.length s > String.length prefix
       && String.sub s 0 (String.length prefix) = prefix)
    then Some(Str.string_after s (String.length prefix))
    else None

(* Convenience for forcing an option *)
let really = function Some(optVal) -> optVal | None -> failwith "really None"

type basic_arg_info = {
    minus_o_pos     : int option;
    output_file     : string option;
    deps_outputfile : string option;
    driver          : string option;
    language_std    : string option;
    input_language  : string option
};;

(* It is surprisingly tricky to figure out which 'cpp' to run and with which
 * arguments. *)
let guessCppCommandAndLang (info : basic_arg_info) : (string list) * string =
  let langOfStd std =
      if info.input_language <> None then really info.input_language
      else if None <> matchesPrefix "c++" std || None <> matchesPrefix "gnu++" std
           then "c++"
           else "c" (* FIXME: other languages are possible *)
  in
  let langOfDriver drv =
      (* FIXME: need to basename the drv *)
      if info.input_language <> None then really info.input_language
      (* need to test for 'clang++' before 'clang' because the latter is a prefix of former *)
      else if None <> matchesPrefix "c++"     drv
           || None <> matchesPrefix "g++"     drv
           || None <> matchesPrefix "clang++" drv
           then "c++"
      else if None <> matchesPrefix "cc"    drv
           || None <> matchesPrefix "gcc"   drv
           || None <> matchesPrefix "clang" drv
           then "c"
      else (* if we're just run as 'cpp', then really the suffix of the input filename
            * should decide, but we don't know which arg is the input file *)
            "c"
  in
  let depsArgs = match info.deps_outputfile with
     None -> []
   | Some(depsFile) -> ["-M"; "-MF"; depsFile]
  in
  match info.driver with
    None -> (match info.language_std with
        None -> failwith "can't run the right cpp without -std=xxx option or a -driver"
      | Some(std) -> "cpp" :: depsArgs, langOfStd (really info.language_std) (* -std= should do the trick *)
    )
  | Some(d) ->
        d :: "-E" :: depsArgs, langOfDriver d (* This -E may be redundant, but leave it for good measure *)


(* chunkedArgs is a list with exactly the same number of entries
 * as the original arg list, but where adjacent options belong together,
 * the earlier ones appear as [] and the completed chunk appears as [arg1; arg2] or whatever.
 * As we go, we snarf various properties that interest us, and we
 * gobble (replace with []) any arg that is private to us, i.e. that the real cpp doesn't grok. *)
let scanAndChunkCppArgs (argList: string list) : string list list * basic_arg_info =
    let minusOPos = ref None in
    let seenStd = ref None in
    let originalOutFile = ref None in
    let driver = ref None in
    let seenMd = ref false in
    let seenMf = ref false in
    let depsOutputFile = ref None in
    let explicitLang = ref None in
    let readingExtraArg = ref None in
    let chunkedArgs = List.mapi (fun i -> fun arg ->
        match arg with
          | "-o" -> (minusOPos := Some(i); readingExtraArg := Some(`ArgNamingOutputFile); [])
          (* PROBLEM: different versions of cpp behave subtly differently,
           * especially w.r.t. their default version.
           * For example, cpp-4.9 defaults to gnu89 mode,
           * whereas cpp-7.2 defaults to gnu99 mode.
           * If we see code like this:
           * #define U blah
           * U"x"
           *
           * ... this will get preprocessed differently (unicode literals in gnu99).
           * This is basically the code's fault, but it can cause surprises
           * e.g. if the client compiler was gcc-4.9, it was expecting to invoke
           * cpp-4.9 where the problem above would not arise. Ideally we would
           * invoke the same version of cpp that the compiler would invoke. To that
           * end, our wrapper scrapes the executable of its parent pid and passes
           * that as a -driver option. Alternatively the user may specify -std=xxx
           * explicitly. If we don't get either of those, we fail early rather than
           * do something subtly wrong. *)
          | "-driver" -> (readingExtraArg := Some(`ArgNamingDriver); [])
          | s when None <> matchesPrefix "-std=" s ->
                (seenStd := matchesPrefix "-std=" s; [s])
          | "-x" -> (readingExtraArg := Some(`ArgNamingLang); [])
          | "-plugin" -> (readingExtraArg := Some(`ArgNamingPlugin); [])
                (* NOTE that the driver adds an extra arg to -MD, and indeed that's the point of
                 * -MD as opposed to -MF. So don't be deceived by the manual pages showing -MD
                 * without an argument: it really does have an argument as far as we're concerned.
                 * We might have -MF in the mix too, though. That means we should output the
                 * dependencies to *both* files, I think.
                 * Final complication: if we have -MD and -E on the command line, it changes
                 * the semantics of -o.
                        -MD -MD is equivalent to -M -MF file, except that -E is not implied.

                          [WHERE:]

                          -M:  Instead of outputting the result of preprocessing, output a rule...
                               ... implies -E ...
                          -MF: When used with -M or -MM, specifies a file to write the
                               dependencies to.  If no -MF switch is given the preprocessor sends
                               the rules to the same place it would have sent preprocessed output.

                        [for -MD:]
                        The driver determines file based on whether an -o option is given.
                        If it is, the driver uses its argument but with a suffix of .d,
                        otherwise it takes the name of the input file, removes any
                        directory components and suffix, and applies a .d suffix.

                 * Since we want to be able to *add* both -E and -o,
                 * with their usual meanings, we must delete -MD and simulate it.
                 * We can ALMOST simulate -MD using -MF because we are always running the preprocessing
                 * step separately, i.e. "implying -E" is always fine with us.
                 * (PROBLEM: -MF seems to disable the preprocessed output! Or at least -M -MF does.)
                 *
                 * We handle this as follows.
                 *
                 * We delete both -MD and -MF and then selectively reinstate them.
                 * We never use -MD for real because we want to re-run the driver -E and -o, and
                 * the combination -MD, -E and -o changes the meaning of -MD. So we should use
                 * multiple -MF options.
                 *
                 * If *both* -MD <file> and -MF <file> are present, which one wins? The
                 * answer is -MF. We don't generate both.
                 *
                 * GAH: our approach of rewriting -MD to -M -MF <file> means that preprocessor output
                 * is suppressed! We get an empty .i file as our output. Do we need to do two runs to
                 * handle cases where the user passed       (no -E) -MD -o <realoutputfile> to the driver?
                 * Maybe not: I can get the behaviour with (tested with GCC 9's cc1)
                 * /path/to/cc1 -quiet -MD foo.d -E foo.c -o foo.i
                 *
                 * So how does that differ from what we actually run?
                 * We start with
                 * cc -MD -E -o foo.i foo.c
                 * ... and rewrite it to
                 * cc -E foo.c -o foo.i -M -MF foo.d
                 * ... which -### reveals becomes a cc1 command
                 * cc1 -E -quiet -M -MF foo.d foo.c -o foo.i
                 * The difference is we do "-M -MF file" whereas what works is "-MD file".
                 * Note that this '-MD file' is specific to cc1; and presumably means
                 * "write deps side effectingly" -- whereas cc and cpp do not take an option to -MD.
                 *
                 * I think the fix is to pass -MD -MF <file> instead of -M -MF <file>
                 * in cases where -M was not passed originally, i.e. where side-effecting output was requested.
                 * We risk triggering the extra behaviour "uses [-o's] argument but with a suffix of .d",
                 * but if we also give -MF it's probably harmless if it just generates the depfile twice
                 * (once in the -MF file, once in the -o) -- CHECK that it does this.
                 *
                 * FIXME: also handle case where the user passed -E -MD -o <depoutputfile>  to the driver.
                 *
                 * vvv- I wrote the below but now I don't see it happening...
                 * NOTE that the use of -MD without -MF is already handled by the driver, which
                 * generates the extra argument to -MF. So CHECK whether the other works too.
                 *)
          | "-MD" -> (seenMd := true; readingExtraArg := Some(`ArgNamingDependencyOutputFile(false)); [])
          | "-MMD" -> (seenMd := true; readingExtraArg := Some(`ArgNamingDependencyOutputFile(false)); [])
          | "-MF" -> (seenMf := true; readingExtraArg := Some(`ArgNamingDependencyOutputFile(true)); [])
          | "-MMF" -> (seenMf := true; readingExtraArg := Some(`ArgNamingDependencyOutputFile(true)); [])
          | _ -> (
            let wasReadingExtraArg = !readingExtraArg in
            readingExtraArg := None;
            match wasReadingExtraArg with
                None -> [arg]
              | Some(`ArgNamingDriver) -> driver := Some(arg); (* -driver is our fake option *) []
              | Some(`ArgNamingOutputFile) -> originalOutFile := Some(arg); ["-o"; arg]
              | Some(`ArgNamingLang) -> explicitLang := Some(arg); ["-x"; arg]
              | Some(`ArgNamingDependencyOutputFile(nameIsExplicitlyRequested)) -> (
                    if !depsOutputFile = None || nameIsExplicitlyRequested
                    then depsOutputFile := Some(arg) else (); [])
           )
        ) argList
    in (chunkedArgs, {
      minus_o_pos     = !minusOPos;
      output_file     = !originalOutFile;
      deps_outputfile = !depsOutputFile;
      driver          = !driver;
      language_std    = !seenStd;
      input_language  = !explicitLang
    })


(* FIXME: this function should be unwrapped into arg-processing and cpp-invoking parts.
 * We really do a few separate things:
 * - filter out cilpp arguments (-save-temps, -driver, -plugin, -pass)
 * - chunk arguments according to meaning (... "-o", file... -> ... ["-o", file] ... 
 * - rewrite them as needed for our redirection and simulation of -MD
 *      (simulation of -MD is a side-effect of always passing -o,
 *      which changes the meaning of -MD -- see below)
 *
 * Probably the right factoring is functions which do the following
 *  - scan and chunk args, outputting a record of 'interesting information'
 *  - filter out cilpp's arguments  (this one can go in cilpp... or does our "-driver" hack
 *          generalise beyond cilpp? yes -- it's how we know how to invoke the preprocessor;
 *          any preprocessor tap or filter needs this)
 *  - rewrite to divert to a temporary file
 *)


let runCommand cmdFriendlyName argvList =
    (*
    let _ =
    output_string Pervasives.stderr ("About to execute cpp: " ^
        (List.fold_left (fun s -> fun arg -> (if s = "" then s else s ^ " ") ^ arg) "" argvList)
        ^ "\n")
    in
    *)
    (* FIXME: we have left the fd open *)
    match fork () with
        | 0 -> (try execvp (List.hd argvList) (Array.of_list argvList)
            with Unix_error(err, _, _) ->
                output_string Pervasives.stderr ("cannot exec " ^ cmdFriendlyName ^ ": " ^ (error_message err) ^ "\n");
                exit 255
          )
        | childPid ->
            let pid, status = wait () in
            match status with
                | WEXITED 255 -> ()
                | WEXITED 0 -> ()
                | WEXITED status ->
                    failwith (cmdFriendlyName ^ " exited with nonzero code")
                | WSIGNALED signal ->
                    failwith (cmdFriendlyName ^ " killed by signal")
                | WSTOPPED signal ->
                    failwith (cmdFriendlyName ^ " stopped")

