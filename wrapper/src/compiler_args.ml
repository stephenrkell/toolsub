(* compiler_args.ml: -- a simple bit of compiler wrapping, intended
 * for custom preprocessors (cilpp, cccppp).
 *
 * This file *only* knows about the C preprocessor and its command line.
 * It is *NOT* concerned with cc or cc1 commands -- except that it will
 * sometimes generate a command line for them, one as a way to run the
 * "correct" "real" C preprocessor.
 *)


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
    suppress_ppout  : bool;
    driver          : string option;
    language_std    : string option;
    input_language  : string option
};;

(* Given a cpp invocation that has already been scanned using scanAndChunkCppArgs,
 * figure out how to run the "real" C preprocessor it requires.
 *
 * It is surprisingly tricky to figure out which 'cpp' to run and with which
 * arguments.
 *
 * We don't always run a 'cpp' at all' we sometimes instead use "$driver -E". This
 * may have unwanted consequences: according to the GCC manual, in a driver
 * command, "-MD" changes the semantics of "-o" if "-E" is also present.
 *
           If -MD is used in conjunction with -E, any -o switch is
           understood to specify the dependency output file, but if used
           without -E, each -o is understood to specify a target object
           file.
 *
 * So that would mean that
 * if "-MD" and "-E" is present, "-o" is equivalent to "-MF", whereas
 * if just "-MD" is present, "-o" names an output file of compilation.
 * My attempts to validate this have failed, however.
 * $ rm -f hello.o hello.i hello.d && cc -E -MD -o hello.o hello.c
 * $ file hello.o
 * hello.o: C source, ASCII text
 * $ rm -f hello.o hello.i hello.d && cc -MD -o hello.o hello.c
 * $ file hello.o
 * hello.o: hello.o: ELF 64-bit LSB pie executable...
 *
 * ... so I think reality is simpler.
 *
 * Ideally we would output a "driver-safe" version of the invocation, i.e. one
 * where "-o" never appears together with "-MD". We can just do "-MD" "-MF" with
 * the driver to set explicitly the dependency output filename. And since the
 * "-o means dependency file" semantics doesn't seem to happen, I think we are OK.
 *)
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
   | Some(depsFile) -> ["-MD"; "-MF"; depsFile]
   (* FIXME: this forces the "side-effecting" behaviour of -MD. But it's possible
    * to run cpp with just -M or -MM, and send the depfile to stdout or wherever -o says.
    * In such cases, we are not really preprocessing at all. We should filter these out
    * before we get to here: if we're 'cpp' it's when we see '-M' or '-MM'. cilpp now
    * does this, but perhaps it should be in the wrapper scripts. *)
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
    let seenMf = ref false in
    let seenMOrMm = ref None in
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
          | "-M" -> (seenMOrMm := Some("-M"); [arg])
          | "-MM" -> (seenMOrMm := Some("-MM"); [arg])
          | "-MF" -> (seenMf := true; readingExtraArg := Some(`ArgNamingDependencyOutputFile(true)); [])
          (* there is no -MMF *)
          | _ -> (
            let wasReadingExtraArg = !readingExtraArg in
            readingExtraArg := None;
            match wasReadingExtraArg with
                None -> [arg]
              | Some(`ArgNamingDriver) -> driver := Some(arg); (* -driver is our fake option *) []
              | Some(`ArgNamingOutputFile) -> originalOutFile := Some(arg); ["-o"; arg]
              | Some(`ArgNamingLang) -> explicitLang := Some(arg); ["-x"; arg]
              | Some(`ArgNamingDependencyOutputFile(nameIsExplicitlyRequested)) -> (
                    if !depsOutputFile = None || nameIsExplicitlyRequested (* always true, currently *)
                    then depsOutputFile := Some(arg) else (); [])
              (* pattern-match failure here means we created a bad Some(...)
               * maybe due to using a polymorphic variant belonging to a 'subclass'? *)
           )
        ) argList
    in (chunkedArgs, {
      minus_o_pos     = !minusOPos;
      output_file     = !originalOutFile;
      deps_outputfile = !depsOutputFile;
      suppress_ppout  = None <> !seenMOrMm;
      driver          = !driver;
      language_std    = !seenStd;
      input_language  = !explicitLang
    })

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

