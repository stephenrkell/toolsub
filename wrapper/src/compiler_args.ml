(* compiler_args.ml: -- a simple bit of compiler wrapping, intended
 * for custom preprocessors (cilpp, cccppp). *)


open Printf
open Unix
module D = Dynlink
external mkstemp: string -> Unix.file_descr * string = "caml_mkstemp"
external mkstemps: string -> int -> Unix.file_descr * string = "caml_mkstemps"

type extra_arg =
    OutputFile
  | Plugin
  | Driver
  | DependencyOutputFile of bool (* nameIsExplicitlyRequested *)

(* FIXME: this function should be unwrapped into arg-processing and cpp-invoking parts. *)
let parseArgsAndRunCppDivertingToTempFile suffix =
    let minusOPos = ref None in
    let saveTemps = ref false in
    let seenStd = ref None in
    let readingExtraArg = ref None in
    let originalOutfile = ref None in
    let driver = ref None in
    let seenMd = ref false in
    let seenMf = ref false in
    let depsOutputFile = ref None in
    let ppPluginsToLoadReverse = ref [] in
    let ppPassesToRunReverse = ref [] in
    let (newTempFd, newTempName) = mkstemps (String.concat "" ["/tmp/tmp.XXXXXX.cpp."; suffix]) (5 + String.length suffix) in
    let rewrittenArgs = List.flatten (List.mapi (fun i -> fun arg ->
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
        if i = 0 then [] (* we fill "cpp" or whatever later *) else
        match arg with
          | "-o" -> (minusOPos := Some(i); readingExtraArg := Some(OutputFile); [arg])
          | "-save-temps" -> saveTemps := true; [] (* i.e. accept -Wp,-save-temps; compiler doesn't grok it*)
          | "-driver" -> (readingExtraArg := Some(Driver); [])
          | "-plugin" -> (readingExtraArg := Some(Plugin); [])
                (* NOTE that the driver adds an extra arg to -MD, and indeed that's the point of
                 * -MD as opposed to -MF. So don't be deceived by the manual pages showing -MD
                 * without an argument: it really does have an argument as far as we're concerned.
                 * We might have -MF in the mix too, though. That means we should output the
                 * dependencies to *both* files, I think.
                 * Final complication: if we have -MD and -E on the command line, it changes
                 * the semantics of -o. Since we want to be able to *add* both -E and -o,
                 * with their usual meanings, we must delete -MD and simulate it.
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
                 * FIXME: handle case where the user passed -E -MD -o <depoutputfile> to the driver.
                 * NOTE that the use of -MD without -MF is already handled by the driver, which
                 * generates the extra argument to -MF. So CHECK whether the other works too. *)
          | "-MD" -> (seenMd := true; readingExtraArg := Some(DependencyOutputFile(false)); [])
          | "-MMD" -> (seenMd := true; readingExtraArg := Some(DependencyOutputFile(false)); [])
          | "-MF" -> (seenMf := true; readingExtraArg := Some(DependencyOutputFile(true)); [])
          | "-MMF" -> (seenMf := true; readingExtraArg := Some(DependencyOutputFile(true)); [])
          | s when String.length s > String.length "-fpass-"
                && String.sub s 0 (String.length "-fpass-") = "-fpass-" ->
                let passName = String.sub s (String.length "-fpass-") (String.length s - String.length "-fpass-")
                in ppPassesToRunReverse := passName :: !ppPassesToRunReverse; []
          | s when String.length s > String.length "-std="
                && String.sub s 0 (String.length "-std=") = "-std=" ->
                (seenStd := Some(s); [s])
          | _ -> (
            let replacement = match !readingExtraArg with
                None -> [arg]
              | Some(Driver) -> driver := Some(arg); []
              | Some(OutputFile) -> originalOutfile := Some(arg); [newTempName]
              | Some(Plugin) -> ppPluginsToLoadReverse := arg :: !ppPluginsToLoadReverse; []
              | Some(DependencyOutputFile(nameIsExplicitlyRequested)) -> (
                    if !depsOutputFile = None || nameIsExplicitlyRequested
                    then depsOutputFile := Some(arg) else (); [])
            in
            readingExtraArg := None; replacement
          )
        ) (Array.to_list Sys.argv))
    in
    let cppCommandPrefix =  match !driver with
        None -> (match !seenStd with
            None -> failwith "can't run the right cpp without -std=xxx option or a -driver"
          | Some(_) -> ["cpp"] (* -std= should do the trick *)
        )
      | Some(d) ->
            d :: ["-E"] (* This -E may be redundant, but leave it for good measure *)
    in
    let depsArgs = match !depsOutputFile with
            None -> []
          | Some(depsFile) -> ["-M"; "-MF"; depsFile]
    in
    let outArgs = match !minusOPos with
        None -> (* there was no -o, so add one *) [ "-o"; newTempName ]
      | _ -> []
    in
    let allArgs = cppCommandPrefix @ rewrittenArgs @ outArgs @ depsArgs
    in
    (*
    let _ =
    output_string Pervasives.stderr ("About to execute cpp: " ^
        (List.fold_left (fun s -> fun arg -> (if s = "" then s else s ^ ", ") ^ arg) "" allArgs)
        ^ "\n")
    in
    *)
    (* FIXME: we have left the fd open *)
    (match fork () with
        | 0 -> (try execvp (List.hd allArgs) (Array.of_list allArgs)
            with Unix_error(err, _, _) ->
                output_string Pervasives.stderr ("cannot exec cpp: " ^ (error_message err) ^ "\n");
                exit 255
          )
        | childPid ->
            let pid, status = wait () in
            match status with
                | WEXITED 255 -> ()
                | WEXITED 0 -> ()
                | WEXITED status ->
                    failwith "cpp exited with nonzero code";
                | WSIGNALED signal ->
                    failwith "cpp killed by signal";
                | WSTOPPED signal ->
                    failwith "cpp stopped";
    );
    (newTempName, originalOutfile, !saveTemps, List.rev !ppPluginsToLoadReverse, List.rev !ppPassesToRunReverse)

