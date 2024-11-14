open Compiler_args
open Unix
open Feature

external mkstemp: string -> Unix.file_descr * string = "caml_mkstemp"
external mkstemps: string -> int -> Unix.file_descr * string = "caml_mkstemps"

let runCppDivertingToTempFile maybeSuffix argChunks basicInfo =
    let saveTemps = ref false in
    let ppPluginsToLoadReverse = ref [] in
    let ppPassesToRunReverse = ref [] in
    let realCpp = ref None in
    let readingExtraArg = ref None in
    (* chunkedArgs is a list with exactly the same number of entries
     * as the original arg list, but where adjacent options belong together,
     * the earlier ones appear as [] and the completed chunk appears as [arg1; arg2] or whatever.
     * As we go, we snarf various properties that interest us, and we
     * gobble (replace with []) any arg that is private to us, i.e. that the real cpp doesn't grok.
     * This is really the only crucial argument processing that we need to do here:
     * pull out "-save-temps", "-plugin" and "-fpass-*". *)
    let reChunkedArgs = List.mapi (fun i -> fun argChunk ->
        match argChunk with
          | ["-save-temps"] -> saveTemps := true; [] (* i.e. accept -Wp,-save-temps; compiler doesn't grok it*)
          | ["-realcpp"] -> (readingExtraArg := Some(`ArgNamingRealCPP); [])
          | ["-plugin"] -> (readingExtraArg := Some(`ArgNamingPlugin); [])
          | [s] when None <> matchesPrefix "-fpass-" s ->
                let passName = really (matchesPrefix "-fpass-" s) in
                ppPassesToRunReverse := passName :: !ppPassesToRunReverse; []
          | [] -> []
          | [arg] -> (
            let wasReadingExtraArg = !readingExtraArg in
            readingExtraArg := None;
            match wasReadingExtraArg with
                None -> argChunk (* i.e. no-op *)
              | Some(`ArgNamingPlugin) -> ppPluginsToLoadReverse := arg :: !ppPluginsToLoadReverse; []
              | Some(`ArgNamingRealCPP) -> realCpp := Some(arg); []
           )
          | _ -> ( (* This case matches non-singleton lists i.e. already-formed chunks *)
            let wasReadingExtraArg = !readingExtraArg in
                readingExtraArg := None;
                if None <> wasReadingExtraArg then
                (* This means we are trying to form a chunk, given the preceding argument,
                 * but instead we saw something already chunked-up. Flag an error. *)
                failwith ""
                else argChunk
            )
    ) argChunks
    in
    (* What we don't do is identify which arguments denote input files.
     * We'd like to guess the right suffix for a temporary file, but this
     * is challenging. Luckily we insist on either -driver or -std=, and
     * these are enough -- but don't forget -x lang if we saw it. FIXME:
     * testing for driver names is really gross. Can we really not find
     * the input filename? Or at least make a guess and use it instead
     * of hardcoded "c" below? *)
    let cppCommandPrefix, guessedLang = guessCppCommandAndLang basicInfo !realCpp in
    let suffixOfLang l = match l with
        "c++" -> "ii"
      | "c" -> "i" (* FIXME: other languages are possible *)
      | _ -> failwith (l ^ " is not a language")
    in
    let (newTempFd, newTempName) =
        let suffix = if maybeSuffix <> None then really maybeSuffix else suffixOfLang guessedLang
        in mkstemps ("/tmp/tmp.XXXXXX.cpp." ^ suffix) (String.length ".cpp." + String.length suffix)
    in
    let rewrittenArgs = List.flatten (List.mapi (fun i -> fun argChunk ->
        if i = 0 then [] (* we fill "cpp" or whatever from cppCommandPrefix *) else
        match argChunk with
          | ["-o"; filename] ->  ["-o"; newTempName]
          | _ -> argChunk) reChunkedArgs)
      @ ( (* we might not have seen "-o" -- ensure there is a -o argument *)
      match basicInfo.minus_o_pos with
        None -> (* there was no -o, so add one *) [ "-o"; newTempName ]
      | _ -> [])
    in
    runCommand (* 'cpp' here is used only in error messages... *) "cpp" (cppCommandPrefix @ rewrittenArgs);
    (newTempName, basicInfo.output_file, !saveTemps, List.rev !ppPluginsToLoadReverse, List.rev !ppPassesToRunReverse)

let () =
    let argList = Array.to_list Sys.argv in
    let (argChunks, basicInfo) = scanAndChunkCppArgs argList in
    if basicInfo.suppress_ppout then
        (* the command doesn't generate any preprocessed output, so we have nothing
         * to do... just run the original command. This should arguably get filtered
         * out in the wrapper scripts, so that cilpp does not have to handle it,
         * i.e. an extension of just handling vanilla "cc -E" or "cpp" invocations. *)
         runCommand "cpp" (* <-- only used in error messages *) argList
    else
    let (newTempName, originalOutfile, saveTemps, ppPluginsToLoad, ppPassesToRun)
     = runCppDivertingToTempFile (Some "i") argChunks basicInfo in
    (* Okay, run CIL; we need the post-preprocessing line directive style *)
    Cil.lineDirectiveStyle := Some Cil.LinePreprocessorOutput;
    (* We have to use logical operators to avoid breaking code that does -Werror=format-string
     * ... this involves an expression-level check of the first argument to printf, which
     * might be a conditional expression. So we can't substitute it with a temporary assigned
     * in an if/else construct. *)
    Cil.useLogicalOperators := true;
    let initialCilFile = Frontc.parse newTempName () in
    let currentCilFile = initialCilFile in
    Cil.printerForMaincil := Cil.plainCilPrinter;
    (* We are not printing for CIL input *)
    Cil.print_CIL_Input := false;
    let (chan, str) = match originalOutfile with
            None -> Pervasives.stdout, "(stdout)"
          | Some(fname) -> (Pervasives.open_out fname, fname)
    in
    let _ = Cil.dumpFile Cil.plainCilPrinter chan str currentCilFile
    in
    let status = if !Errormsg.hadErrors then 1 else 0 in
    (* delete temporary file unless -save-temps *)
    (if saveTemps then () else Unix.unlink newTempName;
    exit status)
