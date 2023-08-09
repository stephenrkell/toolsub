(* cccppp.ml -- a simple CIL driver that replaces the C preprocessor.
 *
 * Stephen Kell <stephen.kell@kcl.ac.uk>
 * Copyright 2019 University of Kent
 * Copyright 2023 King's College London
 *)
 
(* First we preprocess into a temporary file;
 * we pass through to cpp all our arguments except for any following "-o".
 * Then we run cccppp and output to the intended -o file.
 *)
open Compiler_args
open Unix

let () =
    let argList = Array.to_list Sys.argv in
    let (argChunks, basicInfo) = scanAndChunkCppArgs argList in
    let (newTempFd, newTempName) = mkstemps ("/tmp/tmp.XXXXXX.cpp.ii") (String.length ".cpp.ii") in
    let rewrittenArgs = List.flatten (List.mapi (fun i -> fun argChunk ->
        if i = 0 then [] (* we fill "cpp" or whatever from cppCommandPrefix *) else
        match argChunk with
          | ["-o"; filename] -> ["-o"; newTempName]
          | _ -> argChunk) argChunks)
      @ ( (* we might not have seen "-o" -- ensure there is a -o argument *)
      match basicInfo.minus_o_pos with
        None -> (* there was no -o, so add one *) [ "-o"; newTempName ]
      | _ -> [])
    in
    let cppCommandPrefix, guessedLang = guessCppCommandAndLang basicInfo in
    runCommand "cpp" (cppCommandPrefix @ rewrittenArgs);
    let infd = openfile newTempName [O_RDONLY] 0o640 in
    (* delete temporary file unless -save-temps *)
    (*let () = if saveTemps then () else unlink newTempName in *)
    (* dup2 our stdout to the original out file, and our stdin to the temp *)
    (* First we copy the prelude to the output. If we don't have a prelude
     * argument (FIXME: look for one in ppPassesToRun) we use the default. *)
    let () = match basicInfo.output_file with
        Some(fname) -> let outfd = Unix.openfile fname [O_RDWR; O_CREAT] 0o640 in
            ((output_string Pervasives.stderr ("output should go to " ^ fname ^ "\n"); Pervasives.flush Pervasives.stderr);
            dup2 outfd stdout)
          | None -> (
            output_string Pervasives.stderr ("output should go to stdout \n"); Pervasives.flush Pervasives.stderr;
            ()
            )
    in
    let () = dup2 infd stdin in
    (* We are run as 'cccppp' and we want to find and run 'cccppp-tool'.
     * Here 'is_relative' is also handling the case where we were run via $PATH
     * and so the argv[0] is just a command name. I THINK. *)
    let ourCommandName = Filename.basename (Sys.executable_name) in
    let toolPath = if Filename.is_relative Sys.argv.(0) (* TODO: perhaps try Sys.executable_name ? *)
        then Filename.concat Filename.current_dir_name (ourCommandName ^ "-tool")
        else Filename.concat (Filename.dirname Sys.executable_name) (ourCommandName ^ "-tool")
    in
    let preludePath = List.fold_left Filename.concat (Filename.dirname toolPath)
        [".."; "include"; "prelude.hpp"]
    in
    (* print the line marker for the prelude *)
    (output_string Pervasives.stdout ("# 1 \"" ^ preludePath ^ "\"\n"); flush Pervasives.stdout;
    (* FIXME: manage quoting properly -- probably by avoiding Unix.system *)
    Unix.system ("cat '" ^ preludePath ^ "'");
    (* FIXME: pass "/dev/stdin" if we can figure out how to tell clang it's C++ *)
    execv toolPath [|toolPath; newTempName ; "--"|]
    )
