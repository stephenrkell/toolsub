(* cccppp.ml -- a simple CIL driver that replaces the C preprocessor.
 *
 * Copyright 2019   Stephen Kell <S.R.Kell@kent.ac.uk>
 *)
 
(* First we preprocess into a temporary file;
 * we pass through to cpp all our arguments except for any following "-o".
 * Then we run cccppp and output to the intended -o file.
 *)
open Compiler_args
open Unix

let () =
    let (newTempName, originalOutfile, saveTemps, ppPluginsToLoad, ppPassesToRun)
    = parseArgsAndRunCppDivertingToTempFile "ii" in
    let infd = openfile newTempName [O_RDONLY] 0o640 in
    (* delete temporary file unless -save-temps *)
    (*let () = if saveTemps then () else unlink newTempName in *)
    (* dup2 our stdout to the original out file, and our stdin to the temp *)
    let () = match !originalOutfile with
        Some(fname) -> let outfd = Unix.openfile fname [O_RDWR; O_CREAT] 0o640 in
            ((output_string Pervasives.stderr ("output should go to " ^ fname ^ "\n"); Pervasives.flush Pervasives.stderr);
            dup2 outfd stdout)
          | None -> (
            output_string Pervasives.stderr ("output should go to stdout \n"); Pervasives.flush Pervasives.stderr;
            ()
            )
    in
    let () = dup2 infd stdin in
    let toolPath = if Filename.is_relative Sys.argv.(0) (* TODO: perhaps try Sys.executable_name ? *)
        then Filename.concat Filename.current_dir_name "cccppp-tool"
        else Filename.concat (Filename.dirname Sys.executable_name) "cccppp-tool"
    in
    (* FIXME: pass "/dev/stdin" if we can figure out how to tell clang it's C++ *)
    execv toolPath [|toolPath; newTempName ; "--"|]
