open Unix
open Cilpp_common

let () =
    let (currentCilFile, outputFile) = prepareCilFile Sys.argv in
    Cil.printerForMaincil := Cil.defaultCilPrinter;
    (* We are not printing for CIL input *)
    Cil.print_CIL_Input := false;
    let (chan, str) = match !outputFile with
            None -> Pervasives.stdout, "(stdout)"
          | Some(fname) -> (Pervasives.open_out fname, fname)
    in
    let _ = Cil.dumpFile Cil.plainCilPrinter chan str currentCilFile
    in
    let status = if !Errormsg.hadErrors then 1 else 0 in
    exit status
