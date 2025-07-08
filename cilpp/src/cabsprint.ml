open Unix
open Cilpp_common

let () = 
        let _ = prepareCilFile ~printOnlyCABS:true Sys.argv in
        let status = if !Errormsg.hadErrors then 1 else 0 in
        exit status
