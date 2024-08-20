(* cilpp -- a simple CIL driver that replaces the C preprocessor.
 *
 * Copyright 2018--19   Stephen Kell <stephen.kell@cl.cam.ac.uk>
 *   and embodying parts of CIL's main.ml, which is
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
 
(* First we preprocess into a temporary file;
 * we pass through to cpp all our arguments except for any following "-o".
 * Then we run CIL and output to the intended -o file.
 *
 * We have an identity crisis. Do we emulate 'cpp' or 'cc -E'?
 * Although the former sounds right, the latter is better for wrapper scripts
 * in many cases, because they may not know which 'cpp' command to run, but
 * can usually figure out the driver (albeit from parent-PID hackery
 * or from an explicit -driver option or CC_DRIVER variable or...).
 * It may be significant also that GNU 'make' defaults CPP to '$(CC) -E'.
 * However, guessing the driver is always a hack too (using parent PID).
 *
 * Can we "do both"? In short, yes. We have to pick a thing to run and run it, to do the
 * preprocessing. But do we need to know *how* to invoke it? Not really. The caller tells us,
 * by the arguments they pass! Whereas we used to accept -driver, now we accept -real-cpp.
 * It can be "cpp" or "$driver -E" or anything else.
 *
 * Our wrapper script will always use "$driver -E" as the -real-cpp, but as cilpp we don't
 * care. We do need to get our own options; to be safe we stop scanning at the first option
 * we don't understand (or '--'). But we can also use the CC_IDENTIFY_ARGS feature of the
 * wrapper script: if it sets CC_IDENTIFIED_ARGS, it can tell us where to find our args at 
 * any point in the command line.
 *)
open Compiler_args
open Unix
open Feature

let debug_level : int = int_of_string (Sys.getenv "DEBUG_CC")

let debug_println level str = if level > debug_level then ()
    else (output_string Pervasives.stderr (str ^ "\n"); flush Pervasives.stderr)
    
(* Test whether a string matches a prefix... but instead of returning a
 * boolean, return None if it doesn't and Some(suffix) if it does, i.e.
 * returning the part of the string that follows the prefix. *)
let matchesPrefix (prefix: string) (s: string) : string option =
    if (String.length s > String.length prefix
       && String.sub s 0 (String.length prefix) = prefix)
    then Some(Str.string_after s (String.length prefix))
    else None

let matchesSuffix (suffix: string) (s: string) : string option =
    if (String.length s > String.length suffix
       && String.sub s (String.length s - String.length suffix) (String.length suffix) = suffix)
    then Some(String.sub s 0 (String.length s - String.length suffix))
    else None

(* Convenience for forcing an option *)
let really = function Some(optVal) -> optVal | None -> failwith "really None"

(* FIXME: replicate shell semantics more properly *)
let wordSplit str = String.split_on_char ' ' str

let runCommand cmdFriendlyName argvList =
    (*
    let _ =
    output_string Pervasives.stderr ("About to execute cpp: " ^
        (List.fold_left (fun s -> fun arg -> (if s = "" then s else s ^ " ") ^ arg) "" argvList)
        ^ "\n")
    in
    *)
    (* FIXME: have we left the temporary fd open? caller needs to handle this *)
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

(* These are our SARGs (options taking separate arguments) *)
type cilpp_extra_arg = [
    `ArgNamingOutputFile
  | `ArgNamingPlugin
  | `ArgNamingRealCPP
  ]
(* 
 *)
let () =
    let argArr = Sys.argv in
    let argList = Array.to_list argArr in
    (* We used to scan our own args. We no longer want to do this. How to get around
     * the "identity crisis" (cpp versus cc -E) and our desire to accept "new" options
     * like "-plugin" and "-real-cpp"? We can get the wrapper script to scan our
     * arguments. We can implement a '--' option, since it is not a valid option to
     * cpp/cc. We should stop our scan on '--' or the first unrecognised arg. For
     * invocations without '--' or where our options appear to its right, we require
     * our invoker to have done the scanning and tell us via CC_IDENTIFIED_ARGS. In turn,
     * we (or rather our invoker's invoker) supply CC_IDENTIFY_ARGS using the JARG/SARG
     * syntax we use in the wrapper script. That script will do the scanning and set
     * CC_IDENTIFIED_ARGS accordingly, to something like "3,4-o 5-DBLAH=foo".
     *
     * How does the "invoker's invoker" thing work? I was imagining a 'cilpp-cflags'
     * program that can take care of this.
     *
     * If we are being run as the driver, then our identified args may be prefixed with
     * -Xpreprocessor, but the numbered ranges above will not include this (even if there
     * is an -Xpreprocessor in the middle of a separate-arg option.
     *
     * This creates a problem: is "-real-cpp" cc-like or cpp-like? In the former case,
     * we will have to keep the -Xpreprocessor, but in the latter case, we will not.
     * This rather defeats our attempt at saying "it's for the caller to give us
     * arguments that are well-matched with -real-cpp". EXCEPT IT DOESN'T: it's perfect!
     * The wrapper script *does* give us cc-like arguments, so it needs to give us a
     * cc-like real-cpp. So we never need to drop -Xpreprocessor ourselves. The only
     * trick is that our arguments, like -plugin, might be confounded by it. So we
     * always drop "-Xpreprocessor" when reading an extra arg. *)
    let finished = ref false in
    let saveTemps = ref false in
    let outputFile = ref None in
    let ppPluginsToLoadReverse = ref [] in
    let ppPassesToRunReverse = ref [] in
    let realCpp = ref None in
    let readingExtraArg : cilpp_extra_arg option ref = ref None in
    let stripPositions = ref [0] (* always strip argv[0] *) in
    let isXGuarded i = i>0 && argArr.(i-1) = "-Xpreprocessor" in
    let processArgAt i ignored =
        let arg = argArr.(i) in
        let stripThisOne = fun () -> if isXGuarded i then stripPositions := i-1 :: i :: !stripPositions
                                                     else stripPositions :=        i :: !stripPositions;
                                      debug_println 1 ("After stripThisOne, strip list now has " ^ (string_of_int (List.length !stripPositions)) ^ " entries");
                                      () in
        let stripNextOne = fun () -> let _ = debug_println 1 ("Stripping next from " ^ (string_of_int i)) in
                                     (
                                        if isXGuarded i then (* we expect the *next* arg to be "-Xpreprocessor" *)
                                            (if argArr.(i+1) <> "-Xpreprocessor" then failwith ("inconsistent -Xpreprocessor guarding of " ^ argArr.(i) ^ " but not its argument")
                                            else stripPositions := (i+2) :: (i+1) :: !stripPositions)
                                        else stripPositions := (i+1) :: !stripPositions
                                     ); debug_println 1 ("Strip list now has " ^ (string_of_int (List.length !stripPositions)) ^ " entries"); ()
        in
        match arg with
        | _  when !finished -> ()
        | _  when i=0 -> ()
        | "-o" -> (readingExtraArg := Some(`ArgNamingOutputFile);
                   stripThisOne (); stripNextOne(); ())
        | "--output" -> (readingExtraArg := Some(`ArgNamingOutputFile);
                   stripThisOne (); stripNextOne(); ())
        | s when None <> matchesPrefix "--output=" s ->
              let outputFileName = really (matchesPrefix "--output=" s) in
              outputFile := Some(outputFileName); stripThisOne (); ()
            (* PROBLEM: how to distinguish a -save-temps directed at us
             * versus a -save-temps directed at the real cpp? i.e. do we stripThisOne () ?
             * Answer: actually it doesn't matter. A plain preprocess-only driver invocation
             * will not generate any temps, so we are fine to strip it unconditionally i.e.
             * assume the -save-temps we see only applies to us. *)
        | "-save-temps" -> saveTemps := true; stripThisOne (); () (* i.e. accept -Wp,-save-temps *)
            (* ... but in general there may be a problem here nayway. We surely can't assume our
             * args are not understood by the underlying tool.
             * FIXME: this seems like a problem, for any option that could apply to both
             * independently. We're perhaps a bit lucky that -save-temps is a degenerate case? *)

            (* How does "-real-cpp" get set in a "normal" use case?
             * If run via our wrapper mode (see argv check above),
             * we will run WRAPPER which will run us, i.e. the compiler
             * driver is somewhere above us in the process tree. But who
             * scrapes its options? Well, whoever is setting
             *     CPP=us
             * should really set
             *     CPP="us -real-cpp='/path/to/compiler/driver -E'"
             * ... I think? *)
        | "-real-cpp" -> (readingExtraArg := Some(`ArgNamingRealCPP);
                          stripThisOne (); stripNextOne(); ())
        | "-plugin" -> (readingExtraArg := Some(`ArgNamingPlugin);
                        stripThisOne (); stripNextOne (); ())
        | s when None <> matchesPrefix "-fpass-" s ->
              let passName = really (matchesPrefix "-fpass-" s) in
              ppPassesToRunReverse := passName :: !ppPassesToRunReverse;
              stripThisOne (); ()
        | "--" -> debug_println 1 ("Gave up processing args (saw --) at position " ^ (string_of_int i));
                  finished := true; stripThisOne (); () (* Explicit end of our args, so terminate *)
        | "-Xpreprocessor" when None = !readingExtraArg || isXGuarded (i-1) ->
            (* Handling of -Xpreprocessor is subtle. We want to keep, i.e. pass to the real cpp,
             * an -Xpreprocessor that is *not* guarding one of *our* private options,
             * but strip one that *is* guarding one of our options.
             * We handle this in the stripThisOne / stripNextOne logic.
             * We always skip over -Xpreprocessor in the main loop. *)
            ()
        | arg ->
              if isXGuarded (i-1) then
                if arg <> "-Xpreprocessor" then failwith "inconsistent guarding mumble"
                else (* this is -Xpreprocessor and we've already stripped it if we should do -- go round again *) ()
              else
              (   let wasReadingExtraArg = !readingExtraArg in
                  readingExtraArg := None;
                  match wasReadingExtraArg with
                      None -> (
                        debug_println 1 ("Gave up processing args (unrecog'n) at position " ^ (string_of_int i)
                            ^ " (`" ^ arg ^ "')");
                        finished := true;
                        ()
                      ) (* we don't recognise this opt, and it's not an SARG, so terminate *)
                    | Some(`ArgNamingPlugin) -> ppPluginsToLoadReverse := arg :: !ppPluginsToLoadReverse; ()
                    | Some(`ArgNamingRealCPP) -> realCpp := Some(arg); ()
                    | Some(`ArgNamingOutputFile) -> outputFile := Some(arg); ()
              )
    in
    List.iteri processArgAt argList
    ;
    debug_println 1 ("CC_IDENTIFIED_ARGS is " ^ (try Sys.getenv "CC_IDENTIFIED_ARGS" with Not_found -> "(unset)"));
    (* Now process our CC_IDENTIFIED_ARGS env var, possibly overriding the above.
     * The env var should take precedence because there's a certain partialness
     * in the above approach to scanning: we give up when we see something we don't
     * recognise. If the caller has set options in our env var
     * specially, always use that because the wrapper knows these. *)
    let identified = Sys.getenv "CC_IDENTIFIED_ARGS" in
    let identifiedEnts = wordSplit identified in
    let identifiedPairs : (int * string) option list = List.map
        (fun s -> let iStr = Str.global_replace (Str.regexp "^\\([0-9]+\\).*$") "\\1" s in
            try Some(int_of_string iStr, s)
            with Failure _ -> (debug_println 1 ("could not grok CC_IDENTIFIED_ARGS token: " ^ s ^ " (" ^ iStr ^ ")"); None)
        )
        identifiedEnts
        (* just snarf the numeric chars, and just use the index they denote
         * FIXME: second item in pair is bogus, but also, is ignored by processArgAt... clean this up. *)
    in
    (List.iter (fun x -> finished := false; match x with None -> () | Some(i, arg) ->
        debug_println 1 ("Got identified index " ^ (string_of_int i)); processArgAt i arg) identifiedPairs)
    ;
    (* Can we run the real cpp now? We need a new tmpfile for the output. This should
     * have the same suffix as our output file. If we're run from the driver we should
     * always have a named output file... only '-' is a problem. *)
    let maybeSuffix = match !outputFile with
        None -> None
      | Some(outputFileName) ->
             (* same as output file's; else assume '.i'? this is a bit nasty *)
             match String.rindex_opt outputFileName '.' with
                 None -> (* hmm, no suffix *) None
               | Some(n) -> Some(String.sub outputFileName n ((String.length outputFileName) - n))
    in
    let (newTempFd, newTempName) =
        let suffix = if maybeSuffix <> None then really maybeSuffix else ".i"
        in mkstemps ("/tmp/tmp.XXXXXX.cilpp" ^ suffix) (String.length ".cilpp" + String.length suffix)
    in
    (* Rewrite args. This will strip any arguments that only we understand,
     * and drop '-o' if it existed. *)
    let _ = debug_println 1 ("Stripping " ^ (string_of_int (List.length !stripPositions)) ^ " from an arglist of length " ^ (string_of_int (List.length argList))) in
    let strippedArgs = List.flatten (List.mapi (fun i -> fun arg ->
        if List.mem i !stripPositions then
         let _ = debug_println 1 ("Decided to consume+strip arg at position " ^ (string_of_int i) ^ " (`" ^ argArr.(i) ^ "')") in
         [] 
        else [arg]
    ) argList)
    in
    let newArgs =
        (match !realCpp with Some(argString) -> wordSplit argString | None -> ["cpp"])
        @ strippedArgs @ ["-o"; newTempName]
    in
    runCommand (* 'cpp' here is used only in error messages... *) "cpp" newArgs
    ;
    let ppPluginsToLoad = List.rev !ppPluginsToLoadReverse in
    let ppPassesToRun = List.rev !ppPassesToRunReverse in
    (* Okay, run CIL; we need the post-preprocessing line directive style *)
    Cil.lineDirectiveStyle := Some Cil.LinePreprocessorOutput;
    (* We have to use logical operators to avoid breaking code that does -Werror=format-string
     * ... this involves an expression-level check of the first argument to printf, which
     * might be a conditional expression. So we can't substitute it with a temporary assigned
     * in an if/else construct. *)
    Cil.useLogicalOperators := true;
    let initialCilFile = Frontc.parse newTempName () in
    (* do passes *)
    List.iter Feature.loadWithDeps ppPluginsToLoad;
    let features = ppPassesToRun in
    List.iter Feature.enable features;
    (* Errormsg.verboseFlag := true; *)
    let currentCilFile = initialCilFile in
    (* HACKED based on CIL's main.ml:
     * Scan all the registered features and, if they are 
     * enabled then run them on the current file *)
    List.iter
      (fun fdesc -> 
        if fdesc.Feature.fd_enabled then begin
          if !Errormsg.verboseFlag then 
            ignore (Errormsg.log "Running CIL feature %s (%s)\n" 
                      fdesc.Feature.fd_name fdesc.Feature.fd_description);
          try
          (* Run the feature, and see how long it takes. *)
          Stats.time fdesc.Feature.fd_name
            fdesc.Feature.fd_doit currentCilFile
          with Not_found -> (output_string Pervasives.stderr ("CIL pass " ^ fdesc.Feature.fd_name ^ " raised Not_found!\n"); raise Not_found);
          (* See if we need to do some checking *)
          if !Cilutil.doCheck && fdesc.Feature.fd_post_check then begin
            ignore (Errormsg.log "CIL check after %s\n" fdesc.Feature.fd_name);
            if not (Check.checkFile [] currentCilFile) && !Cilutil.strictChecking then begin
              Errormsg.error ("Feature \"%s\" left CIL's internal data "
                       ^^"structures in an inconsistent state. "
                       ^^"(See the warnings above)") fdesc.Feature.fd_name
            end
          end
        end)
      (Feature.list_registered ());
    Cil.printerForMaincil := Cil.defaultCilPrinter;
    (* We are not printing for CIL input *)
    Cil.print_CIL_Input := false;
    let (chan, str) = match !outputFile with
            None -> Pervasives.stdout, "(stdout)"
          | Some(fname) -> (Pervasives.open_out fname, fname)
    in
    let _ = Cil.dumpFile Cil.defaultCilPrinter chan str currentCilFile
    in
    let status = if !Errormsg.hadErrors then 1 else 0 in
    (* delete temporary file unless -save-temps *)
    (if !saveTemps then () else Unix.unlink newTempName;
    exit status)
