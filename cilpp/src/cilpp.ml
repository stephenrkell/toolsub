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
(* If we're run as 'cilpp-wrapper', we run the generic wrapper but with
 * - CC_IDENTIFY_ARGS set to the above description of our arguments.
 * - CPP set to us, possibly with a -real-cpp arg added...
 *      ... using CC_DRIVER? Oh, but we don't have that yet!
 *      Either replicate how we get it, or... turn this bit of code into a shell script.
 *      The latter is actually pretty easy. Our wrapper script is sourceable.
 *
 *
 *
 * How do we locate the generic wrapper? Use CC_WRAPPER or else our dirname/wrapper.
 *
 * FIXME: want a similar treatment for ccwrap, i.e. cilpp-ccwrap will invoke the
 * generic wrapcc with the right environment. Do we use CCWRAP or WRAPPER to
 * locate the real script?
 *)
let actAsWrapper (realCilpp : string) =
    let ourArgSpec = "-save-temps\n\
-real-cppSARG\n\
-pluginSARG\n\
-fpass-JARG\n\
-oSARG\n\
--output=JARG" (* FIXME: what about --output SARG ? Is this accepted? YES. *)
    in
    putenv "CC_IDENTIFY_ARGS" ourArgSpec;
    (* The purpose of acting as a wrapper is that we set up to run ourselves
     * "as we'd most like to be invoked", even though we will still work in
     * other ways e.g. even without CC_IDENTIFIED_ARGS. Same for CC_DRIVER: 
     * if we have it, use it to set our real cpp, under the reasonable
     * assumption that the options we're getting are appropriate to that
     * driver (remember: we're running as a wrapper, on a 'lifted' command line!).
     * That means we should set  CPP="us -real-cpp='/path/to/compiler/driver -E'"
     * Unless we have CPP already? Then we should use that as our real CPP. *)
    let explicitUCpp = try Some(Unix.getenv "CPP")
                      with Not_found -> None
    in
    let defaultUCpp = try ((Unix.getenv "CC_DRIVER") ^ " -E") (* FIXME: use lists not space-sep *)
                      with Not_found -> "cpp" (* this is also our default, so no need to give it as -real-cpp,
                                               * but doing so doesn't hurt *)
    in
    putenv "CPP" ("'" ^ realCilpp ^ "' -real-cpp '" ^ (match explicitUCpp with 
                                                         Some(ucpp) -> ucpp
                                                       | None -> defaultUCpp ^ "'"));
    let wrapperPath = try Unix.getenv "WRAPPER"
                      with Not_found -> Filename.concat (Filename.dirname Sys.argv.(0)) "wrapper"
    in
    (* Don't use runCommand, because it forks; use execve *)
    Array.set Sys.argv 0 wrapperPath;
    Unix.execv wrapperPath Sys.argv (* on failure: will propagate Unix_error to toplevel *)

let () =
    if Sys.argv.(0) = "cilpp-wrapper" || None <> (matchesSuffix "/cilpp-wrapper" Sys.argv.(0)) then
        actAsWrapper (let linkTarget = (Unix.readlink Sys.argv.(0)) in
                      if Filename.is_relative linkTarget
                      then Filename.concat (Filename.dirname Sys.argv.(0)) linkTarget
                      else linkTarget)
    else
    let argList = Array.to_list Sys.argv in
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
    List.iteri (fun i -> fun arg ->
        let stripThisOne = fun () -> stripPositions := i :: !stripPositions; () in
        let stripNextOne = fun () -> stripPositions := (i+1) :: !stripPositions; () in
        match arg with
        | _  when !finished -> ()
        | "-o" -> (readingExtraArg := Some(`ArgNamingOutputFile);
                   stripThisOne (); ())
        | "--output" -> (readingExtraArg := Some(`ArgNamingOutputFile);
                   stripThisOne (); ())
        | s when None <> matchesPrefix "--output=" s ->
              let outputFileName = really (matchesPrefix "--output=" s) in
              outputFile := Some(outputFileName); stripThisOne (); ()
            (* PROBLEM: how to distinguish a -save-temps directed at us
             * versus a -save-temps directed at the real cpp? i.e. do we stripThisOne () ?
             * A plain preprocess-only invocation will not generate any temps, so we are
             * fine to strip it.... *)
        | "-save-temps" -> saveTemps := true; stripThisOne (); () (* i.e. accept -Wp,-save-temps *)
            (* ... but in general we can't assume our args are not understood by the underlying
             * tool. FIXME: this seems like a problem, for any option that could apply to both
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
        | "--" -> finished := true; stripThisOne (); () (* Explicit end of our args, so terminate *)
        | "-Xpreprocessor" when None = !readingExtraArg ->
              stripThisOne (); ()
        | arg ->
              let wasReadingExtraArg = !readingExtraArg in
              readingExtraArg := None;
              match wasReadingExtraArg with
                  None -> (finished := true; ()) (* we don't recognise this opt, and it's not an SARG, so terminate *)
                | Some(`ArgNamingPlugin) -> ppPluginsToLoadReverse := arg :: !ppPluginsToLoadReverse; ()
                | Some(`ArgNamingRealCPP) -> realCpp := Some(arg); ()
                | Some(`ArgNamingOutputFile) -> outputFile := Some(arg); ()
    ) argList
    ;
    (* Now process our CC_IDENTIFIED_ARGS env var, possibly overriding the above.
     * The env var should take precedence because there's a certain partialness
     * in the above approach to scanning: we give up when we see something we don't
     * recognise. If the caller has set options in our env var
     * specially, always use that because the wrapper knows these. *)
    (* FIXME: do this *)
    
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
    let strippedArgs = List.flatten (List.mapi (fun i -> fun arg ->
        if List.mem i !stripPositions then [] else [arg]
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
