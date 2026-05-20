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

open Cilpp_common

let () = runWithPrinter GoblintCil.defaultCilPrinter
