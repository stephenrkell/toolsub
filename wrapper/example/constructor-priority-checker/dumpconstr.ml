open GoblintCil

let (inputFile : GoblintCil.file) = Frontc.parse Sys.argv.(1) ()

(* A function attribute is reduced to an int option option; None if not a constructor *)
let attrToString (c : int option option) = match c with
        None -> "not a constructor"
      | Some(None) -> "constructor with no priority"
      | Some(Some(n)) -> "constructor with priority " ^ (string_of_int n)

(* A constructor is described by an int option *)
let extractConstrPriorities (attrs : GoblintCil.attributes) (ctxt : string) : int option list = begin
    List.flatten (List.map (fun a ->
        match a with
            Attr(tag, _) when tag <> "constructor" -> []
          | Attr(_, [AInt(n)]) -> [Some(n)]
          | Attr(_, []) -> [None]
          | Attr(_, _) -> output_string stderr
            ("Warning: constructor attribute at " ^ ctxt ^ " given unknown arguments\n"); []
    ) attrs)
end

(* If a programmer says 'constructor(NNN)?' and later
 * 'constructor(MMM)?', what do we regard their intention
 * as? According to GCC bug 87592, "the one on the definition
 * should win". But we don't have that information.
 * Currently, the first one wins. So we take the leftmore. *)
let mergePrios (p1 : int option) (p2 : int option) = p1

let collectConstrPrio (ctxt : string) (l: location) (collected: int option option) (seenConstr : int option) : int option option =
      match (collected, Some(seenConstr)) with
      | (None, x) -> x         (* initial case can't be bad? FIXME: actually... *)
      | (x, y) when x = y -> x (* same is good *)
      | (Some(coll), maybeConstr) (* seen something not equal *) ->
            (output_string Out_channel.stderr ("Warning: at " ^
                l.file ^ ":" ^ (string_of_int l.line) ^": function " ^ ctxt ^
                " declared as " ^
                (attrToString maybeConstr) ^
                " when previously it was declared as " ^
                (attrToString (Some coll)) ^ "\n");
                match maybeConstr with
                    None -> Some(coll)
                  | Some(maybePrio) -> Some(mergePrios coll maybePrio))

(* PROBLEM: Cil seems to do some merging of attrs itself, so we get multiple
 * constructor attributes on a vardecl even when each prototype has <= 1.
 * So we do our 'collect' within a single attribute list as well as across
 * vardecls. The point is to "collect" the programmer intent. *)

(* Top-level: for each function we fold over its vardecls and, for each one,
 * fold over its attributes. Prints a warning if we see a divergence. At
 * the end, we print the result of our folding. *)
let doOneFunction (f: fundec) : unit =
    let (final : int option option)
    = List.fold_left (fun maybeSeen -> fun (GVarDecl(vdec, l), dec) ->
        (* Fold over this vardecl's attributes *)
        let (collectedOverAttrs : int option option) 
        =  List.fold_left (collectConstrPrio f.svar.vname l)
            None (extractConstrPriorities dec.dattr vdec.vname)
        in
        (* Merge by applying the fold operation one more time, if
         * the answer wasn't "no constructors here at all". *)
        match collectedOverAttrs with
            None -> maybeSeen
          | Some(overAllAttrs) -> collectConstrPrio f.svar.vname l maybeSeen overAllAttrs
    ) None f.svar.vvardecls
    in
    match final with
        None -> ()
      | Some(maybePrio) -> output_string Pervasives.stdout ((f.svar.vname ^ " constructor" ^
             match maybePrio with
                 Some(prio) -> (" " ^ (string_of_int prio))
              |  None -> ""
        ) ^ "\n")

(* top level *)
let _ = List.map (fun g -> begin match g with
    GFun(f, _) -> doOneFunction f
  | _       -> ()
end) inputFile.globals
