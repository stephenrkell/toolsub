module Cil = GoblintCil
(* XXX: for clients that don't 'open' Cil, why are the below correct? *)
module Feature = Cil.Feature (* XXX: what about stuff moved into MainFeature by goblint? *)
module Cprint = Cil.Cprint
module Frontc = Cil.Frontc
module Errormsg = Cil.Errormsg
module Stats = Cil.Stats
module Cilutil = Cil.Cilutil
module Check = Cil.Check
