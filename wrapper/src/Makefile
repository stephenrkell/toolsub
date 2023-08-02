THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
SRCROOT := $(realpath $(dir $(THIS_MAKEFILE))/../..)

.PHONY: default
default: compiler_args.cmx mkstemp_stubs.o

OCAMLFIND ?= ocamlfind
OCAMLOPTFLAGS += -fPIC
CFLAGS += -I `$(OCAMLFIND) ocamlc -where` -fPIC
OCAMLFLAGS +=  -package findlib

export OCAMLOPTFLAGS
export OCAMLFLAGS
export OCAMLFIND

# IMPORTANT: only list cmxa files here iff they are *not*
# covered by a -package argument to ocamlfind -- CHECK the
# Makefile in our parent directory too (it should be clean).
#
# The effect of -package is to delegate to ocamlfind the 
# inclusion by -I of the relevant package. The effect of
#  -linkpkg is to delegate # to ocamlfind the inclusion of the
# package cmxa on the command line. If we both use -linkpkg
# with -package of a given package, and give the cmxa on the
# command line, bad things can happen.
#
# So list only the packages whose cmxa files live in either
# (1) locations we can predict from this Makefile, or
# (2) locations that are "standard" so don't need specifying.
# That means standard OCaml libs are okay to give as a .cmxa, 
# and CIL libs are okay, but findlib is not; use -package.
# NOTE also that order matters: put more depended-on entries
# further left (i.e. the opposite of the usual link order).
OCAMLLIBS := str.cmxa nums.cmxa unix.cmxa cil.cmxa #frontc.cmxa
# we use -linkall to include the whole cmxa, so that plugins can
# dynlink against any of its symbols.
	
%.cmxs: %.cmx
	$(OCAMLFIND) ocamlopt -shared -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $+
%.cmx %.cmi: %.ml
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -c "$<"
%.cmo %.cmi: %.ml
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -c "$<"
%.cma: %.cmo
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -a $+

clean:
	rm -f *.o *.cmxa *.cmx *.cmo *.cmxs
