THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
srcroot := $(realpath $(dir $(THIS_MAKEFILE))/../..)

-include $(srcroot)/config.mk

.PHONY: default
default: cilpp cilprint

OCAMLFIND ?= ocamlfind
ifneq ($(MAKECMDGOALS),clean)
ifeq ($(CIL_INSTALL),)
$(error Please set CIL_INSTALL to your CIL directory (containing a file cil/cil.cmxa))
endif
endif

OCAMLOPTFLAGS += -fPIC
CFLAGS += -fPIC

OCAMLFIND ?= ocamlfind
OCAMLFLAGS += -package unix -package findlib -package dynlink -package zarith -package str
OCAMLFLAGS += -I $(dir $(THIS_MAKEFILE))/lib
OCAMLFLAGS += -I $(dir $(THIS_MAKEFILE))/../../wrapper/lib
OCAMLFLAGS += -I ${CIL_INSTALL}
vpath %.cmx $(dir $(THIS_MAKEFILE))/../../wrapper/lib
vpath %.o $(dir $(THIS_MAKEFILE))/../../wrapper/lib

export OCAMLFLAGS
export OCAMLFIND
export CIL_INSTALL


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
OCAMLLIBS := goblintCil.cmxa mainFeature.cmx
# we use -linkall to include the whole cmxa, so that plugins can
# dynlink against any of its symbols.
cilpp: cilpp_common.ml cilpp.ml mkstemp_stubs.o $(CIL_INSTALL)/goblintCil.cmxa
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $(OCAMLLIBS) $(filter-out %/goblintCil.cmxa,$+) -linkpkg -linkall
cilprint: cilpp_common.ml cilprint.ml mkstemp_stubs.o $(CIL_INSTALL)/goblintCil.cmxa
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $(OCAMLLIBS) $(filter-out %/goblintCil.cmxa,$+) -linkpkg -linkall

%.cmxs: %.cmx
	$(OCAMLFIND) ocamlopt -shared -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $+
%.cmx %.cmi: %.ml
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -c "$<"
%.cmo %.cmi: %.ml
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -c "$<"
%.cma: %.cmo
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -a $+

clean:
	rm -f cilpp-wrapper
	rm -f *.o *.cmxa *.cmx *.cmo *.cmxs
	rm -f cilpp cilprint

# FIXME: add some quick self-tests of our wrapping mechanism(s)
#.PHONY: test
#	gcc -wrapper `pwd`/cilpp-wrapper 
