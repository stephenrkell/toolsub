THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
WRAPPER ?= $(dir $(realpath $(THIS_MAKEFILE)))wrapper
srcroot := $(dir $(realpath $(THIS_MAKEFILE)))/../../..

.PHONY: default
default: dumpconstr

-include $(srcroot)/config.mk

OCAMLFIND ?= ocamlfind
ifeq ($(CIL_INSTALL),)
$(error Please set CIL_INSTALL to your CIL directory (containing a file cil/cil.cmxa))
endif
OCAMLFLAGS +=  -package findlib
OCAMLFLAGS += -I $(CIL_INSTALL)

%.cmxs: %.cmx 
	$(OCAMLFIND) ocamlopt -shared -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $+
%.cmx %.cmi: %.ml
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -c "$<"
%.cmo %.cmi: %.ml
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -c "$<"
%.cma: %.cmo
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -a $+
# we don't want the .o file that gets left behind, as it confuses make
%: %.cmx
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $(OCAMLLIBS) $< -linkpkg
	rm -f $*.o

dumpconstr: OCAMLLIBS += $(CIL_INSTALL)goblintCil.cmxa -package unix -package str -package num
dumpconstr: dumpconstr.cmx

.SECONDARY: dumpconstr

.PHONY: clean
clean:
	rm -f *.cmx *.cma *.cmo *.cmxa *.cmxs *.cmi
	rm -f dumpconstr

# the 'hasconstr' file illustrates the tool catching a problem.
# You will need to fix the problem before 'make hasconstr' works.
hasconstr: dumpconstr
hasconstr: CFLAGS += -wrapper $(WRAPPER)  -no-integrated-cpp

# for debugging the wrapper
# export DEBUG_CC := 1
