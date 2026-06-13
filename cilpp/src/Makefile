THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
srcroot := $(realpath $(dir $(THIS_MAKEFILE))/../..)

-include $(srcroot)/config.mk

.PHONY: default
default: cilpp cilprint cabsprint

ifneq ($(USE_GOBLINT),)
CIL_CMXA := $(CIL_LIBDIR)/goblintCil.cmxa
else
CIL_CMXA := $(CIL_LIBDIR)/cil.cmxa
endif

OCAMLFIND ?= ocamlfind
ifneq ($(MAKECMDGOALS),clean)
ifeq ($(CIL_LIBDIR),)
$(error Please set CIL_LIBDIR to a directory containing a file $(notdir $(CIL_CMXA)))
endif
endif

OCAMLOPTFLAGS += -fPIC
CFLAGS += -fPIC

OCAMLFIND ?= ocamlfind

OCAMLFLAGS += -package unix -package findlib -package dynlink
OCAMLFLAGS += -package str
OCAMLFLAGS += -I $(dir $(THIS_MAKEFILE))/lib
OCAMLFLAGS += -I $(dir $(THIS_MAKEFILE))/../../wrapper/lib
OCAMLFLAGS += -I $(CIL_LIBDIR)

ifneq ($(USE_GOBLINT),)
OCAMLFLAGS += -package zarith
OCAMLFLAGS += -I $(CIL_LIBDIR)/mainFeature
# set some flags that we don't pass on to goblintCil_compat, which would be circular
cilpp cilprint cabsprint $(patsubst %.ml,%.cmx,$(filter-out goblintCil_compat.ml,$(wildcard *.ml))): private \
OCAMLFLAGS += -I $(realpath $(dir $(THIS_MAKEFILE))) -open GoblintCil_compat -open MainFeature
#cilpp cilprint cabsprint: goblintCil_compat.cmx
else
OCAMLFLAGS += -package num
endif

vpath %.cmx $(dir $(THIS_MAKEFILE))/../../wrapper/lib
vpath %.o $(dir $(THIS_MAKEFILE))/../../wrapper/lib

export OCAMLFLAGS
export OCAMLFIND
export CIL_LIBDIR

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
OCAMLLIBS += $(CIL_CMXA)
ifneq ($(USE_GOBLINT),)
OCAMLLIBS += $(CIL_LIBDIR)/mainFeature/mainFeature.cmxa
COMPAT_MODULES := goblintCil_compat.cmx
else
COMPAT_MODULES :=
endif

# Need to be careful about building a left-to-right topsort order, i.e.
# the most depended-on things come first.
#
# For goblintCil_compat.cmx, I think it works if it's the first item in $+.
#
# Order that works:
# ocamlfind ocamlopt -o "cilpp" -fPIC \
  -package unix -package findlib -package dynlink -package str \
 -I .//lib -I .//../../wrapper/lib \
 -I /usr/local/src/cil-mbyzhangAlt/_build/install/default/lib/goblint-cil \
  -package zarith \
  -I /usr/local/src/cil-mbyzhangAlt/_build/install/default/lib/goblint-cil/mainFeature \
  -I /var/local/stephen/work/devel/toolsub.git/cilpp/src \
  -open GoblintCil_compat -open MainFeature \
  /usr/local/src/cil-mbyzhangAlt/_build/install/default/lib/goblint-cil/goblintCil.cmxa \
  /usr/local/src/cil-mbyzhangAlt/_build/install/default/lib/goblint-cil/mainFeature/mainFeature.cmxa \
  goblintCil_compat.cmx \
  cilpp_common.ml cilpp.ml .//../../wrapper/lib/mkstemp_stubs.o \
  -linkpkg -linkall

# we use -linkall to include the whole cmxa, so that plugins can
# dynlink against any of its symbols.
%: $(COMPAT_MODULES) cilpp_common.ml %.ml mkstemp_stubs.o
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $(OCAMLLIBS) $+ -linkpkg -linkall

%.cmxs: %.cmx
	$(OCAMLFIND) ocamlopt -shared -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $+
%.cmx %.cmi: %.ml
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -c $+
%.cmo %.cmi: %.ml
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -c $+
%.cma: %.cmo
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -a $+

clean:
	rm -f cilpp-wrapper
	rm -f *.o *.cmxa *.cmx *.cmo *.cmxs
	rm -f cilpp cilprint cabsprint

# FIXME: add some quick self-tests of our wrapping mechanism(s)
#.PHONY: test
#	gcc -wrapper `pwd`/cilpp-wrapper 
