THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

.PHONY: default
default: cccppp cccppp-tool

CXX_OBJS := $(patsubst %.cpp,%.o,$(wildcard *.cpp))

# This is just a big dump of all the clang libraries that could
# possibly be relevant... the linker will figure otu what to pull in.
# This changes by version of LLVM. They are all in a group, so
# the order doesn't matter (it's alphabetical).
dump cccppp-tool: LDLIBS += \
        -Wl,--start-group \
        -lclangAST \
        -lclangASTMatchers \
        -lclangAnalysis \
        -lclangBasic \
        -lclangDoc \
        -lclangDriver \
        -lclangEdit \
        -lclangFrontend \
        -lclangFrontendTool \
        -lclangHandleCXX \
        -lclangLex \
        -lclangParse \
        -lclangSema \
        -lclangEdit \
        -lclangRewrite \
        -lclangRewriteFrontend \
        -lclangStaticAnalyzerFrontend \
        -lclangStaticAnalyzerCheckers \
        -lclangStaticAnalyzerCore \
        -lclangCrossTU \
        -lclangIndex \
        -lclangSerialization \
        -lclangTidy \
        -lclangToolingCore \
        -lclangTooling \
        -lclangFormat \
        -Wl,--end-group \
        `llvm-config-13 --libs`

$(CXX_OBJS): CXXFLAGS += -std=c++14 -fPIC -g `llvm-config-13 --cxxflags` -UNDEBUG

dump cccppp-tool: LDFLAGS += `llvm-config-13 --ldflags`

cccppp-tool: main.o
	$(CXX) -o $@ $+ $(LDFLAGS) $(LDLIBS)
dump: dump.o
	$(CXX) -o $@ $+ $(LDFLAGS) $(LDLIBS)

OCAMLOPTFLAGS += -fPIC
CFLAGS += -fPIC

OCAMLFIND ?= ocamlfind
OCAMLFLAGS +=  -package findlib -package dynlink
OCAMLFLAGS += -I $(dir $(THIS_MAKEFILE))/lib
OCAMLFLAGS += -I $(dir $(THIS_MAKEFILE))/../../wrapper/lib
vpath %.cmx $(dir $(THIS_MAKEFILE))/../../wrapper/lib
vpath %.o $(dir $(THIS_MAKEFILE))/../../wrapper/lib

OCAMLLIBS := str.cmxa nums.cmxa unix.cmxa

cccppp: compiler_args.cmx cccppp.ml mkstemp_stubs.o
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $(OCAMLLIBS) $+ -linkpkg -linkall

%.cmxs: %.cmx
	$(OCAMLFIND) ocamlopt -shared -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $+
%.cmx %.cmi: %.ml
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -c "$<"
%.cmo %.cmi: %.ml
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -c "$<"
%.cma: %.cmo
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -a $+

clean:
	rm -f *.o *.cmxa *.cmx *.cmo *.cmxs *.cmi
	rm -f cccppp cccppp-tool
