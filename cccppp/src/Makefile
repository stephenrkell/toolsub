THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

.PHONY: default
default: cccppp cccppp-tool

CXX_OBJS := $(patsubst %.cpp,%.o,$(wildcard *.cpp))

dump cccppp-tool: LDLIBS += \
        -Wl,--start-group \
        -lclangAST \
        -lclangASTMatchers \
        -lclangAnalysis \
        -lclangBasic \
        -lclangDriver \
        -lclangEdit \
        -lclangFrontend \
        -lclangFrontendTool \
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
        -lclangToolingCore \
        -lclangTooling \
        -lclangFormat \
        -Wl,--end-group \
        -lLLVM-8

$(CXX_OBJS): CXX := clang-8
$(CXX_OBJS): CXXFLAGS += -std=c++14 -g `llvm-config-8 --cflags` -UNDEBUG

dump cccppp-tool: LDFLAGS += `llvm-config-8 --ldflags`

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
