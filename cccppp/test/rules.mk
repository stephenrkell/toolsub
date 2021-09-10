THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
CCCPPP_PREFIX ?= $(realpath $(dir $(THIS_MAKEFILE))/..)

# HACK: for now, force c++11
BASIC_CXXFLAGS += -std=c++11 -save-temps

# We build all our tests by running them through our cccppp wrapper.
CXXFLAGS += `$(CCCPPP_PREFIX)/bin/cccppp-cxxflags` $(BASIC_CXXFLAGS)

%.vanilla.ii: %.cpp
	$(CXX) $(BASIC_CXXFLAGS) -E -o $@ $<
%.vanilla.ii: %.cc
	$(CXX) $(BASIC_CXXFLAGS) -E -o $@ $<
%.ii: %.cpp
	$(CXX) $(CXXFLAGS) -E -o $@ $<
%.ii: %.cc
	$(CXX) $(CXXFLAGS) -E -o $@ $<
%.ast: %.vanilla.ii
	$(CCCPPP_PREFIX)/src/dump $< 2>$@ || (rm -f $@; false)
%.ii.diff: %.vanilla.ii %.ii
	diff -u $+ > "$@"; true # diff returns 0 only if identical


%.o: %.ii
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# cancel the built-in rules that we don't want to use
%.o: %.cc
%.o: %.cpp
%: %.o
%.o: %.s
