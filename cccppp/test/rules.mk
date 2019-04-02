THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
CCCPPP_PREFIX ?= $(realpath $(dir $(THIS_MAKEFILE))/..)

# We build all our tests by running them through our cccppp wrapper.
CXXFLAGS += `$(CCCPPP_PREFIX)/bin/cccppp-cxxflags`

