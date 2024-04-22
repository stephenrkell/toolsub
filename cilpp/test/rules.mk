THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
CILPP_PREFIX ?= $(realpath $(dir $(THIS_MAKEFILE))/..)

BASIC_CFLAGS += -save-temps

# We build all our tests by running them through our cilpp wrapper.
CFLAGS += `$(CILPP_PREFIX)/bin/cilpp-cflags` $(BASIC_CFLAGS)
# With no passes enabled, it still builds via CIL and so should
# catch problems with the cilpp infrastructure.

.PHONY: clean
clean::
	rm -f *.i *.s *.o *.ii $$(basename $$(pwd))
