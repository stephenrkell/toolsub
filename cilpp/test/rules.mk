THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
CILPP_PREFIX ?= $(realpath $(dir $(THIS_MAKEFILE))/..)

BASIC_CFLAGS += -save-temps

# We build all our tests by running them through our cccppp wrapper.
CFLAGS += `$(CILPP_PREFIX)/bin/cilpp-cflags` $(BASIC_CFLAGS)

.PHONY: clean
clean::
	rm -f *.i *.s *.o *.ii $$(basename $$(pwd))
