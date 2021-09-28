THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
srcroot := $(dir $(realpath $(THIS_MAKEFILE)))/..

-include $(srcroot)/config.mk

.PHONY: default
default: src

.PHONY: src
src:
	$(MAKE) -C src

.PHONY: clean
clean:
	$(MAKE) -C src clean
