THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

.PHONY: default
default: src

.PHONY: src
src:
	$(MAKE) -C src

.PHONY: clean
clean:
	$(MAKE) -C src clean
