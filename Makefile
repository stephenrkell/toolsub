-include config.mk

.PHONY: default
default: cilpp-recursive wrapper-recursive #cccppp-recursive

.PHONY: cilpp-recursive wrapper-recursive cccppp-recursive

cilpp-recursive: wrapper-recursive
	$(MAKE) -C cilpp

cccppp-recursive: wrapper-recursive
	$(MAKE) -C cccppp

wrapper-recursive:
	$(MAKE) -C wrapper

.PHONY: clean
clean:
	$(MAKE) -C cilpp clean
	$(MAKE) -C cccppp clean
	$(MAKE) -C wrapper clean
