.PHONY: default src-recursive lib-symlink clean-recursive clean
default: src-recursive lib-symlink

src-recursive:
	$(MAKE) -C src
lib-symlink: src-recursive
	for f in src/compiler_args.cmx src/compiler_args.o src/compiler_args.cmi src/mkstemp_stubs.o; do \
	    test -e "$$f" && ln -sf ../"$$f" lib; done

clean-recursive:
	$(MAKE) -C src clean
clean: clean-recursive
