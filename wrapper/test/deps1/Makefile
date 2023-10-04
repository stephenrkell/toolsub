.PHONY: check clean-check
check: foo.i
	test -e foo.d # depfile should have been created
	test -s foo.i # .i file must not be empty
clean-check:
	$(MAKE) clean
	$(MAKE) check

CPPFLAGS += -wrapper ../../bin/wrapper -no-integrated-cpp -MD

%.i: %.c
	$(CC) $(CPPFLAGS) -E -o $@ $<

.PHONY: clean
clean:
	rm -f *.d *.i
