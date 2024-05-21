'wrapper' is a collection of shell scripts and utility functions for
'compiler wrapping', i.e. intercepting certain stages in the
compilation process. Initially these were written specially for use
with gcc's '-wrapper' option to intercept preprocessing, but are now
somewhat broader.

Compiler invocation is surprisingly knotty. Some terms:

- 'driver' refers to the 'cc' or similar executable, e.g. 'gcc' or
'clang' or so on. (So far there is no attempt to support MSVC, but
ideally that would change.)

- 'cc1' is the actual compiler. The details of its command line are
internal to the compiler, i.e. known only to the driver. However,  in
practice they don't diverge *too* much from the compiler  driver's
documented options.

- 'cpp' is the C preprocessor. Nowadays it tends not to be used, as
preprocessing is done by 'cc1' (either as a separate special
invocation, or more normally, integrated within a single run).
Therefore 'cpp' can be a red herring. It has a documented command line,
but frustratingly, it has a subtly different semantics in  some corners,
e.g. '-MD' means something slightly different for  'cpp' than it does
for 'cc'.

- 'as' and 'ld' etc are additional tools that, like 'cpp', have
documented (-to-some-extent) command lines and may be wrapped.

- 'collect2' is a GCC tool that lightly wraps 'ld'. FIXME: remember/
explain exactly what it adds and why.

There are three command-line protocols that are supported by
drivers and could be used as a basis for wrapping.

- '-wrapper <script>': only GCC supports this, but it is the
easiest to use. For any tool that would ordinarily be run by the
driver, <script> is run, with

- '-###': both Clang and GCC also supports this. It simply dumps
the list of commands that would be run, but does not run them.

- '-Bprefix': changes the path prefix in which the driver looks for
its subservient tools such as 'cc1'. Both GCC and Clang support
this. With Clang, 'prefix' need not be a directory (but often is).

The usual issue with wrapping is that the above features may
already be used in any given build system, to perform its own
wrapping or introspection. To add one more layer of
wrapping/introspection therefore requires some finesse.

Currently 'wrapper' supports only the '-wrapper' protocol, but
support the others is desired also.

The main usage of 'wrapper' is to run or include it while setting
variables such as CPPWRAP or ASWRAP, to commands or shell functions
that should be used.

There is also some OCaml code in src/ which implements similar
rewriting of cpp (only) command lines. The main motivation was to
allow writing a 'cilpp' tool as a simpler driver of CIL
(alternative to the 'cilly' Perl script).

Some known client tools of 'wrapper' are:

- constructor-priority-checker (see example/; also used by liballocs build; uses CC1WRAP)

- cilpp  (in this repo; uses CPPWRAP)

- cccppp (in this repo; uses CPPWRAP)

- dbgcov (external; uses CPP)

- liballocs (allocs-wrapper: wraps 'as' using ASWRAP and defines 'my_cc1' but curiously does not set CC1WRAP)

TODO:

- test cases
    - how to test 'wrapper'? one big test is that it can compile a large codebase
- heed only *WRAP options; eliminate CPP
- renamings: CC1WRAP => CCWRAP (and rewrite options to driver form);
             CPPWRAP can stay as-is? (precedent: GNU Make defaults $(CPP) to 'cc -E')
- completely eliminate 'cpp' handling? what if we really do want to fake the preprocessor, e.g. in a build system that runs 'cpp'?
- compiler_args.ml:
   - firstly, make it correct w.r.t. whether it's processing cpp or driver command lines
   - secondly, can we eliminate this whole thing? it is used only by dbgcov, cilpp, cccppp
- cilpp:
   - firstly, rename to 'cilc' and make it a substitute only for cc -E (or, trivially, cc -MM?) command lines
   - secondly, restore the option to run it in place of 'cpp' if run as 'cilpp' (use Sys.argv.(0)) -- optional!
       - might be trivial! does/should it behaviour vary, whether it's given "-E <cc opts>" or "<cpp opts>"?
         It needs to run the "real" preprocessor.
