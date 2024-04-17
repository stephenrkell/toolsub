# Subvert your toolchain

A productive way to build novel tools or runtime environments is to (1)
lightly subvert the existing tools which compile, assemble, link and/or
load your code, and then (2) do the same for the run-time environment

This project collects utilities useful for part 1 of this exercise: the
toolchain part.

Such lightly subversive acts might insert instrumentation, do
build-time logging, tweak the command-line options, generate additional
metadata, substitute alternative tools in certain cases, and so on.
It is usually easier to do this than to build a new toolchain that can
act as a drop-in replacement.

However, it is still not easy! This repository contains various
utilities which make it easier to create your own lightly subverted
toolchain. Currently it contains the following.

* wrapper, a collection of shell scripts and utility functions for
intercepting certain stages in the compilation process (especially
preprocessing). Initially these were written specially for use with
gcc's '-wrapper' option, but are now somewhat broader.

* cilpp, a more lightweight CIL driver compared to CIL's default
'cilly', implemented using 'wrapper' to be invokable simply by using
the -B option on the command line.

* cccppp, a Clang tool which lightly rewrites C++ source to make it
amenable to instrumentation of built-in operators. The idea is that all
built-in operator applications are wrapped in templates that are
defined as a standard "prelude"; instrumentation can be expressed by
supplying an alternative collection of prelude templates.

* compilerwrapper.py -- a Python wrapper script for gcc and similar
compiler command lines. This is useful when you can't assume support
for gcc-style "-B" or "-wrapper" options, or other cases when you
really need to get between the compiler and the user/script that is
invoking it.

* (FIXME: add this: ) gold-plugin-base.cpp: a skeleton gold plugin
which provides facilities for tweaking linker behaviour, including
overriding its command-line options.
