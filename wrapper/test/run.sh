#!/bin/bash

# Some test cases are regression tests that we run specially;
# others are stress tests, a.k.a. found codebases supporting a "./configure && make" workflow
# that let us set CFLAGS at configure time. gnu-hello is the only one of these for now...
# How do we vendor these without bloating our repo?
# One idea is to keep a list of download URLs and sha1sums here
# and grab them on demand.

declare -a urls
declare -a sha1sums
sources=("https://ftp.gnu.org/gnu/hello/hello-2.12.tar.gz" \
         "https://ftp.gnu.org/gnu/tar/tar-1.35.tar.gz" )
sha1sums=(336b8ae5d6e72383c53ebd0d3e62d41e8266ba8b \
          92848830c920cbaa44abc3ab70e02b0ce8f1e212)

${MAKE:-make} -C deps1 clean-check && \
ctr=0; for t in gnu-hello tar; do
	({ cd "$t" || \
      { wget -O "$t".tar.gz "${sources[$ctr]}" && \
        test "$(sha1sum <"$t".tar.gz | tr -cd '[0-9a-f]' )" == ${sha1sums[$ctr]} && \
        mkdir "$t" && cd "$t" && tar --strip-components=1 -xzf ../"$t".tar.gz; }; } &&  \
    { test -e config.status && make clean || CFLAGS=`../../bin/wrapper-cflags` ./configure; } && make || break)
    ctr=$(( $ctr + 1 ))
done
