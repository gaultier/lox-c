#!/bin/sh
set -aue

if command -v patdiff > /dev/null
then
    export DIFFTOOL=patdiff
else
    export DIFFTOOL="diff"
fi

LOXBIN="${LOXBIN:-./lox-release}"
TMPDIR="${TMPDIR:-/tmp}"
PARALLEL_OPTS="${PARALLEL_OPTS:- --bar -k --timeout 2}"

find test -name '*.lox' -type f | sort | parallel $PARALLEL_OPTS " \
    awk -F '// expect: ' '/expect/{print \$2}' < {} > $TMPDIR/{/}.expected; \
    echo 5 | $LOXBIN run {} > $TMPDIR/{/}.output 2>&1; \
    $DIFFTOOL $TMPDIR/{/}.expected $TMPDIR/{/}.output || exit 1"
