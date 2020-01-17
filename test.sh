#!/bin/sh
set -aue

if command -v patdiff
then
    export DIFFTOOL=patdiff
else
    export DIFFTOOL="diff"
fi

LOXBIN="${LOXBIN:-./lox-release}"

find test -name '*.lox' -type f | sort | parallel --bar -k --timeout 4 " \
    awk -F '// expect: ' '/expect/{print \$2}' < {} > $TMPDIR/{/}.expected; \
    echo 5 | $LOXBIN run {} > $TMPDIR/{/}.output 2>&1; \
    $DIFFTOOL $TMPDIR/{/}.expected $TMPDIR/{/}.output || exit 1"
