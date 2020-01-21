#!/bin/sh
set -aue

DIFF_TOOL="${DIFF_TOOL:-diff}"
LOXBIN="${LOXBIN:-./lox-release}"
TMPDIR="${TMPDIR:-/tmp}"
PARALLEL_OPTS="${PARALLEL_OPTS:- --bar -k --timeout 2}"

find -s test -name '*.lox' -type f | parallel $PARALLEL_OPTS " \
    awk -F '// expect: ' '/expect/{print \$2}' < {} > $TMPDIR/{/}.expected; \
    echo 5 | $LOXBIN run {} > $TMPDIR/{/}.output 2>&1; \
    $DIFF_TOOL $TMPDIR/{/}.expected $TMPDIR/{/}.output || exit 1"
