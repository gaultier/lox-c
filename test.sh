#!/usr/bin/env bash

set -eu

DIFF_TOOL="${DIFF_TOOL:-diff}"
LOXBIN="${LOXBIN:-./lox-release}"
TMPDIR="${TMPDIR:-/tmp}"
PARALLEL_OPTS="${PARALLEL_OPTS:- --bar -k --timeout 2}"

find test -name '*.lox' -type f \
    -and -not -name 'fib.lox' \
    -and -not -name 'count.lox' | sort | parallel ${PARALLEL_OPTS} \
    "${DIFF_TOOL} <(${LOXBIN} run {} 2>&1) <(./expect.awk {}) || ( echo {} && exit 1 )"
