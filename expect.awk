#!/usr/bin/env awk -f

BEGIN {FS="// expect: "}

/expect/{print $2}
