#!/usr/bin/awk -f

BEGIN {FS="// expect: "}

/expect/{print $2}
