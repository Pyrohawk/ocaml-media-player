#!/bin/sh
# restart with tclsh \
exec tclsh "$0" "$@"

set step [expr {tan (1) / 99}]
for  {set i 1} {$i <= 100} {incr i} {
  set n [expr {100 * pow (atan ($step * (100 - $i)), 3)}]
  puts [format "%.2f" $n]
}

