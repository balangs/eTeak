#!/bin/sh

# teak -L -l after-v=1:loop=2 a
# teak -L -l simple=1 a
# teak -L -l loop=1 a
teak -L -l loop=1 a
teak -o a -n a.teak --test-protocol --gates

RUNTIME=`teak-config`/share/teak/runtime/verilog

cver +define+DUT=teak_top +define+TECHFILE='"'${EXAMPLE_CELLS}'"' \
	+define+HAS_GO \
	+define+HAS_DONE \
	+define+DUMPFILE='"'a.vcd'"' \
	${RUNTIME}/monitors.v \
	${RUNTIME}/runtime.v \
	${RUNTIME}/example.v \
	${RUNTIME}/top.v \
	startup.v \
	a.v
