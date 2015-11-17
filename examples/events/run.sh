#!/bin/sh

# eTeak -L -l after-v=1:loop=2 a
# eTeak -L -l simple=1 a
# eTeak -L -l loop=1 a
eTeak -L -l loop=1 a
eTeak -o a -n a.teak --test-protocol --gates

RUNTIME=`eTeak-runtime`/verilog

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
