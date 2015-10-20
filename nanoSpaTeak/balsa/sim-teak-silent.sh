#!/bin/sh

RUNTIME=`teak-config`/share/teak/runtime/verilog

EXAMPLE_CELLS=${RUNTIME}/example.v

cver +suppress_warns+3107+3108 +define+DUT=teak__spaHarvard_V5T +define+TECHFILE='"'${EXAMPLE_CELLS}'"' \
	+define+TEAK \
	+define+NO_ACTIVATE \
	+define+NLSTFILE='"/tmp/teak_tmp/loopbufferingteakfiles/teak_toplevel.v"' +define+RESET \
	+define+PROGFILE='"/tmp/teak_tmp/nanoSpaTeak/tests/tmp.hex"' \
	+define+DUMPFILE='"/tmp/a.vcd"' +define+DUMPVARS=1 /tmp/teak_tmp/nanoSpaTeak/balsa/test-spaHarvardT-silent.v \
	${RUNTIME}/monitors.v \
	${RUNTIME}/runtime.v > /tmp/tmp-sim.out 2> /dev/null

sleep 2
grep  -c PASS /tmp/tmp-sim.out
