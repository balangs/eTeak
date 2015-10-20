#!/bin/sh

DO_COMPILE=no
if [ $# != 0 ]; then
	if [ $1 = "do-compile" ]; then
		DO_COMPILE=yes
	fi
fi

set -xe

for file in memoryDualPort.v dummyCoproIrqFiq.v; do
	rm -f ${file}
	ln -s ../tests/${file} ${file}
done

#OPTS="-O -q stom"
#OPTS="-O -q svrr"
OPTS="-O"
# LATCHES="--latches l1:v1:o1:i1:b1:g1:f1:t1"
# LATCHES="-L -l loop=1:scp:scp-limit=1000"
LATCHES="-L -l simple=1"

if [ ${DO_COMPILE} = yes ]; then
	rm -f teak-unlatched.teak teak.v

	teak -v -t _spaHarvard_V5T -o teak-unlatched nanoSpaHarvard_deparameterised &&
	teak -v --gates --test-protocol ${LATCHES} -n teak-unlatched.teak -o teak
fi

RUNTIME=`teak-config`/share/teak/runtime/verilog

EXAMPLE_CELLS=${RUNTIME}/example.v

OUTPUT=/tmp/tmp-sim.$RANDOM.out

cver +suppress_warns+3107+3108 +define+DUT=teak__spaHarvard_V5T +define+TECHFILE='"'${EXAMPLE_CELLS}'"' \
	+define+TEAK \
	+define+NO_ACTIVATE \
	+define+NLSTFILE='"/tmp/teak_tmp/loopbufferingteakfilesmapped/teak_toplevel.v"' +define+RESET \
	+define+PROGFILE='"/tmp/teak_tmp/nanoSpaTeak/tests/tmp.hex"' \
	+define+DUMPFILE='"/tmp/a.vcd"' +define+DUMPVARS=0 /tmp/teak_tmp/nanoSpaTeak/balsa/test-spaHarvardT.v \
	${RUNTIME}/monitors.v \
	${RUNTIME}/runtime.v #> $OUTPUT
	
#grep  -c PASS $OUTPUT
