#!/bin/sh

set -x

TOP=encode_test
IN=encode

OPTS="-O -q soom"

rm ${IN}.v
teak -v -L -l simple=1 --gates ${OPTS} -t ${TOP} --test-protocol ${IN}

RUNTIME=`teak-config`/share/teak/runtime/verilog

cver +define+DUT=teak_${TOP} ${IN}.v \
	${RUNTIME}/runtime.v ${RUNTIME}/top.v ${RUNTIME}/example.v \
	${RUNTIME}/monitors.v

# cver +define+HAS_GO +define+HAS_DONE +define+DUT=teak_${TOP} ${IN}.v \
#	${RUNTIME}/runtime.v ${RUNTIME}/top.v ${RUNTIME}/example.v \
#	${RUNTIME}/monitors.v
