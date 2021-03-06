#!/bin/sh

DO_COMPILE=yes
DO_LATCH=yes
DO_SIM=yes

while [ $# != 0 ]; do
	case $1 in
	compile)
		DO_COMPILE=yes
		DO_LATCH=yes
		DO_SIM=yes
		;;
	latch)
		DO_COMPILE=no
		;;
	sim)
		DO_COMPILE=no
		DO_LATCH=no
		;;
	*) echo Unrecognised option $1
		exit 1
		;;
	esac
	shift
done

set -xe

#OPTS="-O -q stom"
OPTS="-O"
LATCHES="-L -l loop=1"

if [ ${DO_COMPILE} = yes ]; then
	rm -f teak-unlatched.teak teak.v

	eTeak -v ${OPTS} -t test -o teak-unlatched sim
fi

if [ ${DO_LATCH} = yes ]; then
	eTeak -v --gates --test-protocol ${LATCHES} -n teak-unlatched.teak -o teak
fi

if [ ${DO_SIM} = yes ]; then
	RUNTIME=`eTeak-runtime`/verilog

	#	+define+HAS_GO \
	#	+define+HAS_DONE \

	cver +define+DUT=teak_test \
		+define+TKR_STR_COUNT=10000 \
		${RUNTIME}/monitors.v \
		${RUNTIME}/runtime.v \
		${RUNTIME}/top.v \
		${RUNTIME}/example.v \
		startup.v \
		teak.v
fi
