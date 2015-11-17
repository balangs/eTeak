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

OPTS="-O"
# LATCHES="-L -l loop=1:after-v=1"
LATCHES="-L -l loop=1"

if [ ${DO_COMPILE} = yes ]; then
	rm -f teak-unlatched.teak teak.v

	eTeak -v ${OPTS} -t Sparkler2 -o teak-unlatched sparkler
fi

if [ ${DO_LATCH} = yes ]; then
	eTeak -v --gates --test-protocol ${LATCHES} -n teak-unlatched.teak -o teak
fi

# cver -s
if [ ${DO_SIM} = yes ]; then
	RUNTIME=`eTeak-runtime`/verilog

	cver \
		+define+TKR_STR_COUNT=10000 \
		${RUNTIME}/monitors.v \
		${RUNTIME}/runtime.v \
		${RUNTIME}/example.v \
		top.v \
		startup.v \
		teak.v
fi
