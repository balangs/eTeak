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
#LATCHES="-L -l loop=1:scp:scp-limit=0"
#LATCHES=""
LATCHES="-L -l before-i=3"

if [ ${DO_COMPILE} = yes ]; then
	rm -f teak-unlatched.teak shifter.v

	eTeak -v ${OPTS} -t Shifter -o teak-unlatched shifter
fi

if [ ${DO_LATCH} = yes ]; then
	eTeak -v --gates --test-protocol ${LATCHES} -n teak-unlatched.teak -o shifter
fi

if [ ${DO_SIM} = yes ]; then

	iverilog -D DUT=teak_Shifter -I`eTeak-runtime`/verilog  m_harness.v
fi
