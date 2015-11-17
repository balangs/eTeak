#!/bin/bash

if [ $# = 0 ]; then
	echo 'usage: try.sh <balsa-file-prefix>'
	exit 1
fi

FILE=$1
shift
FILE=${FILE%%.balsa}
BALSA_FILE=${FILE}.balsa
TEAK_FILE=${FILE}.teak
SOLN_FILE=${FILE}.report
PN_FILE=${FILE}.hl_net
MCI_FILE=${FILE}.mci
DOT_FILE=${FILE}.dot
MPSAT_LOG_FILE=${FILE}.mpsat.log

if ! [ -f ${BALSA_FILE} ]; then
	echo Can"'"t find Balsa file: ${BALSA_FILE}
	exit 1
fi

OPTIM=
LATCH=
# Default teak options
if [ $# = 0 ]; then
	OPTIM="-O"
	LATCH="-L -l loop=1"
fi

set -e

echo Making PN file with Teak
eTeak ${LATCH} ${OPTIM} -t top $* ${BALSA_FILE}
eTeak -S -s type=pn:no-simulate:write-pn-format=hl_net:write-pn=${PN_FILE} ${TEAK_FILE}
eTeak -S -s type=pn:no-simulate:write-pn-format=dot:write-pn=${DOT_FILE} ${TEAK_FILE}

# Ignore punf's error output for now
echo Running punf
if punf ${PN_FILE}; then
	:
else
	:
fi
echo

echo Running mpsat
mpsat -D ${MCI_FILE} | tee ${MPSAT_LOG_FILE}
echo

if grep -q SOLUTION ${MPSAT_LOG_FILE}; then
	time=0
	grep -A1 SOLUTION ${MPSAT_LOG_FILE} | tail -1 | tr '[,]' '[\012]' |
		grep 'L[0-9]*\.' | grep -v '\.[0-9][0-9]*\.' | sed 's/\.r$/ R/' | sed 's/\.a$/ SPACER/' | (
	while read line; do
		echo '#'$time $line;
		time=$((time + 1));
	done
	) > ${SOLN_FILE}

	echo Solution in ${SOLN_FILE}
	eTeak --gui -g show-time-window:time-step=1 -e ${SOLN_FILE} ${TEAK_FILE}
fi
