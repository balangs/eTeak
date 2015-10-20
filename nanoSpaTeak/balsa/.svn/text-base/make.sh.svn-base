#!/bin/sh

SBUF=1
OPTS=
IMPL=teak
TECH=example

if [ $# != 0 ]; then
	IMPL=$1
	shift
fi
if [ $# != 0 ]; then
	SBUF=$1
	shift
fi
if [ $# != 0 ]; then
	OPTS=$1
	shift
fi
if [ $# != 0 ]; then
	TECH=$1
	shift
fi
if [ $# != 0 ]; then
	echo 'usage: make.sh {<impl>} {<sbuf>} {<opts>} {<tech>}'
	exit 1
fi

# case $# in
# 	3)
# 		IMPL=$1
# 		SBUF=$2
# 		OPTS=$3
# 		;;
# 	1)
# 		IMPL=$1
# 		;;
# 	0)
# 		;;
# 	*)
# 		echo 'usage: make.sh {<impl>} {<sbuf>} {<opts>}'
# 		exit 1
# esac

echo IMPL=${IMPL}
echo SBUF=${SBUF}
echo OPTS=${OPTS}
echo TECH=${TECH}

set -x -e

case ${IMPL} in
	teak)
		TEAK=`which teak`
		TEAKDIR=`dirname ${TEAK}`
		INCL="-I ${TEAKDIR}/../share/teak"
		# teak -O --simple-buffer 3 sim
		OPTIM=-O
		if [ ${SBUF} != none ]; then
			teak ${OPTIM} --simple-buffer ${SBUF} sim
		else
			teak ${OPTIM} sim
		fi
		# teak -O sim
		#BUF=1
		# TECH=example/teak/Mbuf=${BUF}:Vbuf=${BUF}:Sbuf=${BUF}:Fbuf=${BUF}:Jbuf=${BUF}:Jstrong #:Jstrong
		FULLTECH=${TECH}/teak/${OPTS}
		;;
	teak_balsac)
		INCL="-I .."
		teak -i balsac sim
		FULLTECH=${TECH}
		;;
	balsa)
		INCL=
		balsa-c sim
		FULLTECH=${TECH}/dual_b
		;;
	*)
		echo Unrecognised implementation ${IMPL}
		exit 1
		;;
esac

# balsa-netlist -X ${FULLTECH} -s -v --with-completions --with-errors sim
balsa-netlist -X ${FULLTECH} -s -v sim
balsa-make-impl-test -b -X ${FULLTECH} -o top.v -D /tmp/nanoSpa.vcd --dump-depth 2 sim test
balsa-verilog-sim ${INCL} -s cver -D file ../tests/dhry.hex -B sim -f top.v
