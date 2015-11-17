# set -x

if [ $# == 0 ]; then
	echo 'Usage: '$0' {teak|pn|cver|veriwell|vcs|ncverilog|modelsim|iverilog}*'
	exit 0
fi

TOP=a
IN=a

OPTS="-O"

rm -f ${IN}.v
eTeak -L -l simple=1 --gates ${OPTS} -t ${TOP} --test-protocol ${IN}

RUNTIME=`eTeak-runtime`/verilog
export PATH
export LM_LICENSE_FILE
CADTOOLS=/home/cadtools5

while [ $# != 0 ]; do
	case $1 in
		teak)
			eTeak --simulate ${IN}
			;;
		pn)
			eTeak --simulate -s type=pn ${IN}
			;;
		cver)
			cver +define+DUT=teak_${TOP} ${IN}.v \
				+define+HAS_GO +define+HAS_DONE \
				+define+DUMPFILE='"/tmp/a-cver.vcd"' \
				${RUNTIME}/runtime.v ${RUNTIME}/top.v ${RUNTIME}/example.v \
				${RUNTIME}/monitors.v
			;;
		ncverilog)
			LM_LICENSE_FILE=${LM_LICENSE_FILE}:${CADTOOLS}/cds_2007/lic/license.teaching
			PATH=${CADTOOLS}/cds_2008_2009/ldv_2009/tools/inca/bin:${PATH}

			ncverilog +define+DUT=teak_${TOP} ${IN}.v \
				+define+HAS_GO +define+HAS_DONE \
				+define+DUMPFILE='"/tmp/a-ncverilog.vcd"' \
				${RUNTIME}/runtime.v ${RUNTIME}/top.v ${RUNTIME}/example.v \
				${RUNTIME}/monitors.v
			;;
		vcs)
			export VCS_HOME=${CADTOOLS}/synopsys/Y-2006.06-SP1
			LM_LICENSE_FILE=/home/cadtools5/synopsys/license/key_2010_cadmaster
			PATH=${VCS_HOME}/bin:${PATH}

			vcs +define+DUT=teak_${TOP} ${IN}.v \
				+define+HAS_GO +define+HAS_DONE \
				+define+DUMPFILE='"/tmp/a-vcs.vcd"' \
				${RUNTIME}/runtime.v ${RUNTIME}/top.v ${RUNTIME}/example.v \
				${RUNTIME}/monitors.v &&
			./simv
			;;
		modelsim)
			LM_LICENSE_FILE=${LM_LICENSE_FILE}:${CADTOOLS}/mgc/lic/license.dat
			PATH=${CADTOOLS}/mgc/lnx/modelsim6.3a/modeltech/bin:${PATH}

			vlib work
			vlog +define+DUT=teak_${TOP} ${IN}.v \
				+define+HAS_GO +define+HAS_DONE \
				+define+DUMPFILE='"/tmp/a-modelsim.vcd"' \
				${RUNTIME}/runtime.v ${RUNTIME}/top.v ${RUNTIME}/example.v \
				${RUNTIME}/monitors.v
			vsim -c -do "run 100000; quit" top tkr_global tkr_monitor_ctrl
			;;
		veriwell)
			PATH=/mnt/scratch/bardsley/verilog/bin:${PATH}

			veriwell +define+DUT=teak_${TOP} ${IN}.v \
				+define+HAS_GO +define+HAS_DONE \
				+define+DUMPFILE='"/tmp/a-veriwell.vcd"' \
				${RUNTIME}/runtime.v ${RUNTIME}/top.v ${RUNTIME}/example.v \
				${RUNTIME}/monitors.v
			;;
		iverilog)
			iverilog -D DUT=teak_${TOP} ${IN}.v \
				-D HAS_GO -D HAS_DONE \
				-D DUMPFILE='"/tmp/a-icarus.vcd"' \
				${RUNTIME}/runtime.v ${RUNTIME}/top.v ${RUNTIME}/example.v \
				${RUNTIME}/monitors.v &&
				./a.out
			;;
		*)
			echo Unrecognised option $1
			exit 1
			;;
	esac
	shift
done


