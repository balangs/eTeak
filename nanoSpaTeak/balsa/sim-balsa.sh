#!/bin/sh

PATH=/home/amulinks/balsa/linux/4.0/bin:${PATH}
export PATH

DO_COMPILE=yes
if [ $# != 0 ]; then
	if [ $1 = "no-compile" ]; then
		DO_COMPILE=no
	fi
fi

set -xe

if [ ${DO_COMPILE} = yes ]; then
	make clean
	make
	balsa-c nanoSpaHarvard_deparameterised
	balsa-netlist -X example/dual_b -s -n net nanoSpaHarvard_deparameterised
	cp nanoSpaHarvard_deparameterised.v balsa.v
fi

for file in memoryDualPort.v dummyCoproIrqFiq.v; do
	rm ${file}
	ln -s ../tests/${file} ${file}
done

EXAMPLE_CELLS=balsa-example-cells.v

cver +define+DUT=teak__spaHarvard_V5T +define+TECHFILE='"'${EXAMPLE_CELLS}'"' \
	+define+NLSTFILE='"balsa.v"' +define+RESET +define+PROGFILE='"../tests/dhry.hex"' \
	+define+DUMPFILE='"/tmp/sim-balsa.vcd"' +define+DUMPVARS=1 ../tests/test-spaHarvard.v
