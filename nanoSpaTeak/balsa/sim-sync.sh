#!/bin/sh

PATH=/home/amulinks/balsa/linux/4.0/bin:${PATH}
export PATH

balsa-c -c no-eager-inputs -b -c warn-multicast nanoSpaTypes
balsa-c -c no-eager-inputs -b -c warn-multicast nanoFetch
balsa-c -c no-eager-inputs -b -c warn-multicast nanoLSMcontrol
balsa-c -c no-eager-inputs -b -c warn-multicast pipelineRegister
balsa-c -c no-eager-inputs -b -c warn-multicast nanoExecuteSupport
balsa-c -c no-eager-inputs -b -c warn-multicast nanoDecode
balsa-c -c no-eager-inputs -b -c warn-multicast nanoRegBank
balsa-c -c no-eager-inputs -b -c warn-multicast nanoAlu
balsa-c -c no-eager-inputs -b -c warn-multicast nanoShifter
balsa-c -c no-eager-inputs -b -c warn-multicast nanoExecuteControl
balsa-c -c no-eager-inputs -b -c warn-multicast nanoMultSupport
balsa-c -c no-eager-inputs -b -c warn-multicast nanoMBoothR3rolled
balsa-c -c no-eager-inputs -b -c warn-multicast nanoMBoothR3unrolled
balsa-c -c no-eager-inputs -b -c warn-multicast nanoMultiplier
balsa-c -c no-eager-inputs -b -c warn-multicast nanoExecute
balsa-c -c no-eager-inputs -b -c warn-multicast nanoSpaHarvard
balsa-c -c no-eager-inputs -b -c warn-multicast nanoSpa
balsa-c -c no-eager-inputs -b -c warn-multicast sim

FULLTECH=example/sync
INCL=

balsa-netlist -X ${FULLTECH} -s -v sim
balsa-make-impl-test -b -X ${FULLTECH} -o top.v -D /tmp/nanoSpa.vcd --dump-depth 2 sim test
# balsa-make-impl-test -b -X ${FULLTECH} -o top.v -D /tmp/nanoSpa.vcd sim test
balsa-verilog-sim ${INCL} -s cver -D file ../tests/hello.hex -B sim -f top.v
