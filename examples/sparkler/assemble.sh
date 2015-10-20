#!/bin/sh

if [ $# != 1 ]; then
	echo 'usage: assemble <file>'
fi

FILE=$1
BASE=${FILE%%.s}
HEXDUMP=../../bin/hexdump

if test ! -x ${HEXDUMP};
then
	gcc -o ${HEXDUMP} ${HEXDUMP}.c
fi

cat > ${BASE}.ld <<EOF
MEMORY
{
	MEM (xrw) : ORIGIN = 0, LENGTH = 16K
}

SECTIONS
{
    .text :
    {
	    . = ALIGN(4);

	   	_start = .;
        *(.text)                   /* remaining code */
    } >MEM
}
EOF

sparc-linux-as -o ${BASE}.o ${FILE}
sparc-linux-ld -o ${BASE}.elf ${BASE}.ld ${BASE}.o
sparc-linux-objcopy -O binary ${BASE}.elf ${BASE}.bin
sparc-linux-objdump -d ${BASE}.elf > ${BASE}.lst
${HEXDUMP} -a -b < ${BASE}.bin > ${BASE}.hex
