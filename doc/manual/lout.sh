#!/bin/bash

LOG=log
OLDLOG=oldlog
LOUT=lout

rm -f ${LOG} ${OLDLOG}

RUNCOUNT=0

until cmp ${LOG} ${OLDLOG} 2> /dev/null; do
	if [ -f ${LOG} ]; then mv -f ${LOG} ${OLDLOG}; fi
	RUNCOUNT=$((RUNCOUNT + 1));
	${LOUT} manual.lout > manual.ps 2> ${LOG}
	echo Run ${RUNCOUNT}, `cat ${LOG} | wc -l` error/warning lines
done

if [ `cat ${LOG} | wc -l` != "0" ]; then
	# rm -f manual.ps
	cat log
	# exit 1
fi
