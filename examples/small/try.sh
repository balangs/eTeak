#!/bin/sh

if [ $# == 0 ]; then
	FILES=`ls -1 *.balsa`
else
	FILES=$*
fi

for a in ${FILES}; do
	FILE=a
	if ! [ -r ${a} ]; then
		if [ -r ${a}.balsa ]; then
			a=${a}.balsa
		else
			echo "Can't find file $a"
			exit 1
		fi
	fi
	if ! teak ${a} > log 2>&1 ; then
		cat $a | grep '^-- EXPECT' | sed 's/^-- EXPECT //' > expected
		if cmp expected log > /dev/null 2> /dev/null; then
			echo 'PASS (EXPECTED ERRORS)   '$a
		else
			echo 'FAIL (UNEXPECTED ERRORS) '$a
			diff log expected
		fi
	else
		    echo 'PASS (NO ERRORS)         '$a
	fi
	grep '^teak:' log
done
