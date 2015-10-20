#!/bin/sh

rm -f *.vcd
rm -f `ls *.v | egrep -v '(top\.v|startup\.v)'`
rm -f verilog.log
rm -f *.teak
rm -f *.report
rm -f *.ps *.pdf *.svg
rm -f *.breeze
rm -f *.tree*
