#	eTeak synthesiser for the Balsa language
#	Copyright (C) 2012-2016 The University of Manchester

#	This program is free software: you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation, either version 3 of the License, or
#	(at your option) any later version.

#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.

#	You should have received a copy of the GNU General Public License
#	along with this program.  If not, see <http://www.gnu.org/licenses/>.

#	Mahdi Jelodari <m.j.1989@ieee.org> 
#	School of Computer Science, The University of Manchester
#	Oxford Road, MANCHESTER, M13 9PL, UK


#!/bin/sh

DO_COMPILE=yes
DO_LATCH=yes
DO_SIM=yes
DO_EVERY=yes
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
LATCHES="-L -l loop=1"

if [ ${DO_COMPILE} = no ]; then
	rm -f teak-unlatched.teak teak.v

	./eteak -v ${OPTS} -t Sparkler2 -o teak-unlatched sparkler
fi

if [ ${DO_LATCH} = no ]; then
	./eteak -v --gates --test-protocol ${LATCHES} -n teak-unlatched.teak -o teak
fi

if [ ${DO_EVERY} = no ]; then
	
        ./eteak -v -O -o teak --gates --test-protocol -L -l loop=1 sparkler
fi

# cver -s
if [ ${DO_SIM} = yes ]; then
	#RUNTIME=`teak-config`/share/teak/runtime/verilog
	#RUNTIME= /home/mahdi/Desktop/teak/runtime/verilog
	cver \
		+define+TKR_STR_COUNT=10000 \
		monitors.v \
		/home/mahdi/Desktop/teak/runtime/verilog/runtime.v \
		/home/mahdi/Desktop/teak/runtime/verilog/example.v \
		top.v \
		startup.v \
		teak.v
fi
