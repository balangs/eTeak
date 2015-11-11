/*
	eTeak synthesiser for the Balsa language
	Copyright (C) 2012- The University of Manchester

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.

	Mahdi Jelodari <mamagham@cs.man.ac.uk> (and others, see AUTHORS)
	School of Computer Science, The University of Manchester
	Oxford Road, MANCHESTER, M13 9PL, UK

*/
module SRLOR_L(

output q,
input S,
input E,
input rst
);

wire n_0;
wire n_1;
wire E_;

wire q_ ;

AND2 Ix (E_, ~E, ~rst);
AND2 I0 (n_0, E_, S);
AND2 I1 (n_1, E_, ~S);

NOR2 I2 (q_, n_0, q);
NOR3 I3 (q, q_, n_1, rst);

endmodule
