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
module ectrl(

 input iv_l,
 output is_l,

 output ov_r,
 input os_r,

 output Es,
 output Em,

 input clk,
 input reset

);

wire ns_l ;
wire L_11 ;
wire L_12 ;
wire L_13 ;
wire L_14 ;
wire L_15 ;
wire L_16 ;
wire nL_14 ;


INV I0 (ns_l, is_l);
INV I1 (nL_14, L_14);

AND2 I2 (Em, iv_l, ns_l);
AND2 I3 (Es, L_12, nL_14);

AND2 I4 (L_13, os_r, ov_r);
AND2 I5 (L_16, L_12, L_14);

OR2 I6 (L_11, iv_l, is_l);
OR2 I7 (L_15, L_12, L_14);

SRLOR_L I8 (L_12, L_11, clk, reset);
SRLOR_L I9 (L_14, L_13, clk, reset);

SRLOR_H I10 (ov_r, L_15, clk, reset);
SRLOR_H I11 (is_l, L_16, clk, reset);

endmodule
