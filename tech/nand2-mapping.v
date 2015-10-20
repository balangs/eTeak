/*
	Teak synthesiser for the Balsa language
	Copyright (C) 2007-2010 The University of Manchester

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

	Andrew Bardsley <bardsley@cs.man.ac.uk> (and others, see AUTHORS)
	School of Computer Science, The University of Manchester
	Oxford Road, MANCHESTER, M13 9PL, UK
*/
/*
	nand2-mapping.v : Teak tech mapping for a technology with only a 2-input NAND gate called nand2
*/

module tkg_nand2 (output o, input i0, i1);
	NAND2 I0 (o, i0, i1);
endmodule

module tkg_buff (output o, input i);
	BUFF I0 (o, i);
endmodule

module tkg_mutex (input ar, br, output ag, bg);
	MUTEX I0 (ar, br, ag, bg);
endmodule

// tkg_{and,or,nand,nor}[23] : 2/3 input AND/OR/NAND/NOR gates
module tkg_and2 (output o, input i0, i1);
	wire na01;
	nand2 I0 (na01, i0, i1);
	inv I1 (o, na01);
endmodule
module tkg_and3 (output o, input i0, i1, i2);
	wire a01;
	and2 I0 (a01, i0, i1);
	and2 I1 (o, a01, i2);
endmodule
module tkg_or2 (output o, input i0, i1);
	wire n0, n1;
	inv I0 (n0, i0);
	inv I1 (n1, i1);
	nand2 I2 (o, n0, n1);
endmodule
module tkg_or3 (output o, input i0, i1, i2);
	wire o01;
	or2 I0 (o01, i0, i1);
	or2 I1 (o, o01, i2);
endmodule
// module tkg_nand2 (output o, input i0, i1);
// endmodule
module tkg_nand3 (output o, input i0, i1, i2);
	wire a01;
	and2 I0 (a01, i0, i1);
	nand2 I1 (o, a01, i2);
endmodule
module tkg_nor2 (output o, input i0, i1);
	wire no;
	or2 I0 (no, i0, i1);
	inv I1 (o, no);
endmodule
module tkg_nor3 (output o, input i0, i1, i2);
	wire o01;
	or2 I0 (o01, i0, i1);
	nor2 I1 (o, o01, i2);
endmodule

// tkg_c[23] : 2/3 input symmetric C-elements
module tkg_c2 (output o, input i0, i1);
	ao222 I0 (o, i0, i1, i0, o, i1, o);
endmodule
module tkg_c3 (output o, input i0, i1, i2);
	wire c01;
	c2 I0 (c01, i0, i1);
	c2 I1 (o, c01, i2);
endmodule
// tkg_c2r1 : 2 input symmetric C-element with active high reset
module tkg_c2r1 (output o, input i0, i1, r);
	wire c01, nr;
	inv I0 (nr, r);
	ao222 I1 (c01, i0, i1, i0, o, i1, o);
	and2 I2 (o, c01, nr);
endmodule
// tkg_c1u1 : asymmetric C-element with one 'symmetric' and one 'up' input
module tkg_c1u1 (output o, input s0, u0);
	ao22 I0 (o, s0, u0, s0, o);
endmodule

// tkg_ao22 : AND-OR-22.  o = i0&i1 | i2&i3
module tkg_ao22 (output o, input i0, i1, i2, i3);
	wire na01, na23;
	nand2 I0 (na01, i0, i1);
	nand2 I1 (na23, i2, i3);
	nand2 I2 (o, na01, na23);
endmodule
// tkg_ao222 : AND-OR-222.  o = i0&i1 | i2&i3 | i4&i5
module tkg_ao222 (output o, input i0, i1, i2, i3, i4, i5);
	wire na01, na23, na45;
	nand2 I0 (na01, i0, i1);
	nand2 I1 (na23, i2, i3);
	nand2 I2 (na45, i4, i5);
	nand3 I3 (o, na01, na23, na45);
endmodule

// tkg_gnd : logic 0 connection
module tkg_gnd (output o);
/*
	wire no;
	and2 I0 (o, o, no);
	inv I1 (no, o);
	*/
	GND I0 (o);
endmodule

// tkg_inv : inverter
module tkg_inv (output o, input i);
	nand2 I0 (o, i, i);
endmodule

// tkg_buff : non-inverting logical buffer
// module tkg_buff (output o, input i);
// endmodule

// tkg_mutex : mutual exclusion element.  ag&bg is never true.
// module tkg_mutex (input ar, br, output ag, bg);
// endmodule
