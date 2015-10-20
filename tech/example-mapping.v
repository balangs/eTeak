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
	example-mapping.v : Teak tech mapping for the `example' technology
*/

// tkg_{and,or,nand,nor}[23] : 2/3 input AND/OR/NAND/NOR gates
module tkg_and2 (output o, input i0, i1);
	AND2 I0 (o, i0, i1);
endmodule
module tkg_and3 (output o, input i0, i1, i2);
	AND3 I0 (o, i0, i1, i2);
endmodule
module tkg_or2 (output o, input i0, i1);
	OR2 I0 (o, i0, i1);
endmodule
module tkg_or3 (output o, input i0, i1, i2);
	OR3 I0 (o, i0, i1, i2);
endmodule
module tkg_nand2 (output o, input i0, i1);
	NAND2 I0 (o, i0, i1);
endmodule
module tkg_nand3 (output o, input i0, i1, i2);
	NAND3 I0 (o, i0, i1, i2);
endmodule
module tkg_nor2 (output o, input i0, i1);
	NOR2 I0 (o, i0, i1);
endmodule
module tkg_nor3 (output o, input i0, i1, i2);
	NOR3 I0 (o, i0, i1, i2);
endmodule

// tkg_c[23] : 2/3 input symmetric C-elements
module tkg_c2 (output o, input i0, i1);
	C2 I0 (o, i0, i1);
endmodule
module tkg_c3 (output o, input i0, i1, i2);
	C3 I0 (o, i0, i1, i2);
endmodule
// tkg_c2r1 : 2 input symmetric C-element with active high reset
module tkg_c2r1 (output o, input i0, i1, r);
	C2R I0 (o, i0, i1, r);
endmodule
// tkg_c1u1 : asymmetric C-element with one 'symmetric' and one 'up' input
module tkg_c1u1 (output o, input s0, u0);
	AO22 I0 (o, s0, u0, s0, o);
endmodule

// tkg_ao22 : AND-OR-22.  o = i0&i1 | i2&i3
module tkg_ao22 (output o, input i0, i1, i2, i3);
	AO22 I0 (o, i0, i1, i2, i3);
endmodule
// tkg_ao222 : AND-OR-222.  o = i0&i1 | i2&i3 | i4&i5
module tkg_ao222 (output o, input i0, i1, i2, i3, i4, i5);
	AO222 I0 (o, i0, i1, i2, i3, i4, i5);
endmodule

// tkg_gnd : logic 0 connection
module tkg_gnd (output o);
	GND I0 (o);
endmodule

// tkg_inv : inverter
module tkg_inv (output o, input i);
	INV I0 (o, i);
endmodule

// tkg_buff : non-inverting logical buffer
module tkg_buff (output o, input i);
	BUFF I0 (o, i);
endmodule

// tkg_mutex : mutual exclusion element.  ag&bg is never true.
module tkg_mutex (input ar, br, output ag, bg);
	MUTEX I0 (ar, br, ag, bg);
endmodule
