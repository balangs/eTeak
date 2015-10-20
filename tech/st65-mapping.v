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
	st65-mapping.v : Teak tech mapping for ST 65nm library
*/

/*
HS65_LS_AND2X4 (Z, A, B, C);
HS65_LS_AND3X4 (Z, A, B, C);
HS65_LS_AO222X4 (Z, A, B, C, D, E, F);
HS65_LS_AO22X4 (Z, A, B, C, D);
HS65_LS_BFX2 (Z, A);
HS65_LS_IVX2 (Z, A);
HS65_LS_NAND2X2 (Z, A, B);
HS65_LS_NAND3X2 (Z, A, B, C);
HS65_LS_NOR2X2 (Z, A, B);
HS65_LS_NOR3X1 (Z, A, B, C);
HS65_LS_OR2X4 (Z, A, B);
HS65_LS_OR3X4 (Z, A, B, C);
HS65_LS_PAO2X4 (Z, A, B, P);
HS65_LS_PAO3X4 (Z, A, B, C, P);
*/

// tkg_{and,or,nand,nor}[23] : 2/3 input AND/OR/NAND/NOR gates
module tkg_and2 (output o, input i0, i1);
	HS65_LS_AND2X4 I0 (o, i0, i1);
endmodule
module tkg_and3 (output o, input i0, i1, i2);
	HS65_LS_AND3X4 I0 (o, i0, i1, i2);
endmodule
module tkg_or2 (output o, input i0, i1);
	HS65_LS_OR2X4 I0 (o, i0, i1);
endmodule
module tkg_or3 (output o, input i0, i1, i2);
	HS65_LS_OR3X4 I0 (o, i0, i1, i2);
endmodule
module tkg_nand2 (output o, input i0, i1);
	HS65_LS_NAND2X2 I0 (o, i0, i1);
endmodule
module tkg_nand3 (output o, input i0, i1, i2);
	HS65_LS_NAND3X2 I0 (o, i0, i1, i2);
endmodule
module tkg_nor2 (output o, input i0, i1);
	HS65_LS_NOR2X2 I0 (o, i0, i1);
endmodule
module tkg_nor3 (output o, input i0, i1, i2);
	HS65_LS_NOR3X1 I0 (o, i0, i1, i2);
endmodule

// tkg_c[23] : 2/3 input symmetric C-elements
module tkg_c2 (output o, input i0, i1);
	HS65_LS_PAO2X4 I0 (o, i0, i1, o);
endmodule
module tkg_c3 (output o, input i0, i1, i2);
	HS65_LS_PAO3X4 I0 (o, i0, i1, i2, o);
endmodule
// tkg_c2r1 : 2 input symmetric C-element with active high reset
module tkg_c2r1 (output o, input i0, i1, r);
	// tkg_FIXME
endmodule
// tkg_c1u1 : asymmetric C-element with one 'symmetric' and one 'up' input
module tkg_c1u1 (output o, input s0, u0);
	HS65_LS_AO22X1 I0 (o, s0, u0, s0, o);
endmodule

// tkg_ao22 : AND-OR-22.  o = i0&i1 | i2&i3
module tkg_ao22 (output o, input i0, i1, i2, i3);
	HS65_LS_AO22X1 I0 (o, i0, i1, i2, i3);
endmodule
// tkg_ao222 : AND-OR-222.  o = i0&i1 | i2&i3 | i4&i5
module tkg_ao222 (output o, input i0, i1, i2, i3, i4, i5);
	HS65_LS_AO222X2 I0 (o, i0, i1, i2, i3, i4, i5);
endmodule

// tkg_gnd : logic 0 connection
module tkg_gnd (output o);
	// tkg_FIXME
	HS65_LS_GND I0 (o);
endmodule

// tkg_inv : inverter
module tkg_inv (output o, input i);
	HS65_LS_IVX2 I0 (o, i);
endmodule

// tkg_buff : non-inverting logical buffer
module tkg_buff (output o, input i);
	HS65_LS_BFX2 I0 (o, i);
endmodule

// tkg_mutex : mutual exclusion element.  ag&bg is never true.
module tkg_mutex (input ar, br, output ag, bg);
	// tkg_FIXME
	MUTEX I0 (ar, br, ag, bg);
endmodule
