//
// amust-cells.v
// Amust cell library
//
// 31 Mar 2004, A Bardsley
// 
// Added c2rax1 and gnd modules for Teak compatibility
//
// 04 Aug 2009, L Tarazona
//

`timescale 1ns/1ps
`define gate_delay 0.090

module and2x1 (Z, A, B);
  output Z;
  input A, B;

  and #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module and2x2 (Z, A, B);
  output Z;
  input A, B;

  and #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module and2x4 (Z, A, B);
  output Z;
  input A, B;

  and #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module and3x1 (Z, A, B, C);
  output Z;
  input A, B, C;

  and #(`gate_delay, `gate_delay) I0 (Z, A, B, C);
endmodule

module and3x2 (Z, A, B, C);
  output Z;
  input A, B, C;

  and #(`gate_delay, `gate_delay) I0 (Z, A, B, C);
endmodule

module and3x4 (Z, A, B, C);
  output Z;
  input A, B, C;

  and #(`gate_delay, `gate_delay) I0 (Z, A, B, C);
endmodule

module ao21x1 (Z, A, B, C);
  output Z;
  input A, B, C;

  wire S;

  and U0 (S, A, B);
  or #(`gate_delay, `gate_delay) I0 (Z, S, C);
endmodule

module ao221x1 (Z, A, B, C, D, E);
  output Z;
  input A, B, C, D, E;

  wire S1, S2;

  and I0 (S1, A, B);
  and I1 (S2, C, D);
  or #(`gate_delay, `gate_delay) I2 (Z, S1, S2, E);
endmodule

module ao222x1 (Z, A, B, C, D, E, F);
  output Z;
  input A, B, C, D, E, F;

  wire S1, S2, S3;

  and I0 (S1, A, B);
  and I1 (S2, C, D);
  and I2 (S3, E, F);
  or #(`gate_delay, `gate_delay) I3 (Z, S1, S2, S3);
endmodule

module ao22x1 (Z, A, B, C, D);
  output Z;
  input A, B, C, D;

  wire S1, S2;

  and I0 (S1, A, B);
  and I1 (S2, C, D);
  or #(`gate_delay, `gate_delay) I2 (Z, S1, S2);
endmodule

module aoi21x1 (Z, A, B, C);
  output Z;
  input A, B, C;

  wire S;

  and I0 (S, A, B);
  nor #(`gate_delay, `gate_delay) I1 (Z, S, C);
endmodule

module aoi221x1 (Z, A, B, C, D, E);
  output Z;
  input A, B, C, D, E;

  wire S1, S2;

  and I0 (S1, A, B);
  and I1 (S2, C, D);
  nor #(`gate_delay, `gate_delay) I2 (Z, S1, S2, E);
endmodule

module aoi222x1 (Z, A, B, C, D, E, F);
  output Z;
  input A, B, C, D, E, F;

  wire S1, S2, S3;

  and I0 (S1, A, B);
  and I1 (S2, C, D);
  and I2 (S3, E, F);
  or #(`gate_delay, `gate_delay) I3 (Z, S1, S2, S3);
endmodule

module aoi22x1 (Z, A, B, C, D);
  output Z;
  input A, B, C, D;

  wire S0, S1;

  and I0 (S0, A, B);
  and I1 (S1, C, D);
  nor #(`gate_delay, `gate_delay) I2 (Z, S0, S1);
endmodule

module bufx1 (Z, A);
  output Z;
  input A;

  buf #(`gate_delay, `gate_delay) I1 (Z, A);
endmodule

module bufx2 (Z, A);
  output Z;
  input A;

  buf #(`gate_delay, `gate_delay) I1 (Z, A);
endmodule

module bufx4 (Z, A);
  output Z;
  input A;

  buf #(`gate_delay, `gate_delay) I1 (Z, A);
endmodule

module c1_2x1 (Z, D, S);
  output Z;
  input D, S;

  UDP_c2p #(`gate_delay, `gate_delay) U0 (Z, D, S);
endmodule

module c1_3ax1 (Z, A, D, S);
  output Z;
  input A, D, S;

  wire S0;

  or I0 (S0, A, D);
  UDP_c2p #(`gate_delay, `gate_delay) U0 (Z, S, S0);
endmodule

module c1_5x1 (Z, A, B, S);
  output Z;
  input A, B, S;

  wire S0;

  xnor I0 (S0, A, B);
  UDP_c2 #(`gate_delay, `gate_delay) U0 (Z, S, S0);
endmodule

module c2_1x1 (Z, D, S);
  output Z;
  input D, S;

  UDP_c2m #(`gate_delay, `gate_delay) U0 (Z, D, S);
endmodule

module c2_2ax1 (Z, Ci, Co, D, S);
  output Z;
  input Ci, Co, D, S;

  UDP_c3mpr #(`gate_delay, `gate_delay) U0 (Z, S, Ci, D, Co);
endmodule

module c2_2rx1 (Z, Ci, Co, D, S);
  output Z;
  input Ci, Co, D, S;

  UDP_c3mps #(`gate_delay, `gate_delay) U0 (Z, S, Ci, D, Co);
endmodule

module c2_2x1 (Z, Ci, D, S);
  output Z;
  input Ci, D, S;

  UDP_c3mp #(`gate_delay, `gate_delay) U0 (Z, S, Ci, D);
endmodule

module c2ax1 (Z, A, B, R);
  output Z;
  input A, B, R;

  UDP_c2r #(`gate_delay, `gate_delay) U0 (Z, A, B, R);
endmodule

module c2rax1 (Z, A, B, R);
  output Z;
  input A, B, R;

  wire pR;

  invx1 U0 (pR, R);
  c2ax1 U1 (Z, A, B, pR);
endmodule

module c2rx1 (Z, A, B, R);
  output Z;
  input A, B, R;

  UDP_c2s #(`gate_delay, `gate_delay) U0 (Z, A, B, R);
endmodule

module c2x1 (Z, A, B);
  output Z;
  input A, B;

  UDP_c2 #(`gate_delay, `gate_delay) U0 (Z, A, B);
endmodule

module c3_2ax1 (Z, Ci, Co, D, S);
  output Z;
  input Ci, Co, D, S;

  UDP_c3mr #(`gate_delay, `gate_delay) U0 (Z, D, S, Ci, Co);
endmodule

module c3_2rx1 (Z, Ci, Co, D, S);
  output Z;
  input Ci, Co, D, S;

  UDP_c3ms #(`gate_delay, `gate_delay) U0 (Z, D, S, Ci, Co);
endmodule

module c3_2x1 (Z, Ci, D, S);
  output Z;
  input Ci, D, S;

  UDP_c3m #(`gate_delay, `gate_delay) U0 (Z, D, S, Ci);
endmodule

module c3x1 (Z, A, B, C);
  output Z;
  input A, B, C;

  UDP_c3 #(`gate_delay, `gate_delay) U0 (Z, A, B, C);
endmodule

module demux2x1 (Q0, Q1, D, S);
  output Q0, Q1;
  input D, S;
  
  UDP_demux2_top_half #(`gate_delay, `gate_delay) U0 (Q0, D, S);
  UDP_demux2_bottom_half #(`gate_delay, `gate_delay) U1 (Q1, D, S);
endmodule

module dtypex1 (Q, QN, CD, CP, D);
  input D, CP, CD;
  output Q, QN;

  wire Q1;

  UDP_dff #(`gate_delay, `gate_delay) U0 (Q1, D, CP, CD);
  not #(`gate_delay, `gate_delay) I0 (QN, Q1);
  buf #(`gate_delay, `gate_delay) I1 (Q, Q1);
endmodule

module feedx1 (Z, A);
  output Z;
  input A;

  assign Z = A;
endmodule

module gnd (o);
	output o;
	supply0 gnd;
	assign o = gnd;
endmodule


module invx1 (Z, A);
  output Z;
  input A;

  not #(`gate_delay, `gate_delay) I0 (Z, A);
endmodule

module invx2 (Z, A);
  output Z;
  input A;

  not #(`gate_delay, `gate_delay) I0 (Z, A);
endmodule

module invx4 (Z, A);
  output Z;
  input A;

  not #(`gate_delay, `gate_delay) I0 (Z, A);
endmodule

module keepx1 (Z, A);
  output Z;
  input A;

  UDP_nkeep #(`gate_delay, `gate_delay) U0 (Z, A);
endmodule

module logic0 (zero);
  output zero;

  supply0 zero;
endmodule

module logic1 (one);
  output one;

  supply1 one;
endmodule

module mutex1 (G1, G2, R1, R2);
  output G1, G2;
  input R1, R2;

  UDP_mutex_top_half #(`gate_delay, `gate_delay) U0 (G1, R1, R2, G2);
  UDP_mutex_bottom_half #(`gate_delay, `gate_delay) U1 (G2, R2, R1, G1);
endmodule

module mux2x1 (Z, A, B, S);
  input A, B, S;
  output Z;
  
  UDP_mux2 #(`gate_delay, `gate_delay) U0 (Z, A, B, S);
endmodule

module nand2x1 (Z, A, B);
  output Z;
  input A, B;

  nand #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module nand2x2 (Z, A, B);
  output Z;
  input A, B;

  nand #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module nand3x1 (Z, A, B, C);
  output Z;
  input A, B, C;

  nand #(`gate_delay, `gate_delay) I0 (Z, A, B, C);
endmodule

module nand3x2 (Z, A, B, C);
  output Z;
  input A, B, C;

  nand #(`gate_delay, `gate_delay) I0 (Z, A, B, C);
endmodule

module nmux2x1 (Z, A, B, S);
  input A, B, S;
  output Z;

  wire S0;

  UDP_mux2 #(`gate_delay, `gate_delay) U0 (S0, A, B, S);
  not I0 (Z, S0);
endmodule

module nor2x1 (Z, A, B);
  output Z;
  input A, B;

  nor #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module nor2x2 (Z, A, B);
  output Z;
  input A, B;

  nor #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module nor3x1 (Z, A, B, C);
  output Z;
  input A, B, C;

  nor #(`gate_delay, `gate_delay) I0 (Z, A, B, C);
endmodule

module or2x1 (Z, A, B);
  output Z;
  input A, B;

  or #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module or2x2 (Z, A, B);
  output Z;
  input A, B;

  or #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module or2x4 (Z, A, B);
  output Z;
  input A, B;

  or #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module or3x1 (Z, A, B, C);
  output Z;
  input A, B, C;

  or #(`gate_delay, `gate_delay) I0 (Z, A, B, C);
endmodule

module or3x2 (Z, A, B, C);
  output Z;
  input A, B, C;

  or #(`gate_delay, `gate_delay) I0 (Z, A, B, C);
endmodule

module or3x4 (Z, A, B, C);
  output Z;
  input A, B, C;

  or #(`gate_delay, `gate_delay) I0 (Z, A, B, C);
endmodule

module sc1_2x1 (Z, Zb, A, B);
  output Z, Zb;
  input A, B;

  UDP_c2p #(`gate_delay, `gate_delay) U0 (Z, A, B);
  not I0 (Zb, Z);
endmodule

module sc2x1 (Z, A, B);
  output Z;
  input A, B;

  UDP_c2 #(`gate_delay, `gate_delay) U0 (Z, A, B);
endmodule

module sc3x1 (Z, A, B, C);
  output Z;
  input A, B, C;

  UDP_c3 #(`gate_delay, `gate_delay) U0 (Z, A, B, C);
endmodule

module selemx1 (Aa, Br, Ar, Ba);
  output Aa, Br;
  input Ar, Ba;

  wire S, NS;

  UDP_c2p U0 (S, Ar, Ba);
  and #(`gate_delay, `gate_delay) I0 (Br, Ar, NS);
  nor #(`gate_delay, `gate_delay) I1 (Aa, Ba, NS);
  not I2 (NS, S);
endmodule

module th23w2x1 (Z, A, B, C);
  output Z;
  input A, B, C;

  UDP_th23w2 #(`gate_delay, `gate_delay) U0 (Z, A, B, C);
endmodule

module th23x1 (Z, A, B, C);
  output Z;
  input A, B, C;

  UDP_th23 #(`gate_delay, `gate_delay) U0 (Z, A, B, C);
endmodule

module tlatchx1 (Q, D, G);
  output Q;
  input D, G;
  
  UDP_latch #(`gate_delay, `gate_delay) U0 (Q, G, D);
endmodule

module tribufx1 (Z, A, E);
  output Z;
  input A, E;
  
  bufif1 #(`gate_delay, `gate_delay) U0 (Z, A, E);
endmodule

module triinvx1 (Z, A, E);
  output Z;
  input A, E;

  notif1 #(`gate_delay, `gate_delay) U0 (Z, A, E); 
endmodule

module xnor2x1 (Z, A, B);
  output Z;
  input A, B;

  xnor #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

module xor2x1 (Z, A, B);
  output Z;
  input A, B;

  xor #(`gate_delay, `gate_delay) I0 (Z, A, B);
endmodule

primitive UDP_c2 (out, in0, in1);
  output out;
  input in0, in1;

  reg out;

  table
    // ,-------- in0
    // | ,------- in1
    // | |   ,---- out
    // | |   |   ,- out'
    // v v : v : v

       0 0 : ? : 0;
       0 ? : 0 : 0;
       ? 0 : 0 : 0;
       ? 1 : 1 : 1;
       1 ? : 1 : 1;
       1 1 : ? : 1;
       x x : x : x;
  endtable
endprimitive

primitive UDP_c2m (s, ins, inm);
  output s;
  input ins, inm;

  reg s;

  table
    // ,-------- ins
    // | ,------- inm
    // | |   ,---- s
    // | |   |   ,- s'
    // v v : v : v

       0 0 : ? : 0;
       1 ? : ? : 1;
       0 1 : ? : -;
       x x : x : x;
  endtable
endprimitive

primitive UDP_c2p (s, ins, inp);
  output s;
  input ins, inp;

  reg s;

  table
    // ,-------- ins
    // | ,------- inp
    // | |   ,---- s
    // | |   |   ,- s'
    // v v : v : v

       0 ? : ? : 0;
       1 0 : 0 : 0;
       1 ? : 1 : 1;
       1 1 : ? : 1;
       x x : x : x;
  endtable
endprimitive

primitive UDP_c2r (out, in0, in1, r0);
  output out;
  input in0, in1, r0;

  reg out;

  table
    // ,--------- in0
    // | ,-------- in1
    // | | ,------- r0
    // | | |   ,---- out
    // | | |   |   ,- out'
    // v v v : v : v

       ? ? 0 : ? : 0;
       0 0 1 : ? : 0;
       0 ? 1 : 0 : 0;
       ? 0 1 : 0 : 0;
       ? 1 1 : 1 : 1;
       1 ? 1 : 1 : 1;
       1 1 1 : ? : 1;
       x x x : x : x;
  endtable
endprimitive

primitive UDP_c2s (out, in0, in1, s0);
  output out;
  input in0, in1, s0;

  reg out;

  table
    // ,--------- in0
    // | ,-------- in1
    // | | ,------- s0
    // | | |   ,---- out
    // | | |   |   ,- out'
    // v v v : v : v

       ? ? 1 : ? : 1;
       0 0 0 : ? : 0;
       0 ? 0 : 0 : 0;
       ? 0 0 : 0 : 0;
       ? 1 0 : 1 : 1;
       1 ? 0 : 1 : 1;
       1 1 0 : ? : 1;
       x x x : x : x;
  endtable
endprimitive

primitive UDP_c3 (out, in0, in1, in2);
  output out;
  input in0, in1, in2;

  reg out;

  table
    // ,--------- in0
    // | ,-------- in1
    // | | ,------- in2
    // | | |   ,---- out
    // | | |   |   ,- out'
    // v v v : v : v

       0 0 0 : ? : 0;
       0 ? ? : 0 : 0;
       ? 0 ? : 0 : 0;
       ? ? 0 : 0 : 0;
       1 ? ? : 1 : 1;
       ? 1 ? : 1 : 1;
       ? ? 1 : 1 : 1;
       1 1 1 : ? : 1;
       x x x : x : x;
  endtable
endprimitive

primitive UDP_c3m (s, ins0, ins1, inm);
  output s;
  input ins0, ins1, inm;

  reg s;

  table
    // ,--------- ins0
    // | ,-------- ins1
    // | | ,------- inm
    // | | |   ,---- s
    // | | |   |   ,- s'
    // v v v : v : v

       0 0 0 : ? : 0;
       1 1 ? : ? : 1;
       0 0 1 : ? : -;
       0 1 ? : ? : -;
       1 0 ? : ? : -;
       x x x : x : x;
  endtable
endprimitive

primitive UDP_c3mp (s, ins, inm, inp);
  output s;
  input ins, inm, inp;

  reg s;

  table
    // ,--------- ins
    // | ,-------- inm
    // | | ,------- inp
    // | | |   ,---- s
    // | | |   |   ,- s'
    // v v v : v : v

       0 0 ? : ? : 0;
       1 ? 1 : ? : 1;
       0 1 ? : ? : -;
       1 ? 0 : ? : -;
       x x x : x : x;
  endtable
endprimitive

primitive UDP_c3mpr (s, ins, inm, inp, r0);
  output s;
  input ins, inm, inp, r0;

  reg s;

  table
    // ,---------- ins
    // | ,--------- inm
    // | | ,-------- inp
    // | | | ,------- r0
    // | | | |   ,---- s
    // | | | |   |   ,- s'
    // v v v v : v : v

       ? ? ? 0 : ? : 0;
       0 0 ? 1 : ? : 0;
       1 ? 1 1 : ? : 1;
       0 1 ? 1 : ? : -;
       1 ? 0 1 : ? : -;
       x x x x : x : x;
  endtable
endprimitive

primitive UDP_c3mps (s, ins, inm, inp, s0);
  output s;
  input ins, inm, inp, s0;

  reg s;

  table
    // ,---------- ins
    // | ,--------- inm
    // | | ,-------- inp
    // | | | ,------- s0
    // | | | |   ,---- s
    // | | | |   |   ,- s'
    // v v v v : v : v

       ? ? ? 1 : ? : 1;
       0 0 ? 0 : ? : 0;
       1 ? 1 0 : ? : 1;
       0 1 ? 0 : ? : -;
       1 ? 0 0 : ? : -;
       x x x x : x : x;
  endtable
endprimitive

primitive UDP_c3mr (s, ins0, ins1, inm, r0);
  output s;
  input ins0, ins1, inm, r0;

  reg s;

  table
    // ,---------- ins0
    // | ,--------- ins1
    // | | ,-------- inm
    // | | | ,------- r0
    // | | | |   ,---- s
    // | | | |   |   ,- s'
    // v v v v : v : v

       ? ? ? 0 : ? : 0;
       0 0 0 1 : ? : 0;
       1 1 ? 1 : ? : 1;
       0 0 1 1 : ? : -;
       0 1 ? 1 : ? : -;
       1 0 ? 1 : ? : -;
       x x x x : x : x;
  endtable
endprimitive

primitive UDP_c3ms (s, ins0, ins1, inm, s0);
  output s;
  input ins0, ins1, inm, s0;

  reg s;

  table
    // ,---------- ins0
    // | ,--------- ins1
    // | | ,-------- inm
    // | | | ,------- s0
    // | | | |   ,---- s
    // | | | |   |   ,- s'
    // v v v v : v : v

       ? ? ? 1 : ? : 1;
       0 0 0 0 : ? : 0;
       1 1 ? 0 : ? : 1;
       0 0 1 0 : ? : -;
       0 1 ? 0 : ? : -;
       1 0 ? 0 : ? : -;
       x x x x : x : x;
  endtable
endprimitive

primitive UDP_demux2_bottom_half (out, in, sel);
  output out;
  input in, sel;
  
  table
    // ,----- in
    // | ,---- sel
    // | |   ,- out
    // v v : v

       0 1 : 0;
       1 1 : 1;
       ? 0 : 0;
  endtable
endprimitive

primitive UDP_demux2_top_half (out, in, sel);
  output out;
  input in, sel;
  
  table
    // ,----- in
    // | ,---- sel
    // | |   ,- out
    // v v : v

       0 0 : 0;
       1 0 : 1;
       ? 1 : 0;
  endtable
endprimitive

primitive UDP_dff (q, d, cp, cd);
  output q;
  input d, cp, cd;

  reg q;

  table
    // ,--------------- cp
    // |    ,----------- d
    // |    |    ,------- cd
    // |    |    |   ,---- q
    // |    |    |   |   ,- q'
    // v    v    v : v : v
       ?    ?    0 : ? : 0;
       (01) 0    1 : ? : 0;
       (01) 1    1 : ? : 1;
       (0?) 1    1 : 1 : 1;
       (0?) 0    1 : 0 : 0;
       (?0) ?    1 : ? : -;
       ?    (??) 1 : ? : -;
  endtable
endprimitive

primitive UDP_latch (out, en, in);
  output out;
  input en, in;

  reg out;
 
  initial out = 1'b0;
 
  table
    // ,-------- en
    // | ,------- in
    // | |   ,---- out
    // | |   |   ,- out'
    // v v : v : v

       1 1 : ? : 1;
       1 0 : ? : 0;
       0 ? : ? : -; 
       x 0 : ? : -;
       x 1 : ? : -;
  endtable
endprimitive 

primitive UDP_mutex_bottom_half (G, R1, R2, G2state);
  output G;
  input R1, R2, G2state;

  reg G;
 
 table
    // ,--------- R1
    // | ,-------- R2
    // | | ,------- G2state
    // | | |   ,---- G
    // | | |   |   ,- G'
    // v v v : v : v

       0 ? ? : ? : 0;
       1 ? 1 : ? : 0;
       1 0 0 : ? : 1;
       1 x 0 : ? : 1;
       1 ? x : ? : 1;
       1 1 0 : 0 : 0;
       1 1 0 : 1 : 1;
       x x x : ? : x;
  endtable
endprimitive

primitive UDP_mutex_top_half (G, R1, R2, G2state);
  output G;
  input R1, R2, G2state;
 
  table
    // ,-------- R1
    // | ,------- R2
    // | | ,------ G2state
    // | | |   ,--- G
    // v v v : v

       0 ? ? : 0;
       1 ? 1 : 0;
       1 ? 0 : 1;
       1 ? x : 1;
       x x x : x;
  endtable
endprimitive

primitive UDP_mux2 (out, i0, i1, sel);
  output out;
  input i0, i1, sel;
  
  table
    // ,------ i0
    // | ,----- i1
    // | | ,---- sel
    // | | |   ,- out
    // v v v : v

       0 ? 0 : 0;
       1 ? 0 : 1;
       x ? 0 : x;
       ? 0 1 : 0;
       ? 1 1 : 1;
       ? x 1 : x;
       0 0 ? : 0;
       1 1 ? : 1;
  endtable
endprimitive

primitive UDP_nkeep (nout, in);
  output nout;
  input in;

  reg nout;

  initial nout = 1'b1; 
  
  table
    // ,------- in
    // |   ,---- nout
    // |   |   ,- nout'
    // v : v : v

       0 : ? : 1;
       1 : ? : 0;
       x : 1 : 1;
       x : 0 : 0;
       x : x : x;
  endtable
endprimitive

primitive UDP_th23 (out, in0, in1, in2);
  output out;
  input in0, in1, in2;

  reg out;

  table
    // ,--------- in0
    // | ,-------- in1
    // | | ,------- in2
    // | | |   ,---- out
    // | | |   |   ,- out'
    // v v v : v : v

       0 0 0 : ? : 0;
       0 ? ? : 0 : 0;
       ? 0 ? : 0 : 0;
       ? ? 0 : 0 : 0;
       1 ? ? : 1 : 1;
       ? 1 ? : 1 : 1;
       ? ? 1 : 1 : 1;
       1 1 x : ? : 1;
       1 x 1 : ? : 1;
       x 1 1 : ? : 1;
       x x x : x : x;
  endtable
endprimitive

primitive UDP_th23w2 (out, in0, in1, in2);
  output out;
  input in0, in1, in2;
 
  reg out;
 
  table
    // ,--------- in0
    // | ,-------- in1
    // | | ,------- in2
    // | | |   ,---- out
    // | | |   |   ,- out'
    // v v v : v : v

       0 0 0 : ? : 0;
       0 ? ? : 0 : 0;
       ? 0 ? : 0 : 0;
       ? ? 0 : 0 : 0;
       ? 1 ? : 1 : 1;
       ? ? 1 : 1 : 1;
       1 x x : ? : 1;
       x 1 1 : ? : 1;
       x x x : x : x;
  endtable
endprimitive
