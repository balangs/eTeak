`timescale 1ns/1ps

`define prop_delay (0.1)

module AND2 (o, a, b);
    output o;
    input a, b;
    and #(`prop_delay,`prop_delay) (o, a, b);
endmodule

module AND3 (o, a, b, c);
    output o;
    input a, b, c;
    and #(`prop_delay,`prop_delay) (o, a, b, c);
endmodule

module NAND2 (o, a, b);
    output o;
    input a, b;
    nand #(`prop_delay,`prop_delay) (o, a, b);
endmodule

module NAND3 (o, a, b, c);
    output o;
    input a, b, c;
    nand #(`prop_delay,`prop_delay) (o, a, b, c);
endmodule

module OR2 (o, a, b);
    output o;
    input a, b;
    or #(`prop_delay,`prop_delay) (o, a, b);
endmodule

module OR3 (o, a, b, c);
    output o;
    input a, b, c;
    or #(`prop_delay,`prop_delay) (o, a, b, c);
endmodule

module NOR2 (o, a, b);
    output o;
    input a, b;
    nor #(`prop_delay,`prop_delay) (o, a, b);
endmodule

module NOR3 (o, a, b, c);
    output o;
    input a, b, c;
    nor #(`prop_delay,`prop_delay) (o, a, b, c);
endmodule

module AO22 (o, a1, a2, b1, b2);
    output o;
    input a1, a2, b1, b2;
    wire a, b;
    and (a, a1, a2);
    and (b, b1, b2);
    or #(`prop_delay,`prop_delay) (o, a, b);
endmodule

module AO222 (o, a1, a2, b1, b2, c1, c2);
    output o;
    input a1, a2, b1, b2, c1, c2;
    wire a, b, c;
    and (a, a1, a2);
    and (b, b1, b2);
    and (c, c1, c2);
    or #(`prop_delay,`prop_delay) (o, a, b, c);
endmodule

module BUFF (o, i);
    output o;
    input i;
    assign o = i;
endmodule

module INV (o, i);
    output o;
    input i;
    not #(`prop_delay,`prop_delay) (o, i);
endmodule

module GND (o);
    output o;
    supply0 gnd;
    assign o = gnd;
endmodule

module C2 (o, a, b);
    output o;
    input a, b;
    UDP_C2 #(`prop_delay,`prop_delay) (o, a, b);
endmodule

module C2R (o, a, b, r);
    output o;
    input a, b, r;
    UDP_C2R #(`prop_delay,`prop_delay) (o, a, b, r);
endmodule

module AC2 (o, a, u); // FIXME, change to C1U1
    output o;
    input a, u;
    UDP_C1U1 #(`prop_delay,`prop_delay) (o, a, u);
endmodule

module C3 (o, a, b, c);
    output o;
    input a, b, c;
    UDP_C3 #(`prop_delay,`prop_delay) (o, a, b, c);
endmodule

module MUTEX (ar, br, ag, bg);
    input ar, br;
    output ag, bg;
    // FIXME

    // assign ag = ar;
    // assign bg = br;
    UDP_mutex_top_half #(`prop_delay, `prop_delay) (ag, ar, br, bg);
    UDP_mutex_bottom_half #(`prop_delay, `prop_delay) (bg, br, ar, ag);
endmodule

primitive UDP_C2 (o, a, b);
    output o;
    input a, b;
    reg o;

    table /*
        a b : o : o' */
        1 1 : ? : 1;
        0 0 : ? : 0;
        1 0 : ? : -;
        0 1 : ? : -;
    endtable
endprimitive

primitive UDP_C1U1 (o, a, u);
    output o;
    input a, u;
    reg o;

    table /*
        a u : o : o' */
        1 1 : ? : 1;
        0 ? : ? : 0;
        1 0 : ? : -;
    endtable
endprimitive

primitive UDP_C2R (o, a, b, r);
    output o;
    input a, b, r;
    reg o;

    table /*
        a b r : o : o' */
        1 1 0 : ? : 1;
        0 0 0 : ? : 0;
        1 0 0 : ? : -;
        0 1 0 : ? : -;
        ? ? 1 : ? : 0;
    endtable
endprimitive

primitive UDP_C3 (o, a, b, c);
    output o;
    input a, b, c;
    reg o;

    table /*
        a b c : o : o' */
        1 1 1 : ? : 1;
        0 0 0 : ? : 0;
        1 0 0 : ? : -;
        1 1 0 : ? : -;
        0 1 0 : ? : -;
        0 1 1 : ? : -;
        0 0 1 : ? : -;
        1 0 1 : ? : -;
    endtable
endprimitive

/*
module top;
    reg a, b;
    wire o;

    C2 I0 (o, a, b);

    initial begin
        $monitor ("%t a = %x b = %x o = %x", $time, a, b, o);
        a = 0; b = 0;
        #1 a = 1; b = 0;
        #1 a = 1; b = 1;
        #1 a = 1; b = 0;
        #1 a = 0; b = 0;
    end
endmodule
*/

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
