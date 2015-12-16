package main

type bits struct {
	uint
}

type bits_1 struct { bits }
func (b *bits_1) size() uint { return 1 }

//-- Common type declarations
//type byte is 8 bits


//type nybble is 4 bits
//type nibble is nybble
//-- Signed types
//type sbyte is 8 signed bits
type sbyte int8

//type snybble is 4 signed bits
//type snibble is snybble

//-- cardinal : Arbitrary parameterising natural number [0..n]
//type cardinal is 32 bits
type cardinal uint32

//-- long : Longer number for indexing into large structures
//type long is 64 bits
type long uint64

//-- Constants for true and false in type bit
//constant true = 1 : bit
//constant false = 0 : bit



