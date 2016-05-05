package main

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


func smashU32(v uint32) []bool {
	var bits []bool
	for i:=31; i>=0; i-- {
		b := (v & uint32(1<<uint(i))) != 0
		bits = append(bits,bool(b))
	}
	return bits
}


func smashI32(v int32) []bool {
	return smashU32(uint32(v))
}

func unsmashU32(bits []bool) uint32 {
	ret := uint32(0);
	off := uint(len(bits))
	if (off > 32) { off = 32 }

	for _,b := range bits {
		off--
		if b {
			ret = ret | (1 << off)
		}
	}
	return ret
}

func unsmashI32(bits []bool) int32 {
	return int32(unsmashU32(bits))
}
