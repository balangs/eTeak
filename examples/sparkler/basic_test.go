package main

import (
	"testing"
	"fmt"
)

var _1 = true
var _0 = false

var tests = map[uint32][32]bool{
	0x00000013: { _0,_0,_0,_0, _0,_0,_0,_0, _0,_0,_0,_0, _0,_0,_0,_0,
			_0,_0,_0,_0, _0,_0,_0,_0, _0,_0,_0,_1, _0,_0,_1,_1 },
	0x10000000: { _0,_0,_0,_1, _0,_0,_0,_0, _0,_0,_0,_0, _0,_0,_0,_0,
			_0,_0,_0,_0, _0,_0,_0,_0, _0,_0,_0,_0, _0,_0,_0,_0 },
	0x80008000: { _1,_0,_0,_0, _0,_0,_0,_0, _0,_0,_0,_0, _0,_0,_0,_0,
			_1,_0,_0,_0, _0,_0,_0,_0, _0,_0,_0,_0, _0,_0,_0,_0 },
	0xAAAAAAAA: { _1,_0,_1,_0, _1,_0,_1,_0, _1,_0,_1,_0, _1,_0,_1,_0,
			_1,_0,_1,_0, _1,_0,_1,_0, _1,_0,_1,_0, _1,_0,_1,_0 },
}


func TestSmashU32(t *testing.T) {
	var tmp [32]bool
	for k,v := range tests {
		fmt.Printf("testing %x\n", k)
		copy(tmp[:],smashU32(k)[:])
		if tmp != v {
			t.Errorf("%x failed, gave %v, expecting %v", k, smashU32(k), v)
		}
		if len(smashU32(k))!=32 {
			t.Errorf("Wrong size array returned")
		}
	}
}

func TestUnsmashU32(t *testing.T) {
	for k,v := range tests {
		fmt.Printf("testing %x\n", k)
		tmp := unsmashU32(v[:])
		if tmp != k {
			t.Errorf("%v failed, gave %x, expecting %x", v, tmp, k)
		}
	}
}

