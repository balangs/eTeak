/*(--
	Sparkler Balsa SPARC description

	2007-05-07 Andrew Bardsley
--)

import [types]
*/

package main

import "fmt"

func smashU8(v byte) []bool {
	var bits []bool
	for i := 7; i >= 0; i-- {
		b := (v & byte(1<<uint(i))) != 0
		bits = append([]bool{bool(b)}, bits...)
	}
	return bits
}

func smashU32(v uint32) []bool {
	var bits []bool
	for i := 31; i >= 0; i-- {
		b := (v & uint32(1<<uint(i))) != 0
		bits = append([]bool{bool(b)}, bits...)
	}
	return bits
}

func smashI32(v int32) []bool {
	return smashU32(uint32(v))
}

func unsmashU32(bits []bool) uint32 {
	ret := uint32(0)
	off := uint(0)

	for _, b := range bits {
		if b {
			ret = ret | (1 << off)
		}
		off++
	}
	return ret
}

func unsmashI32(bits []bool) int32 {
	return int32(unsmashU32(bits))
}

type ShiftDirection int

const (
	left ShiftDirection = iota
	right
)

type ShiftOp struct {
	direction ShiftDirection
	fill      bool
}

func PackWord(lsw uint32, leastBits int, msw uint32, mostBits int) uint32 {
	bitsL := smashU32(lsw)[leastBits:]
	bitsR := smashU32(msw)[(32 - mostBits):]
	return unsmashU32(append(bitsR, bitsL...))
}

func shift_n(dI byte, s ShiftOp, distanceBit uint32, distance uint32, i <-chan uint32, o chan<- uint32) {
	remaining := 32 - distance
	c := make(chan uint32)

	shift_body := func(o chan<- uint32) {
		_i := <-i
		if smashU8(dI)[distanceBit] {
			switch s.direction {
			case left:
				o <- PackWord(0, int(distance), _i, int(remaining))
			case right:
				if s.fill {
					o <- PackWord(_i, int(remaining), 0, int(distance))
				} else {
					o <- PackWord(_i, int(remaining), uint32(1<<32-1), int(distance))
				}
			}
		} else {
			o <- _i
		}
	}
	if distance > 1 {
		go shift_n(dI, s, distanceBit-1, distance/2, c, o)
		shift_body(c)
	} else {
		shift_body(o)
	}
}

func Shifter(
	shift <-chan ShiftOp,
	distanceI <-chan byte,
	result chan<- uint32,
	arg <-chan uint32) {

	for {
		dI := <-distanceI
		s := <-shift

		shift_n(dI, s, 4, 16, arg, result)
	}
}

func ShiftF(shift ShiftOp, distance byte, d uint32) uint32 {
	s := make(chan ShiftOp)
	distanceI := make(chan byte)
	r := make(chan uint32)
	arg := make(chan uint32)

	go Shifter(s, distanceI, r, arg)
	distanceI <- distance
	s <- shift
	arg <- d
	return <-r

}

func main() {
	var j = uint32(1<<32 - 1)
	for i := byte(0); i < 32; i++ {
		result := ShiftF(ShiftOp{right, true}, i, j)
		fmt.Printf("%d >> %d, Shift -> %d, %d\n", j, i, result, j<<i)
	}
	print()
}
