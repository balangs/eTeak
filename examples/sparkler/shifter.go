/*(--
	Sparkler Balsa SPARC description

	2007-05-07 Andrew Bardsley
--)

import [types]
*/

package main

/*
type ShiftDirection is enumeration left, right end

type ShiftOp is record
	direction : ShiftDirection;
	fill : bit
end
*/

type ShiftDirection int
const (
	left ShiftDirection = iota
	right
)

type ShiftOp struct {
	direction ShiftDirection
	fill bool
}

/*
procedure Shifter (
	input shift : ShiftOp;
	input distanceI : 5 bits;
	output result : Word;
	input arg : Word
) is
begin
*/

func Shifter (
	shift chan ShiftOp,
	distanceI chan byte,
	result chan Word,
	arg chan Word) {
/*
	loop
		distanceI, shift -> then
			local
				procedure shift_n (
					parameter distanceBit : cardinal;
					parameter distance : cardinal;
					input i : Word;
					output o : Word
				) is
*/
	dI := <-distanceI
	s := <-shift

	shift_n := func (distanceBit cardinal, distance cardinal, i <-chan Word, o chan<- Word) {
/*
				local
					constant remaining = 32 - distance

					function PackWordLeft (lsw : distance bits; msw : remaining bits) = (#lsw @ #msw as Word)
					function PackWordRight (lsw : remaining bits; msw : distance bits) = (#lsw @ #msw as Word)

					channel c : Word
*/

		remaining := 32 - distance
//		PackWordLeft := func (lsw Word, msw Word) Word {lsw._bitslice(distance), 
	}
/*
					procedure shift_body (
						output o : Word
					) is
					begin
						i -> then
							local
								function i_lswLeft = (#i[remaining-1..0] as remaining bits)
								function i_mswRight = (#i[31..distance] as remaining bits)
							begin
								if #distanceI[distanceBit] then
									case shift of
									  {left, ?} then o <- PackWordLeft (0, i_lswLeft ())
									| {right, 0} then o <- PackWordRight (i_mswRight (), 0)
									| {right, 1} then o <- PackWordRight (i_mswRight (), ( -1 as distance bits))
									end
								else o <- i
								end
							end
						end
					end
				begin
					if distance > 1 then
						shift_n (distanceBit - 1, distance / 2, c, o) ||
						shift_body (c)
					else
						shift_body (o)
					end
				end
			begin
				shift_n (4, 16, arg, result)
			end
		end
	end
end
*/

}

