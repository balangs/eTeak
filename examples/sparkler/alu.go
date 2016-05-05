/*(--
	Sparkler Balsa SPARC description

	2007-05-07 Andrew Bardsley
--)

import [types]
import [debug]
import [shifter]
*/

package main
//import "fmt"


/*
procedure Alu (
	input op : AluOp;
	output result : Word;
	output flags : Flags;
	input lhs, rhs : Word
) is
*/

/*
func Alu (op chan AluOp,
	result chan Word,
	flags chan Flags,
	lhs chan Word
	rhs chan Word) {

	postRhs chan word
	addCarryIn chan bool
	shift chan ShiftOp
	shiftDist chan byte
	shiftResult chan Word
	shiftArg chan Word
*/

/*
*
	channel postRhs : Word
	channel addCarryIn : bit

	channel shift : ShiftOp
	channel shiftDist : 5 bits
	channel shiftResult : Word
	channel shiftArg : Word
begin
	Shifter (shift, shiftDist, shiftResult, shiftArg) ||
	loop
		op, lhs, rhs -> then
			case op.kind of
			  sub, subcc, subx, subxcc then postRhs <- not rhs
			else postRhs <- rhs
			end ||
			case op.kind of
			  sub, subcc then addCarryIn <- 1
			| addx, addxcc then addCarryIn <- op.c
			| subx, subxcc then addCarryIn <- not op.c
			else addCarryIn <- 0
			end ||
			postRhs, addCarryIn -> then
				local
					type PartResult is record result : Word; carry : bit end
					channel logicResult : Word
					channel addResult, mergedResult : PartResult

					function AddArgPack (carry : bit; arg : Word) = (#carry @ #arg as 33 bits)
				begin
					begin
						if debug then print "Operating on ", Hex (lhs), " ", Hex (postRhs) end;
						case op.kind of
						  add, addcc, addx, addxcc, subcc, subx, subxcc then
							addResult <- (#(AddArgPack (addCarryIn, lhs) +
								AddArgPack (addCarryIn, postRhs))[33..1] as PartResult)
						| and_, andcc, andn, andncc then logicResult <- lhs and rhs
						| or_, orcc, orn, orncc then logicResult <- lhs or rhs
						| xor_, xorcc, xnor, xnorcc then logicResult <- lhs xor rhs
						| sll then shift <- {left, 0}
						| srl then shift <- {right, 0}
						| sra then shift <- {right, #postRhs[31]}
						else continue
						end ||
						case op.kind of
						  sll, srl, sra then
							shiftDist <- (postRhs as 5 bits) || shiftArg <- lhs
						else continue
						end
					end ||
					select
					  addResult then mergedResult <- addResult
					| logicResult then mergedResult <- {logicResult, 0}
					| shiftResult then mergedResult <- {shiftResult, 0}
					end ||
					mergedResult -> then
						result <- mergedResult.result ||
						if debug then
							print "RESULT: ", mergedResult.result, " =0: ", mergedResult.result = 0
						end ||
						flags <- {
							(-- z --) mergedResult.result = 0,
							(-- n --) #(mergedResult.result)[31],
							(-- v --) mergedResult.carry xor (#(mergedResult.result)[31]
								xor (#lhs[31] xor #postRhs[31])),
							(-- c --) mergedResult.carry /= (op.kind = sub)
						}
					end
				end
			end
		end
	end
end
*/


