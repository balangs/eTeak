/*((--
	Sparkler Balsa SPARC description

	2007-05-07 Andrew Bardsley
--)
*/

package main

//import [balsa.types.basic]
import . "basic"

/*
(--
    31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9  8  7  6  5  4  3  2  1  0 
   .--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.
   |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   '--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'--'
   .-----.-----------------------------------------------------------------------------------------.
 1 |0  1 |  disp30                                                                                 | call
   '-----'-----------------------------------------------------------------------------------------'
   .-----.--------------.--------.-----------------------------------------------------------------.
2a |0  0 |  rd          |  op2   |  imm22                                                          | sethi, bicc,
   '-----'--------------'--------'-----------------------------------------------------------------' fbfcc, cbccc
   .-----.--.-----------.--------.-----------------------------------------------------------------.
2b |0  0 |a |  cond     |  op2   |  disp22                                                         | sethi, bicc,
   '-----'--'-----------'--------'-----------------------------------------------------------------' fbfcc, cbccc
   .-----.--------------.-----------------.--------------.--.-----------------------.--------------.
3a | op  |  rd          |  op3            |  rs1         |i |  asi                  |  rs2         |
   '-----'--------------'-----------------'--------------'--'-----------------------'--------------'
   .-----.--------------.-----------------.--------------.--.--------------------------------------.
3b | op  |  rd          |  op3            |  rs1         |i |  simm13                              |
   '-----'--------------'-----------------'--------------'--'--------------------------------------'
   .-----.--------------.-----------------.--------------.--------------------------.--------------.
3c | op  |  rd          |  op3            |  rs1         |  opf                     |  rs2         |
   '-----'--------------'-----------------'--------------'--------------------------'--------------'
--)
*/
//type Word is 32 bits
//type SWord is 32 signed bits -- signed word for sign extension uses
//type HalfWord is 16 bits
//type SHalfWord is 16 signed bits
type Word uint32
type SWord int32
type HalfWord uint16
type SHalfWord int16

//type Reg is 5 bits

type Reg byte

/*
type Cond is enumeration
	bn   = 0x0, tn   = 0x0,
	be   = 0x1, te   = 0x1,
	ble  = 0x2, tle  = 0x2,
	bl   = 0x3, tl   = 0x3,
	bleu = 0x4, tleu = 0x4,
	bcs  = 0x5, tcs  = 0x5,
	bneg = 0x6, tneg = 0x6,
	bvs  = 0x7, tvs  = 0x7,
	ba   = 0x8, ta   = 0x8,
	bne  = 0x9, tne  = 0x9,
	bg   = 0xa, tg   = 0xa,
	bge  = 0xb, tge  = 0xb,
	bgu  = 0xc, tgu  = 0xc,
	bcc  = 0xd, tcc  = 0xd,
	bpos = 0xe, tpos = 0xe,
	bvc  = 0xf, tvc  = 0xf
over 4 bits
*/

type Cond int
const (
	bn Cond = iota
	be Cond
	ble Cond
	bl Cond
	bleu Cond
	bcs Cond
	bneg Cond
	bvs Cond
	ba Cond
	bne Cond
	bg Cond
	bge Cond
	bgu Cond
	bcc Cond
	bpos Cond
	bvc Cond
)
const (
	tn Cond = iota
	te Cond
	tle Cond
	tl Cond
	tleu Cond
	tcs Cond
	tneg Cond
	tvs Cond
	ta Cond
	tne Cond
	tg Cond
	tge Cond
	tgu Cond
	tcc Cond
	tpos Cond
	tvc Cond
)

//type Op2 is enumeration
//	b = 2, sethi = 4
//over 3 bits

type Op2 int
const (
	b Op2 = 2
	sethi Op2 = 4
)

/*type Op3 is enumeration
	-- op = 2
	add    = 0x00, and_   = 0x01, or_    = 0x02, xor_   = 0x03,
	sub    = 0x04, andn   = 0x05, orn    = 0x06, xnor   = 0x07,
	addx   = 0x08,                umul   = 0x0A, smul   = 0x0B,
	subx   = 0x0C,                udiv   = 0x0E, sdiv   = 0x0F,
	addcc  = 0x10, andcc  = 0x11, orcc   = 0x12, xorcc  = 0x13,
	subcc  = 0x14, andncc = 0x15, orncc  = 0x16, xnorcc = 0x17,
	addxcc = 0x18,                umulcc = 0x1A, smulcc = 0x1B,
	subxcc = 0x1C,                udivcc = 0x1E, sdivcc = 0x1F,
	sll = 0x25, srl = 0x26, sra = 0x27,
	jmpl = 0x38,
	save = 0x3C, restore = 0x3D,
	-- op = 3
	ld = 0x00, ldub = 0x01, lduh = 0x02, ldd = 0x03,
	st = 0x04, stb  = 0x05, sth  = 0x06, std = 0x07,
	ldsb = 0x09, ldsh = 0x0A
over 6 bits
*/

type Op3 int
const (
	// op = 2
	add    Op3 = 0x00; and_   Op3 = 0x01; or_    Op3 = 0x02; xor_   Op3 = 0x03
	sub    Op3 = 0x04; andn   Op3 = 0x05; orn    Op3 = 0x06; xnor   Op3 = 0x07
	addx   Op3 = 0x08; umul   Op3 = 0x0A; smul   Op3 = 0x0B
	subx   Op3 = 0x0C; udiv   Op3 = 0x0E; sdiv   Op3 = 0x0F
	addcc  Op3 = 0x10; andcc  Op3 = 0x11; orcc   Op3 = 0x12; xorcc  Op3 = 0x13
	subcc  Op3 = 0x14; andncc Op3 = 0x15; orncc  Op3 = 0x16; xnorcc Op3 = 0x17
	addxcc Op3 = 0x18; umulcc Op3 = 0x1A; smulcc Op3 = 0x1B
	subxcc Op3 = 0x1C; udivcc Op3 = 0x1E; sdivcc Op3 = 0x1F
	sll  Op3 = 0x25, srl Op3 = 0x26, sra Op3 = 0x27,
	jmpl Op3 = 0x38,
	save Op3 = 0x3C, restore Op3 = 0x3D,
	// op Op3 = 3
	ld Op3 = 0x00, ldub Op3 = 0x01, lduh Op3 = 0x02, ldd Op3 = 0x03,
	st Op3 = 0x04, stb  Op3 = 0x05, sth  Op3 = 0x06, std Op3 = 0x07,
	ldsb Op3 = 0x09, ldsh Op3 = 0x0A
)

//-- constant Op3cc = (0b01xxxx as Op3)

/*type Inst is record
	low : 30 bits;
	op : 2 bits
end
*/

func (w *Word) _bitslice(offset int, size int) {
	return (w >> offset) & ((2^size)-1)
}

type Inst Word
func (w *Inst) low() { w._bitslice(0,30) }
func (w *Inst) op() { w._bitslice(30,2) }

type Inst1 is record
	disp30 : 30 bits;
	op : 2 bits
end

type Inst2 is record
	_ : 22 bits;
	op2 : Op2;
	rd : Reg;
	op : 2 bits
end

type Inst2a is record
	imm22 : 22 bits;
	op2 : Op2;
	rd : Reg;
	op : 2 bits
end

type Inst2b is record
	disp22 : 22 signed bits;
	op2 : 3 bits;
	cond : Cond;
	a : bit;
	op : 2 bits
end

type Inst3 is record
    _ : 14 bits;
	rs1 : Reg;
	op3 : Op3;
	rd : Reg;
	op : 2 bits
end

type Inst3a is record
	rs2 : Reg;
	asi : 8 bits;
	i : bit; -- 0
	rs1 : Reg;
	op3 : Op3;
	rd : Reg;
	op : 2 bits
end

type Inst3b is record
	simm13 : 13 bits;
	i : bit; -- 1
	rs1 : Reg;
	op3 : Op3;
	rd : Reg;
	op : 2 bits
end

type Inst3c is record
	rs2 : Reg;
	osf : 9 bits;
	rs1 : Reg;
	op3 : Op3;
	rd : Reg;
	op : 2 bits
end

type MemAccessDir is enumeration write, read end
type MemAccessKind is enumeration instruction, byte, halfword, word end

type MemLoad is record
	width : MemAccessKind;
	isSigned : bit
end

type MemAccess is record
	dir : MemAccessDir;
	kind : MemAccessKind
end

function PackWordHigh_imm22 (w : 22 bits) = ((0 as array 10 of bit) @ #w as Word)
function PackWord_disp30 (w : 30 bits) = ((0 as array 2 of bit) @ #w as Word)
function PackWord_disp22 (w : 22 signed bits) = ((((0 as array 2 of bit) @ #w as 24 signed bits) as SWord) as Word)
function PackWordLow_simm13 (w : 13 bits) = (((w as 13 signed bits) as SWord) as Word)

type Flags is record
	z, n, v, c : bit
end

type AluOp is record
	kind : Op3;
	c : bit
end

constant WindowCount = 1

if WindowCount = 1 then
	type Window is 1 bits
else
	type Window is log WindowCount bits
end

(--
constant WindowCount = 4
type Window is log WindowCount bits
--)

type FetchedPC is record
	pc : Word;
	newStream : bit
end

type FetchedInst is record
	inst : Inst;
	pc : FetchedPC
end

type ImmOrReg is enumeration immediate, register end

type InstKind is enumeration nop, branch, alu, load, loadd, store, stored end

type Mem is record
	dir : MemAccessDir;
	kind : MemAccessKind;
	isSigned : bit
end

type DecodedInst is record
	kind : InstKind;
	branchCond : Cond;
	rEn : array 3 of bit;
	rSel : array 3 of Reg;
	wEn : bit;
	wSel : Reg;
	aluOp : Op3;
	immRhs : ImmOrReg;
	imm : Word;
	memAccess : Mem
end

