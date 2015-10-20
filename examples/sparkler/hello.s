
	.section .text
	.globl _start
_start:
	sethi %hi(hello), %g1		/* 00_g1_100_(top 22 of LLC0) */
	ldsb [%g1+%lo(hello)], %g2	/* 11_g2_001001_g1_1_(low of LLC0) */
	or %g1, %lo(hello), %g3		/* 10_g3_000010_g1_1_(low of LLC0) */
	cmp	%g2, 0					/* subcc %g2, 0, %g0  10_g0_010100_g2_1_(0 as 13 bits) */
	be .LL9						/* 00_0_0001_010_(0x20, 8 instr) */
	ldub [%g1+%lo(hello)], %g2	/* 11_g2_000001_g1_1_(low of LLC0) */
	sethi %hi(outChar), %g4		/* 00_g4_100_(top 22 of 0x200) */
.LL4:
	add	%g3, 1, %g3				/* 10_g3_000000_g3_1_(1 as 13 bits) */
	ld [%g4+%lo(outChar)], %g1	/* 11_g1_000000_g4_1_(0x200 as 13 bits) */
	stb	%g2, [%g1]				/* 11_g2_000101_g1_0_00000000_g0 */
	ldsb [%g3], %g2				/* 11_g2_001001_g3_0_00000000_g0 */
	cmp	%g2, 0					/* subcc %g2, 0, %g0  10_g0_010100_g2_1_(0 as 13 bits) */
	bne	.LL4 					/* 00_0_1001_010_(-6 instr) */
	ldub [%g3], %g2				/* 11_g2_000001_g3_0_00000000_g0 */
.LL9:
	stb %g0, [%g0+0]
	nop

outChar:	
	.word 0x0000FFFF
hello:
	.asciz	"Hello, world\n\x9FK\0"
