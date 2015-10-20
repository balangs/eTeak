
	.section .text
	.globl _start
_start:
	mov r4, #-1

	adr r0, string
	bl puts
	adr r0, string2
	bl puts

	mov r4, #-2
	str r0, [r4]
	str r0, [r4]

end: b end

puts:
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	ldrb r1, [r0], #1
	str r1, [r4]
	subs r1, r1, #0
	beq putsend
	b puts
putsend:
	mov pc, lr

string:
	.ascii "Hello world\n\0"

string2:
	.ascii "Boing!\n\0"

	.end
