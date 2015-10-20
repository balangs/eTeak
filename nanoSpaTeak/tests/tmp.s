_start:
	mov r4, #-1
	
	adr r0, data
	ldmia r0, {r1-r3, r5-r6}
	
	str r1, [r4]
	str r2, [r4]
	str r3, [r4]
	str r5, [r4]
	str r6, [r4]
	
	mov r4, #-2
	str r0, [r4]
	str r0, [r4]
	
end: b end
	
		
data:
	.ascii "H\0\n"
data1:	
	.ascii "E\0\n"
data2:
	.ascii "L\0\n"
data3:
	.ascii "L\0\n"
data4:
	.ascii "O\0\n"
	
	.end
