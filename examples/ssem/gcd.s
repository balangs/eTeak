L1:     data loop-1      ; NB Program starts at addr 1
loop:	LDN gcdarg2      ; Load second arg inverted
        STO tmp
        LDN tmp          ; uninvert
        SUB gcdarg1      ; Subtract first arg
        STO tmp          ; store uninverted
        TEST
        JMP greq         ; jump if b >= a
        LDN tmp          ; -tmp
        STO gcdarg1      ; a-b
        JMP L1   
greqo:	STO gcdarg2      ; Doesn't matter if equal as use arg1
        SUB one
        TEST
        JMP L1
        STOP
tmp:       data 0x1000000
gcdarg1:   data 0xC
gcdarg2:   data 8
one:       data 1
greq:      data greqo -1 ; PC is incremented before fetch
