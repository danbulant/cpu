#bankdef mainbank {
    #bits 32
    #outp 0
}

#subruledef register {
    ; special registers are write-ignored (readonly)
    ; always returns 0
    0 =>  0b00000
    ; IR => 0b00010
    ; program counter. Use jump to modify
    PC => 0b11110
    ; alu flags, written to by ALU operations
    ; 0 = zero
    ; 4 = carry
    ; 5 = sign
    FLAGS => 0b11111

    A => 0b00001
    B => 0b00010
    C => 0b00011
    D => 0b00100
    E => 0b00101
    F => 0b00110
    G => 0b00111
    H => 0b01000
    I => 0b01001
    J => 0b01010
    K => 0b01011
    L => 0b01100
    M => 0b01101
    N => 0b01110
    O => 0b01111
    P => 0b10000
}

#ruledef {
    ; no operation
    nop => 0x00000000
    ; r1 = !r2
    not r{r1: register}, r{r2: register} => 0b000000 @ r1 @ r2 @ 0x0000
    ; r1 = r2 ^ r3
    xor r{r1: register}, r{r2: register}, r{r3: register} => 0b000001 @ r1 @ r2 @ r3 @ 0x0000`11
    ; r1 = r2 | r3
    or r{r1: register}, r{r2: register}, r{r3: register} => 0b000010 @ r1 @ r2 @ r3 @ 0x0000`11
    ; r1 = r2 & r3
    and r{r1: register}, r{r2: register}, r{r3: register} => 0b000011 @ r1 @ r2 @ r3 @ 0x0000`11
    ; r1 = r2 << 1
    shl r{r1: register}, r{r2: register} => 0b000100 @ r1 @ r2 @ 0x0000
    ; r1 = r2 >> 1
    shr r{r1: register}, r{r2: register} => 0b000101 @ r1 @ r2 @ 0x0000
    ; r1 = r2 << 1 | (r2 & 1)
    rotl r{r1: register}, r{r2: register} => 0b000110 @ r1 @ r2 @ 0x0000
    ; r1 = r2 >> 1 | (r2 & 0x8000)
    rotr r{r1: register}, r{r2: register} => 0b000111 @ r1 @ r2 @ 0x0000
    ; r1 = r2 + r3
    add r{r1: register}, r{r2: register}, r{r3: register} => 0b001000 @ r1 @ r2 @ r3 @ 0x0000`11
    ; r1 = r2 - r3
    sub r{r1: register}, r{r2: register}, r{r3: register} => 0b001001 @ r1 @ r2 @ r3 @ 0x0000`11
    ; r1 = r2 + 1
    inc r{r1: register}, r{r2: register} => 0b001010 @ r1 @ r2 @ 0x0000
    ; r1 = r2 - 1
    dec r{r1: register}, r{r2: register} => 0b001011 @ r1 @ r2 @ 0x0000
    ; r1, r2 = r3 * r4 (r1 has low bits, r2 has high bits)
    mul r{r1: register}, r{r2: register}, r{r3: register}, r{r4: register} => 0b001100 @ r1 @ r2 @ r3 @ r4 @ 0x0000`6

    ; r1 = r2 | imm
    ori r{r1: register}, r{r2: register}, {imm: i16} => 0b010010 @ r1 @ r2 @ imm
    ; r1 = r2 & imm
    andi r{r1: register}, r{r2: register}, {imm: i16} => 0b010011 @ r1 @ r2 @ imm
    ; r1 = r2 + imm
    addi r{r1: register}, r{r2: register}, {imm: i16} => 0b011000 @ r1 @ r2 @ imm

    ; r1 = r2
    mov r{r1: register}, r{r2: register} => 0b100000 @ r1 @ r2 @ 0x0000
    ; RAM[r1] = r2
    sw [r{r1: register}], r{r2: register} => 0b100101 @ r1 @ r2 @ 0x0000
    ; r1 = RAM[r2]
    lw r{r1: register}, [r{r2: register}] => 0b100111 @ r1 @ r2 @ 0x0000

    ; goto r1
    jmp r{r1: register} => 0b101000 @ r1 @ 0x00000000`21
    ; if(alu flags zero) goto r1
    jz  r{r1: register} => 0b101010 @ r1 @ 0x00000000`21
    ; if(not alu flags zero) goto r1
    jnz r{r1: register} => 0b101011 @ r1 @ 0x00000000`21

    ; r1[16..32] = imm (sets high bits)
    movh r{r1: register}, {imm: i16} => 0b110000 @ r1 @ 0b00000 @ imm

    ; goto imm
    jmpi {imm: i26} => 0b111000 @ imm
    ; if(alu flags zero) goto imm
    jzi {imm: i26} => 0b111010 @ imm
    ; if(not alu flags zero) goto imm
    jnzi {imm: i26} => 0b111011 @ imm

    ; r1 = keyboard
    read r{r1: register} => 0b111100 @ r1 @ 0x00000000`21
    ; draw at r1, the color r2
    draw r{r1: register}, r{r2: register} => 0b111110 @ r1 @ r2 @ 0x0000
    ; clear screen
    draw clear => 0b111111 @ 0x00000000`26
}

#ruledef {
    ; zeroes out the register
    zero r{r1: register} => asm { movh r{r1}, 0 }
    ; loads a 32bit immediate in two instructions
    movl r{r1: register}, {imm: i32} => asm {
        movh r{r1}, ({imm} >> 16)
        ori r{r1}, r{r1}, ({imm} & 0xffff)
    }
    ; move half word, loads bottom 16 bits of r1
    movhw r{r1: register}, {imm: i16} => asm {
        ori r{r1}, r0, {imm}
    }
}

nop ; no operation

movhw rA, 0x0101 ; XY, so 1,1
movl rB, 0xffffff ; white
draw rA, rB ; draw at 1,1 white
nop
nop
draw clear ; clear screen

not rA, rB ; rA = !rB, i.e. 0xffffffff
movhw rA, 0x11 ; rA = 0x11
movhw rB, 0x22 ; rB = 0x22
xor rC, rA, rB ; rC = rA ^ rB, i.e. 0x33
or rD, rA, rB ; rD = rA | rB, i.e. 0x33
and rE, rA, rD ; rE = rA & rD, i.e. 0x11
shl rF, rE ; rF = rE << 1, i.e. 0x22
shr rG, rF ; rG = rF >> 1, i.e. 0x11
rotl rF, rG ; rF = rG << 1 | (rG & 1), i.e. 0x22
rotr rH, rG ; rG = rF >> 1 | (rF & 0x80000000), i.e. 0x80000008
add rI, rA, rB ; rI = rA + rB, i.e. 0x33
sub rJ, rI, rB ; rJ = rI - rB, i.e. 0x11
inc rK, rA ; rK = rA + 1, i.e. 0x12
dec rL, rK ; rL = rK - 1, i.e. 0x11
mul rM, rN, rA, rB ; rM, rN = rA * rB, i.e. 0x242, 0x0

ori rA, rA, 0x44 ; rA = rA | 0x44, i.e. 0x55
andi rA, rA, 0x33 ; rA = rA & 0x33, i.e. 0x11
addi rM, rM, 0x2 ; rM = rM + 1, i.e. 0x244

mov rA, rB ; rA = rB, i.e. 0x22
movhw rC, end ; rC = &end
sw [rC], rA ; RAM[&end] = rA, i.e. 0x22
lw rD, [rC] ; rD = RAM[&end], i.e. 0x22

movhw rG, 1 ; rG = 0, rG[16..32] = 1, i.e. 0x00010000

movhw rD, jmp1
jmp rD ; goto jmp1
nop ; jump still executes an instruction after it
nop ; these should be skipped
nop
nop

jmp1:
    movhw rD, jmp2
    xor r0, rA, rA ; sets the zero flag
    jz rD
    nop
    nop ; these should be skipped
    nop
    nop

jmp2:
    movhw rD, jmp3
    xor r0, rA, rA ; sets the zero flag
    jnz rD
    nop
    nop ; these should NOT be skipped
    nop
    nop

jmp3:
    jmpi jmp4 ; goto jmp4
    nop
    nop ; these should be skipped
    nop
    nop

jmp4:
    xor r0, rA, rA ; sets the zero flag
    jzi jmp5
    nop
    nop ; these should be skipped
    nop
    nop

jmp5:
    xor r0, rA, rA ; sets the zero flag
    jnzi jmp6
    nop
    nop ; these should NOT be skipped
    nop
    nop

jmp6:
    read rA ; rA = keyboard

nop
nop
nop

draw clear
movhw rA, 0
loop:
    draw rA, rA
    inc rA, rA
    jmpi loop
    nop

end: