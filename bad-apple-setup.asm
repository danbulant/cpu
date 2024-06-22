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
    ; flags work as follows:
    ;   zero: r1 is zero
    ;   carry: always zero
    ;   sign: r1 sign bit
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


; rA contains current memory address to read pixel data
movhw rA, end
; rB contains the color of white (do not change)
movl rB, 0xffffff
; rC contains current pixel color (based on rB)
; rD contains the current coords (truncated to last 2 bytes, high 6 bytes are ignored)
movhw rD, 0
; rE is used as a temporary register
; rF is subpixel counter (0-32)
movhw rF, 32
; rG is number of words per frame
movhw rG, 2048
; rH is pixel data

loadmem:
    ; load next word
    lw rH, [rA]
    ; move word pointer
    inc rA, rA
    movhw rF, 32

loop:
    ; shift pixel data (<<1 with overflow)
    ; this makes it read left to right
    rotl rH, rH
    ; get current pixel on/off (&1)
    andi rE, rH, 1
    ; multiply by white (*rB)
    mul rC, r0, rB, rE
    ; draw
    draw rD, rC
    ; move coords
    ; coords are XY, so Y is incremented first and then overflows into incrementing X and reseting Y to 0
    inc rD, rD
    ; decrement counter
    dec rF, rF
    ; if counter is zero, get next word
    jzi loadmem
    nop
    jmpi loop
    nop



end:

; #d32 0b10100101111100001010010111110000
; #d32 0b11100101111100001010010111110000
; #d32 0b10000101111100001010010111110000
; #d32 0b10001111111100001010010111110000
