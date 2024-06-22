# CPU

A custom 32bit CPU architecture inspired by MIPS, designed for educational purposes.

## Features

- logisim circuit design (ALU + CPU itself)
- emulator written in Rust
- customasm assembly included (main.asm has example/test code, bad-apple-setup.asm plays video from RAM)
- 32bit word (incl. RAM, RAM addresses are 24bits)
- 16 general purpose registers (A-P)
- 0 register is always 0
- PC and ALU FLAGS accessible as read-only registers
- capable of playing bad apple (and other short B/W videos that fit into RAM)
- flexible instruction set (each instruction can choose which registers it uses)
- RISC design
- each instruction is 1 word (32bits)
- keyboard for input, 256x256 color display for output
