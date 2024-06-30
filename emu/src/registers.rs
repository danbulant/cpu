use std::fmt::Debug;

pub struct Registers {
    pub regs: [u32;16],
    pub pc: u32,
    pub flags: u32,
    /// scratch value that's never read from to satisfy rust
    pub scratch: u32
}

impl Registers {
    pub fn new() -> Registers {
        Registers {
            regs: [0;16],
            pc: 0,
            flags: 0,
            scratch: 0
        }
    }
}

const CHARS_STR: [&str; 16] = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P"];
const CHARS: [char; 16] = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P'];

impl Debug for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut x = f.debug_struct("Registers");
        x
            .field("pc", &self.pc)
            .field("flags", &self.flags);
        for (i, c) in CHARS_STR.iter().enumerate() {
            // annotate with alphabet
            // let c = (b'A' + i as u8) as char;
            x.field(c, &format!("{:>12} {:>8x} {:#034b}", self.regs[i], self.regs[i], self.regs[i]));
        }
        x.finish()
    }
}

pub const REG_FLAGS: usize = 0b11111;
pub const REG_PC: usize = 0b11110;

impl std::ops::Index<usize> for Registers {
    type Output = u32;

    fn index(&self, index: usize) -> &Self::Output {
        match index {
            0 => &self.scratch,
            REG_PC => &self.pc,
            REG_FLAGS => &self.flags,
            index => &self.regs[index - 1]
        }
    }
}

impl std::ops::IndexMut<usize> for Registers {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match index {
            0 => &mut self.scratch,
            x if x & 0b10000 != 0 => &mut self.scratch,
            index => &mut self.regs[index - 1],
        }
    }
}

pub fn print_to_stdout(display: &[u32]) {
    for y in 0..256 {
        for x in 0..256 {
            let pixel = display[(x * 256 + y) as usize];
            let red = pixel >> 16 & 0xFFu32;
            let green = pixel >> 8 & 0xFFu32;
            let blue = pixel & 0xFF;
            print!("\x1b[38;2;{red};{green};{blue}m#", red=red, green=green, blue=blue);
        }
        println!();
    }
    println!("\x1b[0m");
}

pub fn reg_to_ascii(reg: usize) -> char {
    match reg {
        0 => '0',
        REG_PC => 'X',
        REG_FLAGS => 'Z',
        reg => CHARS[reg - 1]
    }
}