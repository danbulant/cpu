mod winit_app;

use std::env::args;
use std::fmt::Debug;
use std::io::Read;
use std::num::NonZeroU32;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::sync::atomic::AtomicU64;
use std::thread;
use signal_hook::consts::SIGINT;
use signal_hook::iterator::Signals;

use winit::event::{Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::Window;

#[derive(Debug)]
enum Instruction {
    Alu(AluInstruction),
    Core(CoreInstruction)
}

macro_rules! back_to_enum {
    ($(#[$meta:meta])* $vis:vis enum $name:ident {
        $($(#[$vmeta:meta])* $vname:ident $(= $val:expr)?,)*
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $($(#[$vmeta])* $vname $(= $val)?,)*
        }

        // impl std::convert::TryFrom<i32> for $name {
        //     type Error = ();
        // 
        //     fn try_from(v: i32) -> Result<Self, Self::Error> {
        //         match v {
        //             $(x if x == $name::$vname as i32 => Ok($name::$vname),)*
        //             _ => Err(()),
        //         }
        //     }
        // }

        impl std::convert::From<i32> for $name {
            fn from(v: i32) -> Self {
                match v {
                    $(x if x == $name::$vname as i32 => $name::$vname,)*
                    _ => panic!("Invalid instruction"),
                }
            }
        }
    }
}

back_to_enum! {
    #[repr(u8)]
    #[derive(Debug, Copy, Clone)]
    enum AluInstruction {
        Not,
        Xor,
        Or,
        And,
        Shl,
        Shr,
        Rotl,
        Rotr,
        Add,
        Sub,
        Inc,
        Dec,
        Mul,
    }
}

back_to_enum! {
    #[repr(u8)]
    #[derive(Debug, Copy, Clone)]
    enum CoreInstruction {
        Mov = 0,
        Sw = 0b0101,
        Lw = 0b111,
        Jmp = 0b1000,
        Jz = 0b1010,
        Jnz = 0b1011,
        Read = 0b1100,
        Draw = 0b1110,
        DClear = 0b1111,
    }
}

struct Registers {
    regs: [u32;16],
    pc: u32,
    flags: u32,
    /// scratch value that's never read from to satisfy rust
    scratch: u32
}

impl Debug for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut x = f.debug_struct("Registers");
        x
            .field("pc", &self.pc)
            .field("flags", &self.flags);
        for i in 0..16 {
            // annotate with alphabet
            let c = (b'A' + i as u8) as char;
            x.field(&c.to_string(), &format!("{:#} {:#x} {:#b}", self.regs[i], self.regs[i], self.regs[i]));
        }
        x.finish()
    }
}

impl std::ops::Index<usize> for Registers {
    type Output = u32;

    fn index(&self, index: usize) -> &Self::Output {
        match index {
            0 => &0,
            0b11110 => &self.pc,
            0b11111 => &self.flags,
            index => &self.regs[index - 1]
        }
    }
}

impl std::ops::IndexMut<usize> for Registers {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match index {
            0 => &mut self.scratch,
            0b11110 => &mut self.scratch,
            0b11111 => &mut self.scratch,
            index => &mut self.regs[index - 1]
        }
    }
}

fn print_to_stdout(display: &[u32]) {
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

fn reg_to_ascii(reg: usize) -> char {
    match reg {
        0 => '0',
        0b11110 => 'X',
        0b11111 => 'Z',
        reg => (b'A' + reg as u8 - 1) as char
    }
}

fn run_event_loop(display: Arc<Mutex<Box<[u32]>>>) {
    let event_loop = EventLoop::new().unwrap();
    let mut app = winit_app::WinitAppBuilder::with_init(|elwt| {
        let window = {
            let window = elwt.create_window(Window::default_attributes());
            Rc::new(window.unwrap())
        };
        let context = softbuffer::Context::new(window.clone()).unwrap();
        let surface = softbuffer::Surface::new(&context, window.clone()).unwrap();

        (window, surface)
    }).with_event_handler(|state, event, elwt| {
        let (window, surface) = state;
        elwt.set_control_flow(ControlFlow::Wait);

        match event {
            Event::WindowEvent { window_id, event: WindowEvent::RedrawRequested } if window_id == window.id() => {
                surface
                    .resize(
                        NonZeroU32::new(256).unwrap(),
                        NonZeroU32::new(256).unwrap(),
                    )
                    .unwrap();

                let mut buffer = surface.buffer_mut().unwrap();
                let display = display.lock().unwrap();
                for (i, pixel) in display.iter().enumerate() {
                    let x = i >> 8;
                    let y = i & 0xFF;
                    buffer[x + (y*256)] = *pixel;
                }

                buffer.present().unwrap();
                window.request_redraw();
            }
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                window_id,
            } if window_id == window.id() => {
                elwt.exit();
            }
            _ => {}
        }
    });

    event_loop.run_app(&mut app).unwrap();
}

fn main() {
    let mut signals = Signals::new([SIGINT]).unwrap();
    println!("emulator init");
    let mut mem = vec![0u32;2usize.pow(24)].into_boxed_slice();
    let display = Arc::new(Mutex::new(vec![0u32;256*256].into_boxed_slice()));
    let mut registers = Registers {
        regs: [0;16],
        pc: 0,
        flags: 0,
        scratch: 0
    };
    let stdin = std::io::stdin();

    let file = args().nth(1).unwrap();
    let file = std::fs::File::open(file).unwrap();
    let file = std::io::BufReader::new(file);
    let debug = false;

    for (i, byte) in file.bytes().enumerate() {
        mem[i / 4] = mem[i / 4] << 8 | byte.unwrap() as u32;
    }
    let instrcount = Arc::new(AtomicU64::new(0));

    let instrcount2 = instrcount.clone();
    thread::spawn(move || {
        let instrcount = instrcount2;
        for sig in signals.forever() {
            if sig == SIGINT {
                println!("Emulator exiting");
                println!("Instruction count: {}", instrcount.load(std::sync::atomic::Ordering::Relaxed));
                std::process::exit(0);
            }
        }
    });

    let display2 = display.clone();
    let cpu_thread = thread::spawn(move || {
        let display = display2;
        println!("starto!");
    
        let mut jmpto = 0;
        let mut jmpin = 0;
        loop {
            let instr = mem[registers.pc as usize];
            let is_alu = instr & (1 << 31) == 0;
            let is_imm = instr & (1 << 30) != 0;
            let opcode = (instr >> 26) & 0b1111;
            let imm = instr & 0xFFFF;
            let r1: usize = instr as usize >> 21 & 0b11111;
            let r2: usize = instr as usize >> 16 & 0b11111;
            let r3: usize = instr as usize >> 11 & 0b11111;
            let r4: usize = instr as usize >> 6 & 0b11111;
            let last_value = if is_imm { imm } else { registers[r3] };
            let full_instr;
            if is_alu {
                let instr = AluInstruction::from(opcode as i32);
                full_instr = Instruction::Alu(instr);
                if debug { dbg!(&instr); }
                let mut cout = 0u32;
                match instr {
                    AluInstruction::Not => {
                        registers[r1] = !last_value;
                    },
                    AluInstruction::Xor => {
                        registers[r1] = registers[r2] ^ last_value;
                    },
                    AluInstruction::Or => {
                        registers[r1] = registers[r2] | last_value;
                    },
                    AluInstruction::And => {
                        registers[r1] = registers[r2] & last_value;
                    },
                    AluInstruction::Shl => {
                        cout = registers[r2] & (1 << 31);
                        registers[r1] = registers[r2] << 1;
                    },
                    AluInstruction::Shr => {
                        cout = registers[r2] & 1;
                        registers[r1] = registers[r2] >> 1;
                    },
                    AluInstruction::Rotl => {
                        cout = registers[r2] & (1 << 31);
                        registers[r1] = registers[r2].rotate_left(1);
                    },
                    AluInstruction::Rotr => {
                        cout = registers[r2] & 1;
                        registers[r1] = registers[r2].rotate_right(1);
                    },
                    AluInstruction::Add => {
                        let original = registers[r2];
                        registers[r1] = registers[r2].wrapping_add(last_value);
                        cout = if original > registers[r1] { 1 } else { 0 };
                    },
                    AluInstruction::Sub => {
                        let original = registers[r2];
                        registers[r1] = registers[r2].wrapping_sub(last_value);
                        cout = if original < registers[r1] { 1 } else { 0 };
                    },
                    AluInstruction::Inc => {
                        let original = registers[r2];
                        registers[r1] = registers[r2].wrapping_add(1);
                        cout = if original > registers[r1] { 1 } else { 0 };
                    },
                    AluInstruction::Dec => {
                        let original = registers[r2];
                        registers[r1] = registers[r2].wrapping_sub(1);
                        cout = if original < registers[r1] { 1 } else { 0 };
                    },
                    AluInstruction::Mul => {
                        let val1 = registers[r3] as u64;
                        let val2 = registers[r4] as u64;
                        let res = val1.wrapping_mul(val2);
                        registers[r1] = res as u32;
                        registers[r2] = (res >> 32) as u32;
                    }
                }
                let is_zero = registers[r1] == 0;
                let sign = registers[r1] & (1 << 31) != 0;
                registers.flags = (cout << 3) | (is_zero as u32) | ((sign as u32)<<4);
            } else {
                let instr = CoreInstruction::from(opcode as i32);
                full_instr = Instruction::Core(instr);
                if debug { dbg!(&instr); }
                match instr {
                    CoreInstruction::Mov => {
                        if is_imm {
                            // movh
                            registers[r1] = imm << 16;
                        } else {
                            // mov
                            registers[r1] = registers[r2];
                        }
                    },
                    CoreInstruction::Sw => {
                        mem[registers[r1] as usize] = registers[r2];
                    },
                    CoreInstruction::Lw => {
                        registers[r1] = mem[registers[r2] as usize];
                    },
                    CoreInstruction::Jmp => {
                        jmpin = 2;
                        jmpto = if is_imm { imm } else { registers[r1] };
                    },
                    CoreInstruction::Jz =>{
                        if registers.flags & 1 != 0 {
                            jmpin = 2;
                            jmpto = if is_imm { imm } else { registers[r1] };
                        }
                    },
                    CoreInstruction::Jnz => {
                        if registers.flags & 1 == 0 {
                            jmpin = 2;
                            jmpto = if is_imm { imm } else { registers[r1] };
                        }
                    },
                    CoreInstruction::Read => {
                        // TODO: implement read
                    },
                    CoreInstruction::Draw => {
                        let pos = registers[r1] & 0xFFFF;
                        let color = registers[r2] & 0xFFFFFF;
                        display.lock().unwrap()[pos as usize] = color;
                    },
                    CoreInstruction::DClear => {
                        display.lock().unwrap().iter_mut().for_each(|x| *x = 0);
                    }
                }
            }
            match jmpin {
                2 => {
                    jmpin -= 1;
                    registers.pc += 1;
                },
                1 => {
                    registers.pc = jmpto;
                    jmpin -= 1;
                },
                _ => registers.pc += 1
            }
            let currentinstrcount = instrcount.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            if currentinstrcount % (if debug { 1 } else { 100_000}) == 0 {
                // break;
                if debug {
                    println!("Instruction count: {}", currentinstrcount);
                    println!("{:#x} | {full_instr:?} {}", registers.pc, match is_alu {
                        true => if opcode == 0b001100 { // mul
                            format!("{}, {}, {}, {}", reg_to_ascii(r1), reg_to_ascii(r2), reg_to_ascii(r3), reg_to_ascii(r4))
                        } else {
                            match is_imm {
                                true => format!("{}, {}, {:#x}", reg_to_ascii(r1), reg_to_ascii(r2), imm),
                                false => format!("{}, {}, {}", reg_to_ascii(r1), reg_to_ascii(r2), reg_to_ascii(r3))
                            }
                        },
                        false => if opcode == 0b100000 { // mov
                            match is_imm {
                                true => format!("{}, {:#x}", reg_to_ascii(r1), imm),
                                false => format!("{}, {}", reg_to_ascii(r1), reg_to_ascii(r2))
                            }
                        } else {
                            match is_imm {
                                true => format!("{:#x}", imm),
                                false => format!("{}", reg_to_ascii(r1))
                            }
                        }
                    });
                    println!("r1: {}", reg_to_ascii(r1));
                    println!("r2: {}", reg_to_ascii(r2));
                    println!("r3: {}", reg_to_ascii(r3));
                    println!("r4: {}", reg_to_ascii(r4));
                    dbg!(imm, is_imm, is_alu);
                    dbg!(&registers);
                    dbg!(instr, jmpin, jmpto, opcode);
                    println!("Waiting for input line");
                    let mut line = String::new();
                    stdin.read_line(&mut line).unwrap();
                    if line == "d\n" {
                        println!();
                        print_to_stdout(&display.lock().unwrap());
                        println!();
                    }
                }
            }
        }
    });
    
    run_event_loop(display);
    cpu_thread.join().unwrap();
}
