//! # A simple emulator for a custom architecture
//! 
//! JIT WIP.
//! 
//! ~~R8D-R15D are used as GPRs. As the CPU has 16 registers, they're all saved in memory (pointed to by RSI) and swapped as needed.
//! R8 always contains rA or rI, R9 rB or rJ and so on.~~
//! A and B registers are used as scratch

mod winit_app;
mod cpu;
mod registers;

use std::cell::UnsafeCell;
use std::env::args;
use std::fmt::Debug;
use std::io::Read;
use std::num::NonZeroU32;
use std::rc::Rc;
use std::thread;

use signal_hook::consts::SIGINT;
use signal_hook::iterator::Signals;

use winit::event::{Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::Window;
use crate::cpu::Cpu;
use crate::registers::{print_to_stdout, reg_to_ascii, Registers};

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

        impl std::convert::From<u8> for $name {
            fn from(v: u8) -> Self {
                match v {
                    $(x if x == $name::$vname as u8 => $name::$vname,)*
                    _ => panic!("Invalid instruction"),
                }
            }
        }
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
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

impl From<u8> for AluInstruction {
    fn from(value: u8) -> Self {
        if value > AluInstruction::Mul as u8 {
            panic!("Invalid instruction");
        }
        unsafe { std::mem::transmute(value) }
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

fn run_event_loop(display: &[u32]) {
    let event_loop = EventLoop::new().unwrap();
    let mut app = winit_app::WinitAppBuilder::with_init(|elwt| {
        let window = {
            let window = elwt.create_window(Window::default_attributes());
            Rc::new(window.unwrap())
        };
        let context = softbuffer::Context::new(window.clone()).unwrap();
        let mut surface = softbuffer::Surface::new(&context, window.clone()).unwrap();

        surface
            .resize(
                NonZeroU32::new(256).unwrap(),
                NonZeroU32::new(256).unwrap(),
            )
            .unwrap();
        elwt.set_control_flow(ControlFlow::Wait);

        (window, surface)
    }).with_event_handler(|state, event, elwt| {
        let (window, surface) = state;

        match event {
            Event::WindowEvent { window_id, event: WindowEvent::RedrawRequested } if window_id == window.id() => {
                let mut buffer = surface.buffer_mut().unwrap();
                buffer.copy_from_slice(&display);

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
    let mut cpu = Cpu::new();

    let filename = args().nth(1).unwrap();
    let file = std::fs::File::open(filename).unwrap();
    let mut file = std::io::BufReader::new(file);

    let mut i = 0;
    loop {
        let mut buf = [0;4];
        match file.read_exact(&mut buf) {
            Ok(_) => {},
            Err(_) => break
        }
        cpu.mem[i] = u32::from_be_bytes(buf);
        i += 1;
    }

    let now = std::time::Instant::now();
    // let instrcount = UnsafeCell::from(0u64);
    // let instrcountreadptr = instrcount.get();
    // let instrcountmutptr = instrcount.get();
    // let instrcount = unsafe { &*instrcountreadptr };
    thread::spawn(move || {
        for sig in signals.forever() {
            if sig == SIGINT {
                println!("Emulator exiting");
                // println!("Instruction count: {}", instrcount);
                println!("Time elapsed: {:?}", now.elapsed());
                // let hz = *instrcount as f64 / now.elapsed().as_secs_f64();
                // println!("Instructions per second: {}MHz", hz / 1_000_000.0);
                std::process::exit(0);
            }
        }
    });

    let display = &cpu.display as &[u32] as *const [u32];
    let cpu_thread = thread::spawn(move || {
        let mut cpu = cpu;
        cpu.start();
    });
    
    run_event_loop(unsafe { &*display });
    cpu_thread.join().unwrap();
}
