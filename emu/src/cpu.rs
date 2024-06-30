use std::collections::{HashMap, HashSet};
use std::io::stdin;
use std::slice;
use crate::registers::{print_to_stdout, Registers};

use dynasmrt::{dynasm, ExecutableBuffer, DynasmApi, DynasmLabelApi, AssemblyOffset};
use crate::{AluInstruction, CoreInstruction, Instruction};

/// Cpu object
/// Although mem and display are not pinned, it is assumed that they will not be moved and will result in UB if they are
pub struct Cpu {
    pub mem: Box<[u32]>,
    pub display: Box<[u32]>,
    registers: Registers,
    assembled: HashMap<u32, Assembled>,
    jmp_targets: HashSet<u32>,
    /// Setup function for the CPU
    /// When called, it will set up relevant registers and jump to it's first argument (u64 address)
    setup: Option<Setup>
}

struct Setup {
    buf: ExecutableBuffer,
    offset: AssemblyOffset
}

#[derive(Debug)]
struct Assembled {
    buf: ExecutableBuffer,
    /// number of instructions represented
    size: u32,
    /// start offset of the assembly
    offset: AssemblyOffset,
    /// last instruction assembled, used for debugging and possibly in the future for jump handling
    last: InstructionData,
    /// offsets of jump targets
    offsets: HashMap<u32, AssemblyOffset>
}

#[derive(Debug)]
struct InstructionData {
    instr: Instruction,
    is_imm: bool,
    imm: u32,
    r1: usize,
    r2: usize,
    r3: usize,
    r4: usize
}

macro_rules! asm {
    ($ops:ident $($t:tt)*) => {
        dynasm!($ops
            ; .arch x64
            ; .alias reg, r12
            ; .alias mem, r13
            ; .alias display, r14
            $($t)*
        )
    }
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            mem: vec![0; 2usize.pow(24)].into_boxed_slice(),
            display: vec![0; 256 * 256].into_boxed_slice(),
            registers: Registers::new(),
            assembled: HashMap::new(),
            jmp_targets: {
                let mut hs = HashSet::new();
                hs.insert(0);
                hs
            },
            setup: None
        }
    }
    
    /// initializes setup variable if not already initialized. Must be called before using setup.unwrap()
    fn setup(&mut self) {
        if self.setup.is_some() {
            return;
        }
        let mut ops = dynasmrt::x64::Assembler::new().unwrap();

        let memm = &mut *self.mem as *mut _ as *mut ();
        let displaym = &mut *self.display as *mut _ as *mut ();
        let regm = &mut self.registers as *mut _;
        let offset = ops.offset();
        
        asm!(ops
            ; push rbp
            ; mov rbp, rsp
            ; push rbx
            ; push r12
            ; push r13
            ; push r14
            ; mov reg, QWORD regm as _ // set the register pointer
            ; mov mem, QWORD memm as _ // set the memory pointer
            ; mov display, QWORD displaym as _ // set the display pointer
            ; mov r12, QWORD regm as _ // set the register pointer (again for some reason)
            ; jmp rdi
        );
        
        self.setup = Some(Setup {
            buf: ops.finalize().unwrap(),
            offset
        });
    }
    
    /// Merges assembled buffers together if they are adjacent or overlap
    /// This invalidates existing pointer buffers!
    fn merge_assembled(&mut self) {
        
    }
    
    /// Gets the assembled buffer at the given offset
    /// Buffer doesn't necessarily start at the offset, but it will include it in the offsets
    fn get_assembled(&self, offset: u32) -> Option<&Assembled> {
        self.assembled.get(&offset)
    }
    
    pub fn start(&mut self) {
        self.setup();
        let setup = self.setup.as_ref().unwrap();
        let f: extern "C" fn(*const u8) -> () = unsafe { std::mem::transmute(setup.buf.ptr(setup.offset)) };
        let mut offset = 0;
        // let mut max_loops = 3;
        // let io = stdin();
        loop {
            // println!("progressing");
            if !self.assembled.contains_key(&offset) {
                println!("assembling due to progressing");
                self.assemble_offset(offset, None);
            }
            let assembled = self.get_assembled(offset).unwrap();
            f(assembled.buf.ptr(assembled.offset));
            // dbg!(offset, assembled.size);
            // dbg!(&self.registers);
            // max_loops -= 1;
            // wait for line
            // let mut line = String::new();
            // io.read_line(&mut line).unwrap();
            // if max_loops == 0 {
            //     break;
            // }
            offset = self.registers.pc + 1;
        }
    }
    
    pub fn assemble_offset(&mut self, start_offset: u32, end_offset: Option<u32>) {
        let mut ops = dynasmrt::x64::Assembler::new().unwrap();
        let selfptr = self as *const _;
        let regr = &self.registers as *const _;
        let assembly_offset = ops.offset();
        let reg_pc = unsafe { (&self.registers.pc as *const u32).byte_offset_from::<Registers>(regr) };
        let mut offsets = HashMap::new();
        let mut last_instruction = None;
        let mut max_instructions = 100;
        let mut offset = start_offset;
        let reg_scratch = unsafe { (&self.registers.scratch as *const u32).byte_offset_from::<Registers>(regr) };
        self.jmp_targets.insert(start_offset);
        let mut jmp_target_dynlabels = HashMap::new();
        if let Some(end_offset) = end_offset {
            for jmp_target in &self.jmp_targets {
                let label = ops.new_dynamic_label();
                jmp_target_dynlabels.insert(*jmp_target, label);
            }
        }
        loop {
            let instr = self.mem[offset as usize];
            if self.jmp_targets.contains(&offset) {
                let opsoffset = ops.offset();
                offsets.insert(offset, opsoffset);
                jmp_target_dynlabels.entry(offset).or_insert_with(|| ops.new_dynamic_label());
                let label = *jmp_target_dynlabels.get(&offset).unwrap();
                asm!(ops
                    ; =>label
                    ; mov eax, DWORD offset as _ // set the pc to the start offset
                    ; mov [reg + reg_pc as _], eax
                );
            }
            let is_alu = instr & (1 << 31) == 0;
            let is_imm = instr & (1 << 30) != 0;
            let opcode = ((instr >> 26) & 0b1111) as u8;
            let imm = instr & 0xFFFF;
            let r1: usize = instr as usize >> 21 & 0b11111;
            let r2: usize = instr as usize >> 16 & 0b11111;
            let r3: usize = instr as usize >> 11 & 0b11111;
            let r4: usize = instr as usize >> 6 & 0b11111;
            let reg1 = unsafe { (&self.registers[r1] as *const u32).byte_offset_from::<Registers>(regr) };
            let reg2 = unsafe { (&self.registers[r2] as *const u32).byte_offset_from::<Registers>(regr) };
            let reg1m = unsafe { (&mut self.registers[r1] as *mut u32).byte_offset_from::<Registers>(regr) };
            let reg2m = unsafe { (&mut self.registers[r2] as *mut u32).byte_offset_from::<Registers>(regr) };
            let reg3 = unsafe { (&self.registers[r3] as *const u32).byte_offset_from::<Registers>(regr) };
            let reg4 = unsafe { (&self.registers[r4] as *const u32).byte_offset_from::<Registers>(regr) };
            let regflags = unsafe { (&self.registers.flags as *const u32).byte_offset_from::<Registers>(regr) };
            max_instructions -= 1;
            let mut stop = max_instructions == 0;
            let full_instr;
            if is_alu {
                let instr = AluInstruction::from(opcode);
                full_instr = Instruction::Alu(instr);
                match instr {
                    AluInstruction::Not => {
                        if is_imm {
                            asm!(ops
                                ; mov eax, DWORD imm as _
                                ; not eax
                                ; mov [reg + reg1m as _], eax
                            );
                        } else {
                            asm!(ops
                                ; mov eax, [reg + reg2 as _]
                                ; not eax
                                ; mov [reg + reg1m as _], eax
                            );
                        }
                    },
                    AluInstruction::Xor => {
                        if is_imm {
                            asm!(ops
                                ; mov eax, DWORD imm as _
                                ; xor eax, [reg + reg2 as _]
                                ; mov [reg + reg1m as _], eax
                            );
                        } else {
                            asm!(ops
                                ; mov eax, [reg + reg2 as _]
                                ; xor eax, [reg + reg3 as _]
                                ; mov [reg + reg1m as _], eax
                            );
                        }
                    },
                    AluInstruction::Or => {
                        if is_imm {
                            asm!(ops
                                ; mov eax, DWORD imm as _
                                ; or eax, [reg + reg2 as _]
                                ; mov [reg + reg1m as _], eax
                            );
                        } else {
                            asm!(ops
                                ; mov eax, [reg + reg2 as _]
                                ; or eax, [reg + reg3 as _]
                                ; mov [reg + reg1m as _], eax
                            );
                        }
                    },
                    AluInstruction::And => {
                        // registers[r1] = registers[r2] & last_value;
                        if is_imm {
                            asm!(ops
                                ; mov eax, DWORD imm as _
                                ; and eax, [reg + reg2 as _]
                                ; mov [reg + reg1m as _], eax
                            );
                        } else {
                            asm!(ops
                                ; mov eax, [reg + reg2 as _]
                                ; and eax, [reg + reg3 as _]
                                ; mov [reg + reg1m as _], eax
                            );
                        }
                    },
                    AluInstruction::Shl => {
                        // cout = registers[r2] & (1 << 31);
                        // registers[r1] = registers[r2] << 1;
                        asm!(ops
                            ; mov eax, [reg + reg2 as _]
                            ; mov ebx, 1 << 31
                            ; and ebx, eax
                            ; shl eax, 1
                            ; mov [reg + reg1m as _], eax
                        );
                    },
                    AluInstruction::Shr => {
                        // cout = registers[r2] & 1;
                        // registers[r1] = registers[r2] >> 1;
                        asm!(ops
                            ; mov eax, [reg + reg2 as _]
                            ; mov ebx, 1
                            ; and ebx, eax
                            ; shr eax, 1
                            ; mov [reg + reg1m as _], eax
                        );
                    },
                    AluInstruction::Rotl => {
                        // cout = registers[r2] & (1 << 31);
                        // registers[r1] = registers[r2].rotate_left(1);
                        asm!(ops
                            ; mov eax, [reg + reg2 as _]
                            ; mov ebx, 1 << 31
                            ; and ebx, eax
                            ; rol eax, 1
                            ; mov [reg + reg1m as _], eax
                        );
                    },
                    AluInstruction::Rotr => {
                        // cout = registers[r2] & 1;
                        // registers[r1] = registers[r2].rotate_right(1);
                        asm!(ops
                            ; mov eax, [reg + reg2 as _]
                            ; mov ebx, 1
                            ; and ebx, eax
                            ; ror eax, 1
                            ; mov [reg + reg1m as _], eax
                        );
                    },
                    AluInstruction::Add => {
                        // let original = registers[r2];
                        // registers[r1] = registers[r2].wrapping_add(last_value);
                        // cout = if original > registers[r1] { 1 } else { 0 };
                        if is_imm {
                            asm!(ops
                                ; mov eax, DWORD imm as _
                            );
                        } else {
                            asm!(ops
                                ; mov eax, [reg + reg3 as _]
                            );
                        }
                        asm!(ops
                                ; add eax, [reg + reg2 as _]
                                ; setc bl
                                ; mov [reg + reg1m as _], eax
                        );
                    },
                    AluInstruction::Sub => {
                        // let original = registers[r2];
                        // registers[r1] = registers[r2].wrapping_sub(last_value);
                        // cout = if original < registers[r1] { 1 } else { 0 };
                        if is_imm {
                            asm!(ops
                                ; mov eax, DWORD imm as _
                            );
                        } else {
                            asm!(ops
                                ; mov eax, [reg + reg3 as _]
                            );
                        }
                        asm!(ops
                                ; sub eax, [reg + reg2 as _]
                                ; setc bl
                                ; mov [reg + reg1m as _], eax
                        );
                    },
                    AluInstruction::Inc => {
                        // let original = registers[r2];
                        // registers[r1] = registers[r2].wrapping_add(1);
                        // cout = if original > registers[r1] { 1 } else { 0 };
                        asm!(ops
                            ; mov eax, [reg + reg2 as _]
                            ; inc eax
                            ; setc bl
                            ; mov [reg + reg1m as _], eax
                        );
                    },
                    AluInstruction::Dec => {
                        // let original = registers[r2];
                        // registers[r1] = registers[r2].wrapping_sub(1);
                        // cout = if original < registers[r1] { 1 } else { 0 };
                        asm!(ops
                            ; mov eax, [reg + reg2 as _]
                            ; dec eax
                            ; setc bl
                            ; mov [reg + reg1m as _], eax
                        );
                    },
                    AluInstruction::Mul => {
                        // let val1 = registers[r3] as u64;
                        // let val2 = registers[r4] as u64;
                        // let res = val1.wrapping_mul(val2);
                        // registers[r1] = res as u32;
                        // registers[r2] = (res >> 32) as u32;
                        asm!(ops
                            ; mov eax, [reg + reg3 as _]
                            ; mov ebx, [reg + reg4 as _]
                            ; mul ebx
                            ; mov [reg + reg1m as _], eax
                            ; mov [reg + reg2m as _], edx
                        );
                    }
                }
                asm!(ops
                    // set the cout flag
                    ; movzx eax, bl
                    ; shl eax, 3
                    // set the zero flag
                    ; mov ebx, [reg + reg1 as _]
                    ; test ebx, ebx
                    ; setz bl
                    ; or eax, ebx
                    // set the sign flag
                    // ; mov ebx, [reg + reg1 as _]
                    ; and ebx, 1 << 31
                    ; shr ebx, 31 - 4 // shift the sign bit to the 4th bit
                    ; or eax, ebx
                    ; mov [reg + regflags as _], eax
                );
                // let regptr = &mut registers[r1];
                // let is_zero = *regptr == 0;
                // let sign = *regptr & (1 << 31) != 0;
                // registers.flags = (cout << 3) | (is_zero as u32) | ((sign as u32) << 4);
            } else {
                let instr = CoreInstruction::from(opcode);
                full_instr = Instruction::Core(instr);
                match instr {
                    CoreInstruction::Mov => {
                        if is_imm {
                            // movh
                            // registers[r1] = imm << 16;
                            asm!(ops
                                ; mov eax, DWORD imm as _
                                ; shl eax, 16
                                ; mov [reg + reg1m as _], eax
                            );
                        } else {
                            // mov
                            // registers[r1] = registers[r2];
                            asm!(ops
                                ; mov eax, [reg + reg2 as _]
                                ; mov [reg + reg1m as _], eax
                            );
                        }
                    },
                    CoreInstruction::Sw => {
                        // mem[registers[r1] as usize] = registers[r2];
                        asm!(ops
                            ; mov eax, [reg + reg1 as _] // load reg1
                            ; and eax, 0xFFFFFF // mem is 24b
                            // ; shl eax, 2 // multiply by 4 (4B = 32b)
                            // ; add rax, mem as _ // get real address
                            ; mov ebx, [reg + reg2 as _] // load reg2
                            ; mov [mem + 4 * rax], ebx // save the value into the address
                        );
                    },
                    CoreInstruction::Lw => {
                        // registers[r1] = mem[registers[r2] as usize];
                        asm!(ops
                            ; mov eax, [reg + reg2 as _] // load reg2
                            ; and eax, 0xFFFFFF // mem is 24b
                            // ; shl eax, 2 // multiply by 4 (4B = 32b)
                            // ; add rax, mem as _ // get real address (mem + reg2)
                            ; mov rax, [mem + 4 * rax] // load the value at the address
                            ; mov [reg + reg1m as _], rax // save the value into reg1
                        );
                    },
                    CoreInstruction::Jmp => {
                        // jmpin = 2;
                        // jmpto = if is_imm { imm } else { registers[r1] };
                        if is_imm && jmp_target_dynlabels.contains_key(&imm) {
                            asm!(ops
                                ; jmp => jmp_target_dynlabels[&imm]
                            );
                        } /*else if is_imm && (self.assembled.contains_key(&imm)) {
                            // it's not safe to have a pointer to another assembly, it could get invalidated
                            let assembled = self.assembled.get(&imm).unwrap();
                            let offset = assembled.offsets.get(&imm).unwrap();
                            asm!(ops
                                ; mov rax, QWORD assembled.buf.ptr(*offset) as _
                                ; jmp rax
                            );
                        } */else {
                             asm!(ops
                                ; mov rdi, QWORD selfptr as _);
                            if is_imm {
                                asm!(ops
                                    ; mov esi, imm as _
                                );
                                self.jmp_targets.insert(imm);
                            } else {
                                asm!(ops
                                    ; mov esi, [reg + reg1 as _]
                                );
                            }
                            asm!(ops
                                ; mov rax, QWORD get_address as _
                                ; call rax
                                ; jmp rax
                            );
                        }
                        stop = true;
                    },
                    CoreInstruction::Jz => {
                        // if registers.flags & 1 != 0 {
                        //     jmpin = 2;
                        //     jmpto = if is_imm { imm } else { registers[r1] };
                        // }

                        let skip  = ops.new_dynamic_label();
                        asm!(ops
                            ; mov rbx, [reg + regflags as _]
                            ; and rbx, 1 // set real zero flag if zero flag is not set
                            ; jz =>skip // if zero flag is not set
                            );
                        if is_imm && jmp_target_dynlabels.contains_key(&imm) {
                            asm!(ops
                                ; jmp => jmp_target_dynlabels[&imm]
                            );
                        } /*else if is_imm && (self.assembled.contains_key(&imm)) {
                            let assembled = self.assembled.get(&imm).unwrap();
                            let offset = assembled.offsets.get(&imm).unwrap();
                            asm!(ops
                                ; mov rax, QWORD assembled.buf.ptr(*offset) as _
                            );
                        } */else {
                            asm!(ops
                                ; mov rdi, QWORD selfptr as _);
                            if is_imm {
                                asm!(ops
                                    ; mov esi, imm as _
                                );
                                self.jmp_targets.insert(imm);
                            } else {
                                asm!(ops
                                    ; mov esi, [reg + reg1 as _]
                                );
                            }
                            asm!(ops
                                ; mov rax, QWORD get_address as _
                                ; call rax
                            );
                        }
                        asm!(ops
                            ; jmp rax
                            ; =>skip
                        );
                        stop = true;
                    },
                    CoreInstruction::Jnz => {
                        // if registers.flags & 1 == 0 {
                        //     jmpin = 2;
                        //     jmpto = if is_imm { imm } else { registers[r1] };
                        // }

                        let skip  = ops.new_dynamic_label();
                        asm!(ops
                            ; mov rbx, [reg + regflags as _]
                            ; and rbx, 1 // set real zero flag if zero flag is not set
                            ; jnz =>skip // if zero flag is not set
                            );
                        if is_imm && jmp_target_dynlabels.contains_key(&imm) {
                            asm!(ops
                                ; jmp => jmp_target_dynlabels[&imm]
                            );
                        } /*else if is_imm && (self.assembled.contains_key(&imm)) {
                            let assembled = self.assembled.get(&imm).unwrap();
                            let offset = assembled.offsets.get(&imm).unwrap();
                            asm!(ops
                                ; mov rax, QWORD assembled.buf.ptr(*offset) as _
                            );
                        } */else {
                            asm!(ops
                                ; mov rdi, QWORD selfptr as _);
                            if is_imm {
                                asm!(ops
                                    ; mov esi, imm as _
                                );
                                self.jmp_targets.insert(imm);
                            } else {
                                asm!(ops
                                    ; mov esi, [reg + reg1 as _]
                                );
                            }
                            asm!(ops
                                ; mov rax, QWORD get_address as _
                                ; call rax
                                ; jmp rax
                            );
                        }
                        asm!(ops
                            ; jmp rax
                            ; =>skip
                        );
                        stop = true;
                    },
                    CoreInstruction::Read => {
                        // TODO: implement read
                    },
                    CoreInstruction::Draw => {
                        // let pos = registers[r1] & 0xFFFF;
                        // let color = registers[r2] & 0xFFFFFF;
                        // let pos = (pos >> 8) | (pos << 8) & 0xFFFF;
                        // display[pos as usize] = color;
                        asm!(ops                            
                            ; mov eax, [reg + reg1 as _] // load reg1
                            ; and eax, 0xFFFF // mask position
                            ; mov ebx, eax
                            ; shr ebx, 8
                            ; shl eax, 8
                            ; or eax, ebx // swap the bytes
                            ; and eax, 0xFFFF // mask them again, needed both times for proper mask
                            ; mov ebx, [reg + reg2 as _] // load reg2
                            ; and ebx, 0xFFFFFF // mask color
                            ; mov [display + 4 * rax], ebx // save the value into the address
                        );
                    },
                    CoreInstruction::DClear => {
                        asm!(ops
                            ; mov rax, QWORD clear_display as _
                            ; call rax
                        );
                    }
                }
            }
            dbg!(&full_instr);
            if reg1m == reg_scratch || reg2m == reg_scratch {
                asm!(ops
                    ; mov eax, DWORD 0
                    ; mov [reg + reg_scratch as _], eax // reset scratch pointer (segfault things)
                );
            }
            asm!(ops
                ; inc [reg + reg_pc as _] // inc PC
            );
            if let Some(end_offset) = end_offset {
                // if we're not at the end, don't stop even if we hit jmp (faciliate joining exec buffers together)
                if offset < end_offset { stop = false; }
            }
            if self.jmp_targets.contains(&(offset + 1)) {
                // if we jump to the next offset anyway, don't stop (kind of merges this already)
                stop = false;
            }
            offset += 1;
            if stop {
                last_instruction = Some(InstructionData {
                    instr: full_instr,
                    is_imm,
                    imm,
                    r1,
                    r2,
                    r3,
                    r4
                });
                break;
            }
        }
        asm!(ops
            ; pop r14
            ; pop r13
            ; pop r12
            ; pop rbx
            ; pop rbp
            ; ret
        ); // if we don't jump out, return
        let buf = ops.finalize().unwrap();
        let assembled = Assembled {
            buf,
            offset: assembly_offset,
            offsets,
            last: last_instruction.unwrap(),
            size: offset - start_offset
        };
        // let mut file = std::fs::File::create(format!("assembly/{}.bin", start_offset)).unwrap();
        // use std::io::Write;
        // file.write_all(assembled.buf.as_ref()).unwrap();
        // dbg!(&assembled);
        // dbg!(memm, displaym, &self.registers as *const _, &self.registers[0] as *const _);
        self.assembled.insert(start_offset, assembled);
    }
}

extern "C" fn get_address(cpu: *mut Cpu, addr: u32) -> usize {
    let cpu = unsafe { &mut *cpu };
    cpu.jmp_targets.insert(addr);
    match cpu.assembled.get(&addr) {
        Some(assembled) => {
            let offset = assembled.offsets.get(&addr).unwrap();
            assembled.buf.ptr(*offset) as usize
        },
        None => {
            cpu.assemble_offset(addr, None);
            let assembled = cpu.assembled.get(&addr).unwrap();
            let offset = assembled.offsets.get(&addr).unwrap();
            assembled.buf.ptr(*offset) as usize
        }
    }
}

extern "C" fn clear_display(display: *mut u32) {
    unsafe {
        slice::from_raw_parts_mut(display, 256*256)
    }.iter_mut().for_each(|x| *x = 0);
}