use std::collections::{HashMap, HashSet};
use std::slice;
use crate::registers::Registers;

use dynasmrt::{dynasm, ExecutableBuffer, DynasmApi, DynasmLabelApi, AssemblyOffset};
use crate::{AluInstruction, CoreInstruction, Instruction};

pub struct Cpu {
    pub mem: Box<[u32]>,
    pub display: Box<[u32]>,
    registers: Registers,
    assembled: HashMap<u32, Assembled>,
    jmp_targets: HashSet<u32>
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

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            mem: vec![0; 2usize.pow(24)].into_boxed_slice(),
            display: vec![0; 64 * 32].into_boxed_slice(),
            registers: Registers::new(),
            assembled: HashMap::new(),
            jmp_targets: HashSet::new()
        }
    }
    
    pub fn start(&mut self) {
        if !self.assembled.contains_key(&0) {
            self.assemble_offset(0);
        }
        let assembled = self.assembled.get(&0).unwrap();
        let f: extern "C" fn() -> () = unsafe { std::mem::transmute(assembled.buf.ptr(assembled.offset)) };
        f();
    }
    
    pub fn assemble_offset(&mut self, start_offset: u32) {
        let mut ops = dynasmrt::x64::Assembler::new().unwrap();
        let selfptr = &self as *const _;
        dynasm!(ops
            ; .arch x64
            // ; mov 
        );
        let assembly_offset = ops.offset();
        let memm = &mut *self.mem as *mut _ as *mut ();
        let displaym = &mut *self.display as *mut _ as *mut ();
        let mut offsets = HashMap::new();
        let mut last_instruction = None;
        let mut max_instructions = 100;
        let mut offset = start_offset;
        loop {
            let instr = self.mem[offset as usize];
            if self.jmp_targets.contains(&offset) {
                offsets.insert(offset, ops.offset());
            }
            let is_alu = instr & (1 << 31) == 0;
            let is_imm = instr & (1 << 30) != 0;
            let opcode = ((instr >> 26) & 0b1111) as u8;
            let imm = instr & 0xFFFF;
            let r1: usize = instr as usize >> 21 & 0b11111;
            let r2: usize = instr as usize >> 16 & 0b11111;
            let r3: usize = instr as usize >> 11 & 0b11111;
            let r4: usize = instr as usize >> 6 & 0b11111;
            let reg1m = &mut self.registers[r1] as *mut _;
            let reg1 = &self.registers[r1] as *const _;
            let reg2m = &mut self.registers[r2] as *mut _;
            let reg2 = &self.registers[r2] as *const _;
            let reg3 = &self.registers[r3] as *const _;
            let reg4 = &self.registers[r4] as *const _;
            let regflags = &mut self.registers.flags as *mut _;
            max_instructions -= 1;
            let mut stop = max_instructions == 0;
            let full_instr;
            if is_alu {
                let instr = AluInstruction::from(opcode);
                full_instr = Instruction::Alu(instr);
                match instr {
                    AluInstruction::Not => {
                        if is_imm {
                            dynasm!(ops
                                ; mov eax, DWORD imm as _
                                ; not eax
                                ; mov [DWORD reg1m as _], eax
                            );
                        } else {
                            dynasm!(ops
                                ; mov eax, [DWORD reg2 as _]
                                ; not eax
                                ; mov [DWORD reg1m as _], eax
                            );
                        }
                    },
                    AluInstruction::Xor => {
                        if is_imm {
                            dynasm!(ops
                                ; mov eax, DWORD imm as _
                                ; xor eax, [DWORD reg1m as _]
                                ; mov [DWORD reg1m as _], eax
                            );
                        } else {
                            dynasm!(ops
                                ; mov eax, [DWORD reg2 as _]
                                ; xor eax, [DWORD reg1m as _]
                                ; mov [DWORD reg1m as _], eax
                            );
                        }
                    },
                    AluInstruction::Or => {
                        if is_imm {
                            dynasm!(ops
                                ; mov eax, DWORD imm as _
                                ; or eax, [DWORD reg2 as _]
                                ; mov [DWORD reg1m as _], eax
                            );
                        } else {
                            dynasm!(ops
                                ; mov eax, [DWORD reg2 as _]
                                ; or eax, [DWORD reg3 as _]
                                ; mov [DWORD reg1m as _], eax
                            );
                        }
                    },
                    AluInstruction::And => {
                        // registers[r1] = registers[r2] & last_value;
                        if is_imm {
                            dynasm!(ops
                                ; mov eax, DWORD imm as _
                                ; and eax, [DWORD reg2 as _]
                                ; mov [DWORD reg1m as _], eax
                            );
                        } else {
                            dynasm!(ops
                                ; mov eax, [DWORD reg2 as _]
                                ; and eax, [DWORD reg3 as _]
                                ; mov [DWORD reg1m as _], eax
                            );
                        }
                    },
                    AluInstruction::Shl => {
                        // cout = registers[r2] & (1 << 31);
                        // registers[r1] = registers[r2] << 1;
                        dynasm!(ops
                            ; mov eax, [DWORD reg2 as _]
                            ; mov ebx, 1 << 31
                            ; and ebx, eax
                            ; shl eax, 1
                            ; mov [DWORD reg1m as _], eax
                        );
                    },
                    AluInstruction::Shr => {
                        // cout = registers[r2] & 1;
                        // registers[r1] = registers[r2] >> 1;
                        dynasm!(ops
                            ; mov eax, [DWORD reg2 as _]
                            ; mov ebx, 1
                            ; and ebx, eax
                            ; shr eax, 1
                            ; mov [DWORD reg1m as _], eax
                        );
                    },
                    AluInstruction::Rotl => {
                        // cout = registers[r2] & (1 << 31);
                        // registers[r1] = registers[r2].rotate_left(1);
                        dynasm!(ops
                            ; mov eax, [DWORD reg2 as _]
                            ; mov ebx, 1 << 31
                            ; and ebx, eax
                            ; rol eax, 1
                            ; mov [DWORD reg1m as _], eax
                        );
                    },
                    AluInstruction::Rotr => {
                        // cout = registers[r2] & 1;
                        // registers[r1] = registers[r2].rotate_right(1);
                        dynasm!(ops
                            ; mov eax, [DWORD reg2 as _]
                            ; mov ebx, 1
                            ; and ebx, eax
                            ; ror eax, 1
                            ; mov [DWORD reg1m as _], eax
                        );
                    },
                    AluInstruction::Add => {
                        // let original = registers[r2];
                        // registers[r1] = registers[r2].wrapping_add(last_value);
                        // cout = if original > registers[r1] { 1 } else { 0 };
                        if is_imm {
                            dynasm!(ops
                                ; mov eax, DWORD imm as _
                            );
                        } else {
                            dynasm!(ops
                                ; mov eax, [DWORD reg3 as _]
                            );
                        }
                        dynasm!(ops
                                ; add eax, [DWORD reg2 as _]
                                ; setc bl
                                ; mov [DWORD reg1m as _], eax
                        );
                    },
                    AluInstruction::Sub => {
                        // let original = registers[r2];
                        // registers[r1] = registers[r2].wrapping_sub(last_value);
                        // cout = if original < registers[r1] { 1 } else { 0 };
                        if is_imm {
                            dynasm!(ops
                                ; mov eax, DWORD imm as _
                            );
                        } else {
                            dynasm!(ops
                                ; mov eax, [DWORD reg3 as _]
                            );
                        }
                        dynasm!(ops
                                ; sub eax, [DWORD reg2 as _]
                                ; setc bl
                                ; mov [DWORD reg1m as _], eax
                        );
                    },
                    AluInstruction::Inc => {
                        // let original = registers[r2];
                        // registers[r1] = registers[r2].wrapping_add(1);
                        // cout = if original > registers[r1] { 1 } else { 0 };
                        dynasm!(ops
                            ; mov eax, [DWORD reg2 as _]
                            ; inc eax
                            ; setc bl
                            ; mov [DWORD reg1m as _], eax
                        );
                    },
                    AluInstruction::Dec => {
                        // let original = registers[r2];
                        // registers[r1] = registers[r2].wrapping_sub(1);
                        // cout = if original < registers[r1] { 1 } else { 0 };
                        dynasm!(ops
                            ; mov eax, [DWORD reg2 as _]
                            ; dec eax
                            ; setc bl
                            ; mov [DWORD reg1m as _], eax
                        );
                    },
                    AluInstruction::Mul => {
                        // let val1 = registers[r3] as u64;
                        // let val2 = registers[r4] as u64;
                        // let res = val1.wrapping_mul(val2);
                        // registers[r1] = res as u32;
                        // registers[r2] = (res >> 32) as u32;
                        dynasm!(ops
                            ; mov eax, [DWORD reg3 as _]
                            ; mov ebx, [DWORD reg4 as _]
                            ; mul ebx
                            ; mov [DWORD reg1m as _], eax
                            ; mov [DWORD reg2m as _], edx
                        );
                    }
                }
                dynasm!(ops
                    // set the cout flag
                    ; movzx eax, bl
                    ; shl eax, 3
                    // set the zero flag
                    ; mov ebx, [DWORD reg1m as _]
                    ; test ebx, ebx
                    ; setz bl
                    ; or eax, ebx
                    // set the sign flag
                    // ; mov ebx, [DWORD reg1m as _]
                    ; and ebx, 1 << 31
                    ; shr ebx, 31 - 4 // shift the sign bit to the 4th bit
                    ; or eax, ebx
                    ; mov [DWORD regflags as _], eax
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
                            dynasm!(ops
                                ; mov eax, DWORD imm as _
                                ; shl eax, 16
                                ; mov [DWORD reg1m as _], eax
                            );
                        } else {
                            // mov
                            // registers[r1] = registers[r2];
                            dynasm!(ops
                                ; mov eax, [DWORD reg2 as _]
                                ; mov [DWORD reg1m as _], eax
                            );
                        }
                    },
                    CoreInstruction::Sw => {
                        // mem[registers[r1] as usize] = registers[r2];
                        dynasm!(ops
                            ; mov eax, [DWORD reg1 as _] // load reg1
                            ; and eax, 0xFFFFFF // mem is 24b
                            // ; shl eax, 2 // multiply by 4 (4B = 32b)
                            // ; add rax, mem as _ // get real address
                            ; mov ebx, [DWORD reg2 as _] // load reg2
                            ; mov [memm as _ + 4 * rax], ebx // save the value into the address
                        );
                    },
                    CoreInstruction::Lw => {
                        // registers[r1] = mem[registers[r2] as usize];
                        dynasm!(ops
                            ; mov eax, [DWORD reg2 as _] // load reg2
                            ; and eax, 0xFFFFFF // mem is 24b
                            // ; shl eax, 2 // multiply by 4 (4B = 32b)
                            // ; add rax, mem as _ // get real address (mem + reg2)
                            ; mov rax, [memm as _ + 4 * rax] // load the value at the address
                            ; mov [DWORD reg1m as _], rax // save the value into reg1
                        );
                    },
                    CoreInstruction::Jmp => {
                        // jmpin = 2;
                        // jmpto = if is_imm { imm } else { registers[r1] };
                        // we don't technically know our offset yet :( If we need it next run, we'll update it then
                        if is_imm && (self.assembled.contains_key(&imm)) {
                            let assembled = self.assembled.get(&imm).unwrap();
                            dynasm!(ops
                                ; mov rax, QWORD assembled.buf.ptr(assembled.offset) as _
                                ; jmp rax
                            );
                        } else {
                             dynasm!(ops
                                ; mov rdi, QWORD selfptr as _);
                            if is_imm {
                                dynasm!(ops
                                    ; mov esi, imm as _
                                );
                                self.jmp_targets.insert(imm);
                            } else {
                                dynasm!(ops
                                    ; mov esi, [DWORD reg1 as _]
                                );
                            }
                            dynasm!(ops
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
                        dynasm!(ops
                            ; mov rbx, [DWORD regflags as _]
                            ; and rbx, 1 // set real zero flag if zero flag is not set
                            ; jz =>skip // if zero flag is not set
                            );
                        if is_imm && (self.assembled.contains_key(&imm)) {
                            let assembled = self.assembled.get(&imm).unwrap();
                            dynasm!(ops
                                ; mov rax, QWORD assembled.buf.ptr(assembled.offset) as _
                            );
                        } else {
                            dynasm!(ops
                                ; mov rdi, QWORD selfptr as _);
                            if is_imm {
                                dynasm!(ops
                                    ; mov esi, imm as _
                                );
                                self.jmp_targets.insert(imm);
                            } else {
                                dynasm!(ops
                                    ; mov esi, [DWORD reg1 as _]
                                );
                            }
                            dynasm!(ops
                                ; mov rax, QWORD get_address as _
                                ; call rax
                                ; jmp rax
                            );
                        }
                        dynasm!(ops
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
                        dynasm!(ops
                            ; mov rbx, [DWORD regflags as _]
                            ; and rbx, 1 // set real zero flag if zero flag is not set
                            ; jnz =>skip // if zero flag is not set
                            );
                        if is_imm && (self.assembled.contains_key(&imm)) {
                            let assembled = self.assembled.get(&imm).unwrap();
                            dynasm!(ops
                                ; mov rax, QWORD assembled.buf.ptr(assembled.offset) as _
                            );
                        } else {
                            dynasm!(ops
                                ; mov rdi, QWORD selfptr as _);
                            if is_imm {
                                dynasm!(ops
                                    ; mov esi, imm as _
                                );
                                self.jmp_targets.insert(imm);
                            } else {
                                dynasm!(ops
                                    ; mov esi, [DWORD reg1 as _]
                                );
                            }
                            dynasm!(ops
                                ; mov rax, QWORD get_address as _
                                ; call rax
                                ; jmp rax
                            );
                        }
                        dynasm!(ops
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
                        dynasm!(ops
                            ; mov eax, [DWORD reg1 as _] // load reg1
                            ; and eax, 0xFFFF // mask position
                            ; mov ebx, eax
                            ; shr ebx, 8
                            ; shl eax, 8
                            ; or eax, ebx // swap the bytes
                            ; and eax, 0xFFFF // mask them again, needed both times for proper mask
                            ; mov ebx, [DWORD reg2 as _] // load reg2
                            ; and ebx, 0xFFFFFF // mask color
                            ; mov [displaym as _ + 4 * rax], ebx // save the value into the address
                        );
                    },
                    CoreInstruction::DClear => {
                        // display.iter_mut().for_each(|x| *x = 0);
                        dynasm!(ops
                            ; mov rax, QWORD clear_display as _
                            ; call rax
                        );
                    }
                }
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
        dynasm!(ops; ret);
        let buf = ops.finalize().unwrap();
        let assembled = Assembled {
            buf,
            offset: assembly_offset,
            offsets,
            last: last_instruction.unwrap(),
            size: offset - start_offset
        };
        let mut file = std::fs::File::create(format!("assembly/{}.bin", start_offset)).unwrap();
        use std::io::Write;
        file.write_all(assembled.buf.as_ref()).unwrap();
        dbg!(&assembled);
        self.assembled.insert(start_offset, assembled);
    }
}

extern "C" fn get_address(cpu: *mut Cpu, addr: u32) -> usize {
    println!("jump requested to {:x}", addr);
    let cpu = unsafe { &mut *cpu };
    cpu.jmp_targets.insert(addr);
    match cpu.assembled.get(&addr) {
        Some(assembled) => assembled.buf.ptr(assembled.offset) as usize,
        None => {
            cpu.assemble_offset(addr);
            let assembled = cpu.assembled.get(&addr).unwrap();
            assembled.buf.ptr(assembled.offset) as usize
        }
    }
}

extern "C" fn clear_display(display: *mut u32) {
    unsafe {
        slice::from_raw_parts_mut(display, 256*256)
    }.iter_mut().for_each(|x| *x = 0);
}