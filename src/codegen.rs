use std::{collections::{HashMap, HashSet}, hash::{DefaultHasher, Hasher}, io::Write, rc::Rc, cell::RefCell};

use lrpar::Span;

pub struct HintResponse {
    output_register: &'static str,
    temporaries: Vec<&'static str>,
}

use crate::ast::TypeCache;
pub struct Codegen<'a> {
    output: Box<dyn Write>,
    emit_buffer: Option<Vec<Box<dyn Emittable>>>,
    next_label: usize,
    string_table: HashMap<String, String>,
    ref_text: &'a str,
    frame_base: Rc<RefCell<i32>>,
    hints: HashMap<u64, Option<HintResponse>>,
    available_regs: Vec<&'static str>,
    regs_used: HashSet<&'static str>,
    pub type_cache: TypeCache,
    pub function_stack: Vec<String>,
}

impl<'a> Codegen<'a> {
    pub const T0: &'static str = "$t0";
    pub const T1: &'static str = "$t1";
    pub const T2: &'static str = "$t2";
    pub const T3: &'static str = "$t3";
    pub const V0: &'static str = "$v0";
    pub const V1: &'static str = "$v1";
    pub const A0: &'static str = "$a0";
    pub const SP: &'static str = "$sp";
    pub const FP: &'static str = "$fp";
    pub const RA: &'static str = "$ra";
    pub const ZERO: &'static str = "$zero";

    pub fn new(output: Box<dyn Write>, text: &'a str, cache: TypeCache) -> Codegen<'a> {
        Self {
            output,
            next_label: 0,
            ref_text: text,
            string_table: HashMap::new(),
            type_cache: cache,
            frame_base: Rc::new(0.into()),
            function_stack: Vec::new(),
            emit_buffer: None,
            available_regs: Vec::new(),
            hints: HashMap::new(),
            regs_used: HashSet::new(),
        }
    }

    pub fn set_frame_offset(&mut self, offset: i32) {
        *self.frame_base.try_borrow_mut().unwrap() = offset;
    }

    pub fn start_new_function(&mut self, name: String) {
        self.function_stack.push(name);
        self.available_regs = vec!["$t3", "$t4", "$t5", "$t6", "$t7"];
        self.regs_used = HashSet::new();
    }

    pub fn end_function(&mut self) {
        self.function_stack.pop();
    }

    pub fn take_storage_reg(&mut self) -> Option<&'static str> {
        let r = self.available_regs.pop()?;
        self.regs_used.insert(r);
        Some(r)
    }

    pub fn get_frame_ref(&mut self, offset: i32) -> IxRef {
        IxRef(offset, self.frame_base.clone())
    }

    pub fn get_frame_imm(&mut self, offset: i32) -> RefImm {
        RefImm(offset, self.frame_base.clone())
    }

    pub fn get_regs_used(&self) -> Vec<&'static str> {
        self.regs_used.iter()
            .copied()
            .collect()
    }

    pub fn relinquish_regs(&mut self, regs: &[&'static str]) {
        self.available_regs.extend_from_slice(regs);
    }

    pub fn emit<'t, 's: 't, T: Emittable + 'static>(&'s mut self, what: T) {
        if let Some(ref mut buffer) = self.emit_buffer {
            buffer.push(Box::new(what));
        } else {
            self.output
                .write_all(what.emit().as_bytes())
                .unwrap();
        }
    }

    /// Delay the emission of a sequence of instructions.
    /// To emit instructions delimited by `start_buffering` and `finish_buffering,`
    /// call emit on the result of `finish_buffering`.
    pub fn start_buffering(&mut self) {
        self.emit_buffer = Some(Vec::new());
    }

    /// Terminates a sequence of instructions being recorded after
    /// `start_buffering.`
    pub fn finish_buffering(&mut self) -> Option<Vec<Box<dyn Emittable>>> {
        self.emit_buffer.take()
    }

    pub fn next_label(&mut self) -> String {
        let label = self.next_label;
        self.next_label += 1;
        format!("L{}", label)
    }

    pub fn emit_push(&mut self, reg: &'static str) {
        self.emit(("sw", reg, Self::SP, Ix(0)));
        self.emit(("addiu", Self::SP, Self::SP, -4));
    }

    pub fn emit_pop(&mut self, reg: &'static str) {
        self.emit(("lw", reg, Self::SP, Ix(4)));
        self.emit(("addiu", Self::SP, Self::SP, 4));
    }

    pub fn label_for_string(&mut self, string_span: Span) -> String {
        let string = unsafe {
            self.ref_text.get_unchecked(string_span.start()..string_span.end())
        };
        if let Some(s) = self.string_table.get(string) {
            s.clone()
        } else {
            let label = self.next_label();
            self.string_table.insert(string.to_owned(), label.clone());
            label
        }
    }
}

impl Drop for Codegen<'_> {
    fn drop(&mut self) {
        let mut final_emission: Vec<Box<dyn Emittable>> = Vec::new();
        for (key, value) in self.string_table.iter() {
            final_emission.push(Box::new(Directive::Data));
            final_emission.push(Box::new(Label(value.clone())));
            final_emission.push(Box::new(Directive::Asciiz(key.clone())));
        }
        for emission in final_emission {
            self.emit(emission);
        }
        self.output.flush().unwrap();
    }
}

pub type CG<'a> = Codegen<'a>;

type Opcode = &'static str;
type Reg = &'static str;

pub trait Emittable {
    fn emit(&self) -> String;
}

impl Emittable for Box<dyn Emittable> {
    fn emit(&self) -> String {
        self.as_ref().emit()
    }
}

impl Emittable for (Opcode, Reg, Reg) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2)
    }
}

impl Emittable for (Opcode, Label) {
    fn emit(&self) -> String {
        format!("\t{} {}\n", self.0, self.1.0)
    }
} 

impl Emittable for (Opcode, Reg, Label) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2.0)
    }
} 

impl Emittable for (Opcode, Reg, Reg, i32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, self.3)
    }
}

impl Emittable for (Opcode, Reg, Reg, RefImm) {
    fn emit(&self) -> String {
        let ref_offset = *self.3.1.try_borrow().unwrap();
        let regular_offset = self.3.0;
        if regular_offset > 0 {
            format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, regular_offset)
        } else {
            format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, ref_offset + regular_offset)
        }
    }
}

impl Emittable for (Opcode, Reg, i32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2)
    }
}

impl Emittable for (Opcode, Reg, u32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2)
    }
}

impl Emittable for (Opcode, Reg, Reg, Ix) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}({})\n", self.0, self.1, self.3.0, self.2)
    }
}

impl<'a> Emittable for (Opcode, Reg, Reg, IxRef) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}({})\n", self.0, self.1, self.3.0 + *self.3.1.try_borrow().unwrap(), self.2)
    }
}

impl Emittable for (Opcode, Reg, Reg, u32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, self.3)
    }
}

impl Emittable for (Opcode, Reg, Reg, Reg) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, self.3)
    }
}

impl Emittable for (Opcode, Reg, Reg, Label) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, self.3.0)
    }
}

impl Emittable for (Opcode, Reg) {
    fn emit(&self) -> String {
        format!("\t{} {}\n", self.0, self.1)
    }
}

impl Emittable for Opcode {
    fn emit(&self) -> String {
        format!("\t{self}\n")
    }
}

pub struct Comment(pub String);

impl Emittable for Comment {
    fn emit(&self) -> String {
        format!("\t# {}\n", self.0)
    }
}

pub struct Label(pub String);

impl Emittable for Label {
    fn emit(&self) -> String {
        format!("{}:\n", self.0)
    }
}

pub enum Directive {
    Data,
    Asciiz(String),
    Text,
    Space(u32),
}

impl Emittable for Directive {
    fn emit(&self) -> String {
        match self {
            Self::Data => ".data\n".into(),
            Self::Asciiz(s) => format!(".asciiz {}\n", s),
            Self::Text => ".text\n".into(),
            Self::Space(size) => format!(".space {size}\n")
        }
    }
}

pub struct Ix(pub i32);

pub struct IxRef(pub i32, Rc<RefCell<i32>>);

pub struct RefImm(pub i32, Rc<RefCell<i32>>);

impl Emittable for Vec<Box<dyn Emittable>> {
    fn emit(&self) -> String {
        self.iter()
            .map(Emittable::emit)
            .collect::<Vec<_>>()
            .join("")
    }
}
