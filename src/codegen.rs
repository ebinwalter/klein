use std::{cell::{RefCell, Cell}, collections::{HashMap, HashSet}, fmt::Display, io::Write, rc::Rc};

use lrpar::Span;

use crate::ast::TypeCache;
pub struct Codegen<'a> {
    output: Box<dyn Write>,
    next_label: usize,
    string_table: HashMap<String, String>,
    pub ref_text: &'a str,
    reg_list: RegList,
    pub type_cache: TypeCache,
    pub function_stack: Vec<String>
}

pub trait Register: std::fmt::Display {
    fn str(&self) -> &str;
}

impl Register for &str {
    fn str(&self) -> &str {
        self
    }
}

#[allow(dead_code)]
impl<'a> Codegen<'a> {
    pub const T0: &'static str = "$t0";
    pub const T1: &'static str = "$t1";
    pub const T2: &'static str = "$t2";
    pub const T3: &'static str = "$t3";
    pub const T4: &'static str = "$t4";
    pub const T5: &'static str = "$t5";
    pub const T6: &'static str = "$t6";
    pub const T7: &'static str = "$t7";
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
            reg_list: Rc::new(RefCell::new((Vec::new(), HashSet::new()))),
            type_cache: cache,
            function_stack: Vec::new(),
        }
    }

    pub fn emit(&mut self, what: impl Emittable) {
        self.output
            .write_all(what.emit().as_bytes())
            .unwrap();
    }

    pub fn next_label(&mut self) -> String {
        let label = self.next_label;
        self.next_label += 1;
        format!("L{label}")
    }

    pub fn emit_push(&mut self, reg: impl Register) {
        self.emit(("sw", reg, Self::SP, Ix(0)));
        self.emit(("addiu", Self::SP, Self::SP, -4));
    }

    pub fn emit_pop(&mut self, reg: impl Register) {
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

    pub fn reset_regs(&mut self) {
        *self.reg_list.borrow_mut() = (vec![CG::T4, CG::T5, CG::T6, CG::T7], HashSet::new());
    }

    pub fn next_free_reg(&mut self) -> Option<AllocatedRegister> {
        let deref!((ref mut free, ref mut used)) = self.reg_list.borrow_mut();
        free.pop()
            .inspect(|r| { used.insert(r); })
            .map(|r| AllocatedRegister { reg: r, list: Some(self.reg_list.clone()) })
    }

    pub fn save_used(&mut self) -> RegList {
        let mut offset = 0;
        let regs_used = self.reg_list.borrow_mut().1.iter()
            .copied()
            .collect::<Vec<&'static str>>();
        if !regs_used.is_empty() {
            for &reg in regs_used.iter() {
                self.emit(("sw", reg, "$sp", Ix(offset)));
                offset -= 4;
            }
            self.emit(("addi", "$sp", "$sp", offset));
        }
        let regs = self.reg_list.clone();
        self.reg_list = Rc::new(RefCell::new((vec![], HashSet::new())));
        regs
    }

    pub fn restore_used(&mut self, orig_list: RegList) {
        let mut offset = 0;
        if !orig_list.borrow().1.is_empty() {
            for reg in orig_list.borrow().1.iter().copied() {
                offset += 4;
                self.emit(("lw", reg, CG::SP, Ix(offset)));
            }
            self.emit(("addi", CG::SP, CG::SP, offset));
        }
        self.reg_list = orig_list;
    }
}

// The structure used to track what registers we have allocated
pub type RegList = Rc<RefCell<(Vec<&'static str>, HashSet<&'static str>)>>;

pub struct AllocatedRegister {
    pub reg: &'static str,
    list: Option<RegList>,
}

impl AllocatedRegister {
    pub fn new(reg: &'static str) -> Self {
        AllocatedRegister { reg, list: None }
    }
}

impl Register for &AllocatedRegister {
    fn str(&self) -> &'static str {
        self.reg
    }
}

impl Register for AllocatedRegister {
    fn str(&self) -> &'static str {
        self.reg
    }
}

impl Display for AllocatedRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.reg)
    }
}

impl Drop for AllocatedRegister {
    fn drop(&mut self) {
        if let Some(ref list) = self.list {
            let deref!((ref mut free, ref mut used)) = list.try_borrow_mut().unwrap();
            free.push(self.reg);
            used.remove(self.reg);
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

pub trait Emittable {
    fn emit(&self) -> String;
}

impl Emittable for Box<dyn Emittable> {
    fn emit(&self) -> String {
        self.as_ref().emit()
    }
}

impl<T: Register, U: Register> Emittable for (&str, T, U) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1.str(), self.2.str())
    }
}

impl<T: AsRef<str>> Emittable for (&str, Label<T>) {
    fn emit(&self) -> String {
        format!("\t{} {}\n", self.0, self.1.0.as_ref())
    }
} 

impl<S: Register, T: AsRef<str>> Emittable for (&str, S, Label<T>) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2.0.as_ref())
    }
} 

impl<T: Register, U: Register> Emittable for (&str, T, U, i32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, self.3)
    }
}

impl<U: Register> Emittable for (&str, U, i32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2)
    }
}

impl<U: Register> Emittable for (&str, U, u32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2)
    }
}

impl<T: Register, U: Register> Emittable for (&str, T, U, Ix) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}({})\n", self.0, self.1, self.3.0, self.2.str())
    }
}

impl<T: Register, U: Register> Emittable for (&str, T, U, u32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, self.3)
    }
}

impl<T, U, V> Emittable for (&str, T, U, V)
    where T: Register,
          U: Register,
          V: Register
{
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1.str(), self.2.str(), self.3.str())
    }
}

impl<T: AsRef<str>, U: Register, V: Register> Emittable for (&str, U, V, Label<T>) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, self.3.0.as_ref())
    }
}

impl Emittable for (&str, &str) {
    fn emit(&self) -> String {
        format!("\t{} {}\n", self.0, self.1)
    }
}

impl Emittable for &str {
    fn emit(&self) -> String {
        format!("\t{self}\n")
    }
}

pub struct Comment<'a>(pub &'a str);

impl Emittable for Comment<'_> {
    fn emit(&self) -> String {
        format!("\t# {}\n", self.0)
    }
}

pub struct Label<T: AsRef<str>>(pub T);

impl<T: AsRef<str>> Emittable for Label<T> {
    fn emit(&self) -> String {
        format!("{}:\n", self.0.as_ref())
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
            Self::Asciiz(s) => format!(".asciiz {s}\n"),
            Self::Text => ".text\n".into(),
            Self::Space(size) => format!(".space {size}\n")
        }
    }
}

pub struct Ix(pub i32);
