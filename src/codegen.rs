use std::{collections::HashMap, io::Write};

use lrpar::Span;

use crate::ast::TypeCache;
pub struct Codegen<'a> {
    output: Box<dyn Write>,
    next_label: usize,
    string_table: HashMap<String, String>,
    ref_text: &'a str,
    pub type_cache: TypeCache
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
            type_cache: cache
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
        format!("L{}", label)
    }

    pub fn emit_push(&mut self, reg: &str) {
        self.emit(("sw", reg, Self::SP, Ix(0)));
        self.emit(("addiu", Self::SP, Self::SP, -4));
    }

    pub fn emit_pop(&mut self, reg: &str) {
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

impl Emittable for (&str, &str, &str) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2)
    }
}

impl<T: AsRef<str>> Emittable for (&str, Label<T>) {
    fn emit(&self) -> String {
        format!("\t{} {}\n", self.0, self.1.0.as_ref())
    }
} 

impl<T: AsRef<str>> Emittable for (&str, &str, Label<T>) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2.0.as_ref())
    }
} 

impl Emittable for (&str, &str, &str, i32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, self.3)
    }
}

impl Emittable for (&str, &str, i32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2)
    }
}

impl Emittable for (&str, &str, u32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}\n", self.0, self.1, self.2)
    }
}

impl Emittable for (&str, &str, &str, Ix) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}({})\n", self.0, self.1, self.3.0, self.2)
    }
}

impl Emittable for (&str, &str, &str, u32) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, self.3)
    }
}

impl Emittable for (&str, &str, &str, &str) {
    fn emit(&self) -> String {
        format!("\t{} {}, {}, {}\n", self.0, self.1, self.2, self.3)
    }
}

impl<T: AsRef<str>> Emittable for (&str, &str, &str, Label<T>) {
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
            Self::Asciiz(s) => format!(".data\n.asciiz {}\n", s),
            Self::Text => ".text\n".into(),
            Self::Space(size) => format!(".space {size}\n")
        }
    }
}

pub struct Ix(pub i32);
