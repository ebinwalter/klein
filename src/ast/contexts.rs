#[feature(never_type)]
use lrpar::Span;
use std::io::stdout;
use std::io::Write;
use std::ptr;
use std::rc::Rc;
use std::hash::{DefaultHasher, Hasher};
use std::collections::HashMap;

use crate::ast::*;

// We define this type alias because 99% of the time we
// aren't going to care about the lifetimes involved here.
// It makes implementing the AST trait a fair bit more
// concise.
pub type Up<'a, 'b> = &'a mut Unparser<'b>;

enum UnparserOutput {
    Stdout(std::io::Stdout),
    Vec(Vec<u8>)
}

pub struct Unparser<'a> {
    out: UnparserOutput,
    ref_text: &'a [u8],
    indent: usize,
}

impl<'a> Unparser<'a> {
    pub fn new_stdout(ref_text: &'a str) -> Unparser<'a> {
        Unparser {
            out: UnparserOutput::Stdout(stdout()),
            indent: 0,
            ref_text: ref_text.as_bytes(),
        }
    }
    
    pub fn new_buf(ref_text: &'a str) -> Unparser<'a> {
        Unparser {
            out: UnparserOutput::Vec(Vec::new()),
            indent: 0,
            ref_text: ref_text.as_bytes(),
        }
    }

    pub fn as_buf(self) -> Option<Vec<u8>> {
        match self.out {
            UnparserOutput::Stdout(_) => None,
            UnparserOutput::Vec(v) => Some(v)
        }
    }

    pub fn write(&mut self, s: &str) {
        let to_write = s.as_bytes();
        match self.out {
            UnparserOutput::Vec(ref mut v) => v.extend_from_slice(to_write),
            UnparserOutput::Stdout(ref mut s) => {
                s.write_all(to_write).unwrap();
            }
        };
    }

    pub fn space(&mut self) {
        self.write(" ");
    }

    pub fn new_line(&mut self) {
        self.write("\n");
    }

    pub fn end_stmt(&mut self) {
        self.write(";\n");
    }

    pub fn write_span(&mut self, s: Span) {
        let to_write = &self.ref_text[s.start()..s.end()];
        match self.out {
            UnparserOutput::Vec(ref mut v) => v.extend_from_slice(to_write),
            UnparserOutput::Stdout(ref mut s) => {
                s.write_all(to_write).unwrap();
            }
        };
    }

    pub fn write_indent(&mut self) {
        for _ in 0..self.indent {
            let _ = self.write("  ");
        }
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn outdent(&mut self) {
        self.indent = self.indent.saturating_sub(1);
    }
}

pub struct NameAnalysisContext<'a> {
    pub ref_text: &'a str,
    pub sym_tab: SymbolTable,
    pub err_msgs: Vec<String>,
    pub offset_stack: Vec<u32>,
    pub struct_mode: bool
}

impl<'a> NameAnalysisContext<'a> {
    pub fn str_for_id(&self, id: &Id) -> &str {
        unsafe {
            self.ref_text.get_unchecked(id.span.start()..id.span.end())
        }
    }
    pub fn start_frame(&mut self) {
        self.offset_stack.push(4);
    }
    pub fn end_frame(&mut self) {
        self.offset_stack.pop();
    }
    pub fn start_branch(&mut self) {
        self.offset_stack.push(*self.offset_stack.last().unwrap());
    }
    pub fn end_branch(&mut self) {
        self.offset_stack.pop();
    }
    pub fn add_offset(&mut self, size: u32) -> u32 {
        let last = self.offset_stack.last_mut().unwrap();
        let old = *last;
        *last += size;
        old
    }
    pub fn string_for_id(&self, id: &Id) -> String {
        unsafe {
            self.ref_text.get_unchecked(id.span.start()..id.span.end()).to_owned()
        }
    }
    pub fn define_or_err(&mut self, id: &Id, sym: Symbol) {
        let name = self.string_for_id(id); 
        if let Err(SymTabError::Exists) = self.sym_tab.define(name.to_owned(), sym) {
            self.err_msgs.push(format!("Multiple declaration of identifier {name}"));
        }
    }
    pub fn lookup_or_err(&mut self, id: &Id) -> Option<Rc<Symbol>> {
        let name = self.str_for_id(id);
        match self.sym_tab.lookup_global(name) {
            Ok(sym) => Some(sym),
            Err(SymTabError::DoesntExist) => {
                self.err_msgs.push(format!("Undefined identifier {} at line {}, col {}", name, id.line, id.col));
                None
            }
            Err(_) => {
                self.err_msgs.push(
                format!("Empty symbol table encountered while looking up name '{}'",
                    name));
                None
            }
        }
    }
    pub fn lookup(&mut self, id: &Id) -> Option<Rc<Symbol>> {
        let name = self.str_for_id(id);
        match self.sym_tab.lookup_global(name) {
            Ok(sym) => Some(sym),
            Err(SymTabError::DoesntExist) => None,
            Err(_) => {
                panic!("Empty symbol table encountered while looking up name '{}'", name);
            }
        }
    }
    pub fn raise_error(&mut self, at: &dyn Ast, string: String) {
        let (line, col) = at.code_location().unwrap_or((0, 0));
        let m = format!(
            "At line {line}, col {col}: {string}"
        );
        self.err_msgs.push(m);
    }
}

pub type NACtx<'a, 'b> = &'a mut NameAnalysisContext<'b>;

pub struct TypeCache {
    storage: HashMap<u64, Type>,
}

impl TypeCache {
    pub fn new() -> Self {
        Self {
            storage: HashMap::new(),
        }
    }

    pub fn insert<T: ?Sized>(&mut self, what: *const T, ty: &Type) {
        let mut hasher = DefaultHasher::new();
        ptr::hash(what, &mut hasher);
        self.storage.insert(hasher.finish(), ty.clone());
    }

    pub fn get<T: ?Sized>(&self, what: *const T) -> Option<&Type> {
        let mut hasher = DefaultHasher::new();
        ptr::hash(what, &mut hasher);
        self.storage.get(&hasher.finish())
    }
}


pub struct TypeCheckingContext<'a> {
    pub (super) err_msgs: Vec<String>,
    pub (super) ref_text: &'a str,
    pub type_cache: TypeCache, 
}

pub type TCCtx<'a, 'b> = &'a mut TypeCheckingContext<'b>;

impl TypeCheckingContext<'_> {
    pub fn raise_error(&mut self, at: Rc<dyn Ast>,  string: String) {
        let (line, col) = at.code_location().unwrap_or((0, 0));
        let m = format!(
            "At line {line}, col {col}: {string}"
        );
        self.err_msgs.push(m);
    }

    pub fn cache_type<T: ?Sized>(&mut self, what: *const T, ty: &Type) {
        self.type_cache.insert(what, ty)
    }
}

pub struct OffsetsContext {
    pub(super) offset_stack: Vec<i32>,
    aligner: Aligner,
}

impl OffsetsContext {
    pub fn new(offset: i32) -> Self {
        OffsetsContext {
            offset_stack: Vec::new(),
            aligner: Aligner::new_downward(offset, 1),
        }
    }
    pub fn start_frame(&mut self) {
        self.offset_stack.push(self.aligner.next_offset);
        // Adjusted from -8 to -24 to account for saving $ra, $fp, $t4, $t5, $t6, $t7 (6 registers * 4 bytes)
        self.aligner.next_offset = -24;
    }
    pub fn push_var(&mut self, size: u32) -> i32 {
        self.aligner.place(size)
    }
    pub fn start_scope(&mut self) {
        self.offset_stack.push(self.aligner.next_offset);
    }
    /// Ends a scope created by `start_scope`.  Returns the value we can
    /// add to the stack pointer to discard the scope's variables.
    pub fn end_scope(&mut self) -> u32 {
        let out = self.aligner.next_offset;
        self.aligner.next_offset = self.offset_stack.pop().unwrap();
        (self.aligner.next_offset-out).try_into().unwrap()
    }
    /// Ends a frame created by `start_frame`.
    pub fn end_frame(&mut self) -> u32 {
        let mut out = self.aligner.next_offset;
        // Adjust so frame size is word-aligned
        if out % 4 != 0 {
            // + because out is negative
            let extra = 4 + (out % 4);
            out -= extra;
        }
        self.aligner.next_offset = self.offset_stack.pop().unwrap();
        (-out).try_into().unwrap()
    }
}

pub type OCtx<'a> = &'a mut OffsetsContext;

pub struct Aligner {
    next_offset: i32,
    grow_downward: bool,
    min_alignment: i32
}

impl Aligner {
    pub fn new(start: i32, min_alignment: i32) -> Self {
        Aligner {
            next_offset: start,
            grow_downward: false,
            min_alignment
        }
    }
    pub fn new_downward(start: i32, min_alignment: i32) -> Self {
        Aligner {
            next_offset: start,
            grow_downward: true,
            min_alignment
        }
    }
    pub fn place(&mut self, bytes: u32) -> i32 {
        let alignment = match bytes {
            0 => unreachable!(),
            1 => 1,
            2 => 2,
            _x => 4
        }.max(self.min_alignment);
        let bytes = bytes as i32;
        if self.grow_downward {
            if self.next_offset % alignment != 0 {
                self.next_offset -= alignment + self.next_offset % alignment;
            }
            let old = self.next_offset;
            self.next_offset -= bytes;
            old
        } else {
            if self.next_offset % alignment != 0 {
                self.next_offset += alignment - self.next_offset % alignment;
            }
            let old = self.next_offset;
            self.next_offset += bytes;
            old
        }
    }
}

#[cfg(test)]
#[test]
fn aligner_test() {
    let mut a = Aligner::new(4, 1);
    assert_eq!(a.place(1), 4);
    assert_eq!(a.place(1), 5);
    assert_eq!(a.place(1), 6);
    assert_eq!(a.place(4), 8);
    assert_eq!(a.place(4), 12);
    assert_eq!(a.place(1), 16);

    let mut b = Aligner::new_downward(-8, 1);
    assert_eq!(b.place(4), -8);
    assert_eq!(b.place(4), -12);
    assert_eq!(b.place(2), -16);
    assert_eq!(b.place(2), -18);
    assert_eq!(b.place(1), -20);
    assert_eq!(b.place(1), -21);
    assert_eq!(b.place(2), -22);
    assert_eq!(b.place(3), -24);
}
