use lrpar::Span;
use std::io::stdout;
use std::rc::Rc;
use std::hash::Hash;
use std::collections::HashMap;

use crate::ast::*;

// We define this type alias because 99% of the time we
// aren't going to care about the lifetimes involved here.
// It makes implementing the AST trait a fair bit more
// concise.
pub type Up<'a, 'b> = &'a mut Unparser<'b>;

pub struct Unparser<'a> {
    out: Box<dyn std::io::Write>,
    ref_text: &'a [u8],
    indent: usize,
}

impl<'a> Unparser<'a> {
    pub fn new_stdout(ref_text: &'a str) -> Self {
        Unparser {
            out: Box::new(stdout()),
            indent: 0,
            ref_text: ref_text.as_bytes(),
        }
    }

    pub fn write(&mut self, s: &str) {
        let _ = self.out.write(s.as_bytes());
    }

    pub fn space(&mut self) {
        let _ = self.out.write(" ".as_bytes());
    }

    pub fn new_line(&mut self) {
        let _ = self.out.write("\n".as_bytes());
    }

    pub fn end_stmt(&mut self) {
        let _ = self.out.write(";\n".as_bytes());
    }

    pub fn write_span(&mut self, s: Span) {
        let to_write = &self.ref_text[s.start()..s.end()];
        let _ = self.out.write(to_write);
    }

    pub fn write_indent(&mut self) {
        for _ in 0..self.indent {
            let _ = self.out.write("  ".as_bytes());
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
    pub fn str_for_id(&self, id: &IdNode) -> &str {
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
    pub fn string_for_id(&self, id: &IdNode) -> String {
        unsafe {
            self.ref_text.get_unchecked(id.span.start()..id.span.end()).to_owned()
        }
    }
    pub fn define_or_err(&mut self, id: &IdNode, sym: Symbol) {
        let name = self.string_for_id(id); 
        if let Err(SymTabError::Exists) = self.sym_tab.define(name.to_owned(), sym) {
            self.err_msgs.push(format!("Multiple declaration of identifier {name}"));
        }
    }
    pub fn lookup_or_err(&mut self, id: &IdNode) -> Option<Rc<Symbol>> {
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
    pub fn lookup(&mut self, id: &IdNode) -> Option<Rc<Symbol>> {
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

pub struct RCKey(pub Rc<dyn Ast>);

impl PartialEq for RCKey {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(
            Rc::into_raw(self.0.clone()), 
            Rc::into_raw(other.0.clone())
        )
    }
}

impl Eq for RCKey {}

impl Hash for RCKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(&*self.0, state);
    }
}

pub type TypeCache = HashMap<RCKey, TypeNode>;

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

    pub fn cache_type(&mut self, what: &Rc<dyn Ast>, ty: &TypeNode) {
        self.type_cache.insert(RCKey(what.clone()), ty.clone());
    }
}

pub struct OffsetsContext {
    pub(super) offset_stack: Vec<i32>,
    pub(super) offset: i32
}

impl OffsetsContext {
    pub fn start_frame(&mut self) {
        self.offset_stack.push(self.offset);
        self.offset = -8;
    }
    pub fn push_var(&mut self, size: u32) -> i32 {
        let old = self.offset;
        self.offset -= size as i32;
        old
    }
    pub fn start_scope(&mut self) {
        self.offset_stack.push(self.offset);
    }
    pub fn end_scope(&mut self) -> u32 {
        let out = self.offset;
        self.offset = self.offset_stack.pop().unwrap();
        (self.offset-out).try_into().unwrap()
    }
    pub fn end_frame(&mut self) -> u32 {
        let out = self.offset;
        self.offset = self.offset_stack.pop().unwrap();
        (-out).try_into().unwrap()
    }
}

pub type OCtx<'a> = &'a mut OffsetsContext;
