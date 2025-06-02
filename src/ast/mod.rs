use std::collections::HashMap;
use std::fmt::Display;
use std::io::stdout;
use std::hash::Hash;
use std::rc::Rc;
use std::fmt::Write;
use std::cell::{Cell, OnceCell, RefCell};
use std::borrow::{Borrow, BorrowMut};
use crate::symbols::*;
use crate::codegen::*;

use lrpar::Span;

mod decls;
mod exprs;
mod stmts;
mod types;
mod contexts;

pub use types::*;
pub use decls::*;
pub use exprs::*;
pub use stmts::*;
pub use contexts::*;

pub trait Ast {
    /// Write a reconstructed and annotated version of the input to some output,
    /// determined by the Unparser struct.
    ///
    /// The completeness of the annotations depends on how many phases of
    /// compilation have been carried out.
    fn unparse(&self, up: Up);
    /// Record definitions and declarations in the symbol table and associate
    /// various parts of the AST that reference the same symbol.  Also,
    /// determine offsets of members from the base address of a struct.
    fn analyze_names(&self, na: NACtx) {}
    /// Check that our code has types such that code can actually be generated.
    /// Also resolve the names in struct access expressions.
    fn typecheck(&self, tc: TCCtx) -> Option<Type> { None }
    /// Compute offsets for variables declared in functions.
    fn compute_var_offsets(&self, oc: OCtx) {}
    /// Generates code for an AST node _unless_ the AST node should be treated
    /// as an lvalue.  In that case, this method will misbehave, and one needs
    /// to call codegen_lvalue instead.
    fn codegen(&self, cg: &mut Codegen) {
    }
    fn codegen_lvalue(&self, cg: &mut Codegen) {
        // We've written things such that this should never happen.
        // But we should crash and burn if it does happen, because we need to
        // go back and reexamine the structure of things.
        panic!("Attempt to use an unsuitable AST node as an rvalue");
    }
    /// Return a canonical symbol associated with this AST node.
    /// For example, if this is an ID, give the VarDecl symbol associated with
    /// its definition; if this is an AST node for a `struct X` type, give the StructDeclSymbol
    /// associated with that struct.
    fn symbol(&self) -> Option<&Symbol> {
        None
    }
    /// Return a reference-counting pointer to a symbol associated with this node,
    /// if any exists.
    fn symbol_rc(&self) -> Option<Rc<Symbol>> {
        None
    }
    /// Location of the AST node within our source file, if it exists.
    fn code_location(&self) -> Option<(usize, usize)> {
        None
    } 
}

/// Things that can be evaluated.
pub trait Expr: Ast {}
pub type Boxpr = Rc<dyn Expr>;

/// Things that posit something to be used or acted on.
pub trait Decl: Ast {
    fn split_decl(&self, tc: &mut TypeCache) -> Option<(Rc<dyn Decl>, Rc<dyn Stmt>)> {
        None
    }
}
pub type BoxDecl = Rc<dyn Decl>;

/// Things that can be sequenced.
pub trait Stmt: Ast {}
pub type BoxStmt = Rc<dyn Stmt>;

/// Things associated with an address.
pub trait Loc: Expr { }
pub type BoxLoc = Rc<dyn Loc>;

pub enum DeclOrStmt {
    Decl(BoxDecl),
    Stmt(BoxStmt),
}

impl Ast for DeclOrStmt {
    fn unparse(&self, up: Up) {
        match self {
            DeclOrStmt::Decl(d) => d.unparse(up),
            DeclOrStmt::Stmt(s) => s.unparse(up),
        }
    }

    fn analyze_names(&self, na: NACtx) {
        match self {
            DeclOrStmt::Decl(d) => (d.clone() as Rc<dyn Ast>).analyze_names(na),
            DeclOrStmt::Stmt(s) => (s.clone() as Rc<dyn Ast>).analyze_names(na),
        }
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        match self {
            DeclOrStmt::Decl(d) => d.clone().typecheck(tc),
            DeclOrStmt::Stmt(s) => s.clone().typecheck(tc),
        };
        None
    }

    fn compute_var_offsets(&self, oc: OCtx) {
        match self {
            DeclOrStmt::Decl(d) => d.compute_var_offsets(oc),
            DeclOrStmt::Stmt(s) => s.compute_var_offsets(oc),
        }
    }

    fn codegen(&self, cg: &mut Codegen) {
        match self {
            DeclOrStmt::Decl(d) => d.codegen(cg),
            DeclOrStmt::Stmt(s) => s.codegen(cg),
        }
    }
}

pub struct Program {
    pub decls: Vec<BoxDecl>,
}

impl Program {
    pub fn new(decls: Vec<BoxDecl>) -> Rc<Self> {
        Program {decls}.into()
    }
    pub fn print(&self, txt: &str) {
        let mut up = Unparser::new_stdout(txt);
        self.unparse(&mut up);
    }

    pub fn name_analysis(&self, txt: &str) -> bool {
        let mut na = NameAnalysisContext {
            ref_text: txt,
            err_msgs: Vec::new(),
            sym_tab: SymbolTable::new(),
            offset_stack: Vec::new(),
            struct_mode: false
        };
        self.analyze_names(&mut na);
        for err in na.err_msgs.iter() {
            eprintln!("{}", err)
        }
        na.err_msgs.is_empty()
    }

    pub fn type_checking<'a>(&self, txt: &'a str) -> Result<TypeCheckingContext<'a>, ()> {
        let mut tc = TypeCheckingContext {
            ref_text: txt,
            err_msgs: Vec::new(),
            type_cache: TypeCache::new(),
        };
        self.typecheck(&mut tc);
        for err in tc.err_msgs.iter() {
            eprintln!("{}", err)
        }
        if !tc.err_msgs.is_empty() {
            Err(())
        } else {
            Ok(tc)
        }
    }

    pub fn compute_offsets(&self) {
        let mut ctx = OffsetsContext::new(0);
        self.compute_var_offsets(&mut ctx);
    }

    pub fn codegen(&self, cg: &mut Codegen) {
        for decl in self.decls.iter() {
            decl.codegen(cg);
        }
    }
}

impl Ast for Program {
    fn unparse(&self, unparser: Up) {
        for decl in self.decls.iter() {
            decl.unparse(unparser);
        }
    }
    
    fn analyze_names(&self, na: NACtx) {
        for decl in self.decls.iter() {
            decl.analyze_names(na);
        }
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        for decl in self.decls.iter() {
            decl.typecheck(tc);
        }
        None
    }

    fn compute_var_offsets(&self, oc: OCtx) {
        self.decls
            .iter()
            .for_each(|d| d.compute_var_offsets(oc));
    }
}

pub struct ScopeBlock {
    pub list: RefCell<Vec<DeclOrStmt>>,
    pub table: OnceCell<TableLayer>,
    /// Size of variables for this scope on the stack (not including variables declared in nested scopes).
    /// Populated after `compute_var_offsets`.
    pub scope_size: OnceCell<u32>
}

impl ScopeBlock {
    fn new(list: Vec<DeclOrStmt>) -> Rc<Self> {
        ScopeBlock {
            list: list.into(),
            table: OnceCell::new(),
            scope_size: OnceCell::new(),
        }.into()
    }
    fn lift_decls(&self, tc: &mut TypeCache) {
        let mut stmt_list = Vec::new();
        let mut decl_list = Vec::new();
        for item in self.list.try_borrow().unwrap().iter() {
            match item {
                DeclOrStmt::Stmt(stmt) => stmt_list.push(stmt.clone()),
                DeclOrStmt::Decl(decl) => {
                    if let Some((decl, stmt)) = decl.split_decl(tc) {
                        decl_list.push(decl);
                        stmt_list.push(stmt);
                   } else {
                        decl_list.push(decl.clone());
                    }
                }
            }
        }
        let new_list = decl_list.into_iter().map(DeclOrStmt::Decl)
            .chain(stmt_list.into_iter().map(DeclOrStmt::Stmt))
            .collect();
        *self.list.try_borrow_mut().unwrap() = new_list;
    }

    pub fn get_list(&self) -> std::cell::Ref<Vec<DeclOrStmt>> {
        self.list.try_borrow().unwrap()
    } 
}

impl Ast for ScopeBlock {
    fn unparse(&self, up: Up) {
        // If we know how much we need to subtract from the stack pointer to
        // make room for this scope's variables, print that in brackets
        // before we print the block itself.
        if let Some(size) = self.scope_size.get() {
            up.write(&format!("[{size}]"))
        }
        up.write("{");
        up.new_line();
        up.indent();
        self.get_list().iter().for_each(|x| x.unparse(up));
        up.outdent();
        up.write_indent();
        up.write("}");
    }

    fn analyze_names(&self, na: NACtx) {
        na.sym_tab.push_scope();
        self.list.try_borrow().unwrap()
            .iter().for_each(|x| x.analyze_names(na));
        let the_scope = na.sym_tab.pop_scope().unwrap();
        self.table.set(the_scope).expect("Table already set for this scope block");
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        self.list.try_borrow().unwrap().iter().for_each(|x| { x.typecheck(tc); });
        self.lift_decls(&mut tc.type_cache);
        None
    }

    fn compute_var_offsets(&self, oc: OCtx) {
        oc.start_scope();
        self.get_list()
            .iter()
            .for_each(|d| d.compute_var_offsets(oc));
        self.scope_size.set(oc.end_scope()).unwrap();
    }

    fn codegen(&self, cg: &mut Codegen) {
        let size = *self.scope_size.get().unwrap() as i32;
        cg.emit(("addi", CG::SP, CG::SP, -4 * size));
        for item in self.get_list().iter() {
            item.codegen(cg);
        }
        cg.emit(("addi", CG::SP, CG::SP, 4 * size));
    }
}

pub struct Id {
    pub span: Span,
    pub sym: OnceCell<Rc<Symbol>>,
    pub line: usize,
    pub col: usize,
}

impl Clone for Id {
    fn clone(&self) -> Self {
        let cell = OnceCell::new();
        if let Some(v) = self.sym.get() {
            cell.set(v.clone()).unwrap();
        }
        Self {
            span: self.span,
            sym: cell,
            line: self.line,
            col: self.col
        }
    }
}

impl Id {
    pub fn new(span: Span, line: usize, col: usize) -> Rc<Self> {
        Id {
            span,
            line,
            col,
            sym: OnceCell::new(),
        }.into()
    }
    fn set_sym(&self, sym: Rc<Symbol>) {
        self.sym.set(sym).unwrap();
    }
}

impl std::fmt::Debug for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Identifier at line {}, col {}", self.line, self.col))
    }
}

fn span_to_str<'s>(s: &Span, str: &'s str) -> &'s str {
    unsafe {
        str.get_unchecked(s.start()..s.end())
    }
}

impl Ast for Id {
    fn unparse(&self, up: Up) {
        if let Some(sym) = self.sym.get() {
            up.write("$");
            if let Symbol::Var(ref v) = **sym {
                up.write(&format!("[{}]", v.offset.get().map(i32::to_string).unwrap_or("?".into())));
            }
        }
        up.write_span(self.span);
    }


    fn analyze_names(&self, na: NACtx) {
        if let Some(sym) = na.lookup_or_err(self) {
            self.sym.set(sym)
                .expect("Attempt to set IdNode's sym for the second time -- has name analysis been run more than once?");
        }
    }

    fn symbol(&self) -> Option<&Symbol> {
        self.sym.get().map(Borrow::borrow)
    }

    fn symbol_rc(&self) -> Option<Rc<Symbol>> {
        self.sym.get().cloned()
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        if let Symbol::Var(vs) = &**self.sym.get().unwrap() {
            let ty = (*vs.ty).clone();
            
            // Check if this is a struct type being used in an rvalue position
            /*
            if let Type::Struct(_, _) = ty {
                tc.raise_error(
                    Rc::new(self.clone()),
                    "Struct variables cannot be used directly in expressions. Consider using a reference (&) or accessing a field.".into()
                );
                return None;
            }
            */
            
            Some(ty)
        } else {
            tc.raise_error(Rc::new(self.clone()), "Attempted to check the type of a non-variable".into());
            None
        }
    }

    fn code_location(&self) -> Option<(usize, usize)> {
        Some((self.line, self.col))
    }

    fn codegen(&self, cg: &mut Codegen) {
        let sym = self.sym.get().unwrap().clone();
        match *sym {
            Symbol::Var(ref v) => {
                let &is_global = v.global.get().unwrap();
                let load_ins = match v.ty.size() {
                    1 => "lb",
                    4 => "lw",
                    _ => todo!()
                };
                if is_global {
                    cg.emit(Comment(&format!("Pushing value of global variable {}", v.id)));
                    cg.emit((load_ins, CG::T0, Label(format!("_{}", v.id))));
                } else {
                    cg.emit(Comment(&format!("Pushing value of local variable {}", v.id)));
                    cg.emit((load_ins, CG::T0, CG::FP, Ix(*v.offset.get().unwrap())));
                }
                cg.emit_push(CG::T0);
            },
            Symbol::Func(ref f) => {
                cg.emit(("la", CG::T0, Label(format!("_{}", f.id))));
                cg.emit_push(CG::T0);
            },
            _ => unreachable!("struct in expression position"),
        } 
    }

    fn codegen_lvalue(&self, cg: &mut Codegen) {
        let sym = self.sym.get().unwrap().clone();
        let Symbol::Var(ref v) = *sym else { return };
        if *v.global.get().unwrap() {
            cg.emit(Comment(&format!("Pushing addr of global variable {}", v.id)));
            cg.emit(("la", CG::T0, Label(format!("_{}", v.id))));
        } else {
            cg.emit(Comment(&format!("Pushing addr of local variable {}", v.id)));
            cg.emit(("addiu", CG::T0, CG::FP, *v.offset.get().unwrap()));
        }
        cg.emit_push(CG::T0);
    }
}


impl Loc for Id {}

impl Expr for Id {}

pub struct FormalParam { 
    pub ty: Type,
    pub id: Rc<Id>,
}

impl FormalParam {
    pub fn new(ty: Type, id: Rc<Id>) -> Rc<Self> {
        FormalParam {
            ty, id 
        }.into()
    }
}

impl Ast for FormalParam {
    fn unparse(&self, up: Up) {
        if let Type::SelfRef = self.ty {
            up.write("self");
        } else {
            self.ty.unparse(up);
            up.write(" ");
            self.id.unparse(up);
        }
    }
}

impl Ast for Vec<Rc<FormalParam>> {
    fn unparse(&self, up: Up) {
        let mut it = self.iter();
        it.next().inspect(|x| x.unparse(up));
        let () = it.map(|x| {
            up.write(", ");
            x.unparse(up);
        }).collect();
    }
}
