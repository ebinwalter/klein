#![allow(clippy::new_ret_no_self)]
use super::*;

mod ops;
pub use ops::*;

pub struct NullLit;

impl Ast for NullLit {
    fn unparse(&self, up: Up) {
        up.write("null");
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        Some(Type::Reference(Type::Void.into()))
    }

    fn codegen(&self, cg: &mut Codegen) {
        cg.emit_push(CG::ZERO);
    }
}

impl Expr for NullLit {}

pub struct IntLit {
    pub value: usize,
    pub line: usize,
    pub col: usize
}

impl IntLit {
    pub fn new(value: usize, line: usize, col: usize) -> Rc<Self> {
        IntLit { value, line, col }.into()
    }
}

impl Ast for IntLit {
    fn unparse(&self, up: &mut Unparser) {
        up.write(&self.value.to_string());
    }

    fn typecheck(&self, _tc: TCCtx) -> Option<Type> {
        Some(Type::Int)
    }

    fn codegen(&self, cg: &mut Codegen) {
        cg.emit(("addi", CG::T0, CG::ZERO, self.value as u32));
        cg.emit_push(CG::T0);
    }

    fn codegen_register(&self, cg: &mut Codegen) -> Option<&'static str> {
        let Some(reg) = cg.next_free_reg() else {
            self.codegen(cg);
            return None;
        };
        cg.emit(("li", reg, self.value as u32));
        Some(reg)
    }
}

impl Expr for IntLit {}

pub struct StringLit {
    pub span: Span,
    pub line: usize,
    pub col: usize,
}

impl StringLit {
    pub fn new(span: Span, line: usize, col: usize) -> Rc<Self> {
        StringLit { span, line, col }.into()
    }
}

impl Ast for StringLit {
    fn unparse(&self, up: Up) {
        up.write_span(self.span);
    }

    fn typecheck(&self, _tc: TCCtx) -> Option<Type> {
        Some(Type::Reference(Rc::new(Type::Char)))
    }

    fn code_location(&self) -> Option<(usize, usize)> {
        Some((self.line, self.col))
    }

    fn codegen(&self, cg: &mut Codegen) {
        let label = cg.label_for_string(self.span);
        cg.emit(("la", CG::T0, Label(label)));
        cg.emit_push(CG::T0);
    }
}

impl Expr for StringLit {}


pub struct BoolLit {
    pub value: bool,
    pub line: usize,
    pub col: usize
}

impl BoolLit {
    pub fn new(value: bool, line: usize, col: usize) -> Rc<BoolLit> {
        BoolLit { value, line, col }.into()
    }
}

impl Ast for BoolLit {
    fn unparse(&self, up: Up) {
        up.write(if self.value {"true"} else {"false"})
    }

    fn typecheck(&self, _tc: TCCtx) -> Option<Type> {
        Some(Type::Bool)
    }

    fn codegen(&self, cg: &mut Codegen) {
        if self.value {
            cg.emit(("addi", CG::T0, CG::ZERO, 1));
        } else {
            cg.emit(("addi", CG::T0, CG::ZERO, 0));
        }
        cg.emit_push(CG::T0);
    }
}

impl Expr for BoolLit {}

pub struct CharLit {
    span: Span,
    value: OnceCell<u8>,
    line: usize,
    col: usize,
}

impl CharLit {
    pub fn new(span: Span, line: usize, col: usize) -> Self {
        CharLit {
            span,
            value: OnceCell::new(),
            line,
            col
        }
    }
}

impl Ast for CharLit {
    fn unparse(&self, up: &mut Unparser) {
        up.write_span(self.span);
    }

    fn analyze_names(&self, na: NACtx) {
        let lit_str = span_to_str(&self.span, na.ref_text);
        let char_str = &lit_str[1..lit_str.len()-1];
        let (a, b) = char_str.split_at(1);
        if a.chars().nth(0).unwrap() == '\\' {
            if let Ok(n) = b.parse::<u8>() {
                self.value.set(n);
            } else {
                // Escapes taken from https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
                let ch = match b.chars().nth(0).unwrap() {
                    'n' => 10,
                    't' => 9,
                    'r' => 13,
                    'b' => 8,
                    '\\' => 92,
                    '\'' => 39,
                    x => {
                        na.raise_error(self, format!("invalid escape \\{x}"));
                        return;
                    }
                };
                self.value.set(ch);
            }
        } else {
            self.value.set(a.as_bytes()[0]);
        }
    }

    fn code_location(&self) -> Option<(usize, usize)> {
        Some((self.line, self.col))
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        return Some(Type::Char)
    }

    fn codegen(&self, cg: &mut Codegen) {
        cg.emit(("li", CG::T0, *self.value.get().unwrap() as u32));
        cg.emit_push(CG::T0);
    }
}

impl Expr for CharLit {}

pub struct AssignExpr {
    pub loc: BoxLoc,
    pub value: Boxpr,
}

impl AssignExpr {
    pub fn new(loc: BoxLoc, value: Boxpr) -> Rc<Self> {
        AssignExpr { 
            loc, value,
        }.into()
    }
}

impl Ast for AssignExpr {
    fn unparse(&self, up: &mut Unparser) {
        self.loc.unparse(up);
        up.write(" = ");
        self.value.unparse(up);
    }

    fn analyze_names(&self, na: NACtx) {
        self.loc.analyze_names(na);
        self.value.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        let lhs_ty = self.loc.typecheck(tc)?;
        let rhs_ty = self.value.typecheck(tc)?;
        tc.cache_type(self, &lhs_ty);
        if rhs_ty.is_subtype_of(&lhs_ty) {
            Some(rhs_ty)
        } else {
            let m = format!("Assignment of value of type {rhs_ty:?} to variable of type {lhs_ty:?}");
            tc.raise_error(self.value.clone(), m);
            None
        }
    }

    fn code_location(&self) -> Option<(usize, usize)> {
        self.value.code_location()
    }

    fn codegen(&self, cg: &mut Codegen) {
        let store_ins = match cg.type_cache.get(self).unwrap().size() {
            1 => "sb",
            4 => "sw",
            _ => todo!()
        };
        if let Some(loc_reg) = self.loc.codegen_lvalue_register(cg) {
            if let Some(val_reg) = self.value.codegen_register(cg) {
                cg.emit((store_ins, val_reg, loc_reg, Ix(0)));
                cg.relinquish_reg(loc_reg);
                cg.emit_push(val_reg);
                cg.relinquish_reg(val_reg);
            } else {
                self.value.codegen(cg);
                cg.emit_pop(CG::T0);
                cg.emit((store_ins, CG::T0, loc_reg, Ix(0)));
                cg.relinquish_reg(loc_reg);
                cg.emit_push(CG::T0);
            }
        } else {
            self.value.codegen(cg);
            cg.emit_pop(CG::T0);
            cg.emit_pop(CG::T1);
            cg.emit((store_ins, CG::T0, CG::T1, Ix(0)));
            cg.emit_push(CG::T0);
        }
    }

    fn codegen_register(&self, cg: &mut Codegen) -> Option<&'static str> {
        let Some(storage_reg) = cg.next_free_reg() else {
            self.codegen(cg);
            return None;
        };
        let store_ins = match cg.type_cache.get(self).unwrap().size() {
            1 => "sb",
            4 => "sw",
            _ => todo!()
        };
        if let Some(loc_reg) = self.loc.codegen_lvalue_register(cg) {
            if let Some(val_reg) = self.value.codegen_register(cg) {
                cg.emit((store_ins, val_reg, loc_reg, Ix(0)));
                cg.relinquish_reg(loc_reg);
                cg.relinquish_reg(storage_reg);
                Some(val_reg)
            } else {
                self.value.codegen(cg);
                cg.emit_pop(storage_reg);
                cg.emit((store_ins, storage_reg, loc_reg, Ix(0)));
                cg.relinquish_reg(loc_reg);
                Some(storage_reg)
            }
        } else {
            self.value.codegen(cg);
            cg.emit_pop(storage_reg);
            cg.emit_pop(CG::T1);
            cg.emit((store_ins, storage_reg, CG::T1, Ix(0)));
            Some(storage_reg)
        }
    }
}

impl Expr for AssignExpr {}

pub struct CallExpr {
    pub fun: BoxLoc,
    pub args: Vec<Boxpr>,
    pub fun_sym: OnceCell<Rc<FuncSymbol>>
}

impl CallExpr {
    pub fn new(fun: BoxLoc, args: Vec<Boxpr>) -> Boxpr {
        Rc::new(CallExpr { fun, args, fun_sym: OnceCell::new() })
    }
}

impl Ast for CallExpr {
    fn unparse(&self, up: Up) {
        self.fun.unparse(up);
        up.write("(");
        let mut first = true;
        for expr in self.args.iter() {
            if !first {
                up.write(", ");
            }
            expr.unparse(up);
            first = false;
        }
        up.write(")");
    }

    fn analyze_names(&self, na: NACtx) {
        self.fun.analyze_names(na);
        for expr in self.args.iter() {
            expr.analyze_names(na);
        }
        if let Some(sym) = self.fun.symbol_rc() {
            let sym = sym.clone();
            if let Symbol::Func(ref f) = *sym {
                self.fun_sym.set(f.clone())
                    .expect("Attempt to re-set symbol for function call");
            } else {
                na.raise_error((self.fun.clone() as Rc<dyn Ast>).borrow(), "Attempt to call non-function".to_owned());
            }
        }
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        let fun_arg_types = &self.fun_sym
            .get()
            .expect("Unset fun symbol after name analysis phase")
            .arg_types;
        let n1 = fun_arg_types.len();
        let n2 = self.args.len();
        if n1 != n2 {
            let m = format!("This function takes {n1} arguments, but {n2} were provided");
            tc.raise_error(self.fun.clone(), m);
        } else {
            for (i, e1) in self.args.iter().enumerate() {
                let Some(t1) = e1.typecheck(tc) else { continue };
                let t2 = &fun_arg_types[i];
                let arg_no = i + 1;
                if t1 != *t2 {
                    let m = format!("argument #{arg_no} should be of type {t2}, but a {t1} was given");
                    tc.raise_error(e1.clone(), m);
                }
            }
        }
        Some(self.fun_sym.get().unwrap().return_type.clone())
    }

    fn code_location(&self) -> Option<(usize, usize)> {
        self.fun.code_location()
    }

    fn codegen(&self, cg: &mut Codegen) {
        // Hacky thing to get the current instruction pointer, so we
        // can jump to function pointers
        let return_label = cg.next_label();
        cg.emit(("la", "$31", Label(&return_label)));
        for arg in self.args.iter().rev() {
            arg.codegen(cg);
        }
        self.fun.codegen(cg);
        cg.emit_pop(CG::T0);
        cg.emit(("jr", CG::T0));
        cg.emit(Label(return_label));
        for _arg in self.args.iter().rev() {
            cg.emit_pop(CG::ZERO)
        }
        cg.emit_push(CG::V0);
    }
}

impl Expr for CallExpr {}

pub struct AccessExpr {
    pub obj: Boxpr,
    pub field: Rc<Id>,
    // Populated during typechecker phase
    pub sym: OnceCell<Rc<Symbol>>
}

impl AccessExpr {
    pub fn new(obj: Boxpr, field: Rc<Id>) -> Rc<AccessExpr> {
        AccessExpr {
            obj, field, sym: OnceCell::new(),
        }.into()
    } 
}

impl Ast for AccessExpr {
    fn unparse(&self, up: Up) {
        self.obj.unparse(up);
        up.write(".");
        self.field.unparse(up);
    }

    fn analyze_names(&self, na: NACtx) {
        self.obj.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        let obj_ty = self.obj.typecheck(tc)?;
        if let ref x@Type::Struct(_, ref sym) 
             | ref x@Type::Reference(deref!(Type::Struct(_, ref sym))) 
             = obj_ty 
        {
            tc.cache_type(&*self.obj, &x);
            let struct_sym = sym.get()
                .unwrap();
            let struct_scope = struct_sym.scope.try_borrow()
                .expect("Failed to borrow scope for struct type");
            let field_string = span_to_str(&self.field.span, tc.ref_text);
            if let Some(sym_rc) = struct_scope.get(field_string) {
                let Symbol::Var(ref v) = **sym_rc else {
                    let m = format!(
                        "An entry for {} exists in the struct's definition, \
                        but it is not a member variable",
                        field_string
                    );
                    tc.raise_error(self.field.clone(), m);
                    return None
                };
                self.field.set_sym(sym_rc.clone());
                let b = (*v.ty).clone();
                self.sym.set(sym_rc.clone()).unwrap();
                Some(b)
            } else {
                tc.raise_error(self.field.clone(), "field not a part of struct".into());
                None
            }
        } else {
            let m = "Attempt to access a field of something which was not \
                a struct or a pointer to a struct";
            tc.raise_error(self.obj.clone(), m.into());
            None
        }
    }

    fn symbol_rc(&self) -> Option<Rc<Symbol>> {
        self.sym.get().cloned()
    }

    fn symbol(&self) -> Option<&Symbol> {
        self.sym.get().map(Borrow::borrow)
    }

    fn code_location(&self) -> Option<(usize, usize)> {
        self.field.code_location()
    }

    fn codegen(&self, cg: &mut Codegen) {
        let field_sym = self.field.sym.get().unwrap();
        let Symbol::Var(ref vs) = **field_sym else { panic!() };
        let offset = vs.offset.get().unwrap();
        if let Some(Type::Reference(_)) = cg.type_cache.get(&*self.obj) {
            if let Some(r) = self.obj.codegen_register(cg) {
                cg.emit(("move", CG::T0, r));
                cg.relinquish_reg(r);
            } else {
                self.obj.codegen(cg);
                cg.emit_pop(CG::T0);
            }
        } else if let Some(r) = self.obj.codegen_lvalue_register(cg) {
            cg.emit(("move", CG::T0, r));
            cg.relinquish_reg(r);
        } else {
            self.obj.codegen_lvalue(cg);
            cg.emit_pop(CG::T0);
        }
        let load_instr = if let Type::Char = *vs.ty { "lb" } else { "lw" };
        // Updated: Use the dynamic load_instr instead of hardcoded "lw".
        cg.emit((load_instr, CG::T0, CG::T0, Ix(*offset)));
        // --- END CHANGES ---
        cg.emit_push(CG::T0);
    }

    fn codegen_lvalue(&self, cg: &mut Codegen) {
        let field_sym = self.field.sym.get().unwrap();
        let Symbol::Var(ref vs) = **field_sym else { panic!() };
        let offset = vs.offset.get().unwrap();
        self.obj.codegen_lvalue(cg);
        cg.emit_pop(CG::T0);
        cg.emit(("addiu", CG::T0, CG::T0, *offset));
        cg.emit_push(CG::T0);
    }
}

impl Expr for AccessExpr {}

impl Loc for AccessExpr {}

pub struct AddrExpr {
    pub expr: Boxpr,
}

impl AddrExpr {
    pub fn new(expr: Boxpr) -> Rc<Self> {
        AddrExpr { expr }.into()
    }
}

impl Ast for AddrExpr {
    fn unparse(&self, up: Up) {
        up.write("&");
        self.expr.unparse(up);
    }

    fn analyze_names(&self, na: NACtx) {
        self.expr.analyze_names(na);
    }

    fn code_location(&self) -> Option<(usize, usize)> {
        self.expr.code_location()
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        let inner_ty = self.expr.typecheck(tc)?;
        Some(Type::Reference(Rc::new(inner_ty)))
    }

    fn codegen(&self, cg: &mut Codegen) {
        self.expr.codegen_lvalue(cg);
    }
}

impl Expr for AddrExpr {}

pub struct DerefExpr {
    pub ptr: Boxpr,
    deref_sym: OnceCell<Rc<StructDeclSymbol>>,
}

impl DerefExpr {
    pub fn new(expr: Boxpr) -> Rc<Self> {
        Rc::new(DerefExpr {
            ptr: expr,
            deref_sym: OnceCell::new()
        })
    }
}

impl Ast for DerefExpr {
    fn unparse(&self, up: Up) {
        up.write("*");
        self.ptr.unparse(up);
    }

    fn analyze_names(&self, na: NACtx) {
        self.ptr.analyze_names(na);
    }

    fn code_location(&self) -> Option<(usize, usize)> {
        self.ptr.code_location()
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        let inner_ty = self.ptr.typecheck(tc)?;
        if let Type::Reference(pointee) = inner_ty {
            // --- BEGIN CHANGES ---
            // Added: Store the dereferenced type for use in codegen.
            // This ensures we can check for TypeNode::Char later without re-running typecheck.
            tc.cache_type(self, &pointee);
            // --- END CHANGES ---
            Some((*pointee).clone())
        } else {
            tc.raise_error(self.ptr.clone(), "Attempt to dereference a non-pointer".into());
            None
        }
    }

    fn codegen_lvalue(&self, cg: &mut Codegen) {
        self.ptr.codegen(cg);
    }

    fn codegen(&self, cg: &mut Codegen) {
        self.ptr.codegen(cg);
        cg.emit_pop(CG::T1);
        // --- BEGIN CHANGES ---
        // Removed: Unnecessary and inefficient call to self.typecheck(...) for a new context.
        // Added: Check the stored type to choose 'lb' for char (1-byte) or 'lw' for others.
        // This fixes alignment by ensuring byte-level dereferencing for char pointers.
        // Previously, it always used 'lw', causing issues for 1-byte types.
        let load_instr = if let Type::Char = cg.type_cache.get(self).unwrap() { "lb" } else { "lw" };
        // Updated: Use the dynamic load_instr instead of hardcoded "lw".
        cg.emit((load_instr, CG::T0, CG::T1, Ix(0)));
        // --- END CHANGES ---
        cg.emit_push(CG::T0);
    }
}

impl Expr for DerefExpr {}
impl Loc for DerefExpr {}

pub struct IndexExpr {
    pub ptr: Boxpr,
     // --- BEGIN CHANGES ---
    // Added: New field to store the indexed type, set during typecheck.
    // This allows codegen to access the type easily for choosing load instructions,
    // mirroring the pattern in DerefExpr and fixing alignment for char arrays/pointers.
    pub index: Boxpr,
    pub is_ptr: OnceCell<bool>,
}

impl IndexExpr {
    pub fn new(ptr: Boxpr, index: Boxpr) -> Boxpr {
        Rc::new(IndexExpr { 
            ptr, index, 
            is_ptr: OnceCell::new()
        })
    }

    pub fn new_loc(ptr: Boxpr, index: Boxpr) -> BoxLoc {
        Rc::new(IndexExpr { ptr, index, is_ptr: OnceCell::new()})
    }
}

impl Ast for IndexExpr {
    fn unparse(&self, up: Up) {
        self.ptr.unparse(up);
        up.write("[");
        self.index.unparse(up);
        up.write("]");
    }

    fn analyze_names(&self, na: NACtx) {
        self.ptr.analyze_names(na);
        self.index.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        let ptr_ty = self.ptr.typecheck(tc)?.clone();
        let index_ty = self.index.typecheck(tc)?;
        if let Type::Reference(ref r) = ptr_ty {
            self.is_ptr.set(true).unwrap();
        } else {
            self.is_ptr.set(false).unwrap();
        }
        if let Type::Reference(ref r) | Type::Array(ref r, _) = ptr_ty {
            if index_ty != Type::Int {
                tc.raise_error(
                    self.index.clone(),
                    "Indices of an array or pointer must be integers".into()
                );
            }
            tc.cache_type(self, r);
            // --- BEGIN CHANGES ---
            // Added: Store the inner type for use in codegen.
            // This ensures we can check for TypeNode::Char later to choose the correct load instruction.
            let inner_ty = r.as_ref().clone();
            // Updated: Return inner_ty to keep original behavior while storing it.
            Some(inner_ty)
            // --- END CHANGES ---
        } else {
            tc.raise_error(self.ptr.clone(), "Only pointers and arrays can be indexed".into());
            None
        }
    }

    fn code_location(&self) -> Option<(usize, usize)> {
        self.ptr.code_location()
    }

    // fn codegen_lvalue(&self, cg: &mut Codegen) {
    //     let ty = cg.type_cache.get(self).unwrap();
    //     let ty_size = ty.size();
    //     if *self.is_ptr.get().unwrap() {
    //         if let Some(r) = self.ptr.codegen_register(cg) {
    //         }
    //         self.ptr.codegen(cg);   
    //     } else {
    //         self.ptr.codegen_lvalue(cg);
    //     }
    //     self.index.codegen(cg);
    //     cg.emit_pop(CG::T1);
    //     cg.emit(("li", CG::T2, ty_size));
    //     cg.emit(("mul", CG::T1, CG::T1, CG::T2));
    //     cg.emit_pop(CG::T0);
    //     cg.emit(("add", CG::T0, CG::T0, CG::T1));
    //     cg.emit_push(CG::T0);
    // }

    fn codegen_lvalue(&self, cg: &mut Codegen) {
        let ty = cg.type_cache.get(self).unwrap();
        let ty_size = ty.size();
        let reg = if *self.is_ptr.get().unwrap() {
            self.ptr.codegen_register(cg)
        } else {
            self.ptr.codegen_lvalue_register(cg)
        };
        if let Some(r1) = reg {
            if let Some(r2) = self.index.codegen_register(cg) {
                cg.emit(("li", CG::T0, ty_size));
                cg.emit(("mul", CG::T0, CG::T0, r2));
                cg.emit(("add", CG::T0, CG::T0, r1));
                cg.emit_push(CG::T0);
                cg.relinquish_reg(r1);
                cg.relinquish_reg(r2);
            } else {
                cg.emit(("li", CG::T0, ty_size));
                cg.emit_pop(CG::T1);
                cg.emit(("mul", CG::T0, CG::T0, CG::T1));
                cg.emit(("add", CG::T0, CG::T0, r1));
                cg.emit_push(CG::T0);
                cg.relinquish_reg(r1);
            }
        } else {
            self.index.codegen(cg);
            cg.emit_pop(CG::T1);
            cg.emit(("li", CG::T2, ty_size));
            cg.emit(("mul", CG::T1, CG::T1, CG::T2));
            cg.emit_pop(CG::T0);
            cg.emit(("add", CG::T0, CG::T0, CG::T1));
            cg.emit_push(CG::T0);
        }
    }
    fn codegen_lvalue_register(&self, cg: &mut Codegen) -> Option<&'static str> {
        let Some(storage_reg) = cg.next_free_reg() else {
            self.codegen_lvalue(cg);
            return None;
        };
        let ty = cg.type_cache.get(self).unwrap();
        let ty_size = ty.size();
        let reg = if *self.is_ptr.get().unwrap() {
            self.ptr.codegen_register(cg)
        } else {
            self.ptr.codegen_lvalue_register(cg)
        };
        if let Some(r1) = reg {
            if let Some(r2) = self.index.codegen_register(cg) {
                cg.emit(("li", CG::T0, ty_size));
                cg.emit(("mul", CG::T0, CG::T0, r2));
                cg.emit(("add", r1, CG::T0, r1));
                cg.relinquish_reg(r2);
                cg.relinquish_reg(storage_reg);
                Some(r1)
            } else {
                cg.emit(("li", CG::T0, ty_size));
                cg.emit_pop(CG::T1);
                cg.emit(("mul", CG::T0, CG::T0, CG::T1));
                cg.emit(("add", r1, CG::T0, r1));
                cg.relinquish_reg(storage_reg);
                Some(r1)
            }
        } else {
            self.index.codegen(cg);
            cg.emit_pop(CG::T1);
            cg.emit(("li", CG::T2, ty_size));
            cg.emit(("mul", CG::T1, CG::T1, CG::T2));
            cg.emit_pop(CG::T0);
            cg.emit(("add", storage_reg, CG::T0, CG::T1));
            Some(storage_reg)
        }
    }
    fn codegen(&self, cg: &mut Codegen) {
        let ty = cg.type_cache.get(self).unwrap();
        let ty_size = ty.size();
        let load_ins = match ty_size {
            1 => "lb",
            4 => "lw",
            _ => todo!()
        };
        self.codegen_lvalue(cg);
        cg.emit_pop(CG::T0);
        cg.emit((load_ins, CG::T1, CG::T0, Ix(0)));
        cg.emit_push(CG::T1);
    }
    fn codegen_register(&self, cg: &mut Codegen) -> Option<&'static str> {
        let Some(reg) = cg.next_free_reg() else {
            self.codegen(cg);
            return None;
        };
        let ty = cg.type_cache.get(self).unwrap();
        let ty_size = ty.size();
        let load_ins = match ty_size {
            1 => "lb",
            4 => "lw",
            _ => todo!()
        };
        self.codegen_lvalue(cg);
        cg.emit_pop(CG::T0);
        cg.emit((load_ins, reg, CG::T0, Ix(0)));
        Some(reg)
    }
}

impl Loc for IndexExpr {}

impl Expr for IndexExpr {}
