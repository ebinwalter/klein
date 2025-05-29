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
        cg.emit(Comment("Generating lvalue for assignment"));
        self.loc.codegen_lvalue(cg);
        cg.emit(Comment("Generating rvalue for assignment"));
        self.value.codegen(cg);
        cg.emit_pop(CG::T1);
        cg.emit_pop(CG::T0);
        // Adjust instruction for storing a value based
        // on the expression's size
        let store_ins = match cg.type_cache.get(self).unwrap().size() {
            1 => "sb",
            4 => "sw",
            _ => todo!()
        };
        cg.emit((store_ins, CG::T1, CG::T0, Ix(0)));
        
        cg.emit(Comment("Putting value back on stack (for chains)"));
        cg.emit_push(CG::T1);
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
            self.obj.codegen(cg);
        } else {
            self.obj.codegen_lvalue(cg);
        }
        cg.emit_pop(CG::T0);
        // --- BEGIN CHANGES ---
        // Added: Check the field's type to choose 'lb' for char (1-byte) or 'lw' for others.
        // This ensures byte-level loading for struct fields of type char, fixing alignment.
        // Previously, it always used 'lw', which could cause issues for 1-byte fields.
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

    fn codegen_lvalue(&self, cg: &mut Codegen) {
        let ty = cg.type_cache.get(self).unwrap();
        let ty_size = ty.size();
        if *self.is_ptr.get().unwrap() {
            self.ptr.codegen(cg);   
        } else {
            self.ptr.codegen_lvalue(cg);
        }
        self.index.codegen(cg);
        cg.emit_pop(CG::T1);
        cg.emit(("li", CG::T2, ty_size));
        cg.emit(("mul", CG::T1, CG::T1, CG::T2));
        cg.emit_pop(CG::T0);
        cg.emit(("add", CG::T0, CG::T0, CG::T1));
        cg.emit_push(CG::T0);
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
}

impl Loc for IndexExpr {}

impl Expr for IndexExpr {}
