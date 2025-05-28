use super::*;

mod ops;
pub use ops::*;

pub struct IntLit {
    pub value: usize,
    pub line: usize,
    pub col: usize
}

impl Ast for IntLit {
    fn unparse(&self, up: &mut Unparser) {
        up.write(&self.value.to_string());
    }

    fn typecheck(&self, _tc: TCCtx) -> Option<TypeNode> {
        Some(TypeNode::Int)
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

impl Ast for StringLit {
    fn unparse(&self, up: Up) {
        up.write_span(self.span);
    }

    fn typecheck(&self, _tc: TCCtx) -> Option<TypeNode> {
        Some(TypeNode::Reference(Rc::new(TypeNode::Char)))
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

impl Ast for BoolLit {
    fn unparse(&self, up: Up) {
        up.write(if self.value {"true"} else {"false"})
    }

    fn typecheck(&self, _tc: TCCtx) -> Option<TypeNode> {
        Some(TypeNode::Bool)
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
    pub ty: OnceCell<TypeNode>
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        let lhs_ty = self.loc.typecheck(tc)?;
        let rhs_ty = self.value.typecheck(tc)?;
        tc.cache_type(&(self.loc.clone() as _), &lhs_ty);
        self.ty.set(lhs_ty.clone());
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
<<<<<<< HEAD
        // Adjust instruction for storing a value based
        // on the expression's size
        let store_ins = match self.ty.get().unwrap().size() {
            1 => "sb",
            4 => "sw",
            _ => todo!()
        };
        // Recall cached value from `typecheck`
=======
        
        // Get the type of the assignment
        let lhs_type = self.ty.get().unwrap();
        
>>>>>>> da-submit
        // T1 holds value
        cg.emit(Comment("Popping rvalue for assignment"));
        cg.emit_pop(CG::T1);
        // T0 holds location
        cg.emit(Comment("Popping lvalue for assignment"));
        cg.emit_pop(CG::T0);
<<<<<<< HEAD
        cg.emit((store_ins, CG::T1, CG::T0, Ix(0)));
=======
        
        // Use sb for char type, sw for others
        if let TypeNode::Char = lhs_type {
            cg.emit(("sb", CG::T1, CG::T0, Ix(0)));
        } else {
            cg.emit(("sw", CG::T1, CG::T0, Ix(0)));
        }
        
>>>>>>> da-submit
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
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
    pub field: IdNode,
    // Populated during typechecker phase
    pub sym: OnceCell<Rc<Symbol>>
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        let obj_ty = self.obj.typecheck(tc);
        if let Some(TypeNode::Struct(s, sym)) = obj_ty {
            let struct_sym = sym.get()
                .unwrap();
            let struct_scope = struct_sym.scope.try_borrow()
                .expect("Failed to borrow scope for struct type");
            let field_string = span_to_str(self.field.span, tc.ref_text);
            if let Some(sym_rc) = struct_scope.get(field_string) {
                let Symbol::Var(ref v) = **sym_rc else {
                    let m = format!(
                        "An entry for {} exists in the struct's definition, \
                        but it is not a member variable",
                        field_string
                    );
                    tc.raise_error(Rc::new(self.field.clone()), m);
                    return None
                };
                self.field.set_sym(sym_rc.clone());
                let b = (*v.ty).clone();
                self.sym.set(sym_rc.clone()).unwrap();
                return Some(b)
            }
        } else {
            tc.raise_error(self.obj.clone(), "Attempt to access a field of a non-struct".into());
        }
        None
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
        self.obj.codegen_lvalue(cg);
        cg.emit_pop(CG::T0);
        // --- BEGIN CHANGES ---
        // Added: Check the field's type to choose 'lb' for char (1-byte) or 'lw' for others.
        // This ensures byte-level loading for struct fields of type char, fixing alignment.
        // Previously, it always used 'lw', which could cause issues for 1-byte fields.
        let load_instr = if let TypeNode::Char = *vs.ty { "lb" } else { "lw" };
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        let inner_ty = self.expr.typecheck(tc)?;
        Some(TypeNode::Reference(Rc::new(inner_ty)))
    }

    fn codegen(&self, cg: &mut Codegen) {
        self.expr.codegen_lvalue(cg);
    }
}

impl Expr for AddrExpr {}

pub struct DerefExpr {
    pub ptr: Boxpr,
    deref_sym: OnceCell<Rc<StructDeclSymbol>>,
    pub ty: OnceCell<TypeNode>,
}

impl DerefExpr {
    pub fn new(expr: Boxpr) -> Rc<Self> {
        Rc::new(DerefExpr {
            ty: OnceCell::new(),
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        let inner_ty = self.ptr.typecheck(tc)?;
        if let TypeNode::Reference(pointee) = inner_ty {
            // --- BEGIN CHANGES ---
            // Added: Store the dereferenced type for use in codegen.
            // This ensures we can check for TypeNode::Char later without re-running typecheck.
            self.ty.set((*pointee).clone()).unwrap();
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
        let load_instr = if let TypeNode::Char = self.ty.get().unwrap() { "lb" } else { "lw" };
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
    pub ty: OnceCell<TypeNode>,   
    pub index: Boxpr,
    pub is_ptr: OnceCell<bool>,
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        let ptr_ty = self.ptr.typecheck(tc)?.clone();
        let index_ty = self.index.typecheck(tc)?;
        if let TypeNode::Reference(ref r) = ptr_ty {
            self.is_ptr.set(true);
        } else {
            self.is_ptr.set(false);
        }
        if let TypeNode::Reference(ref r) | TypeNode::Array(ref r, _) = ptr_ty {
            if index_ty != TypeNode::Int {
                tc.raise_error(
                    self.index.clone(),
                    "Indices of an array or pointer must be integers".into()
                );
            }
            tc.cache_type(&(self.ptr.clone() as _), r);
            // --- BEGIN CHANGES ---
            // Added: Store the inner type for use in codegen.
            // This ensures we can check for TypeNode::Char later to choose the correct load instruction.
            let inner_ty = r.as_ref().clone();
            self.ty.set(inner_ty.clone()).unwrap();
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
        let ty = cg.type_cache.get(&(self.ptr.clone() as Rc<dyn Ast + 'static>)).unwrap();
        let ty_size = ty.size();
        dbg!(ty_size);
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
        let ty = cg.type_cache.get(&(self.ptr.clone() as Rc<dyn Ast + 'static>)).unwrap();
        let ty_size = ty.size();
        let load_ins = match ty_size {
            1 => "lb",
            4 => "lw",
            _ => todo!()
        };
        self.codegen_lvalue(cg);
        cg.emit_pop(CG::T0);
<<<<<<< HEAD
        cg.emit((load_ins, CG::T1, CG::T0, Ix(0)));
=======
        // --- BEGIN CHANGES ---
        // Added: Check the stored type to choose 'lb' for char (1-byte) or 'lw' for others.
        // This fixes alignment for indexing into char arrays/pointers (e.g., in tests/alignment.kl).
        // Previously, it always used 'lw', causing byte misalignment.
        let load_instr = if let TypeNode::Char = self.ty.get().unwrap() { "lb" } else { "lw" };
        // Updated: Use the dynamic load_instr instead of hardcoded "lw".
        cg.emit((load_instr, CG::T1, CG::T0, Ix(0)));
        // --- END CHANGES ---
>>>>>>> da-submit
        cg.emit_push(CG::T1);
    }
}

impl Loc for IndexExpr {}

impl Expr for IndexExpr {}
