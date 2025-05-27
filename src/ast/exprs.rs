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
        self.loc.codegen_lvalue(cg);
        self.value.codegen(cg);
        // Adjust instruction for storing a value based
        // on the expression's size
        let store_ins = match self.ty.get().unwrap().size() {
            1 => "sb",
            4 => "sw",
            _ => todo!()
        };
        // Recall cached value from `typecheck`
        // T1 holds value
        cg.emit(Comment("Popping rvalue for assignment"));
        cg.emit_pop(CG::T1);
        // T0 holds location
        cg.emit(Comment("Popping lvalue for assignment"));
        cg.emit_pop(CG::T0);
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
        cg.emit(("lw", CG::T0, CG::T0, Ix(*offset)));
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
    deref_sym: OnceCell<Rc<StructDeclSymbol>>
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        let inner_ty = self.ptr.typecheck(tc)?;
        if let TypeNode::Reference(pointee) = inner_ty {
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
        cg.emit(("lw", CG::T0, CG::T1, Ix(0)));
        cg.emit_push(CG::T0);
    }
}

impl Expr for DerefExpr {}
impl Loc for DerefExpr {}

pub struct IndexExpr {
    pub ptr: Boxpr,
    pub index: Boxpr
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
        if let TypeNode::Reference(ref r) | TypeNode::Array(ref r, _) = ptr_ty {
            if index_ty != TypeNode::Int {
                tc.raise_error(
                    self.index.clone(),
                    "Indices of an array or pointer must be integers".into()
                );
            }
            tc.cache_type(&(self.ptr.clone() as Rc<dyn Ast + 'static>), r);
            Some(r.as_ref().clone())
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
        self.ptr.codegen_lvalue(cg);   
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
        cg.emit((load_ins, CG::T1, CG::T0, Ix(0)));
        cg.emit_push(CG::T1);
    }
}

impl Loc for IndexExpr {}

impl Expr for IndexExpr {}
