#![allow(clippy::new_ret_no_self)]
use super::*;

pub struct IfStmt {
    pub cond: Boxpr,
    pub body: ScopeBlock,
}

impl IfStmt {
    pub fn new(cond: Boxpr, body: ScopeBlock) -> Rc<dyn Stmt> {
        Rc::new(IfStmt {
            cond, body
        })
    }
}

impl Ast for IfStmt {
    fn unparse(&self, up: &mut Unparser) {
        up.write_indent();
        up.write("if ");
        self.cond.unparse(up);
        up.write(" ");
        self.body.unparse(up);
        up.new_line();
    }
    
    fn analyze_names(&self, na: NACtx) {
        self.cond.analyze_names(na);
        self.body.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        self.cond.typecheck(tc);
        self.body.typecheck(tc);
        None
    }

    fn compute_var_offsets(&self, oc: OCtx) {
        self.body.compute_var_offsets(oc);
    }

    fn codegen(&self, cg: &mut Codegen) {
        let exit_label = cg.next_label();
        if let Some(reg) = self.cond.codegen_register(cg) {
            cg.emit(("beq", CG::ZERO, reg, Label(&exit_label)));
        } else {
            cg.emit_pop(CG::T0);
            cg.emit(("beq", CG::ZERO, CG::T0, Label(&exit_label)));
        }
        self.body.codegen(cg);
        cg.emit(Label(&exit_label));
    }
}

impl Stmt for IfStmt {}

pub struct IfElseStmt {
    pub cond: Boxpr,
    pub then_body: ScopeBlock,
    pub else_body: ScopeBlock,
}

impl IfElseStmt {
    pub fn new(cond: Boxpr, then_body: ScopeBlock, else_body: ScopeBlock) -> Rc<dyn Stmt> {
        Rc::new(IfElseStmt {
            cond, then_body, else_body
        })
    }
}

impl Ast for IfElseStmt {
    fn unparse(&self, up: &mut Unparser) {
        up.write_indent();
        up.write("if ");
        self.cond.unparse(up);
        up.write(" ");
        self.then_body.unparse(up);
        up.write(" else ");
        self.else_body.unparse(up);
        up.new_line();
    }

    fn analyze_names(&self, na: NACtx) {
        self.cond.analyze_names(na);
        self.then_body.analyze_names(na);
        self.else_body.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        self.cond.typecheck(tc);
        self.then_body.typecheck(tc);
        self.else_body.typecheck(tc);
        None
    }

    fn compute_var_offsets(&self, oc: OCtx) {
        self.then_body.compute_var_offsets(oc);
        self.else_body.compute_var_offsets(oc);
    }

    fn codegen(&self, cg: &mut Codegen) {
        let else_label = &cg.next_label(); 
        let exit_label = &cg.next_label();
        if let Some(reg) = self.cond.codegen_register(cg) {
            cg.emit(("beq", reg, CG::ZERO, Label(else_label)));
        } else {
            cg.emit_pop(CG::T0);
            cg.emit(("beq", CG::T0, CG::ZERO, Label(else_label)));
        }
        self.then_body.codegen(cg);
        cg.emit(("j", Label(exit_label)));
        cg.emit(Label(else_label));
        self.else_body.codegen(cg);
        cg.emit(Label(exit_label));
    }
}

impl Stmt for IfElseStmt {}

impl WhileStmt {
    pub fn new(cond: Boxpr, body: ScopeBlock) -> BoxStmt {
        Rc::new(WhileStmt {
            cond, body
        })
    }
}

pub struct WhileStmt {
    pub cond: Boxpr,
    pub body: ScopeBlock,
}

impl Ast for WhileStmt {
    fn unparse(&self, up: &mut Unparser) {
        up.write_indent();
        up.write("while ");
        self.cond.unparse(up);
        up.write(" ");
        self.body.unparse(up);
        up.new_line();
    }

    fn analyze_names(&self, na: NACtx) {
        self.cond.analyze_names(na);
        self.body.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        self.cond.typecheck(tc);
        self.body.typecheck(tc);
        None
    }

    fn compute_var_offsets(&self, oc: OCtx) {
        self.body.compute_var_offsets(oc);
    }

    fn codegen(&self, cg: &mut Codegen) {
        let check_label = &cg.next_label();
        let end_label = &cg.next_label();
        let string_cond = self.cond.unparse_to_string(cg.ref_text);
        cg.emit(Comment(&format!("while {}", &string_cond)));
        cg.emit(Label(check_label));
        if let Some(reg) = self.cond.codegen_register(cg) {
            cg.emit(("beq", reg, CG::ZERO, Label(end_label)));
        } else {
            cg.emit_pop(CG::T0);
            cg.emit(("beq", CG::T0, CG::ZERO, Label(end_label)));
        }
        self.body.codegen(cg);
        cg.emit(("j", Label(check_label)));
        cg.emit(Comment(&format!("end of while {}", &string_cond)));
        cg.emit(Label(end_label));
    }
}

impl Stmt for WhileStmt {}

pub struct ReturnStmt {
    pub expr: Option<Boxpr>
}

impl ReturnStmt {
    pub fn new(expr: Option<Boxpr>) -> BoxStmt {
        Rc::new(ReturnStmt { expr })
    }
}

impl Ast for ReturnStmt {
    fn unparse(&self, up: &mut Unparser) {
        up.write_indent();
        up.write("return");
        if let Some(e) = &self.expr {
            up.space();
            e.unparse(up);
        }
        up.end_stmt();
    }

    fn analyze_names(&self, na: NACtx) {
        if let Some(ref e) = self.expr {
            e.analyze_names(na);
        }
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        self.expr.clone()?.typecheck(tc);
        None
    }

    fn codegen(&self, cg: &mut Codegen) {
        if let Some(ref e) = self.expr {
            e.codegen(cg);
            cg.emit_pop(CG::V0);
        }
        let fname = cg.function_stack.last().unwrap();
        let return_label = format!("{fname}_exit");
        cg.emit(("j", Label(return_label)));
    }
}

impl Stmt for ReturnStmt {}

pub struct ExprStmt {
    pub expr: Boxpr
}

impl ExprStmt {
    pub fn new(expr: Boxpr) -> BoxStmt {
        Rc::new(ExprStmt { expr })
    }
}

impl Ast for ExprStmt {
    fn unparse(&self, up: Up) {
        up.write_indent();
        self.expr.unparse(up);
        up.end_stmt();
    }

    fn analyze_names(&self, na: NACtx) {
        self.expr.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        self.expr.typecheck(tc);
        None
    }

    fn codegen(&self, cg: &mut Codegen) {
        if let Some(r) = self.expr.codegen_register(cg) {
            drop(r);
        } else {
            cg.emit_pop(CG::ZERO);
        }
    }
} 

impl Stmt for ExprStmt {}

pub struct InputStmt {
    pub loc: BoxLoc
}

impl InputStmt {
    pub fn new(loc: BoxLoc) -> Rc<Self> {
        InputStmt {
            loc
        }.into()
    }
}

impl Ast for InputStmt {
    fn unparse(&self, up: Up) {
        up.write_indent();
        up.write("in -> ");
        self.loc.unparse(up);
        up.end_stmt();
    }

    fn analyze_names(&self, na: NACtx) {
        self.loc.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        use Type::*;
        let loc_type = self.loc.typecheck(tc)?;
        tc.cache_type(&*self.loc, &loc_type);
        match loc_type {
            Int | Bool | Char  => (),
            Array(deref!(Type::Char), _) => (),
            Reference(t) if matches!(*t, Char) => {
                tc.raise_error(
                    self.loc.clone(),
                    "String input is only supported for buffers with a statically known size\n\
                    \tnote: If you want to store a user string in dynamic memory, create an array of characters, take input there, \n\
                    \tand then copy out the data read in by the syscall".into()
                );
            },
            _ => {
                tc.raise_error(
                    self.loc.clone(),
                    "Input is only supported for integers, booleans, characters \
                    and strings".into()
                );
            }
        }
        None
    }

    fn codegen(&self, cg: &mut Codegen) {
        let loc_type = cg.type_cache.get(&*self.loc).unwrap().clone();
        match loc_type {
            Type::Int => {
                self.loc.codegen_lvalue(cg);
                cg.emit(("li", CG::V0, 5));
                cg.emit("syscall");
                cg.emit_pop(CG::T0);
                cg.emit(("sw", CG::V0, CG::T0, Ix(0)));
            },
            Type::Char => {
                self.loc.codegen_lvalue(cg);
                cg.emit(("li", CG::V0, 12));
                cg.emit("syscall");
                cg.emit_pop(CG::T0);
                cg.emit(("sb", CG::V0, CG::T0, Ix(0)));
            },
            Type::Array(deref!(Type::Char), len) => {
                self.loc.codegen_lvalue(cg);
                cg.emit_pop(CG::A0);
                cg.emit(("li", CG::V0, 8));
                cg.emit(("li", "$a1", len));
                cg.emit("syscall");
            }
            _ => unimplemented!(),
        }
    }
}

impl Stmt for InputStmt {}

pub struct OutputStmt {
    pub expr: Boxpr
}

impl OutputStmt {
    pub fn new(expr: Boxpr) -> Rc<Self> {
        OutputStmt {
            expr
        }.into()
    }
}

impl Ast for OutputStmt {
    fn unparse(&self, up: Up) {
        up.write_indent();
        up.write("out <- ");
        self.expr.unparse(up);
        up.end_stmt();
    }

    fn analyze_names(&self, na: NACtx) {
        self.expr.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        use Type::*;
        let expr_type = self.expr.typecheck(tc)?;
        tc.cache_type(&*self.expr, &expr_type);
        match expr_type {
            Int | Bool | Char  => (),
            Reference(deref!(Type::Char)) => (),
            Array(deref!(Type::Char), _) => (),
            _ => {
                tc.raise_error(self.expr.clone(),
                    "Output is only supported for integers, booleans, characters \
                    and strings".into());
            }
        }
        None
    }

    fn codegen(&self, cg: &mut Codegen) {
        match cg.type_cache.get(&*self.expr).unwrap() {
            Type::Int => {
                if let Some(reg) = self.expr.codegen_register(cg) {
                    cg.emit(("move", CG::A0, reg));
                } else {
                    cg.emit_pop(CG::A0);
                }
                cg.emit(("li", CG::V0, 1));
                cg.emit("syscall");
            },
            Type::Char => {
                self.expr.codegen(cg);
                cg.emit_pop(CG::A0);
                cg.emit(("li", CG::V0, 11));
                cg.emit("syscall");
            },
            Type::Array(deref!(Type::Char), _) => {
                if let Some(reg) = self.expr.codegen_lvalue_register(cg) {
                    cg.emit(("move", CG::A0, reg));
                } else {
                    cg.emit_pop(CG::A0);
                }
                cg.emit(("li", CG::V0, 4));
                cg.emit("syscall");
            },
            Type::Reference(deref!(Type::Char)) => {
                if let Some(reg) = self.expr.codegen_register(cg) {
                    cg.emit(("move", CG::A0, reg));
                } else {
                    cg.emit_pop(CG::A0);
                }
                cg.emit(("li", CG::V0, 4));
                cg.emit("syscall");
            },
            _ => unimplemented!(),
        }
    }
}

impl Stmt for OutputStmt {}

pub struct EmitStmt {
    pub text: StringLit,
}

impl Ast for EmitStmt {
    fn unparse(&self, up: Up) {
        up.write_indent();
        up.write("emit ");
        self.text.unparse(up);
        up.end_stmt();
    }

    fn codegen(&self, cg: &mut Codegen) {
        let str_contents = self.text.unparse_to_string(cg.ref_text);
        cg.emit(str_contents.trim_matches('\"'));
    }
}

impl Stmt for EmitStmt {}

pub struct RegisterLit(pub String);

pub struct LoadStmt {
    reg: RegisterLit,
    rvalue: Boxpr,
}

impl LoadStmt {
    pub fn new(reg: RegisterLit, rvalue: Boxpr) -> Self {
        Self { reg, rvalue }
    }
}

impl Ast for LoadStmt {
    fn unparse(&self, up: Up) {
        up.write_indent();
        up.write(&self.reg.0);
        up.write(" <- ");
        self.rvalue.unparse(up);
        up.end_stmt();
    }

    fn analyze_names(&self, na: NACtx) {
        self.rvalue.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        let ty = self.rvalue.typecheck(tc)?;
        tc.cache_type(&*self.rvalue, &ty);
        None
    }

    fn codegen(&self, cg: &mut Codegen) {
        if let Some(reg) = self.rvalue.codegen_register(cg) {
            cg.emit(("move", &*self.reg.0, reg));
        } else {
            cg.emit_pop(&*self.reg.0);
        }
    }
}

impl Stmt for LoadStmt {}

pub struct StoreStmt {
    reg: RegisterLit,
    lvalue: BoxLoc,
}

impl StoreStmt {
    pub fn new(reg: RegisterLit, lvalue: BoxLoc) -> Self {
        Self { reg, lvalue }
    }
}

impl Ast for StoreStmt {
    fn unparse(&self, up: Up) {
        up.write_indent();
        up.write(&self.reg.0);
        up.write(" -> ");
        self.lvalue.unparse(up);
        up.end_stmt();
    }

    fn analyze_names(&self, na: NACtx) {
        self.lvalue.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        let ty = self.lvalue.typecheck(tc)?;
        tc.cache_type(&*self.lvalue, &ty);
        None
    }

    fn codegen(&self, cg: &mut Codegen) {
        let ins = match cg.type_cache.get(&*self.lvalue).unwrap().size() {
            1 => "sb",
            4 => "sw",
            _ => panic!()
        };
        if let Some(reg) = self.lvalue.codegen_register(cg) {
            cg.emit((ins, &*self.reg.0, reg, Ix(0)));
        } else {
            cg.emit_pop(CG::T0);
            cg.emit((ins, &*self.reg.0, CG::T0, Ix(0)));
        }
    }
}

impl Stmt for StoreStmt {}
