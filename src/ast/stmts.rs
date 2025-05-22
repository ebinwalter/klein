use super::*;

pub struct IfStmt {
    pub cond: Boxpr,
    pub body: ScopeBlock,
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        self.cond.typecheck(tc);
        self.body.typecheck(tc);
        None
    }

    fn compute_var_offsets(&self, oc: OCtx) {
        self.body.compute_var_offsets(oc);
    }

    fn codegen(&self, cg: &mut Codegen) {
        let exit_label = cg.next_label();
        self.cond.codegen(cg);
        cg.emit_pop(CG::T0);
        cg.emit(("beq", CG::ZERO, CG::T0, Label(&exit_label)));
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
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
        self.cond.codegen(cg);
        cg.emit_pop(CG::T0);
        cg.emit(("beq", CG::T0, CG::ZERO, Label(else_label)));
        self.then_body.codegen(cg);
        cg.emit(("j", Label(exit_label)));
        cg.emit(Label(else_label));
        self.else_body.codegen(cg);
        cg.emit(Label(exit_label));
    }
}

impl Stmt for IfElseStmt {}

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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
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
        cg.emit(Label(check_label));
        self.cond.codegen(cg);
        cg.emit_pop(CG::T0);
        cg.emit(("beq", CG::T0, CG::ZERO, Label(end_label)));
        self.body.codegen(cg);
        cg.emit(("j", Label(check_label)));
        cg.emit(Label(end_label));
    }
}

impl Stmt for WhileStmt {}

pub struct ReturnStmt {
    pub expr: Option<Boxpr>
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        self.expr.clone()?.typecheck(tc);
        None
    }
}

impl Stmt for ReturnStmt {}

pub struct ExprStmt {
    pub expr: Boxpr
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        self.expr.typecheck(tc);
        None
    }

    fn codegen(&self, cg: &mut Codegen) {
        self.expr.codegen(cg);
        cg.emit_pop(CG::ZERO);
    }
} 

impl Stmt for ExprStmt {}

pub struct InputStmt {
    pub loc: BoxLoc
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        use TypeNode::*;
        let loc_type = self.loc.typecheck(tc)?;
        match loc_type {
            Int | Bool | Char  => (),
            Reference(t) if matches!(*t, Char) => (),
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
}

impl Stmt for InputStmt {}

pub struct OutputStmt {
    pub expr: Boxpr
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

    fn typecheck(&self, tc: TCCtx) -> Option<TypeNode> {
        use TypeNode::*;
        let expr_type = self.expr.typecheck(tc)?;
        match expr_type {
            Int | Bool | Char  => (),
            Reference(t) if matches!(*t, Char) => (),
            _ => {
                tc.raise_error(self.expr.clone(),
                    "Output is only supported for integers, booleans, characters \
                    and strings".into());
            }
        }
        None
    }
}

impl Stmt for OutputStmt {}
