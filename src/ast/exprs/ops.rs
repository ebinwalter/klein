use crate::ast::*;
use crate::codegen::*;

enum OpType {
    Numeric,
    Relational,
    NumericRelational,
    Logical,
}

trait BinOp {
    const SYMBOL: &'static str;
    const OP_TYPE: OpType;
    const OUT_TYPE: Type = Type::Void;
    fn operands(&self) -> (&Boxpr, &Boxpr);
    fn codegen(&self, cg: &mut Codegen, lhs_ty: &Type) {
        panic!("Unimplemented binary operator {}", Self::SYMBOL)
    }
}

impl<T> Ast for T
    where T: BinOp
{
    fn unparse(&self, up: Up) {
        let (lhs, rhs) = self.operands();
        unparse_op(up, Self::SYMBOL, lhs, rhs);
    }
    
    fn analyze_names(&self, na: NACtx) {
        let (lhs, rhs) = self.operands();
        lhs.analyze_names(na);
        rhs.analyze_names(na);
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        let (lhs, rhs) = self.operands();
        let t_lhs = lhs.typecheck(tc);
        let t_rhs = rhs.typecheck(tc);
        if t_lhs.is_none() {
            return Some(Self::OUT_TYPE)
        }
        if t_rhs.is_none() {
            return Some(Self::OUT_TYPE)
        }
        let (t_lhs, t_rhs) = (t_lhs.unwrap(), t_rhs.unwrap());
        let lhs_ast: Rc<dyn Ast> = lhs.clone();
        // We need to cache this because some operators are overloaded
        tc.cache_type(lhs_ast.as_ref(), &t_lhs.clone());
        match Self::OP_TYPE {
            OpType::Numeric => {
                if !matches!(t_lhs, Type::Int | Type::Double | Type::Char) {
                    let m = format!(
                        "{} is a numeric operator which only takes ints or \
                        doubles as operands",
                        Self::SYMBOL
                    );
                    tc.raise_error(lhs.clone(), m);
                    return None
                }
                if t_lhs != t_rhs {
                    let m = format!(
                        "{} is a numeric operator whose operands must be of \
                        the same type",
                        Self::SYMBOL
                    );
                    tc.raise_error(rhs.clone(), m);
                }
                Some(t_lhs)
            }
            OpType::Relational => {
                if t_lhs != t_rhs {
                    let m = format!(
                        "{} is a relational operator whose operands must be of \
                        the same type (got {} and {})",
                        Self::SYMBOL,
                        t_lhs, t_rhs,
                    );
                    tc.raise_error(rhs.clone(), m);
                }
                if let Type::Struct(_, _) = t_lhs {
                    let m = "structs can't be compared directly".into();
                    tc.raise_error(lhs.clone(), m);
                }
                Some(Type::Bool)
            }
            OpType::NumericRelational => {
                if !matches!(t_lhs, Type::Int | Type::Double | Type::Char) {
                    let m = format!(
                        "{} is a numeric relational operator which only takes ints, chars or \
                        doubles as operands",
                        Self::SYMBOL
                    );
                    tc.raise_error(lhs.clone(), m);
                }
                if t_lhs != t_rhs {
                    let m = format!(
                        "{} is a relational numeric operator whose operands must be of \
                        the same type",
                        Self::SYMBOL
                    );
                    tc.raise_error(rhs.clone(), m);
                }
                Some(Type::Bool)
            }
            OpType::Logical => {
                if t_lhs != Type::Bool {
                    tc.raise_error(lhs.clone(),
                        "Arguments to logical operators must be boolean".into());
                }
                if t_rhs != Type::Bool {
                    tc.raise_error(rhs.clone(),
                        "Arguments to logical operators must be boolean".into());
                }
                Some(Type::Bool)
            }
        }
    }

    fn codegen(&self, cg: &mut Codegen) {
        let (lhs, rhs) = self.operands();
        let lhs_ty = cg.type_cache.get(lhs.as_ref())
            .expect("type should've been cached during type checking for \
                     binary operator")
            .clone();
        if let OpType::Logical = Self::OP_TYPE {
            // We leave generating code for operands up to the implementation
            // of the operator because we may want to short-circuit some
            // logical operators
            BinOp::codegen(self, cg, &lhs_ty);
        } else {
            lhs.codegen(cg);
            rhs.codegen(cg);
            cg.emit_pop(CG::T1);
            cg.emit_pop(CG::T0);
            BinOp::codegen(self, cg, &lhs_ty);
        }
    }
}

impl<T> Expr for T
    where T: Ast + BinOp 
{}

pub fn unparse_op(up: Up, op_string: &str, lhs: &Boxpr, rhs: &Boxpr) {
    up.write("(");
    lhs.unparse(up);
    up.write(&format!(" {op_string} "));
    rhs.unparse(up);
    up.write(")");
}

pub struct Times(pub Boxpr, pub Boxpr);

impl BinOp for Times {
    const SYMBOL: &'static str = "*";
    const OP_TYPE: OpType = OpType::Numeric;
    fn operands<'a>(&self) -> (&Boxpr, &Boxpr) {
        (&self.0, &self.1)
    }
}

pub struct Plus(pub Boxpr, pub Boxpr);

impl BinOp for Plus {
    const SYMBOL: &'static str = "+";
    const OP_TYPE: OpType = OpType::Numeric;
    fn operands<'a>(&self) -> (&Boxpr, &Boxpr) {
        (&self.0, &self.1)
    }
    fn codegen(&self, cg: &mut Codegen, lhs_ty: &Type) {
        match lhs_ty {
            Type::Int => {
                cg.emit(("add", CG::T0, CG::T0, CG::T1));
                cg.emit_push(CG::T0);
            },
            Type::Double => {
                todo!("fp arithmetic");
            },
            _ => unreachable!()
        }
    }
}

pub struct Minus(pub Boxpr, pub Boxpr);

impl BinOp for Minus {
    const SYMBOL: &'static str = "-";
    const OP_TYPE: OpType = OpType::Numeric;
    fn operands<'a>(&self) -> (&Boxpr, &Boxpr) {
        (&self.0, &self.1)
    }
    fn codegen(&self, cg: &mut Codegen, lhs_ty: &Type) {
        match lhs_ty {
            Type::Int => {
                cg.emit(("sub", CG::T0, CG::T0, CG::T1));
                cg.emit_push(CG::T0);
            },
            Type::Double => {
                todo!("implement fp arithmetic");
            },
            _ => unreachable!()
        }
    }
}

pub struct Divide(pub Boxpr, pub Boxpr);

impl BinOp for Divide {
    const SYMBOL: &'static str = "/";
    const OP_TYPE: OpType = OpType::Numeric;
    fn operands<'a>(&self) -> (&Boxpr, &Boxpr) {
        (&self.0, &self.1)
    }
}

pub struct Mod(pub Boxpr, pub Boxpr);

impl BinOp for Mod {
    const SYMBOL: &'static str = "%";
    const OP_TYPE: OpType = OpType::Numeric;
    fn operands<'a>(&self) -> (&Boxpr, &Boxpr) {
        (&self.0, &self.1)
    }
}

// Relational operators

pub struct LTExpr {
    pub lhs: Boxpr,
    pub rhs: Boxpr,
}

impl BinOp for LTExpr {
    const SYMBOL: &'static str = "<";
    const OP_TYPE: OpType = OpType::NumericRelational;
    fn operands(&self) -> (&Boxpr, &Boxpr) {
        (&self.lhs, &self.rhs)
    }
    fn codegen(&self, cg: &mut Codegen, lhs_ty: &Type) {
        cg.emit(("slt", CG::T2, CG::T0, CG::T1));
        cg.emit_push(CG::T2);
    }
}

pub struct LTEExpr {
    pub lhs: Boxpr,
    pub rhs: Boxpr,
}

impl BinOp for LTEExpr {
    const SYMBOL: &'static str = "<=";
    const OP_TYPE: OpType = OpType::NumericRelational;
    fn operands(&self) -> (&Boxpr, &Boxpr) {
        (&self.lhs, &self.rhs)
    }
    fn codegen(&self, cg: &mut Codegen, _lhs_ty: &Type) {
        cg.emit(("addi", CG::T1, CG::T1, 1));
        cg.emit(("slt", CG::T2, CG::T0, CG::T1));
        cg.emit_push(CG::T2);
    }
}

pub struct GTExpr {
    pub lhs: Boxpr,
    pub rhs: Boxpr,
}

impl BinOp for GTExpr {
    const SYMBOL: &'static str = ">";
    const OP_TYPE: OpType = OpType::NumericRelational;
    fn operands(&self) -> (&Boxpr, &Boxpr) {
        (&self.lhs, &self.rhs)
    }
    fn codegen(&self, cg: &mut Codegen, lhs_ty: &Type) {
        cg.emit(("slt", CG::T2, CG::T1, CG::T0));
        cg.emit_push(CG::T2);
    }
}

pub struct GTEExpr {
    pub lhs: Boxpr,
    pub rhs: Boxpr,
}

impl BinOp for GTEExpr {
    const SYMBOL: &'static str = ">=";
    const OP_TYPE: OpType = OpType::NumericRelational;
    fn operands(&self) -> (&Boxpr, &Boxpr) {
        (&self.lhs, &self.rhs)
    }
    fn codegen(&self, cg: &mut Codegen, lhs_ty: &Type) {
        cg.emit(("addi", CG::T0, CG::T0, 1));
        cg.emit(("slt", CG::T2, CG::T1, CG::T0));
        cg.emit_push(CG::T2);
    }
}

pub struct EQExpr {
    pub lhs: Boxpr,
    pub rhs: Boxpr,
}

impl BinOp for EQExpr {
    const SYMBOL: &'static str = "==";
    const OP_TYPE: OpType = OpType::Relational;
    fn operands(&self) -> (&Boxpr, &Boxpr) {
        (&self.lhs, &self.rhs)
    }
    fn codegen(&self, cg: &mut Codegen, _lhs_ty: &Type) {
        cg.emit(("slt", "$t2", CG::T0, CG::T1));
        cg.emit(("slt", "$t3", CG::T1, CG::T0));
        cg.emit(("add", "$t2", "$t2", "$t3"));
        cg.emit(("slti", "$t2", "$t2", 1));
        cg.emit_push("$t2");
    }
}

pub struct NEQExpr {
    pub lhs: Boxpr,
    pub rhs: Boxpr,
}

impl BinOp for NEQExpr {
    const SYMBOL: &'static str = "!=";
    const OP_TYPE: OpType = OpType::Relational;
    fn operands(&self) -> (&Boxpr, &Boxpr) {
        (&self.lhs, &self.rhs)
    }
    fn codegen(&self, cg: &mut Codegen, _lhs_ty: &Type) {
        cg.emit(("slt", "$t2", CG::T0, CG::T1));
        cg.emit(("slt", "$t3", CG::T1, CG::T0));
        cg.emit(("add", "$t2", "$t2", "$t3"));
        cg.emit(("slt", "$t2", CG::ZERO, "$t2"));
        cg.emit_push("$t2");
    }
}

pub struct LogicalOr {
    pub lhs: Boxpr,
    pub rhs: Boxpr
}

impl BinOp for LogicalOr {
    const SYMBOL: &'static str = "or";
    const OP_TYPE: OpType = OpType::Logical;
    fn operands(&self) -> (&Boxpr, &Boxpr) {
        (&self.lhs, &self.rhs)
    }
    fn codegen(&self, cg: &mut Codegen, lhs_ty: &Type) {
        let (lhs, rhs) = self.operands();
        lhs.codegen(cg);
    }
}

pub struct LogicalAnd {
    pub lhs: Boxpr,
    pub rhs: Boxpr
}

impl BinOp for LogicalAnd {
    const SYMBOL: &'static str = "and";
    const OP_TYPE: OpType = OpType::Logical;
    fn operands(&self) -> (&Boxpr, &Boxpr) {
        (&self.lhs, &self.rhs)
    }
}

pub struct BitwiseOr {
    pub lhs: Boxpr,
    pub rhs: Boxpr
}

impl BinOp for BitwiseOr {
    const SYMBOL: &'static str = "|";
    const OP_TYPE: OpType = OpType::Numeric;
    fn operands(&self) -> (&Boxpr, &Boxpr) {
        (&self.lhs, &self.rhs)
    }
}

pub struct BitwiseAnd {
    pub lhs: Boxpr,
    pub rhs: Boxpr
}

impl BinOp for BitwiseAnd {
    // TODO: Introduce 'integral' operator type
    // For now, just do dumb shit when we try to apply it
    // to a double
    const OP_TYPE: OpType = OpType::Numeric;
    const SYMBOL: &'static str = "&";
    fn operands(&self) -> (&Boxpr, &Boxpr) {
        (&self.lhs, &self.rhs)
    }
}

pub struct NotExpr {
    pub expr: Boxpr
}

impl Ast for NotExpr {
    fn unparse(&self, up: Up) {
        up.write("!");
        self.expr.unparse(up);
    }

    fn analyze_names(&self, na: NACtx) {
        self.expr.analyze_names(na);
    }

}

impl Expr for NotExpr {}

pub struct NegExpr {
    pub expr: Boxpr
}

impl Ast for NegExpr {
    fn unparse(&self, up: Up) {
        up.write("-");
        self.expr.unparse(up);
    }

    fn analyze_names(&self, na: NACtx) {
        self.expr.analyze_names(na);
    }
}

impl Expr for NegExpr {} 
