%start Program
%right '='
%right '!'
%left '||' '&&'
%nonassoc 'EQ' 'NEQ'
%nonassoc 'LT' 'GT' 'LTE' 'GTE'
%left '|'
%right '&'
%left '+' '-'
%left '%'
%left '[' ']'
%left '*' '/'
%left '.'

%expect 1

%%

Program -> Result<Program, ()>
  : DeclList { Ok(Program { decls: $1? }) }
  ;

DeclList -> Result<Vec<BoxDecl>, ()>
  : DeclList Decl { flatten($1, $2) }
  | { Ok(vec![]) }
  ;

ScopeBlock -> Result<ScopeBlock, ()>
  : '{' DeclOrStmtList '}' { Ok(ScopeBlock { list: RefCell::new($2?), table: OnceCell::new(), scope_size: OnceCell::new() }) }
  ;

DeclOrStmtList -> Result<Vec<DeclOrStmt>, ()>
  : DeclOrStmtList DeclOrStmt { flatten($1, $2) }
  | { Ok(vec![]) }
  ;

DeclOrStmt -> Result<DeclOrStmt, ()>
  : Decl { Ok(DeclOrStmt::Decl($1?)) }
  | Stmt { Ok(DeclOrStmt::Stmt($1?)) }
  ;

Decl -> Result<BoxDecl, ()>
  : StructDecl { $1 }
  | VarDecl { $1 }
  | FunDecl { $1 }
  ;

VarDecl -> Result<BoxDecl, ()>
  : Type Id ';' { Ok(VarDecl::new($1?, $2?, None))}
  | Type Id '=' Expr ';' { Ok(VarDecl::new($1?, $2?, Some($4?)))}
  | 'FUN' Id BareTypeArgs FunOutput ';' { 
    let fun_ty = Type::FunPtr($3?, $4?.into());
    Ok(VarDecl::new(fun_ty, $2?, None))
  }
  | 'FUN' Id BareTypeArgs FunOutput '=' Expr ';' { 
    let fun_ty = Type::FunPtr($3?, $4?.into());
    Ok(VarDecl::new(fun_ty, $2?, Some($6?)))
  }
  ;

StructDecl -> Result<BoxDecl, ()>: 
  'STRUCT' Id '{' DeclList '}' { Ok(StructDecl::new($2?, $4?)) }
  ;

FunDecl -> Result<BoxDecl, ()>
  : 'FUN' Id FunFormals FunOutput ScopeBlock 
  {
    Ok(FunDecl::new($2?, $4?, $3?, Some($5?)))
  }
  | 'FUN' Id FunFormals FunOutput ';' {
    Ok(FunDecl::new($2?, $4?, $3?, None))
  }
  ;

FunOutput -> Result<Type, ()>
  : 'ARROW' BareType { $2 }
  | { Ok(Type::Void) }
  ;

FunFormals -> Result<Vec<Rc<FormalParam>>, ()>
  : '(' ')' { Ok(vec![]) }
  | '(' FormalsList ')' { $2 }
  ;

FormalParam -> Result<Rc<FormalParam>, ()>
  : Type Id { Ok(FormalParam::new($1?, $2?)) }
  | 'FUN' Id BareTypeArgs FunOutput
  {
    Ok(FormalParam::new(Type::FunPtr($3?, $4?.into()), $2?))
  }
  | 'SELF_PARAM' 
  { 
    let span = $1.map_err(|_| ())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    let id = Id::new(span, line, col);
    Ok(FormalParam::new(Type::SelfRef, id.into()))
  }
  ;

FormalsList -> Result<Vec<Rc<FormalParam>>, ()>
  : FormalsList ',' FormalParam { flatten($1, $3) }
  | FormalParam { Ok(vec![$1?]) }
  ;

Id -> Result<Rc<Id>, ()>
  : 'ID' 
  { 
    let span = $1.map_err(|_| ())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    Ok(Id::new(span, line, col)) 
  }
  ;

Type -> Result<Type, ()>:
    'STRUCT' Id { Ok(Type::Struct($2?, OnceCell::new())) }
  | 'VOID' { Ok(Type::Void) }
  | 'PRIM_INT' { Ok(Type::Int) }
  | 'PRIM_BOOL' { Ok(Type::Bool) }
  | 'PRIM_CHAR' { Ok(Type::Char) }
  | 'PRIM_DOUBLE' { Ok(Type::Double) }
  | Type '*' { Ok(Type::Reference(Rc::new($1?))) }
  | Type '[' U32Lit ']' { Ok(Type::Array(Rc::new($1?), $3?)) }
  ;

BareType -> Result<Type, ()>:
    'STRUCT' Id { Ok(Type::Struct($2?, OnceCell::new())) }
  | 'PRIM_INT' { Ok(Type::Int) }
  | 'PRIM_BOOL' { Ok(Type::Bool) }
  | 'PRIM_CHAR' { Ok(Type::Char) }
  | 'PRIM_DOUBLE' { Ok(Type::Double) }
  | Type '*' { Ok(Type::Reference(Rc::new($1?))) }
  | Type '[' U32Lit ']' { Ok(Type::Array(Rc::new($1?), $3?)) }
  | 'FUN' BareTypeArgs FunOutput { Ok(Type::FunPtr($2?, Rc::new($3?))) }
  ;

BareTypeArgs -> Result<Vec<Type>, ()>:
    '(' 'VOID' ')' { Ok(vec![]) }
  | '(' BareTypeList ')' { Ok($2?) }
  ;

BareTypeList -> Result<Vec<Type>, ()>:
    BareType  { Ok(vec![$1?]) }
  | BareTypeList ',' BareType { flatten($1, $3) }
  ;

Stmt -> Result<BoxStmt, ()>
  : IfStmt { $1 }
  | IfElseStmt { $1 }
  | ReturnStmt { $1 }
  | WhileStmt { $1 }
  | Expr ';' { Ok(ExprStmt::new($1?)) }
  | 'OUT' 'LARROW' Expr ';' { Ok(box_stmt(OutputStmt {expr: $3?})) }
  | 'IN' 'ARROW' Loc ';' { Ok(box_stmt(InputStmt {loc: $3?})) }
  ;

ReturnStmt -> Result<BoxStmt, ()>
  : 'RETURN' Expr ';' { Ok(ReturnStmt::new(Some($2?))) }
  | 'RETURN' ';' { Ok(ReturnStmt::new(None)) }
  ;

IfStmt -> Result<BoxStmt, ()>
  : 'IF' Expr ScopeBlock
  {
    Ok(IfStmt::new($2?, $3?))
  }
  ;

IfElseStmt -> Result<BoxStmt, ()>
  : 'IF' Expr ScopeBlock 'ELSE' ScopeBlock
  {
    Ok(IfElseStmt::new($2?, $3?, $5?))
  }
  ;

WhileStmt -> Result<BoxStmt, ()>
  : 'WHILE' Expr ScopeBlock
  {
    Ok(WhileStmt::new($2?, $3?))
  }
  ;

/*
TwoOrMoreExprs -> Result<Vec<Boxpr>, ()>
  : Expr ',' Expr { Ok(vec![$1?, $3?]) }
  | TwoOrMoreExprs ',' Expr { flatten($1, $3) }
  ;
*/

ExprList -> Result<Vec<Boxpr>, ()>
  : Expr { Ok(vec![$1?]) }
  | ExprList ',' Expr { flatten($1, $3) }
  ;

Expr -> Result<Boxpr, ()>
  : NAExpr { $1 }
  | AssignExpr { $1 } 
  | '&' NAExpr { Ok(AddrExpr::new($2?)) }
  ;

NAExpr -> Result<Boxpr, ()>
  : Loc {
    Ok($1? as Boxpr)
  }
  | 'PRIM_NULL' { Ok(box_expr(NullLit)) }
  | CharLit { $1.map(box_expr) }
  | IntLit { $1.map(box_expr) }
  | StringLit { $1.map(box_expr) }
  | BoolLit { $1.map(box_expr) }
  | NAExpr '*' NAExpr { Ok(box_expr(Times($1?, $3?))) }
  | NAExpr '+' NAExpr { Ok(box_expr(Plus($1?, $3?))) }
  | NAExpr '-' NAExpr { Ok(box_expr(Minus($1?, $3?))) }
  | NAExpr '/' NAExpr { Ok(box_expr(Divide($1?, $3?))) }
  | NAExpr '%' NAExpr { Ok(box_expr(Mod($1?, $3?))) }
  | NAExpr 'LT' NAExpr { Ok(box_expr(LTExpr {lhs: $1?, rhs: $3?})) }
  | NAExpr 'GT' NAExpr { Ok(box_expr(GTExpr {lhs: $1?, rhs: $3?})) }
  | NAExpr 'EQ' NAExpr { Ok(box_expr(EQExpr {lhs: $1?, rhs: $3?})) }
  | NAExpr 'LTE' NAExpr { Ok(box_expr(LTEExpr {lhs: $1?, rhs: $3?})) }
  | NAExpr 'GTE' NAExpr { Ok(box_expr(GTEExpr {lhs: $1?, rhs: $3?})) }
  | NAExpr 'NEQ' NAExpr { Ok(box_expr(NEQExpr {lhs: $1?, rhs: $3?})) }
  | NAExpr '||' NAExpr { Ok(box_expr(LogicalOr {lhs: $1?, rhs: $3?})) }
  | NAExpr '&&' NAExpr { Ok(box_expr(LogicalAnd {lhs: $1?, rhs: $3?})) }
  | NAExpr '|' NAExpr { Ok(box_expr(BitwiseOr {lhs: $1?, rhs: $3?})) }
  | NAExpr '&' NAExpr { Ok(box_expr(BitwiseAnd {lhs: $1?, rhs: $3?})) }
  | '(' Expr ')' { Ok($2?) }
  | Loc '(' ExprList ')' { Ok(CallExpr::new($1?, $3?))}
  | Loc '(' ')' { Ok(CallExpr::new($1?, vec![]))}
  | '-' NAExpr { Ok(box_expr(NegExpr {expr: $2?})) }
  | '!' NAExpr { Ok(box_expr(NotExpr {expr: $2?})) }
  ;

Loc -> Result<BoxLoc, ()>
  : Id { Ok(($1? as Rc<dyn Loc>).clone()) }
  | NAExpr '.' Id { Ok(AccessExpr::new($1?, $3?)) }
  | '*' NAExpr { Ok(DerefExpr::new($2?)) }
  | NAExpr '[' Expr ']' { Ok(IndexExpr::new_loc( $1?, $3?,) as Rc<dyn Loc>)}
  ;

CharLit -> Result<CharLit, ()>
  : 'CHARLIT' {
    let span = $1.map_err(|_|())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    Ok(CharLit::new(span, line, col))
  }
  ;

AssignExpr -> Result<Boxpr, ()>
  : Loc '=' Expr {
    Ok(AssignExpr::new($1?, $3?))
  }
  ;

IntLit -> Result<IntLit, ()>
  : 'INT' 
  {
    let v = $1.map_err(|_| ())?;
    let span = $1.map_err(|_| ())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    parse_usize($lexer.span_str(v.span()))
      .map(|x| IntLit {
        value: x,
        line, col
      })
  }
  ;

U32Lit -> Result<u32, ()>
  : 'INT' 
  {
    let v = $1.map_err(|_| ())?;
    let span = $1.map_err(|_| ())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    parse_u32($lexer.span_str(v.span()))
  }
  ;

BoolLit -> Result<BoolLit, ()>
  : 'TRUE' 
  {
    let span = $1.map_err(|_| ())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    Ok(BoolLit {
      value: true,
      line, col
    })
  }
  ;

BoolLit -> Result<BoolLit, ()>
  : 'FALSE' 
  {
    let span = $1.map_err(|_| ())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    Ok(BoolLit {
      value: false,
      line, col
    })
  }
  ;

StringLit -> Result<StringLit, ()>
  : 'STRINGLIT' 
  { 
    let span = $1.map_err(|_| ())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    Ok(StringLit {span, line, col})
  }
  ;

%%

use crate::ast::*;
use std::rc::Rc;
use std::cell::{OnceCell, RefCell};
fn flatten<T>(lhs: Result<Vec<T>, ()>, rhs: Result<T, ()>)
           -> Result<Vec<T>, ()>
{
    let mut flt = lhs?;
    flt.push(rhs?);
    Ok(flt)
}

#[inline(always)]
fn box_expr<T: Expr + 'static>(expr: T) -> Boxpr {
  Rc::new(expr) as Boxpr
}

#[inline(always)]
fn box_decl<T: Decl + 'static>(decl: T) -> BoxDecl {
  Rc::new(decl) as BoxDecl
}

#[inline(always)]
fn box_stmt<T: Stmt + 'static>(stmt: T) -> BoxStmt {
  Rc::new(stmt) as BoxStmt 
}

#[inline(always)]
fn box_loc<T: Loc + 'static>(loc: T) -> BoxLoc {
  Rc::new(loc) as BoxLoc
}

fn parse_i32(s: &str) -> Result<i32, ()> {
  match s.parse::<i32>() {
    Ok(val) => Ok(val),
    Err(_) => {
      eprintln!("{} can't be represented as a 32-bit integer", s);
      Err(())
    }
  }
}

fn parse_u32(s: &str) -> Result<u32, ()> {
  match s.parse() {
    Ok(val) => Ok(val),
    Err(_) => {
      eprintln!("{} can't be represented as a 32-bit integer", s);
      Err(())
    }
  }
}

fn parse_usize(s: &str) -> Result<usize, ()> {
  match s.parse::<usize>() {
    Ok(val) => Ok(val),
    Err(_) => {
      eprintln!("{} can't be represented as an address-sized integer", s);
      Err(())
    }
  }
}
