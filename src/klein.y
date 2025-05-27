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
  : StructDecl { $1.map(box_decl) }
  | VarDecl { $1.map(box_decl) }
  | FunDecl { $1.map(box_decl) }
  ;

VarDecl -> Result<VarDecl, ()>
  : Type Id ';' { Ok(VarDecl {ty: $1?, id: $2?, init: None, sym: OnceCell::new() }) }
  | Type Id '=' Expr ';' { Ok(VarDecl {ty: $1?, id: $2?, init: Some($4?), sym: OnceCell::new() })}
  ;

StructDecl -> Result<StructDecl, ()>
  : 'STRUCT' Id '{' DeclList '}' { Ok(StructDecl {id: $2?, decls: $4?, scope: OnceCell::new() }) }
  ;

FunDecl -> Result<FunDecl, ()>
  : 'FUN' Id FunFormals FunOutput ScopeBlock 
  {
    Ok(FunDecl {
      id: $2?,
      formals: FormalsList { list: $3? },
      ret_ty: $4?,
      body: Some($5?),
      frame_size: OnceCell::new(),
    })
  }
  | 'FUN' Id FunFormals FunOutput ';' {
    Ok(FunDecl {
      id: $2?,
      formals: FormalsList { list: $3? },
      ret_ty: $4?,
      body: None,
      frame_size: OnceCell::new(),
    })
  }
  ;

FunOutput -> Result<TypeNode, ()>
  : 'ARROW' Type { $2 }
  | { Ok(TypeNode::Void) }
  ;

FunFormals -> Result<Vec<FormalParam>, ()>
  : '(' ')' { Ok(vec![]) }
  | '(' FormalsList ')' { $2 }
  ;

FormalParam -> Result<FormalParam, ()>
  : Type Id { Ok(FormalParam {ty: $1?, id: $2?}) }
  | 'SELF_PARAM' 
  { 
    let span = $1.map_err(|_| ())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    Ok(FormalParam {
      ty: TypeNode::SelfRef, 
      id: IdNode { 
        span, line, col, sym: OnceCell::new() 
      } 
    })
  }
  ;

FormalsList -> Result<Vec<FormalParam>, ()>
  : FormalsList ',' FormalParam { flatten($1, $3) }
  | FormalParam { Ok(vec![$1?]) }
  ;

Id -> Result<IdNode, ()>
  : 'ID' 
  { 
    let span = $1.map_err(|_| ())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    Ok(IdNode {
      span, 
      sym: OnceCell::new(),
      line,
      col
    })
  }
  | 'SELF_PARAM' 
  { 
    let span = $1.map_err(|_| ())?.span();
    let ((line, col), _) = $lexer.line_col(span);
    Ok(IdNode {
      span, 
      sym: OnceCell::new(),
      line,
      col
    })
  }

  ;

Type -> Result<TypeNode, ()>:
    'STRUCT' Id { Ok(TypeNode::Struct($2?, OnceCell::new())) }
  | 'VOID' { Ok(TypeNode::Void) }
  | 'PRIM_INT' { Ok(TypeNode::Int) }
  | 'PRIM_BOOL' { Ok(TypeNode::Bool) }
  | 'PRIM_CHAR' { Ok(TypeNode::Char) }
  | 'PRIM_DOUBLE' { Ok(TypeNode::Double) }
  | Type '*' { Ok(TypeNode::Reference(Rc::new($1?))) }
  | Type '[' U32Lit ']' { Ok(TypeNode::Array(Rc::new($1?), $3?)) }
  ;

Stmt -> Result<BoxStmt, ()>
  : IfStmt { $1.map(box_stmt) }
  | IfElseStmt { $1.map(box_stmt) }
  | ReturnStmt { $1.map(box_stmt) }
  | WhileStmt { $1.map(box_stmt) }
  | Expr ';' { Ok(box_stmt(ExprStmt {expr: $1?})) }
  | 'OUT' 'LARROW' Expr ';' { Ok(box_stmt(OutputStmt {expr: $3?})) }
  | 'IN' 'ARROW' Loc ';' { Ok(box_stmt(InputStmt {loc: $3?})) }
  ;

ReturnStmt -> Result<ReturnStmt, ()>
  : 'RETURN' Expr ';' { Ok(ReturnStmt {expr: Some($2?)}) }
  | 'RETURN' ';' { Ok(ReturnStmt {expr: None}) }
  ;

IfStmt -> Result<IfStmt, ()>
  : 'IF' Expr ScopeBlock
  {
    Ok(IfStmt { cond: $2?, body: $3? })
  }
  ;

IfElseStmt -> Result<IfElseStmt, ()>
  : 'IF' Expr ScopeBlock 'ELSE' ScopeBlock
  {
    Ok(IfElseStmt {cond: $2?, then_body: $3?, else_body: $5?})
  }
  ;

WhileStmt -> Result<WhileStmt, ()>
  : 'WHILE' Expr ScopeBlock
  {
    Ok(WhileStmt {cond: $2?, body: $3?})
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
  | AssignExpr { Ok(box_expr($1?)) } 
  | '&' NAExpr { Ok(box_expr(AddrExpr {expr: $2?})) }
  ;

NAExpr -> Result<Boxpr, ()>
  : Loc {
    Ok($1? as Boxpr)
  }
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
  | Loc '(' ExprList ')' { Ok(box_expr(CallExpr {fun: $1?, args: $3?, fun_sym: OnceCell::new()}))}
  | Loc '(' ')' { Ok(box_expr(CallExpr {fun: $1?, args: vec![], fun_sym: OnceCell::new()}))}
  | '-' NAExpr { Ok(box_expr(NegExpr {expr: $2?})) }
  | '!' NAExpr { Ok(box_expr(NotExpr {expr: $2?})) }
  ;

Loc -> Result<BoxLoc, ()>
  : Id { $1.map(box_loc) }
  | NAExpr '.' Id { Ok(box_loc(AccessExpr {obj: $1?, field: $3?, sym: OnceCell::new()})) }
  | '*' NAExpr { Ok(DerefExpr::new($2?)) }
  | NAExpr '[' Expr ']' { Ok(box_loc(IndexExpr {
    ptr: $1?,
    index: $3?
  }))}
  ;

AssignExpr -> Result<AssignExpr, ()>
  : Loc '=' Expr { Ok(AssignExpr { loc: $1?, value: $3?, ty: OnceCell::new()}) }
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
