use std::collections::HashMap;
use std::rc::Rc;
use std::cell::{RefCell, OnceCell};
use std::borrow::{Borrow, BorrowMut};
use std::iter::Iterator;
use crate::ast::{FunDecl, TypeNode};

use lrpar::Span;

#[derive(Debug)]
pub enum Symbol {
    Var(Rc<VarSymbol>),
    Func(Rc<FuncSymbol>),
    Struct(Rc<StructDeclSymbol>),
}

impl Symbol {
    pub fn struct_sym(&self) -> Option<Rc<StructDeclSymbol>> {
        match self {
            Symbol::Var(vs) => {
                let TypeNode::Struct(_, ref ssym) = *vs.ty else { 
                    return None 
                }; 
                return Some(ssym.get()?.clone());
            },
            Symbol::Struct(rc) => Some(rc.clone()),
            _ => None
        }
    }

    pub fn string_id(&self) -> &str {
        match self {
            Self::Struct(r) => &r.id,
            Self::Func(f) => &f.id,
            Self::Var(v) => &v.id,
        }
    }
}

#[derive(Debug)]
pub struct VarSymbol {
    pub id: String,
    pub ty: Rc<TypeNode>,
    pub offset: OnceCell<i32>,
    pub global: OnceCell<bool>,
}

pub struct FuncSymbol {
    pub id: String,
    pub scope: TableLayer,
    pub has_body: bool,
    pub arg_types: Vec<TypeNode>,
    pub return_type: TypeNode,
}

impl std::fmt::Debug for FuncSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FuncSymbol")
            .field("id", &self.id)
            .field("has_body", &self.has_body)
            .field("arg_types", &self.arg_types)
            .finish()
    }
}

#[derive(Debug)]
pub struct StructDeclSymbol {
    pub id: String,
    pub scope: TableLayer,
    pub size: OnceCell<u32>
}

pub type TableLayer = Rc<RefCell<HashMap<String, Rc<Symbol>>>>;

pub struct SymbolTable {
    map_stack: Vec<TableLayer>,
}

impl SymbolTable {
    pub fn new_layer() -> TableLayer {
        Rc::new(RefCell::new(HashMap::new()))
    }

    pub fn new() -> Self {
        SymbolTable {
            map_stack: vec![Rc::new(HashMap::new().into())],
        }
    }
    
    pub fn define(&mut self, name: String, mut symbol: Symbol) -> Result<Rc<Symbol>, SymTabError> {
        let mlen = self.map_stack.len();
        let Some(map) = self.map_stack.last_mut() else { 
            return Err(SymTabError::Empty) 
        };
        if let Symbol::Var(ref mut vs) = symbol {
            if mlen == 1 {
                vs.global.set(true);
            } else {
                vs.global.set(false);
            }
        }
        let mut map = map.try_borrow_mut().unwrap();
        if let Some(sym) = map.get(&name) {
            // Allow for forward declaration of functions
            let Symbol::Func(ref func_rc) = **sym else { 
                return Err(SymTabError::Exists) 
            };
            if let FuncSymbol { has_body: true, .. } = **func_rc {
                return Err(SymTabError::Exists)
            }
        }
        let rc = Rc::new(symbol);
        map.insert(name, rc.clone());
        Ok(rc)
    }

    pub fn push_scope(&mut self) -> TableLayer {
        let layer = Rc::new(RefCell::new(HashMap::new()));
        self.map_stack.push(layer.clone());
        layer
    }

    pub fn push_this_scope(&mut self, ref layer: TableLayer) {
        self.map_stack.push(layer.clone());
    }

    pub fn pop_scope(&mut self) -> Result<TableLayer, SymTabError> {
        self.map_stack.pop().ok_or(SymTabError::Empty)
    }

    pub fn lookup_local(&self, name: &str) -> Result<Rc<Symbol>, SymTabError> {
        self.map_stack.last()
            .ok_or(SymTabError::Empty)
            .and_then(|m| {
                let map = m.try_borrow().unwrap();
                map
                    .get(name)
                    .ok_or(SymTabError::DoesntExist)
                    .cloned()
            })
        }



    pub fn lookup_global(&self, name: &str) -> Result<Rc<Symbol>, SymTabError> {
        self.map_stack.iter().rev()
            .filter_map(|layer| layer
                .try_borrow().unwrap()
                .get(name)
                .map(Clone::clone))
            .next()
            .ok_or(SymTabError::DoesntExist)
    }
}

#[derive(Debug)]
pub enum SymTabError {
    Empty,
    Exists,
    DoesntExist
}
