use super::*;

#[derive(Debug, Clone)]
pub enum TypeNode {
    Struct(IdNode, OnceCell<Rc<StructDeclSymbol>>),
    Bool,
    Int,
    Char,
    Double,
    Void,
    SelfRef,
    Reference(Rc<TypeNode>),
    Array(Rc<TypeNode>, u32),
}

impl TypeNode {
    pub fn size(&self) -> u32 {
        match self {
            Self::Struct(_, decl) => {
                *decl.get().unwrap().size.get().unwrap()
            }
            Self::Bool => 4,
            Self::Int => 4,
            Self::Char => 1,
            Self::Double => 8,
            Self::Void => 4,
            Self::Reference(_) => 4,
            Self::Array(t, count) => t.size() * count,
            _ => todo!()
        }
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Struct(_, decl), Self::Struct(_, other_decl)) => {
                Rc::ptr_eq(decl.get().unwrap(), other_decl.get().unwrap())
            },
            (Self::Bool, Self::Bool) => true,
            (Self::Int, Self::Char) => true,
            (Self::Double, Self::Double) => true,
            (Self::Void, Self::Void) => true,
            (Self::Reference(t1), Self::Reference(t2)) => t1.is_subtype_of(t2),
            // Arrays are internally represented as pointers, but we don't want
            // to let users arbitrarily convert pointers into sized arrays
            (Self::Array(t1, _count), Self::Reference(t2)) => t1 == t2,
            (a, b) if a == b => true,
            _ => false
        }
    }
}

impl Display for TypeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(id, _sym) => {
                let s = format!(
                    "struct {}", id.symbol().unwrap().string_id()
                );
                f.write_str(&s)
            },
            Self::Bool => f.write_str("bool"),
            Self::Int => f.write_str("int"),
            Self::Char => f.write_str("char"),
            Self::Double => f.write_str("double"),
            Self::Void => f.write_str("void"),
            Self::Array(t, count) => {
                t.fmt(f).and_then(|_| f.write_str(&format!("[{count}]")))
            },
            Self::Reference(t) => {
                t.fmt(f).and_then(|_| f.write_char('*'))
            },
            _ => todo!()
        }
    }
}

impl PartialEq for TypeNode {
    fn eq(&self, other: &Self) -> bool {
        if let TypeNode::Reference(inner) = other {
            if let TypeNode::Void = **inner {
                return true;
            }
        }
        match self {
            TypeNode::Struct(_this_id, this_cell) => {
                if let TypeNode::Struct(_other_id, other_cell) = other {
                    Rc::ptr_eq(this_cell.get().unwrap(), other_cell.get().unwrap())
                } else {
                    false
                }
            },
            TypeNode::Int => matches!(other, TypeNode::Int),
            TypeNode::Bool => matches!(other, TypeNode::Bool),
            TypeNode::Char => matches!(other, TypeNode::Char),
            TypeNode::Double => matches!(other, TypeNode::Double),
            TypeNode::Void => matches!(other, TypeNode::Void),
            TypeNode::SelfRef => todo!(),
            TypeNode::Reference(self_inner) => {
                if let TypeNode::Void = **self_inner {
                     true
                } else if let TypeNode::Reference(other_inner) = other {
                    self_inner == other_inner
                } else {
                    false
                }
            },
            _ => false
        }
    }
}

impl Ast for TypeNode {
    fn unparse(&self, up: Up) {
        match self {
            TypeNode::Struct(id, _) => {
                up.write("struct ");
                id.unparse(up);
            }
            TypeNode::Bool => up.write("bool"),
            TypeNode::Int => up.write("int"),
            TypeNode::Double => up.write("double"),
            TypeNode::Char => up.write("char"),
            TypeNode::SelfRef => up.write("self"),
            TypeNode::Void => up.write("void"),
            TypeNode::Reference(ty) => {
                ty.unparse(up);
                up.write("*");
            },
            TypeNode::Array(ty, count) => {
                ty.unparse(up);
                up.write(&format!("[{count}]"));
            }
        }
    }

    fn analyze_names(&self, na: NACtx) {
        use TypeNode::*;
        match self {
            Void => (),
            TypeNode::Struct(id, cell) => {
                if let Some(s) = na.lookup(id) {
                    if let Symbol::Struct(ref rcs) = *s {
                        cell.set(rcs.clone()).unwrap();
                    } else {
                        na.raise_error(
                            id,
                            format!("Usage of non-struct symbol {} in a struct type", 
                                na.string_for_id(id)));
                    }
                } else {
                    let err = format!(
                        "Usage of undefined symbol {} in a struct type ",
                        na.string_for_id(id)
                    );
                    na.raise_error(id, err);
                }
            },
            TypeNode::Array(ty, _count) => {
                ty.analyze_names(na);
            }
            TypeNode::Reference(ty) => {
                ty.analyze_names(na);
            }
            TypeNode::Int | TypeNode::Bool | TypeNode::Char
                | TypeNode::Double => (),
            _ => todo!(),
        }
    }
}
