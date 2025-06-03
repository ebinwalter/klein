use super::*;

#[derive(Debug, Clone)]
pub enum Type {
    Struct(Rc<Id>, OnceCell<Rc<StructDeclSymbol>>),
    Bool,
    Int,
    Char,
    Double,
    Void,
    SelfRef,
    Reference(Rc<Type>),
    Array(Rc<Type>, u32),
}

impl Type {
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
            Self::FunPtr(_, _) => 4,
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
            (Self::Reference(t1), Self::Reference(t2)) => match (&**t1, &**t2) {
                (Self::Void, _) => true,
                (_, Self::Void) => true,
                (a, b) => a.is_subtype_of(b)
            }
            (Self::FunPtr(args1, r1), Self::FunPtr(args2, r2)) => {
                if args1.len() != args2.len() {
                    return false;
                }
                for (ix, arg) in args1.iter().enumerate() {
                    // This isn't backwards:  
                    if !args2[ix].is_subtype_of(arg) {
                        return false;
                    }
                }
                r1.is_subtype_of(r2)
            }
            // Arrays are internally represented as pointers, but we don't want
            // to let users arbitrarily convert pointers into sized arrays
            (Self::Array(t1, _count), Self::Reference(t2)) => t1 == t2,
            (a, b) if a == b => true,
            _ => false
        }
    }
}

impl Display for Type {
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

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        if let Type::Reference(inner) = other {
            if let Type::Void = **inner {
                return true;
            }
        }
        match self {
            Type::Struct(_this_id, this_cell) => {
                if let Type::Struct(_other_id, other_cell) = other {
                    Rc::ptr_eq(this_cell.get().unwrap(), other_cell.get().unwrap())
                } else {
                    false
                }
            },
            Type::Int => matches!(other, Type::Int),
            Type::Bool => matches!(other, Type::Bool),
            Type::Char => matches!(other, Type::Char),
            Type::Double => matches!(other, Type::Double),
            Type::Void => matches!(other, Type::Void),
            Type::SelfRef => todo!(),
            Type::Reference(self_inner) => {
                if let Type::Void = **self_inner {
                     true
                } else if let Type::Reference(other_inner) = other {
                    self_inner == other_inner
                } else {
                    false
                }
            },
            _ => false
        }
    }
}

impl Ast for Type {
    fn unparse(&self, up: Up) {
        match self {
            Type::Struct(id, _) => {
                up.write("struct ");
                id.unparse(up);
            }
            Type::Bool => up.write("bool"),
            Type::Int => up.write("int"),
            Type::Double => up.write("double"),
            Type::Char => up.write("char"),
            Type::SelfRef => up.write("self"),
            Type::Void => up.write("void"),
            Type::Reference(ty) => {
                ty.unparse(up);
                up.write("*");
            },
            Type::Array(ty, count) => {
                ty.unparse(up);
                up.write(&format!("[{count}]"));
            },
            Type::FunPtr(arg_types, ret_ty) => {
                up.write("fun(");
                let mut i = arg_types.iter();
                while let Some(arg) = i.next() {
                    arg.unparse(up);
                    if i.len() > 0 {
                        up.write(", ");
                    }
                }
                up.write(") -> ");
                ret_ty.unparse(up);
            }
        }
    }

    fn analyze_names(&self, na: NACtx) {
        use Type::*;
        match self {
            Void => (),
            Type::Struct(id, cell) => {
                if let Some(s) = na.lookup(id) {
                    if let Symbol::Struct(ref rcs) = *s {
                        cell.set(rcs.clone()).unwrap();
                    } else {
                        na.raise_error(
                            id.as_ref(),
                            format!("Usage of non-struct symbol {} in a struct type", 
                                na.string_for_id(id)));
                    }
                } else {
                    let err = format!(
                        "Usage of undefined symbol {} in a struct type ",
                        na.string_for_id(id)
                    );
                    na.raise_error(id.as_ref(), err);
                }
            },
            Type::Array(ty, _count) => {
                ty.analyze_names(na);
            }
            Type::Reference(ty) => {
                ty.analyze_names(na);
            }
            Type::FunPtr(args, ret_ty) => {
                args.iter().for_each(|x| x.analyze_names(na));
                ret_ty.analyze_names(na);
            }
            Type::Int | Type::Bool | Type::Char
                | Type::Double => (),
            _ => todo!(),
        }
    }
}
