use super::*;

pub struct StructDecl {
    pub id: Rc<Id>,
    pub decls: Vec<BoxDecl>,
    pub scope: OnceCell<TableLayer>
}

impl StructDecl {
    pub fn new(id: Rc<Id>, decls: Vec<BoxDecl>) -> Rc<Self> {
        StructDecl {
            id: id.into(),
            decls,
            scope: OnceCell::new(),
        }.into()
    }
}

impl Ast for StructDecl {
    fn unparse(&self, up: Up) {
        up.write("struct ");
        self.id.unparse(up);
        up.write(" {");
        up.new_line();
        up.indent();
        for decl in self.decls.iter() {
            decl.unparse(up);
        }
        up.outdent();
        up.write_indent();
        up.write("}");
        up.new_line();
    }

    fn analyze_names(&self, na: NACtx) {
        let name = na.string_for_id(&self.id);
        let decl_scope = SymbolTable::new_layer();
        let decl_sym = Rc::new(StructDeclSymbol {
            id: name.clone(),
            scope: decl_scope.clone(),
            size: OnceCell::new(),
        });
        na.struct_mode = true;
        na.define_or_err(&self.id, Symbol::Struct(decl_sym.clone()));
        na.sym_tab.push_this_scope(decl_scope);
        let mut offset = 0;
        for decl in self.decls.iter() {
            decl.analyze_names(na);
            let Some(sym) = decl.symbol_rc() else { continue };
            let Symbol::Var(ref vs) = *sym else { continue };
            let size = vs.ty.size();
            if offset % size != 0 {
                offset += size - (offset % size);
            }
            vs.offset.set(offset as i32).unwrap();
            offset += size;
        }
        if offset % 4 != 0 {
            offset += 4 - (offset % 4);
        }
        decl_sym.size.set(offset).unwrap();
        let name = &decl_sym.id;
        println!("size of struct {name}: {offset} bytes");
        let scope = na.sym_tab.pop_scope()
            .expect("Extra pop_scope occurred during name analysis inside struct");
        self.scope.set(scope.clone())
            .unwrap();
        na.struct_mode = false;
    }
}

impl Decl for StructDecl {}

pub struct VarDecl {
    pub ty: Type,
    pub id: Rc<Id>,
    pub sym: OnceCell<Rc<VarSymbol>>,
    pub init: Option<Boxpr>,
}

impl VarDecl {
    pub fn new(ty: Type, id: Rc<Id>, init: Option<Boxpr>) -> Rc<Self> {
        VarDecl {
            ty: ty.into(),
            id,
            init,
            sym: OnceCell::new(),
        }.into()
    }
}

impl Decl for VarDecl {
    fn split_decl(&self, tc: &mut TypeCache) -> Option<(Rc<dyn Decl>, Rc<dyn Stmt>)> {
        if let Some(ref init) = self.init {
            let decl = VarDecl {
                ty: self.ty.clone(),
                id: self.id.clone(),
                sym: self.sym.clone(),
                init: None
            };
            let loc = self.id.clone();
            let assignment = AssignExpr::new(
                loc.clone(),
                init.clone(),
            );
            tc.insert(&*assignment, &self.ty);
            let stmt = ExprStmt { expr: assignment };
            Some((Rc::new(decl), Rc::new(stmt)))
        } else {
            None
        }
    }
}

impl Ast for VarDecl {
    fn unparse(&self, up: Up) {
        up.write_indent();
        self.ty.unparse(up);
        up.write(" ");
        self.id.unparse(up);
        if let Some(ref e) = self.init {
            up.write(" = ");
            e.unparse(up);
        }
        up.write(";");
        up.new_line();
    }
    
    fn analyze_names(&self, na: NACtx) {
        // Analyze this beforehand because we don't want the init value to
        // reference the variable we are defining
        self.ty.analyze_names(na);
        if let Some(ref init) = self.init {
            init.analyze_names(na);
        }
        let name = span_to_str(&self.id.span, na.ref_text);
        let sym: Rc<_> = VarSymbol {
            id: name.to_owned(),
            ty: Rc::new(self.ty.clone()),
            offset: OnceCell::new(),
            global: OnceCell::new(),
        }.into();
        if let Ok(_existing) = na.sym_tab.lookup_local(name) {
            let m = format!("Redeclaration of variable {name}");
            na.raise_error(self.id.as_ref(), m);
            return;
        }
        self.sym.set(sym.clone()).unwrap();
        na.sym_tab.define(name.to_owned(), Symbol::Var(sym)).unwrap();
        self.id.set_sym(na.sym_tab.lookup_local(name).unwrap());
    }

    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        tc.cache_type(&*self.id, &self.ty);
        if let Some(init) = &self.init {
            let t = init.typecheck(tc)?;
            let expected = self.ty.clone();
            if !t.is_subtype_of(&expected) {
                let m = format!("Attempt to initialize a variable of type {expected} with a value of type {t}");
                tc.raise_error(init.clone(), m);
            }
            tc.cache_type(self.id.as_ref(), &self.ty);
        }
        None
    }

    fn compute_var_offsets(&self, oc: OCtx) {
        let sym = self.sym.get().unwrap();
        if !sym.global.get().unwrap() {
            sym.offset.set(oc.push_var(self.ty.size())).unwrap();
        }
    }

    fn symbol_rc(&self) -> Option<Rc<Symbol>> {
        self.sym.get().cloned()
            .map(Symbol::Var)
            .map(Rc::new)
    }

    fn codegen(&self, cg: &mut Codegen) {
        let Some(sym) = self.sym.get() else { return };
        let &is_global = sym.global.get().expect("Unsure if symbol is global");
        if is_global {
            let size = sym.ty.size();
            let name = &sym.id;
            cg.emit(Directive::Data);
            cg.emit(".align 4");
            cg.emit(Label(&format!("_{}", name)));
            cg.emit(Directive::Space(size));
        }
    }
}

pub struct FunDecl { 
    pub id: Rc<Id>,
    pub ret_ty: Type,
    pub formals: Vec<Rc<FormalParam>>,
    pub body: Option<ScopeBlock>,
    /// Size of frame (includes former frame pointer)
    pub frame_size: OnceCell<u32>,
    // New fields for saved register offsets
}

impl FunDecl {
    pub fn new(id: Rc<Id>, ret_ty: Type, formals: Vec<Rc<FormalParam>>, body: Option<ScopeBlock>) -> Rc<Self> {
        FunDecl {
            id,
            ret_ty,
            formals,
            body,
            frame_size: OnceCell::new(),
        }.into()
    }
}

impl Decl for FunDecl { }

impl Ast for FunDecl {
    fn unparse(&self, up: Up) {
        up.write_indent();
        up.write("fun ");
        self.id.unparse(up);
        up.write("(");
        self.formals.unparse(up);
        up.write(") -> ");
        self.ret_ty.unparse(up);
        if let Some(size) = self.frame_size.get() {
            up.write(&format!("[{size}]"))
        }
        if let Some(body) = &self.body {
            up.space();
            body.unparse(up)
        } else {
            up.write(";");
        }
        up.new_line();
    }

    fn analyze_names(&self, na: NACtx) {
        let name = span_to_str(&self.id.span, na.ref_text);
        self.ret_ty.analyze_names(na);
        let layer = na.sym_tab.push_scope();
        let my_fun_sym = Rc::new(FuncSymbol {
            has_body: self.body.is_some(),
            id: name.to_owned(),
            arg_types: self.formals.iter().map(|x| x.ty.clone()).collect(),
            return_type: self.ret_ty.clone(),
            scope: layer,
        });
        na.define_or_err(&self.id, Symbol::Func(my_fun_sym.clone()));
        let mut formal_aligner = Aligner::new(4, 4);
        for formal in self.formals.iter() {
            formal.ty.analyze_names(na);
            let formal_name = span_to_str(&formal.id.span, na.ref_text);
            let formal_sym = VarSymbol {
                id: formal_name.to_owned(),
                ty: Rc::new(formal.ty.clone()),
                offset: OnceCell::from(formal_aligner.place(formal.ty.size())),
                global: OnceCell::new()
            };
            na.define_or_err(&formal.id, Symbol::Var(Rc::new(formal_sym)));

        }
        if let Some(ref body) = self.body {
            for item in body.get_list().iter() {
                item.analyze_names(na);
            }
        }
        na.sym_tab.pop_scope().unwrap();
        let final_sym = na.sym_tab.define(name.to_owned(), Symbol::Func(my_fun_sym.clone())).unwrap();
        self.id.set_sym(final_sym);
    }
    
    fn typecheck(&self, tc: TCCtx) -> Option<Type> {
        for arg in self.formals.iter() {
            arg.typecheck(tc);
        }
        if let Some(b) = &self.body {
            b.typecheck(tc);
            b.lift_decls(&mut tc.type_cache);
        }
        None
    }

fn compute_var_offsets(&self, oc: OCtx) {
        oc.start_frame();
        // Allocate space for saved registers $t4-$t7 and record their offsets
        if let Some(b) = &self.body {
            b.get_list().iter().for_each(|item| item.compute_var_offsets(oc));
        }
        self.frame_size.set(oc.end_frame()).unwrap();
    }

    fn codegen(&self, cg: &mut Codegen) {
        let Some(body) = &self.body else { return };
        let my_sym = self.id.sym.get().unwrap().clone();
        let Symbol::Func(ref f) = *my_sym else { panic!() };
        let name = if f.id == "main" {
            "main".to_owned()
        } else {
            format!("_{}", f.id)
        };
        cg.function_stack.push(name.clone());
        cg.emit(Directive::Text);
        // Preamble
        cg.emit(Label(&name));
        // Prologue (save old frame pointer, RA, etc.)
        cg.emit(("sw", CG::RA, CG::SP, Ix(0)));
        cg.emit(("sw", CG::FP, CG::SP, Ix(-4)));
        cg.emit(("move", CG::FP, CG::SP));
        cg.emit(("addu", CG::SP, CG::SP, -24));

        cg.reset_regs();
        // Make room on stack for top level variables in the frame (includes saved registers)
        let &frame_size = self.frame_size.get().unwrap();
        cg.emit(("subu", CG::SP, CG::SP, frame_size));
        // Save temporary registers $t4-$t7 to the stack
        for item in body.get_list().iter() {
            item.codegen(cg);
        }
        cg.emit(Label(&format!("{name}_exit")));
        // Restore temporary registers $t4-$t7 from the stack
        cg.emit(("lw", CG::T0, CG::FP, Ix(-4)));
        cg.emit(("lw", CG::RA, CG::FP, Ix(0)));
        cg.emit(("move", CG::SP, CG::FP));
        cg.emit(("move", CG::FP, CG::T0));
        if name != "main" {
            cg.emit(("jr", CG::RA));
        } else {
            cg.emit(("addi", CG::V0, CG::ZERO, 10));
            cg.emit("syscall");
        }
        cg.function_stack.pop().unwrap();
    }
}
