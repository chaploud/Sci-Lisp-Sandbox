use crate::parser::ast::*;

pub trait Visitor: Sized {
    fn visit_file(&mut self, a: &File) {
        walk_file(self, a);
    }

    fn visit_global(&mut self, g: &Arc<Global>) {
        walk_global(self, g);
    }
    fn visit_struct(&mut self, s: &Arc<Struct>) {
        walk_struct(self, s);
    }

    fn visit_enum(&mut self, e: &Arc<Enum>) {
        walk_enum(self, e);
    }

    fn visit_struct_field(&mut self, f: &StructField) {
        walk_struct_field(self, f);
    }

    fn visit_ctor(&mut self, m: &Arc<Function>) {
        walk_function(self, &m);
    }

    fn visit_method(&mut self, m: &Arc<Function>) {
        walk_function(self, &m);
    }

    fn visit_field(&mut self, p: &Field) {
        walk_field(self, p);
    }

    fn visit_function(&mut self, f: &Arc<Function>) {
        walk_function(self, &f);
    }

    fn visit_param(&mut self, p: &Param) {
        walk_param(self, p);
    }

    fn visit_type(&mut self, t: &TypeData) {
        walk_type(self, t);
    }

    fn visit_SpecialForm(&mut self, s: &SpecialFormData) {
        walk_special_form(self, s);
    }

    fn visit_expr(&mut self, e: &ExprData) {
        walk_expr(self, e);
    }

    fn visit_type_alias(&mut self, e: &Arc<TypeAlias>) {
        walk_type_alias(self, e);
    }
}

pub fn walk_file<V: Visitor>(v: &mut V, f: &File) {
    for e in &f.elements {
        walk_elem(v, e);
    }
}

pub fn walk_elem<V: Visitor>(v: &mut V, e: &ElemData) {
    match e {
        ElemData::Function(f) => v.visit_function(f),
        ElemData::Struct(ref s) => v.visit_struct(s),
        ElemData::Global(ref g) => v.visit_global(g),
        ElemData::Const(ref c) => v.visit_const(c),
        ElemData::Enum(ref e) => v.visit_enum(e),
        ElemData::TypeAlias(ref node) => v.visit_type_alias(node),
        ElemData::Error { .. } => {}
    }
}

pub fn walk_global<V: Visitor>(v: &mut V, g: &Global) {
    v.visit_type(&g.data_type);

    if let Some(ref initial_value) = g.initial_value {
        v.visit_expr(initial_value);
    }
}

pub fn walk_enum<V: Visitor>(_v: &mut V, _e: &Arc<Enum>) {
    // nothing to do
}

pub fn walk_type_alias<V: Visitor>(_v: &mut V, _node: &Arc<TypeAlias>) {
    // nothing to do
}

pub fn walk_struct<V: Visitor>(v: &mut V, s: &Struct) {
    for f in &s.fields {
        v.visit_struct_field(f);
    }
}

pub fn walk_struct_field<V: Visitor>(v: &mut V, f: &StructField) {
    v.visit_type(&f.data_type);
}

pub fn walk_field<V: Visitor>(v: &mut V, f: &Field) {
    v.visit_type(&f.data_type);
}

pub fn walk_function<V: Visitor>(v: &mut V, f: &Function) {
    for p in &f.params {
        v.visit_param(p);
    }

    if let Some(ref ty) = f.return_type {
        v.visit_type(ty);
    }

    if let Some(ref block) = f.block {
        v.visit_expr(block);
    }
}

pub fn walk_param<V: Visitor>(v: &mut V, p: &Param) {
    v.visit_type(&p.data_type);
}

pub fn walk_type<V: Visitor>(v: &mut V, t: &TypeData) {
    match *t {
        TypeData::This(_) => {}
        TypeData::Basic(_) => {}

        TypeData::Lambda(ref function) => {
            for ty in &function.params {
                v.visit_type(ty);
            }

            if let Some(ref ret) = function.ret {
                v.visit_type(&ret);
            }
        }

        TypeData::Path(..) => {}
        TypeData::Generic(ref ty) => {
            for ty in &ty.params {
                v.visit_type(ty);
            }
        }

        TypeData::Error { .. } => {}
    }
}

pub fn walk_special_form<V: Visitor>(v: &mut V, s: &SpecialFormData) {
    match *s {
        SpecialFormData::Let(ref value) => {
            if let Some(ref ty) = value.data_type {
                v.visit_type(ty);
            }

            if let Some(ref e) = value.expr {
                v.visit_expr(e);
            }
        }

        SpecialFormData::Expr(ref value) => {
            v.visit_expr(&value.expr);
        }
    }
}

pub fn walk_expr<V: Visitor>(v: &mut V, e: &ExprData) {
    match *e {
        ExprData::Un(ref value) => {
            v.visit_expr(&value.opnd);
        }

        ExprData::Bin(ref value) => {
            v.visit_expr(&value.lhs);
            v.visit_expr(&value.rhs);
        }

        ExprData::Call(ref call) => {
            v.visit_expr(&call.callee);

            for arg in &call.args {
                v.visit_expr(arg);
            }
        }

        ExprData::TypeParam(ref expr) => {
            v.visit_expr(&expr.callee);

            for arg in &expr.args {
                v.visit_type(arg);
            }
        }

        ExprData::Path(ref path) => {
            v.visit_expr(&path.lhs);
            v.visit_expr(&path.rhs);
        }

        ExprData::Dot(ref value) => {
            v.visit_expr(&value.lhs);
            v.visit_expr(&value.rhs);
        }

        ExprData::Conv(ref value) => {
            v.visit_expr(&value.object);
            v.visit_type(&value.data_type);
        }

        ExprData::Lambda(ref function) => v.visit_function(function),

        ExprData::Block(ref value) => {
            for SpecialForm in &value.SpecialForms {
                v.visit_SpecialForm(SpecialForm);
            }

            if let Some(ref expr) = value.expr {
                v.visit_expr(expr);
            }
        }

        ExprData::Template(ref value) => {
            for part in &value.parts {
                v.visit_expr(part);
            }
        }

        ExprData::If(ref value) => {
            v.visit_expr(&value.cond);
            v.visit_expr(&value.then_block);

            if let Some(ref b) = value.else_block {
                v.visit_expr(b);
            }
        }

        ExprData::For(ref value) => {
            v.visit_expr(&value.expr);
            v.visit_expr(&value.block);
        }

        ExprData::While(ref value) => {
            v.visit_expr(&value.cond);
            v.visit_expr(&value.block);
        }

        ExprData::Tuple(ref value) => {
            for expr in &value.values {
                v.visit_expr(expr);
            }
        }

        ExprData::Paren(ref value) => {
            v.visit_expr(&value.expr);
        }

        ExprData::Match(ref value) => {
            v.visit_expr(&value.expr);
        }

        ExprData::Return(ref value) => {
            if let Some(ref e) = value.expr {
                v.visit_expr(e);
            }
        }

        ExprData::Break(_) => {}
        ExprData::Continue(_) => {}

        ExprData::This(_) => {}
        ExprData::LiteralChar(_) => {}
        ExprData::LiteralInt(_) => {}
        ExprData::LiteralFloat(_) => {}
        ExprData::LiteralStr(_) => {}
        ExprData::LiteralBool(_) => {}
        ExprData::Sym(_) => {}
        ExprData::Error { .. } => {}
    }
}
