use std::sync::Arc;

use crate::parser::ast::*;

macro_rules! dump {
    ($self_:ident, $($message:tt)*) => {{
        for _ in 0..($self_.indent*2) {
            print!(" ");
        }

        println!($($message)*);
    }};
}

pub fn dump_file(ast: &Arc<File>) {
    let mut dumper = AstDumper { indent: 0 };

    dumper.dump_file(ast);
}

pub fn dump_function(function: &Function) {
    let mut dumper = AstDumper { indent: 0 };

    dumper.dump_function(function);
}

pub fn dump_expr<'a>(expr: &'a ExprData) {
    let mut dumper = AstDumper { indent: 0 };

    dumper.dump_expr(expr);
}

pub fn dump_special_form<'a>(special_form: &'a SpecialFormData) {
    let mut dumper = AstDumper { indent: 0 };

    dumper.dump_special_form(special_form);
}

struct AstDumper {
    indent: u32,
}

impl AstDumper {
    fn dump_file(&mut self, f: &File) {
        for el in &f.elements {
            self.dump_elem(el);
        }
    }

    fn dump_elem(&mut self, el: &ElemData) {
        match *el {
            ElemData::Function(ref node) => self.dump_function(node),
            ElemData::Struct(ref node) => self.dump_struct(node),
            ElemData::Global(ref node) => self.dump_global(node),
            ElemData::Const(ref node) => self.dump_const(node),
            ElemData::Enum(ref node) => self.dump_enum(node),
            ElemData::TypeAlias(ref node) => self.dump_associated_type(node),
            ElemData::Error { id, span } => {
                dump!(self, "error @ {} {}", span, id);
            }
        }
    }

    fn dump_global(&mut self, global: &Global) {
        dump!(self, "global @ {} {}", global.span, global.id);
        self.dump_ident(&global.name);

        self.indent(|d| {
            d.dump_type(&global.data_type);

            if let Some(ref initial_value) = global.initial_value {
                d.dump_expr(initial_value);
            } else {
                dump!(d, "<no expr given>");
            }
        });
    }

    fn dump_extern(&mut self, special_form: &ExternPackage) {
        dump!(
            self,
            "extern package @ {} {}",
            special_form.span,
            special_form.id
        );
        self.dump_ident(&special_form.name);
        self.dump_ident(&special_form.identifier);
    }

    fn dump_enum(&mut self, enum_: &Enum) {
        dump!(self, "enum @ {} {}", enum_.span, enum_.id);
        self.dump_ident(&enum_.name);

        self.indent(|d| {
            for value in &enum_.variants {
                d.dump_enum_value(value);
            }
        });
    }

    fn dump_enum_value(&mut self, value: &EnumVariant) {
        dump!(self, "enum variant @ {} {}", value.span, value.id);
        self.dump_ident(&value.name);

        if let Some(ref types) = value.types {
            self.indent(|d| {
                for ty in types {
                    d.dump_type(ty);
                }
            });
        }
    }

    fn dump_struct(&mut self, struc: &Struct) {
        dump!(self, "struct @ {} {}", struc.span, struc.id);
        self.dump_ident(&struc.name);

        self.indent(|d| {
            for field in &struc.fields {
                d.dump_struct_field(field);
            }
        });
    }

    fn dump_struct_field(&mut self, field: &StructField) {
        dump!(self, "field @ {} {}", field.span, field.id);
        self.dump_ident(&field.name);
        self.indent(|d| d.dump_type(&field.data_type));
    }

    fn dump_associated_type(&mut self, t: &TypeAlias) {
        dump!(self, "trait @ {} {}", t.span, t.id);
        self.indent(|d| {
            d.dump_ident(&t.name);
        });
    }

    fn dump_class(&mut self, cls: &Class) {
        dump!(self, "class @ {} {}", cls.span, cls.id);
        self.dump_ident(&cls.name);

        self.indent(|d| {
            dump!(d, "fields");

            d.indent(|d| {
                for field in &cls.fields {
                    d.dump_field(field);
                }
            });
        });
    }

    fn dump_field(&mut self, field: &Field) {
        dump!(self, "field @ {} {}", field.span, field.id);
        self.dump_ident(&field.name);
        self.indent(|d| d.dump_type(&field.data_type));
    }

    fn dump_function(&mut self, function: &Function) {
        dump!(self, "function @ {} {}", function.span, function.id);
        self.dump_ident(&function.name);

        self.indent(|d| {
            dump!(d, "params");
            d.indent(|d| {
                if function.params.is_empty() {
                    dump!(d, "no params");
                } else {
                    for param in &function.params {
                        d.dump_param(param);
                    }
                }
            });

            dump!(d, "returns");

            if let Some(ref ty) = function.return_type {
                d.indent(|d| d.dump_type(ty));
            } else {
                d.indent(|d| dump!(d, "<no return type>"))
            }

            dump!(d, "executes");

            if let Some(ref block) = function.block {
                d.indent(|d| d.dump_expr(block));
            }
        });
    }

    fn dump_param(&mut self, param: &Param) {
        dump!(self, "param @ {} {}", param.span, param.id);
        self.dump_ident(&param.name);

        self.indent(|d| d.dump_type(&param.data_type));
    }

    fn dump_type(&mut self, ty: &TypeData) {
        dump!(self, "type @ {:?} {}", ty.span(), ty.id());
    }

    fn dump_special_form(&mut self, special_form: &SpecialFormData) {
        match *special_form {
            SpecialFormData::Expr(ref expr) => self.dump_special_form_expr(expr),
            SpecialFormData::Let(ref SpecialForm) => self.dump_special_form_let(SpecialForm),
        }
    }

    fn dump_special_form_let(&mut self, special_form: &SpecialFormLetType) {
        dump!(self, "let @ {} {}", special_form.span, special_form.id);

        self.indent(|d| {
            d.dump_special_form_let_pattern(&special_form.pattern);
            dump!(d, "type");
            d.indent(|d| {
                if let Some(ref ty) = special_form.data_type {
                    d.dump_type(ty);
                } else {
                    dump!(d, "<no type given>");
                }
            });

            dump!(d, "expr");
            d.indent(|d| {
                if let Some(ref expr) = special_form.expr {
                    d.dump_expr(expr);
                } else {
                    dump!(d, "<no expr given>");
                }
            });
        });
    }

    fn dump_special_form_let_pattern(&mut self, pattern: &LetPattern) {
        match pattern {
            LetPattern::Sym(ref ident) => dump!(self, "ident {:?}", ident.name),
            LetPattern::Underscore(_) => dump!(self, "_"),
            LetPattern::Tuple(ref tuple) => {
                dump!(self, "tuple");
                self.indent(|d| {
                    for part in &tuple.parts {
                        d.dump_special_form_let_pattern(part);
                    }
                });
            }
        }
    }

    fn dump_expr_for(&mut self, special_form: &ExprForType) {
        dump!(self, "for @ {} {}", special_form.span, special_form.id);

        self.indent(|d| {
            d.dump_special_form_let_pattern(&special_form.pattern);
            dump!(d, "cond");
            d.indent(|d| {
                d.dump_expr(&special_form.expr);
            });
            dump!(d, "body");
            d.indent(|d| {
                d.dump_expr(&special_form.block);
            });
        });
    }

    fn dump_expr_while(&mut self, expr: &ExprWhileType) {
        dump!(self, "while @ {} {}", expr.span, expr.id);

        self.indent(|d| {
            dump!(d, "cond");
            d.indent(|d| {
                d.dump_expr(&expr.cond);
            });

            dump!(d, "body");
            d.indent(|d| {
                d.dump_expr(&expr.block);
            });
        });
    }

    fn dump_special_form_expr(&mut self, special_form: &SpecialFormExprType) {
        dump!(
            self,
            "expr SpecialForm @ {} {}",
            special_form.span,
            special_form.id
        );
        self.indent(|d| {
            d.dump_expr(&special_form.expr);
        });
    }

    fn dump_expr_return(&mut self, ret: &ExprReturnType) {
        dump!(self, "return @ {} {}", ret.span, ret.id);

        self.indent(|d| {
            if let Some(ref expr) = ret.expr {
                d.dump_expr(expr);
            } else {
                dump!(d, "<nothing>");
            }
        });
    }

    fn dump_expr_break(&mut self, special_form: &ExprBreakType) {
        dump!(self, "break @ {} {}", special_form.span, special_form.id);
    }

    fn dump_expr_continue(&mut self, special_form: &ExprContinueType) {
        dump!(self, "continue @ {} {}", special_form.span, special_form.id);
    }

    fn dump_expr(&mut self, expr: &ExprData) {
        match *expr {
            ExprData::Un(ref un) => self.dump_expr_un(un),
            ExprData::Bin(ref bin) => self.dump_expr_bin(bin),
            ExprData::Dot(ref field) => self.dump_expr_dot(field),
            ExprData::LiteralChar(ref lit) => self.dump_expr_lit_char(lit),
            ExprData::LiteralInt(ref lit) => self.dump_expr_lit_int(lit),
            ExprData::LiteralFloat(ref lit) => self.dump_expr_lit_float(lit),
            ExprData::LiteralStr(ref lit) => self.dump_expr_lit_str(lit),
            ExprData::Template(ref tmpl) => self.dump_expr_template(tmpl),
            ExprData::LiteralBool(ref lit) => self.dump_expr_lit_bool(lit),
            ExprData::Sym(ref ident) => self.dump_expr_ident(ident),
            ExprData::Call(ref call) => self.dump_expr_call(call),
            ExprData::TypeParam(ref expr) => self.dump_expr_type_param(expr),
            ExprData::Path(ref path) => self.dump_expr_path(path),
            ExprData::This(ref selfie) => self.dump_expr_self(selfie),
            ExprData::Conv(ref expr) => self.dump_expr_conv(expr),
            ExprData::Lambda(ref expr) => self.dump_expr_lambda(expr),
            ExprData::Block(ref expr) => self.dump_expr_block(expr),
            ExprData::If(ref expr) => self.dump_expr_if(expr),
            ExprData::Tuple(ref expr) => self.dump_expr_tuple(expr),
            ExprData::Paren(ref expr) => self.dump_expr_paren(expr),
            ExprData::Match(ref expr) => self.dump_expr_match(expr),
            ExprData::For(ref expr) => self.dump_expr_for(expr),
            ExprData::While(ref expr) => self.dump_expr_while(expr),
            ExprData::Break(ref expr) => self.dump_expr_break(expr),
            ExprData::Continue(ref expr) => self.dump_expr_continue(expr),
            ExprData::Return(ref ret) => self.dump_expr_return(ret),
            ExprData::Error { id, span } => {
                dump!(self, "error @ {} {}", span, id);
            }
        }
    }

    fn dump_expr_block(&mut self, block: &ExprBlockType) {
        dump!(
            self,
            "block ({} statement(s)) @ {} {}",
            block.SpecialForms.len(),
            block.span,
            block.id
        );

        self.indent(|d| {
            if block.SpecialForms.is_empty() {
                dump!(d, "no statements");
            } else {
                for SpecialForm in &block.SpecialForms {
                    d.dump_special_form(SpecialForm);
                }
            }

            if let Some(ref expr) = block.expr {
                dump!(d, "value");
                d.dump_expr(expr);
            }
        });

        dump!(self, "block end");
    }

    fn dump_expr_if(&mut self, expr: &ExprIfType) {
        dump!(self, "if @ {} {}", expr.span, expr.id);

        self.indent(|d| {
            d.indent(|d| {
                d.dump_expr(&expr.cond);
            });
            dump!(d, "then");
            d.indent(|d| {
                d.dump_expr(&expr.then_block);
            });
            dump!(d, "else");
            d.indent(|d| {
                d.dump_expr(&expr.then_block);
            });
        });
    }

    fn dump_expr_conv(&mut self, expr: &ExprConvType) {
        self.indent(|d| d.dump_expr(&expr.object));
        dump!(self, "as @ {} {}", expr.span, expr.id);
        self.indent(|d| d.dump_type(&expr.data_type));
    }

    fn dump_expr_self(&mut self, selfie: &ExprSelfType) {
        dump!(self, "self @ {} {}", selfie.span, selfie.id);
    }

    fn dump_expr_lit_char(&mut self, literal: &ExprLiteralCharType) {
        dump!(
            self,
            "literal char {} @ {} {}",
            literal.value,
            literal.span,
            literal.id
        );
    }

    fn dump_expr_lit_int(&mut self, literal: &ExprLiteralIntType) {
        dump!(
            self,
            "literal int {} @ {} {}",
            literal.value,
            literal.span,
            literal.id
        );
    }

    fn dump_expr_lit_float(&mut self, literal: &ExprLiteralFloatType) {
        dump!(
            self,
            "literal float {} @ {} {}",
            literal.value,
            literal.span,
            literal.id
        );
    }

    fn dump_expr_lit_str(&mut self, literal: &ExprLiteralStrType) {
        dump!(
            self,
            "literal string {:?} @ {} {}",
            literal.value,
            literal.span,
            literal.id
        );
    }

    fn dump_expr_template(&mut self, tmpl: &ExprTemplateType) {
        dump!(self, "template @ {} {}", tmpl.span, tmpl.id);
        self.indent(|d| {
            for part in &tmpl.parts {
                d.dump_expr(part)
            }
        });
    }

    fn dump_expr_lit_bool(&mut self, literal: &ExprLiteralBoolType) {
        dump!(
            self,
            "literal bool {} @ {} {}",
            literal.value,
            literal.span,
            literal.id
        );
    }

    fn dump_expr_ident(&mut self, ident: &ExprSymType) {
        dump!(self, "ident {} @ {} {}", ident.name, ident.span, ident.id);
    }

    fn dump_expr_un(&mut self, expr: &ExprUnType) {
        dump!(self, "unary {:?} @ {} {}", expr.op, expr.span, expr.id);
        self.indent(|d| d.dump_expr(&expr.opnd));
    }

    fn dump_expr_bin(&mut self, expr: &ExprBinType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "binary {:?} @ {} {}", expr.op, expr.span, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn dump_expr_lambda(&mut self, function: &Arc<Function>) {
        dump!(self, "lambda @ {} {}", function.span, function.id);
        self.indent(|d| d.dump_function(function));
    }

    fn dump_expr_tuple(&mut self, expr: &ExprTupleType) {
        dump!(self, "tuple @ {} {}", expr.span, expr.id);
        self.indent(|d| {
            for expr in &expr.values {
                d.dump_expr(expr);
            }
        });
    }

    fn dump_expr_dot(&mut self, expr: &ExprDotType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "dot @ {} {}", expr.span, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn dump_expr_path(&mut self, expr: &ExprPathType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "path (::) @ {} {}", expr.span, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn dump_expr_call(&mut self, expr: &ExprCallType) {
        dump!(self, "call @ {} {}", expr.span, expr.id);

        self.indent(|d| {
            dump!(d, "callee");
            d.indent(|d| d.dump_expr(&expr.callee));

            for arg in &expr.args {
                d.dump_expr(arg);
            }
        });
    }

    fn dump_expr_paren(&mut self, expr: &ExprParenType) {
        dump!(self, "paren @ {} {}", expr.span, expr.id);
        self.indent(|d| {
            d.dump_expr(&expr.expr);
        });
    }

    fn dump_expr_match(&mut self, expr: &ExprMatchType) {
        dump!(self, "match @ {} {}", expr.span, expr.id);
        self.indent(|d| {
            d.dump_expr(&expr.expr);
        });
    }

    fn dump_expr_type_param(&mut self, expr: &ExprTypeParamType) {
        dump!(self, "type param @ {} {}", expr.span, expr.id);

        self.indent(|d| {
            dump!(d, "callee");
            d.indent(|d| d.dump_expr(&expr.callee));

            for arg in &expr.args {
                d.dump_type(arg);
            }
        });
    }

    fn dump_ident(&self, ident: &Option<Sym>) {
        if let Some(ident) = ident {
            dump!(self, "ident {}", ident.name_as_string);
        } else {
            dump!(self, "missing ident");
        }
    }

    fn indent<F>(&mut self, function: F)
    where
        F: Fn(&mut AstDumper),
    {
        let old = self.indent;
        self.indent = old + 1;

        function(self);

        self.indent = old;
    }
}
