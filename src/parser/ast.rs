use std::fmt;
use std::slice::Iter;
use std::sync::Arc;

use crate::parser::green::{GreenNode, GreenToken};
use crate::parser::span::Span;
use crate::parser::token::TokenKind;
use crate::parser::token::TokenKind::*;

pub mod dump;
pub mod visit;

/*
リテラルはそのまま評価される
記号
' ` ~ ~@ @
| スライス
. オブジェクトのメンバ、メソッド, Enumのメンバ
/ モジュールの名前/関数の名前
& デストラクチャリングやrest

キーワードを含むリスト: 最適化のために各々の処理を組み込みで定義してしまう
(), [], {}, #{}, 内側から再帰的に評価される => Listの場合注意
*/

#[derive(Clone, Debug)]
pub struct File {
    pub green: GreenNode,
    pub elements: Vec<Elem>,
}

impl File {
    #[cfg(test)]
    pub fn function0(&self) -> &Function {
        self.elements[0].to_function().unwrap()
    }

    #[cfg(test)]
    pub fn function(&self, index: usize) -> &Function {
        self.elements[index].to_function().unwrap()
    }

    #[cfg(test)]
    pub fn struct0(&self) -> &Struct {
        self.elements[0].to_struct().unwrap()
    }

    #[cfg(test)]
    pub fn enum0(&self) -> &Enum {
        self.elements[0].to_enum().unwrap()
    }

    #[cfg(test)]
    pub fn global0(&self) -> &Global {
        self.elements[0].to_global().unwrap()
    }
    #[cfg(test)]
    pub fn const0(&self) -> &Const {
        self.elements[0].to_const().unwrap()
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash)]
pub struct NodeId(pub usize);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub type Elem = Arc<ElemData>;

#[derive(Clone, Debug)]
pub enum ElemData {
    List(Arc<List>),
    Vector(Arc<Vector>),
    Map(Arc<Map>),
    Set(Arc<Set>),
    Slice(Arc<Slice>),
    Atom(Arc<Atom>),
    Error { id: NodeId, span: Span },
}

impl ElemData {
    pub fn span(&self) -> Span {
        match self {
            ElemData::List(ref node) => node.span,
            ElemData::Vector(ref node) => node.span,
            ElemData::Map(ref node) => node.span,
            ElemData::Set(ref node) => node.span,
            ElemData::Slice(ref node) => node.span,
            ElemData::Atom(ref node) => node.span,
            ElemData::Error { span, .. } => span.clone(),
        }
    }

    pub fn to_list(&self) -> Option<&List> {
        match self {
            &ElemData::List(ref l) => Some(l),
            _ => None,
        }
    }

    pub fn to_vector(&self) -> Option<&Vector> {
        match self {
            &ElemData::Vector(ref v) => Some(v),
            _ => None,
        }
    }

    pub fn to_map(&self) -> Option<&Map> {
        match self {
            &ElemData::Map(ref m) => Some(m),
            _ => None,
        }
    }

    pub fn to_set(&self) -> Option<&Set> {
        match self {
            &ElemData::Set(ref s) => Some(s),
            _ => None,
        }
    }

    pub fn to_slice(&self) -> Option<&Slice> {
        match self {
            &ElemData::Slice(ref sl) => Some(sl),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SymData {
    pub span: Span,
    pub name_as_string: String,
}

pub type Sym = Arc<SymData>;

#[derive(Clone, Debug)]
pub struct Global {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Sym>,
    pub mutable: bool,
    pub data_type: Type,
    pub initial_value: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct Enum {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Sym>,
    pub type_params: Option<TypeParams>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub name: Option<Sym>,
    pub types: Option<Vec<Type>>,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Sym>,
    pub fields: Vec<StructField>,
    pub type_params: Option<TypeParams>,
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Sym>,
    pub data_type: Type,
}

pub type Type = Arc<TypeData>;

#[derive(Clone, Debug)]
pub enum TypeData {
    This(TypeSelfType),
    Basic(TypeBasicType),
    Lambda(TypeLambdaType),
    Path(TypePathType),
    Generic(TypeGenericType),
    Error { id: NodeId, span: Span },
}

#[derive(Clone, Debug)]
pub struct TypeSelfType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

#[derive(Clone, Debug)]
pub struct TypeLambdaType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub params: Vec<Type>,
    pub ret: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeBasicType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub path: Path,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct TypePathType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub path: Path,
}

#[derive(Clone, Debug)]
pub struct TypeGenericType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub path: Type,
    pub params: Vec<Type>,
}

impl TypeBasicType {
    #[cfg(test)]
    pub fn name(&self) -> String {
        assert_eq!(self.path.names.len(), 1);
        self.path
            .names
            .last()
            .cloned()
            .unwrap()
            .name_as_string
            .clone()
    }
}

impl TypeData {
    pub fn create_self(id: NodeId, span: Span, green: GreenNode) -> TypeData {
        TypeData::This(TypeSelfType { id, span, green })
    }

    pub fn create_basic(
        id: NodeId,
        span: Span,
        green: GreenNode,
        path: Path,
        params: Vec<Type>,
    ) -> TypeData {
        TypeData::Basic(TypeBasicType {
            id,
            span,
            green,
            path,
            params,
        })
    }

    pub fn create_path(id: NodeId, span: Span, green: GreenNode, path: Path) -> TypeData {
        TypeData::Path(TypePathType {
            id,
            span,
            green,
            path,
        })
    }

    pub fn create_generic(
        id: NodeId,
        span: Span,
        green: GreenNode,
        path: Type,
        params: Vec<Type>,
    ) -> TypeData {
        TypeData::Generic(TypeGenericType {
            id,
            span,
            green,
            path,
            params,
        })
    }

    pub fn create_function(
        id: NodeId,
        span: Span,
        green: GreenNode,
        params: Vec<Type>,
        ret: Option<Type>,
    ) -> TypeData {
        TypeData::Lambda(TypeLambdaType {
            id,
            span,
            green,
            params,
            ret,
        })
    }

    pub fn to_basic(&self) -> Option<&TypeBasicType> {
        match *self {
            TypeData::Basic(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_function(&self) -> Option<&TypeLambdaType> {
        match *self {
            TypeData::Lambda(ref val) => Some(val),
            _ => None,
        }
    }

    #[cfg(test)]
    pub fn is_unit(&self) -> bool {
        match self {
            &TypeData::Tuple(ref val) if val.subtypes.len() == 0 => true,
            _ => false,
        }
    }

    #[cfg(test)]
    pub fn to_string(&self) -> String {
        match *self {
            TypeData::This(_) => "Self".into(),
            TypeData::Basic(ref val) => val.name(),

            TypeData::Tuple(ref val) => {
                let types: Vec<String> = val.subtypes.iter().map(|t| t.to_string()).collect();

                format!("({})", types.join(", "))
            }

            TypeData::Lambda(ref val) => {
                let types: Vec<String> = val.params.iter().map(|t| t.to_string()).collect();

                if let Some(ref ret) = val.ret {
                    let ret = ret.to_string();
                    format!("({}) -> {}", types.join(", "), ret)
                } else {
                    format!("({}) -> ()", types.join(", "))
                }
            }

            TypeData::Generic(..) | TypeData::Path(..) => unreachable!(),

            TypeData::Error { .. } => "error type".into(),
        }
    }

    pub fn span(&self) -> Span {
        match *self {
            TypeData::This(ref val) => val.span,
            TypeData::Basic(ref val) => val.span,
            TypeData::Lambda(ref val) => val.span,
            TypeData::Error { span, .. } => span,
            TypeData::Path(ref val) => val.span,
            TypeData::Generic(ref val) => val.span,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            TypeData::This(ref val) => val.id,
            TypeData::Basic(ref val) => val.id,
            TypeData::Lambda(ref val) => val.id,
            TypeData::Error { id, .. } => id,
            TypeData::Path(ref val) => val.id,
            TypeData::Generic(ref val) => val.id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeAlias {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub modifiers: Option<ModifierList>,
    pub name: Option<Sym>,
    pub bounds: Vec<Type>,
    pub ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct ExternPackage {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Sym>,
    pub identifier: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct TypeParams {
    pub span: Span,
    pub params: Vec<TypeParam>,
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    pub span: Span,
    pub name: Option<Sym>,
    pub bounds: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Sym>,
    pub data_type: Type,
    pub primary_ctor: bool,
    pub expr: Option<Expr>,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub enum FunctionKind {
    Function,
    Lambda,
}

impl FunctionKind {
    pub fn is_lambda(&self) -> bool {
        match self {
            &FunctionKind::Lambda => true,
            &FunctionKind::Function => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub kind: FunctionKind,

    pub name: Option<Sym>,
    pub type_params: Option<TypeParams>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub block: Option<Expr>,
}

impl Function {
    pub fn block(&self) -> &Expr {
        self.block.as_ref().unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct Param {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Sym>,
    pub mutable: bool,
    pub data_type: Type,
    pub variadic: bool,
}

pub type SpecialForm = Arc<SpecialFormData>;

#[derive(Clone, Debug)]
pub enum SpecialFormData {
    Let(SpecialFormLetType),
    Expr(SpecialFormExprType),
}

impl SpecialFormData {
    pub fn create_let(
        id: NodeId,
        span: Span,
        pattern: Box<LetPattern>,
        data_type: Option<Type>,
        expr: Option<Expr>,
    ) -> SpecialFormData {
        SpecialFormData::Let(SpecialFormLetType {
            id,
            span,

            pattern,
            data_type,
            expr,
        })
    }

    pub fn create_expr(id: NodeId, span: Span, expr: Expr) -> SpecialFormData {
        SpecialFormData::Expr(SpecialFormExprType { id, span, expr })
    }

    pub fn span(&self) -> Span {
        match *self {
            SpecialFormData::Let(ref SpecialForm) => SpecialForm.span,
            SpecialFormData::Expr(ref SpecialForm) => SpecialForm.span,
        }
    }

    pub fn to_let(&self) -> Option<&SpecialFormLetType> {
        match *self {
            SpecialFormData::Let(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_let(&self) -> bool {
        match *self {
            SpecialFormData::Let(_) => true,
            _ => false,
        }
    }

    pub fn to_expr(&self) -> Option<&SpecialFormExprType> {
        match *self {
            SpecialFormData::Expr(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_expr(&self) -> bool {
        match *self {
            SpecialFormData::Expr(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SpecialFormLetType {
    pub id: NodeId,
    pub span: Span,

    pub pattern: Box<LetPattern>,

    pub data_type: Option<Type>,
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub enum LetPattern {
    Sym(LetSymType),
    Tuple(LetTupleType),
    Underscore(LetUnderscoreType),
}

impl LetPattern {
    pub fn is_ident(&self) -> bool {
        match self {
            LetPattern::Sym(_) => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            LetPattern::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn is_underscore(&self) -> bool {
        match self {
            LetPattern::Underscore(_) => true,
            _ => false,
        }
    }

    pub fn to_name(&self) -> Option<String> {
        match self {
            LetPattern::Sym(ref ident) => ident.name.as_ref().map(|i| i.name_as_string.clone()),
            _ => None,
        }
    }

    pub fn to_ident(&self) -> Option<&LetSymType> {
        match self {
            LetPattern::Sym(ref ident) => Some(ident),
            _ => None,
        }
    }

    pub fn to_tuple(&self) -> Option<&LetTupleType> {
        match self {
            LetPattern::Tuple(ref tuple) => Some(tuple),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct LetUnderscoreType {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LetSymType {
    pub id: NodeId,
    pub span: Span,
    pub mutable: bool,
    pub name: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct LetTupleType {
    pub id: NodeId,
    pub span: Span,
    pub parts: Vec<Box<LetPattern>>,
}

#[derive(Clone, Debug)]
pub struct ExprForType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub pattern: Box<LetPattern>,
    pub expr: Expr,
    pub block: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprWhileType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub cond: Expr,
    pub block: Expr,
}

#[derive(Clone, Debug)]
pub struct SpecialFormExprType {
    pub id: NodeId,
    pub span: Span,

    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprReturnType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprBreakType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

#[derive(Clone, Debug)]
pub struct ExprContinueType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnOp {
    Neg,
    Not,
}

impl UnOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            UnOp::Neg => "-",
            UnOp::Not => "!",
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Is,
    IsNot,
}

impl CmpOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            CmpOp::Eq => "==",
            CmpOp::Ne => "!=",
            CmpOp::Lt => "<",
            CmpOp::Le => "<=",
            CmpOp::Gt => ">",
            CmpOp::Ge => ">=",
            CmpOp::Is => "===",
            CmpOp::IsNot => "!==",
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum BinOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Cmp(CmpOp),
    Or,
    And,
    BitOr,
    BitAnd,
    BitXor,
    ShiftL,
    ArithShiftR,
    LogicalShiftR,
}

impl BinOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            BinOp::Assign => "=",
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Cmp(op) => op.as_str(),
            BinOp::Or => "||",
            BinOp::And => "&&",
            BinOp::BitOr => "|",
            BinOp::BitAnd => "&",
            BinOp::BitXor => "^",
            BinOp::ShiftL => "<<",
            BinOp::ArithShiftR => ">>",
            BinOp::LogicalShiftR => ">>>",
        }
    }

    pub fn is_any_assign(&self) -> bool {
        match *self {
            BinOp::Assign => true,
            _ => false,
        }
    }

    pub fn is_compare(&self) -> bool {
        match *self {
            BinOp::Cmp(cmp) if cmp != CmpOp::Is && cmp != CmpOp::IsNot => true,
            _ => false,
        }
    }
}

pub type Expr = Arc<ExprData>;

#[derive(Clone, Debug)]
pub enum ExprData {
    BoolLiteralExpr(BoolLiteralExprType),
    NilLiteralExpr(NilLiteralExprType),
    I64LiteralExpr(I64LiteralExprType),
    F64LiteralExpr(F64LiteralExprType),
    C64LiteralExpr(C64LiteralExprType),
    StringLiteralExpr(StringLiteralExprType),
    RegexLiteralExpr(RegexLiteralExprType),
    KeywordLiteralExpr(KeywordLiteralExprType),
    SymbolExpr(SymbolExprType),
    TypeIdExpr(TypeIdExprType),
    UnderScoreExpr(UnderScoreExprType),
    QuoteExpr(QuoteExprType),
    SyntaxQuoteExpr(SyntaxQuoteExprType),
    UnquoteExpr(UnquoteExprType),
    UnquoteSplicingExpr(UnquoteSplicingExprType),
    SplicingExpr(SplicingExprType),
    DotExpr(DotExprType),
    PipeExpr(PipeExprType),
    SlashExpr(SlashExprType),
    AndExpr(AndExprType),
    RightArrowExpr(RightArrowExprType),
    ListExpr(ListExprType),
    VectorExpr(VectorExprType),
    MapExpr(MapExprType),
    SetExpr(SetExprType),
    DefExpr(DefExprType),
    ConstExpr(ConstExprType),
    LetExpr(LetExprType),
    SeteExpr(SeteExprType),
    DefnExpr(DefnExprType),
    FnExpr(FnExprType),
    ReturnExpr(ReturnExprType),
    IfExpr(IfExprType),
    WhenExpr(WhenExprType),
    CondExpr(CondExprType),
    DoExpr(DoExprType),
    SwitchExpr(SwitchExprType),
    ForExpr(ForExprType),
    WhileExpr(WhileExprType),
    BreakExpr(BreakExprType),
    ContinueExpr(ContinueExprType),
    EnumExpr(EnumExprType),
    StructExpr(StructExprType),
    MethodExpr(MethodExprType),
    SelfExpr(SelfExprType),
    MacroExpr(MacroExprType),
    TryExpr(TryExprType),
    ThrowExpr(ThrowExprType),
    CatchExpr(CatchExprType),
    FinallyExpr(FinallyExprType),
    ImportExpr(ImportExprType),
    Error { id: NodeId, span: Span },
}

impl ExprData {
    pub fn create_block(
        id: NodeId,
        span: Span,
        green: GreenNode,
        SpecialForms: Vec<SpecialForm>,
        expr: Option<Expr>,
    ) -> ExprData {
        ExprData::Block(ExprBlockType {
            id,
            span,
            green,

            SpecialForms,
            expr,
        })
    }

    pub fn create_if(
        id: NodeId,
        span: Span,
        green: GreenNode,
        cond: Expr,
        then_block: Expr,
        else_block: Option<Expr>,
    ) -> ExprData {
        ExprData::If(ExprIfType {
            id,
            span,
            green,
            cond,
            then_block,
            else_block,
        })
    }

    pub fn create_match(
        id: NodeId,
        span: Span,
        green: GreenNode,
        expr: Expr,
        cases: Vec<MatchCaseType>,
    ) -> ExprData {
        ExprData::Match(ExprMatchType {
            id,
            span,
            green,
            expr,
            cases,
        })
    }

    pub fn create_for(
        id: NodeId,
        span: Span,
        green: GreenNode,
        pattern: Box<LetPattern>,
        expr: Expr,
        block: Expr,
    ) -> ExprData {
        ExprData::For(ExprForType {
            id,
            span,
            green,

            pattern,
            expr,
            block,
        })
    }

    pub fn create_while(
        id: NodeId,
        span: Span,
        green: GreenNode,
        cond: Expr,
        block: Expr,
    ) -> ExprData {
        ExprData::While(ExprWhileType {
            id,
            span,
            green,

            cond,
            block,
        })
    }

    pub fn create_return(id: NodeId, span: Span, green: GreenNode, expr: Option<Expr>) -> ExprData {
        ExprData::Return(ExprReturnType {
            id,
            span,
            green,
            expr,
        })
    }

    pub fn create_break(id: NodeId, span: Span, green: GreenNode) -> ExprData {
        ExprData::Break(ExprBreakType { id, span, green })
    }

    pub fn create_continue(id: NodeId, span: Span, green: GreenNode) -> ExprData {
        ExprData::Continue(ExprContinueType { id, span, green })
    }

    pub fn create_un(id: NodeId, span: Span, green: GreenNode, op: UnOp, opnd: Expr) -> ExprData {
        ExprData::Un(ExprUnType {
            id,
            span,
            green,
            op,
            opnd,
        })
    }

    pub fn create_bin(id: NodeId, span: Span, op: BinOp, lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Bin(ExprBinType {
            id,
            span,

            op,
            initializer: false,
            lhs,
            rhs,
        })
    }

    pub fn create_conv(id: NodeId, span: Span, object: Expr, data_type: Type) -> ExprData {
        ExprData::Conv(ExprConvType {
            id,
            span,

            object,
            data_type,
        })
    }

    pub fn create_literal_char(
        id: NodeId,
        span: Span,
        green: GreenNode,
        full_value: String,
    ) -> ExprData {
        ExprData::LiteralChar(ExprLiteralCharType {
            id,
            span,
            green,
            value: full_value,
        })
    }

    pub fn create_literal_int(id: NodeId, span: Span, green: GreenNode, value: String) -> ExprData {
        ExprData::LiteralInt(ExprLiteralIntType {
            id,
            span,
            green,
            value,
        })
    }

    pub fn create_literal_float(
        id: NodeId,
        span: Span,
        green: GreenNode,
        value: String,
    ) -> ExprData {
        ExprData::LiteralFloat(ExprLiteralFloatType {
            id,
            span,
            green,
            value,
        })
    }

    pub fn create_literal_str(id: NodeId, span: Span, green: GreenNode, value: String) -> ExprData {
        ExprData::LiteralStr(ExprLiteralStrType {
            id,
            span,
            green,
            value,
        })
    }

    pub fn create_template(id: NodeId, span: Span, green: GreenNode, parts: Vec<Expr>) -> ExprData {
        ExprData::Template(ExprTemplateType {
            id,
            span,
            green,
            parts,
        })
    }

    pub fn create_literal_bool(id: NodeId, span: Span, value: bool) -> ExprData {
        ExprData::LiteralBool(ExprLiteralBoolType { id, span, value })
    }

    pub fn create_this(id: NodeId, span: Span, green: GreenNode) -> ExprData {
        ExprData::This(ExprSelfType { id, span, green })
    }

    pub fn create_ident(id: NodeId, span: Span, green: GreenNode, name: String) -> ExprData {
        ExprData::Sym(ExprSymType {
            id,
            span,
            green,
            name,
        })
    }

    pub fn create_paren(id: NodeId, span: Span, green: GreenNode, expr: Expr) -> ExprData {
        ExprData::Paren(ExprParenType {
            id,
            span,
            green,
            expr,
        })
    }

    pub fn create_call(id: NodeId, span: Span, callee: Expr, args: Vec<Expr>) -> ExprData {
        ExprData::Call(ExprCallType {
            id,
            span,

            callee,
            args,
        })
    }

    pub fn create_type_param(
        id: NodeId,
        span: Span,
        op_span: Span,
        callee: Expr,
        args: Vec<Type>,
    ) -> ExprData {
        ExprData::TypeParam(ExprTypeParamType {
            id,
            span,
            op_span,

            callee,
            args,
        })
    }

    pub fn create_path(id: NodeId, span: Span, op_span: Span, lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Path(ExprPathType {
            id,
            span,
            op_span,
            lhs,
            rhs,
        })
    }

    pub fn create_dot(id: NodeId, span: Span, op_span: Span, lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Dot(ExprDotType {
            id,
            span,
            op_span,

            lhs,
            rhs,
        })
    }

    pub fn create_lambda(function: Arc<Function>) -> ExprData {
        ExprData::Lambda(function)
    }

    pub fn create_tuple(id: NodeId, span: Span, green: GreenNode, values: Vec<Expr>) -> ExprData {
        ExprData::Tuple(ExprTupleType {
            id,
            span,
            green,
            values,
        })
    }

    pub fn to_for(&self) -> Option<&ExprForType> {
        match *self {
            ExprData::For(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_for(&self) -> bool {
        match *self {
            ExprData::For(_) => true,
            _ => false,
        }
    }

    pub fn to_while(&self) -> Option<&ExprWhileType> {
        match *self {
            ExprData::While(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_while(&self) -> bool {
        match *self {
            ExprData::While(_) => true,
            _ => false,
        }
    }

    pub fn to_un(&self) -> Option<&ExprUnType> {
        match *self {
            ExprData::Un(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_un(&self) -> bool {
        match *self {
            ExprData::Un(_) => true,
            _ => false,
        }
    }

    pub fn to_bin(&self) -> Option<&ExprBinType> {
        match *self {
            ExprData::Bin(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_bin(&self) -> bool {
        match *self {
            ExprData::Bin(_) => true,
            _ => false,
        }
    }

    pub fn to_paren(&self) -> Option<&ExprParenType> {
        match *self {
            ExprData::Paren(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_paren(&self) -> bool {
        match *self {
            ExprData::Paren(_) => true,
            _ => false,
        }
    }

    pub fn to_ident(&self) -> Option<&ExprSymType> {
        match *self {
            ExprData::Sym(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_ident(&self) -> bool {
        match *self {
            ExprData::Sym(_) => true,
            _ => false,
        }
    }

    pub fn to_call(&self) -> Option<&ExprCallType> {
        match *self {
            ExprData::Call(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_call(&self) -> bool {
        match *self {
            ExprData::Call(_) => true,
            _ => false,
        }
    }

    pub fn to_path(&self) -> Option<&ExprPathType> {
        match *self {
            ExprData::Path(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_path(&self) -> bool {
        match *self {
            ExprData::Path(_) => true,
            _ => false,
        }
    }

    pub fn to_type_param(&self) -> Option<&ExprTypeParamType> {
        match *self {
            ExprData::TypeParam(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match *self {
            ExprData::TypeParam(_) => true,
            _ => false,
        }
    }

    pub fn to_literal_char(&self) -> Option<&ExprLiteralCharType> {
        match *self {
            ExprData::LiteralChar(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_literal_char(&self) -> bool {
        match *self {
            ExprData::LiteralChar(_) => true,
            _ => false,
        }
    }

    pub fn to_literal_int(&self) -> Option<&ExprLiteralIntType> {
        match *self {
            ExprData::LiteralInt(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_literal_int(&self) -> bool {
        match *self {
            ExprData::LiteralInt(_) => true,
            _ => false,
        }
    }

    pub fn to_template(&self) -> Option<&ExprTemplateType> {
        match *self {
            ExprData::Template(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_template(&self) -> bool {
        match *self {
            ExprData::Template(_) => true,
            _ => false,
        }
    }

    pub fn to_literal_float(&self) -> Option<&ExprLiteralFloatType> {
        match *self {
            ExprData::LiteralFloat(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_literal_float(&self) -> bool {
        match *self {
            ExprData::LiteralFloat(_) => true,
            _ => false,
        }
    }

    pub fn to_literal_str(&self) -> Option<&ExprLiteralStrType> {
        match *self {
            ExprData::LiteralStr(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_literal_str(&self) -> bool {
        match *self {
            ExprData::LiteralStr(_) => true,
            _ => false,
        }
    }

    pub fn to_literal_bool(&self) -> Option<&ExprLiteralBoolType> {
        match *self {
            ExprData::LiteralBool(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_literal_bool(&self) -> bool {
        match *self {
            ExprData::LiteralBool(_) => true,
            _ => false,
        }
    }

    pub fn is_literal_true(&self) -> bool {
        match *self {
            ExprData::LiteralBool(ref lit) if lit.value => true,
            _ => false,
        }
    }

    pub fn to_dot(&self) -> Option<&ExprDotType> {
        match *self {
            ExprData::Dot(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_dot(&self) -> bool {
        match *self {
            ExprData::Dot(_) => true,
            _ => false,
        }
    }

    pub fn is_this(&self) -> bool {
        match *self {
            ExprData::This(_) => true,
            _ => false,
        }
    }

    pub fn to_conv(&self) -> Option<&ExprConvType> {
        match *self {
            ExprData::Conv(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_conv(&self) -> bool {
        match *self {
            ExprData::Conv(_) => true,
            _ => false,
        }
    }

    pub fn to_lambda(&self) -> Option<Arc<Function>> {
        match *self {
            ExprData::Lambda(ref val) => Some(val.clone()),
            _ => None,
        }
    }

    pub fn is_lambda(&self) -> bool {
        match self {
            &ExprData::Lambda(_) => true,
            _ => false,
        }
    }

    pub fn to_tuple(&self) -> Option<&ExprTupleType> {
        match *self {
            ExprData::Tuple(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match *self {
            ExprData::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn to_block(&self) -> Option<&ExprBlockType> {
        match *self {
            ExprData::Block(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_block(&self) -> bool {
        match self {
            &ExprData::Block(_) => true,
            _ => false,
        }
    }

    pub fn to_if(&self) -> Option<&ExprIfType> {
        match *self {
            ExprData::If(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_if(&self) -> bool {
        match *self {
            ExprData::If(_) => true,
            _ => false,
        }
    }

    pub fn to_break(&self) -> Option<&ExprBreakType> {
        match *self {
            ExprData::Break(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_break(&self) -> bool {
        match *self {
            ExprData::Break(_) => true,
            _ => false,
        }
    }

    pub fn to_continue(&self) -> Option<&ExprContinueType> {
        match *self {
            ExprData::Continue(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_continue(&self) -> bool {
        match *self {
            ExprData::Continue(_) => true,
            _ => false,
        }
    }

    pub fn to_return(&self) -> Option<&ExprReturnType> {
        match *self {
            ExprData::Return(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_return(&self) -> bool {
        match *self {
            ExprData::Return(_) => true,
            _ => false,
        }
    }

    pub fn needs_semicolon(&self) -> bool {
        match self {
            &ExprData::Block(_) => false,
            &ExprData::If(_) => false,
            &ExprData::Match(_) => false,
            &ExprData::For(_) => false,
            &ExprData::While(_) => false,
            _ => true,
        }
    }

    pub fn span(&self) -> Span {
        match *self {
            ExprData::Un(ref val) => val.span,
            ExprData::Bin(ref val) => val.span,
            ExprData::LiteralChar(ref val) => val.span,
            ExprData::LiteralInt(ref val) => val.span,
            ExprData::LiteralFloat(ref val) => val.span,
            ExprData::LiteralStr(ref val) => val.span,
            ExprData::Template(ref val) => val.span,
            ExprData::LiteralBool(ref val) => val.span,
            ExprData::Sym(ref val) => val.span,
            ExprData::Call(ref val) => val.span,
            ExprData::TypeParam(ref val) => val.span,
            ExprData::Path(ref val) => val.span,
            ExprData::Dot(ref val) => val.span,
            ExprData::This(ref val) => val.span,
            ExprData::Conv(ref val) => val.span,
            ExprData::Lambda(ref val) => val.span,
            ExprData::Block(ref val) => val.span,
            ExprData::If(ref val) => val.span,
            ExprData::Tuple(ref val) => val.span,
            ExprData::Paren(ref val) => val.span,
            ExprData::Match(ref val) => val.span,
            ExprData::For(ref val) => val.span,
            ExprData::While(ref val) => val.span,
            ExprData::Break(ref val) => val.span,
            ExprData::Continue(ref val) => val.span,
            ExprData::Return(ref val) => val.span,
            ExprData::Error { span, .. } => span,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            ExprData::Un(ref val) => val.id,
            ExprData::Bin(ref val) => val.id,
            ExprData::LiteralChar(ref val) => val.id,
            ExprData::LiteralInt(ref val) => val.id,
            ExprData::LiteralFloat(ref val) => val.id,
            ExprData::LiteralStr(ref val) => val.id,
            ExprData::Template(ref val) => val.id,
            ExprData::LiteralBool(ref val) => val.id,
            ExprData::Sym(ref val) => val.id,
            ExprData::Call(ref val) => val.id,
            ExprData::TypeParam(ref val) => val.id,
            ExprData::Path(ref val) => val.id,
            ExprData::Dot(ref val) => val.id,
            ExprData::This(ref val) => val.id,
            ExprData::Conv(ref val) => val.id,
            ExprData::Lambda(ref val) => val.id,
            ExprData::Block(ref val) => val.id,
            ExprData::If(ref val) => val.id,
            ExprData::Tuple(ref val) => val.id,
            ExprData::Paren(ref val) => val.id,
            ExprData::Match(ref val) => val.id,
            ExprData::For(ref val) => val.id,
            ExprData::While(ref val) => val.id,
            ExprData::Break(ref val) => val.id,
            ExprData::Continue(ref val) => val.id,
            ExprData::Return(ref val) => val.id,
            ExprData::Error { id, .. } => id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprIfType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub cond: Expr,
    pub then_block: Expr,
    pub else_block: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprTupleType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub values: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprConvType {
    pub id: NodeId,
    pub span: Span,

    pub object: Expr,
    pub data_type: Type,
}

#[derive(Clone, Debug)]
pub struct ExprUnType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub op: UnOp,
    pub opnd: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprBinType {
    pub id: NodeId,
    pub span: Span,

    pub op: BinOp,
    pub initializer: bool,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprLiteralCharType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLiteralIntType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLiteralFloatType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLiteralStrType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprTemplateType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub parts: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprLiteralBoolType {
    pub id: NodeId,
    pub span: Span,

    pub value: bool,
}

#[derive(Clone, Debug)]
pub struct ExprBlockType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub SpecialForms: Vec<SpecialForm>,
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprSelfType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

#[derive(Clone, Debug)]
pub struct ExprSymType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct ExprCallType {
    pub id: NodeId,
    pub span: Span,

    pub callee: Expr,
    pub args: Vec<Expr>,
}

impl ExprCallType {
    pub fn object(&self) -> Option<&ExprData> {
        if let Some(type_param) = self.callee.to_type_param() {
            if let Some(dot) = type_param.callee.to_dot() {
                Some(&dot.lhs)
            } else {
                None
            }
        } else if let Some(dot) = self.callee.to_dot() {
            Some(&dot.lhs)
        } else {
            None
        }
    }

    pub fn object_or_callee(&self) -> &ExprData {
        self.object().unwrap_or(&self.callee)
    }
}

#[derive(Clone, Debug)]
pub struct ExprParenType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprMatchType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub expr: Expr,
    pub cases: Vec<MatchCaseType>,
}

#[derive(Clone, Debug)]
pub struct MatchCaseType {
    pub id: NodeId,
    pub span: Span,

    pub patterns: Vec<MatchPattern>,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct MatchPattern {
    pub id: NodeId,
    pub span: Span,
    pub data: MatchPatternData,
}

#[derive(Clone, Debug)]
pub enum MatchPatternData {
    Underscore,
    Sym(MatchPatternSym),
}

#[derive(Clone, Debug)]
pub struct MatchPatternSym {
    pub path: Path,
    pub params: Option<Vec<MatchPatternParam>>,
}

#[derive(Clone, Debug)]
pub struct MatchPatternParam {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Sym>,
    pub mutable: bool,
}

pub type Path = Arc<PathData>;

#[derive(Clone, Debug)]
pub struct PathData {
    pub id: NodeId,
    pub span: Span,
    pub names: Vec<Sym>,
}

#[derive(Clone, Debug)]
pub struct ExprTypeParamType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub callee: Expr,
    pub args: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct ExprPathType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprDotType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub lhs: Expr,
    pub rhs: Expr,
}

fn find_token(node: &GreenNode, token: TokenKind) -> Option<GreenToken> {
    node.children
        .iter()
        .find(|c| c.kind() == token)
        .map(|t| t.to_token().expect("should be token"))
}
