use std::fmt;
use std::slice::Iter;
use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use num::complex::Complex64;
use regex::Regex;

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
    pub expressions: Vec<Expr>,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash)]
pub struct NodeId(pub usize);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub type Expr = Arc<ExprData>;

#[derive(Clone, Debug)]
pub enum ExprData {
    String(StringData),
    Regex(RegexData),
    Bool(BoolData),
    Nil(NilData),
    I64(I64Data),
    F64(F64Data),
    C64(C64Data),
    Keyword(KeywordData),
    Symbol(SymbolData),
    TypeAnnotation(Arc<TypeAnnotationData>),
    List(Arc<ListData>),
    Vector(Arc<VectorData>),
    Map(Arc<MapData>),
    Set(Arc<SetData>),
    Slice(Arc<SliceData>),
    Underscore(Arc<UnderscoreData>),
    Quote(Arc<QuoteData>),
    SyntaxQuote(Arc<SyntaxQuoteData>),
    Unquote(Arc<UnquoteData>),
    UnquoteSplicing(Arc<UnquoteSplicingData>),
    Splicing(Arc<SplicingData>),
    Dot(Arc<DotData>),
    Slash(Arc<SlashData>),
    Error { id: NodeId, span: Span },
}

// TODO: Path  #str/hoge
// TODO: DotPath str.hoge

#[derive(Clone, Debug)]
pub struct StringData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub value: std::string::String,
}

#[derive(Clone, Debug)]
pub struct RegexData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub value: regex::Regex,
}

#[derive(Clone, Debug)]
pub struct BoolData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub value: bool,
}

#[derive(Clone, Debug)]
pub struct NilData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

#[derive(Clone, Debug)]
pub struct I64Data {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub value: i64,
}

#[derive(Clone, Debug)]
pub struct F64Data {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub value: f64,
}

#[derive(Clone, Debug)]
pub struct C64Data {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub value: Complex64,
}

#[derive(Clone, Debug)]
pub struct KeywordData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub value: std::string::String,
}

#[derive(Clone, Debug)]
pub struct SymbolData {
    // not need id and green
    pub span: Span,
    pub name: std::string::String,
}

pub type Sym = Arc<SymbolData>;

#[derive(Clone, Debug)]
pub struct TypeAnnotationData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub data_type: Type,
}

#[derive(Clone, Debug)]
pub struct ListData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expressions: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct VectorData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expressions: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct MapData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expressions: IndexMap<Expr, Expr>,
}

#[derive(Clone, Debug)]
pub struct SetData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expressions: IndexSet<Expr>,
}

#[derive(Clone, Debug)]
pub struct SliceData {
    pub id: NodeId,
    pub span: Span,
    pub expressions: (Expr, Expr, Expr),
}

#[derive(Clone, Debug)]
pub struct UnderscoreData {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct QuoteData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct SyntaxQuoteData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct UnquoteData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct UnquoteSplicingData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct SplicingData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct DotData {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,
    pub lhs: Option<Expr>,
    pub rhs: Sym,
}

#[derive(Clone, Debug)]
pub struct SlashData {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,
    pub lhs: Expr,
    pub rhs: Sym,
}

impl ExprData {
    pub fn span(&self) -> Span {
        match self {
            ExprData::String(ref node) => node.span,
            ExprData::Regex(ref node) => node.span,
            ExprData::Bool(ref node) => node.span,
            ExprData::Nil(ref node) => node.span,
            ExprData::I64(ref node) => node.span,
            ExprData::F64(ref node) => node.span,
            ExprData::C64(ref node) => node.span,
            ExprData::Keyword(ref node) => node.span,
            ExprData::Symbol(ref node) => node.span,
            ExprData::TypeAnnotation(ref node) => node.span,
            ExprData::List(ref node) => node.span,
            ExprData::Vector(ref node) => node.span,
            ExprData::Map(ref node) => node.span,
            ExprData::Set(ref node) => node.span,
            ExprData::Slice(ref node) => node.span,
            ExprData::Underscore(ref node) => node.span,
            ExprData::Quote(ref node) => node.span,
            ExprData::SyntaxQuote(ref node) => node.span,
            ExprData::Unquote(ref node) => node.span,
            ExprData::UnquoteSplicing(ref node) => node.span,
            ExprData::Splicing(ref node) => node.span,
            ExprData::Dot(ref node) => node.span,
            ExprData::Slash(ref node) => node.span,
            ExprData::
            ExprData::Error { span, .. } => span.clone(),
        }
    }
}

pub type Type = Arc<TypeData>;

#[derive(Clone, Debug)]
pub enum TypeData {
    Basic(TypeBasic),
    Lambda(TypeLambda),
    Generic(TypeGeneric),
    Array(TypeArray),
    Error { id: NodeId, span: Span },
}

// TODO: pathがいるかも

#[derive(Clone, Debug)]
pub struct TypeBasic {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeLambda {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub params: Vec<Type>,
    pub ret: Option<Type>,
}


#[derive(Clone, Debug)]
pub struct TypeGeneric {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeArray {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub params: Vec<Type>,
    pub shape: Vec<usize>
}

impl TypeData {
    pub fn create_basic(id: NodeId, span: Span, green: GreenNode, params: Vec<Type>) -> TypeData {
        TypeData::Basic(TypeBasic {
            id,
            span,
            green,
            params,
        })
    }

    pub fn create_generic(id: NodeId, span: Span, green: GreenNode, params: Vec<Type>) -> TypeData {
        TypeData::Generic(TypeGeneric {
            id,
            span,
            green,
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
        TypeData::Lambda(TypeLambda {
            id,
            span,
            green,
            params,
            ret,
        })
    }

    pub fn to_basic(&self) -> Option<&TypeBasic> {
        match *self {
            TypeData::Basic(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_function(&self) -> Option<&TypeLambda> {
        match *self {
            TypeData::Lambda(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn span(&self) -> Span {
        match *self {
            TypeData::Basic(ref val) => val.span,
            TypeData::Lambda(ref val) => val.span,
            TypeData::Generic(ref val) => val.span,
            TypeData::Array(ref val) => val.span,
            TypeData::Error { span, .. } => span,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            TypeData::Basic(ref val) => val.id,
            TypeData::Lambda(ref val) => val.id,
            TypeData::Generic(ref val) => val.id,
            TypeData::Array(ref val) => val.id,
            TypeData::Error { id, .. } => id,
        }
    }
}

pub type Elem = Arc<ElemData>;

#[derive(Clone, Debug)]
pub enum ElemData {
    Def(Arc<DefData>),
    Const(Arc<ConstData>),
    Let(Arc<LetData>),
    Scope(Arc<ScopeData>),
    Sete(Arc<SeteData>),
    Defn(Arc<DefnData>),
    Fn(Arc<FnData>),
    Function(Arc<Function>),
    Return(Arc<ReturnData>),
    If(Arc<IfData>),
    When(Arc<WhenData>),
    Cond(Arc<CondData>),
    Switch(Arc<SwitchData>),
    Do(Arc<DoData>),
    For(Arc<ForData>),
    While(Arc<WhileData>),
    Break(Arc<BreakData>),
    Continue(Arc<ContinueData>),
    Try(Arc<TryData>),
    Throw(Arc<ThrowData>),
    Catch(Arc<CatchData>),
    Finally(Arc<FinallyData>),
    Enum(Arc<Enum>),
    Struct(Arc<Struct>),
    Macro(Arc<Macro>),
    Import(Arc<Import>),
    Export(Arc<Export>),
    Error { id: NodeId, span: Span },
}

// TODO: デストラクチャリングの高速実行

#[derive(Clone, Debug)]
pub struct DefData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub name: Option<Sym>,
    pub mutable: bool,
    pub data_type: Type,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct ConstData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub name: Sym,
    pub mutable: bool,
    pub data_type: Type,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct LetData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub data_type: Option<Type>,
    pub value: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ScopeData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub : Vec<Elem>,
}
