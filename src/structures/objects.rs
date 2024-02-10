use crate::structures::span::Span;
use crate::structures::ast::NodeId;

// Sci-Lisp types
pub type Bool = bool;
pub type I64 = i64;
pub type C64 = num::complex::Complex64;
pub type F64 = f64;
pub type Str = std::string::String;
pub type Regex = regex::Regex;
pub type List<T> = std::vec::Vec<T>;
pub type Vector<T> = std::collections::VecDeque<T>;
pub type Map<K, V> = indexmap::IndexMap<K, V>;
pub type Set<T> = indexmap::IndexSet<T>;
pub type Array<T, D> = ndarray::Array<T, D>;
pub type Datetime = chrono::DateTime<chrono::Utc>;
pub type Duraton = chrono::Duration;
pub type OptionType<T> = Option<T>;

#[derive(Clone, Debug)]
pub struct Keyword {
    pub id: NodeId,
    pub name: Str,
}

pub struct Symbol {
    pub span: Span,
    pub name: Str,
}

pub struct Type {
    pub name: Str,
    pub generics: List<Type>,
}

pub struct Macro {
    pub name: Str,
    pub body: Str,
}

pub struct Function {
    pub name: Str,
    pub args: List<Symbol>,
    pub body: Str,
}

pub struct Generator<T> {
    pub name: Str,
    pub args: List<T>,
    pub body: Str,
}

pub trait Iterable<T> {
    type Item;
    fn iter(&self) -> std::vec::IntoIter<Self::Item>;
}

pub trait Collection<T> {
    fn len(&self) -> usize;
}

pub enum MapKey {
    Keyword(Keyword),
    Str(Str),
    I64(I64),
}
