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

#[derive(Clone, Debug)]
pub struct Keyword {
    pub name: Str,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub name: Str,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub name: Str,
    pub generics: Option<List<Type>>,
}

pub trait Iterable<T> {
    type Item;
    fn iter(&self) -> std::vec::IntoIter<Self::Item>;
}

pub trait Collection<T> {
    fn len(&self) -> usize;
}

// Function, Macro
