// Sci-Lisp types
type Bool = bool;
type I64 = i64;
type C64 = num::complex::Complex64;
type F64 = f64;
type Str = std::string::String;
type Regex = regex::Regex;
type List<T> = std::vec::Vec<T>;
type Vector<T> = std::collections::VecDeque<T>;
type Map<K, V> = indexmap::IndexMap<K, V>;
type Set<T> = indexmap::IndexSet<T>;
type Array<T, D> = ndarray::Array<T, D>;
type Datetime = chrono::DateTime<chrono::Utc>;
type Duraton = chrono::Duration;
type OptionType<T> = Option<T>;

pub struct Keyword {
    pub name: Str,
}

pub struct Symbol {
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
