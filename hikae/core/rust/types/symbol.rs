use std::any::TypeId;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use crate::rust::types::any::Any;

#[derive(Clone)]
pub struct Symbol {
    pub name: String,
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{}", self.name)
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{}", self.name)
    }
}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Symbol {}

impl Any for Symbol {
    fn type_id(&self) -> TypeId {
        TypeId::of::<Symbol>()
    }
}
