use std::fmt::Debug;
use std::fmt::Display;

use crate::rust::types::any::Any;
use crate::rust::types::evaluable::Evaluable;

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

impl Any for Symbol {}
impl Evaluable for Symbol {}
