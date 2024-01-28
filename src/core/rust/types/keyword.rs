use std::fmt::Debug;
use std::fmt::Display;

use crate::rust::types::any::Any;

pub struct Keyword {
    pub name: String,
}

impl Debug for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{}", self.name)
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{}", self.name)
    }
}

impl Any for Keyword {}
