use std::fmt::Debug;
use std::fmt::Display;

use crate::rust::types::any::Any;
pub struct Nil;

impl Debug for Nil {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "nil")
    }
}

impl Display for Nil {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "nil")
    }
}

impl Any for Nil {}
