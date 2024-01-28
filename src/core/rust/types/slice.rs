use std::fmt::Debug;
use std::fmt::Display;

use crate::rust::types::any::Any;

pub struct Slice {
    pub start: i64,
    pub end: i64,
    pub step: i64,
}

impl Debug for Slice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}|{}|{}", self.start, self.end, self.step)
    }
}

impl Display for Slice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}|{}|{}", self.start, self.end, self.step)
    }
}

impl Any for Slice {}
