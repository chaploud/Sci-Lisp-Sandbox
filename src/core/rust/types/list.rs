use std::fmt::Debug;
use std::fmt::Display;
use std::sync::Arc;

use crate::rust::types::any::Any;
use crate::rust::types::collection::Collection;
use crate::rust::types::evaluable::Evaluable;
use crate::rust::types::iterable::Iterable;

pub struct List {
    pub value: Vec<Arc<dyn Any>>,
}

impl Debug for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::from("(");
        for (i, v) in self.value.iter().enumerate() {
            if i == 0 {
                result.push_str(&format!("{:?}", v));
            } else {
                result.push_str(&format!(" {:?}", v));
            }
        }
        result.push(')');
        write!(f, "{}", result)
    }
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::from("(");
        for (i, v) in self.value.iter().enumerate() {
            if i == 0 {
                result.push_str(&format!("{}", v));
            } else {
                result.push_str(&format!(" {}", v));
            }
        }
        result.push(')');
        write!(f, "{}", result)
    }
}

impl Any for List {}

impl Iterable for List {}

impl Collection for List {}

impl Evaluable for List {}
