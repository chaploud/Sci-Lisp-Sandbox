use std::fmt::Debug;
use std::fmt::Display;
use std::sync::Arc;
use std::vec::Vec;

use crate::rust::types::any::Any;
use crate::rust::types::collection::Collection;
use crate::rust::types::iterable::Iterable;

pub struct Vector {
    pub value: Vec<Arc<dyn Any>>,
}

impl Debug for Vector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::from("[");
        for (i, v) in self.value.iter().enumerate() {
            if i == 0 {
                result.push_str(&format!("{:?}", v));
            } else {
                result.push_str(&format!(", {:?}", v));
            }
        }
        result.push(']');
        write!(f, "{}", result)
    }
}

impl Display for Vector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::from("[");
        for (i, v) in self.value.iter().enumerate() {
            if i == 0 {
                result.push_str(&format!("{}", v));
            } else {
                result.push_str(&format!(", {}", v));
            }
        }
        result.push(']');
        write!(f, "{}", result)
    }
}

impl Any for Vector {
    fn type_name(&self) -> &'static str {
        "vector"
    }
}

impl Iterable for Vector {}

impl Collection for Vector {
    fn len(&self) -> usize {
        self.value.len()
    }
    fn is_empty(&self) -> bool {
        self.value.is_empty()
    }
}
