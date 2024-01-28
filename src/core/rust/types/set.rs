use std::fmt::Debug;
use std::fmt::Display;
use std::sync::Arc;

use crate::rust::types::any::Any;
use crate::rust::types::collection::Collection;
use crate::rust::types::iterable::Iterable;
use indexmap::IndexSet;

pub struct Set {
    pub value: IndexSet<Arc<dyn Any>>,
}

impl Debug for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::new();
        for (n, val) in self.value.iter().enumerate() {
            if n > 0 {
                result += ", ";
            }
            result += format!("{:?}", val).as_str();
        }
        write!(f, "#{{{}}}", result)
    }
}

impl Display for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::new();
        for (n, val) in self.value.iter().enumerate() {
            if n > 0 {
                result += ", ";
            }
            result += format!("{:?}", val).as_str();
        }
        write!(f, "#{{{}}}", result)
    }
}

impl Any for Set {}

impl Iterable for Set {}

impl Collection for Set {}
