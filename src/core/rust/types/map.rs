use std::fmt::Debug;
use std::fmt::Display;
use std::sync::Arc;

use indexmap::IndexMap;

use super::map_key::MapKey;
use crate::rust::types::any::Any;
use crate::rust::types::collection::Collection;
use crate::rust::types::iterable::Iterable;

pub struct Map {
    pub value: IndexMap<MapKey, Arc<dyn Any>>,
}

impl Debug for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // write the map as a list of key-value pairs
        let mut result = String::new();
        for (n, (key, val)) in self.value.iter().enumerate() {
            if n > 0 {
                result += ", ";
            }
            result += format!("{:?} {:?}", key, val).as_str();
        }
        write!(f, "{{{}}}", result)
    }
}

impl Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // write the map as a list of key-value pairs
        let mut result = String::new();
        for (n, (key, val)) in self.value.iter().enumerate() {
            if n > 0 {
                result += ", ";
            }
            result += format!("{:?} {:?}", key, val).as_str();
        }
        write!(f, "{{{}}}", result)
    }
}

impl Any for Map {}

impl Iterable for Map {}

impl Collection for Map {}
