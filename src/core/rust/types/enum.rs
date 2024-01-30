use std::fmt::Debug;
use std::fmt::Display;

use indexmap::IndexSet;

use crate::rust::types::any::Any;
use crate::rust::types::symbol::Symbol;

#[derive(Debug)]
pub struct Enum {
    pub name: Symbol,
    pub member: IndexSet<Symbol>,
}

impl Enum {
    #[allow(dead_code)]
    pub fn new(name: Symbol, member: Vec<Symbol>) -> Self {
        let member: IndexSet<Symbol> = member.into_iter().collect();
        Self { name, member }
    }
}

impl Display for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::from("<enum>: [");
        for (i, v) in self.member.iter().enumerate() {
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

impl Any for Enum {
    fn type_name(&self) -> &'static str {
        "enum"
    }
}

impl Enum {
    #[allow(dead_code)]
    pub fn get_all_member(&self) -> Vec<Symbol> {
        self.member.iter().cloned().collect()
    }
    #[allow(dead_code)]
    pub fn get(&self, name: &Symbol) -> Option<Symbol> {
        if self.member.contains(name) {
            Some(name.clone())
        } else {
            None
        }
    }
}
