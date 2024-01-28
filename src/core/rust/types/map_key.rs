use std::fmt::Display;

use crate::rust::types::any::Any;
use crate::rust::types::i64;
use crate::rust::types::keyword::Keyword;
use crate::rust::types::string::String;

#[derive(Debug)]
#[allow(dead_code)]
pub enum MapKey {
    I64(i64),
    String(String),
    Keyword(Keyword),
}

impl Display for MapKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MapKey::I64(i) => write!(f, "{}", i),
            MapKey::String(s) => write!(f, "{}", s),
            MapKey::Keyword(k) => write!(f, "{}", k),
        }
    }
}

impl Any for MapKey {}
