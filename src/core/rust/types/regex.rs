use regex::Regex;

use crate::rust::types::any::Any;

impl Any for Regex {
    fn type_name(&self) -> &'static str {
        "regex"
    }
}
