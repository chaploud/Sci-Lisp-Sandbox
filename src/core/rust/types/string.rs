pub use std::string::String;

use crate::rust::types::any::Any;

impl Any for String {
    fn type_name(&self) -> &'static str {
        "string"
    }
}
