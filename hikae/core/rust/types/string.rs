use std::any::TypeId;
pub use std::string::String;

use crate::rust::types::any::Any;

impl Any for String {
    fn type_id(&self) -> std::any::TypeId {
        TypeId::of::<String>()
    }
}
