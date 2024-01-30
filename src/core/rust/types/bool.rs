use crate::rust::types::any::Any;

impl Any for bool {
    fn type_name(&self) -> &'static str {
        "bool"
    }
}
