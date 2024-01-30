use crate::rust::types::any::Any;

pub trait Number: Any {
    fn type_name(&self) -> &'static str {
        "number"
    }
}
