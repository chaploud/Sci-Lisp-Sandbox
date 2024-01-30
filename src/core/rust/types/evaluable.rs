use crate::rust::types::any::Any;

pub trait Evaluable: Any {
    fn type_name(&self) -> &'static str {
        "evaluable"
    }
}
