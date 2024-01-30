use crate::rust::types::any::Any;

pub trait Iterable: Any {
    fn type_name(&self) -> &'static str {
        "iterable"
    }
}
