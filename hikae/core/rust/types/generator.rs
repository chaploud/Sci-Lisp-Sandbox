use crate::rust::types::any::Any;
use crate::rust::types::iterable::Iterable;

pub trait Generator<T: Any>: Any + Iterable {
    fn type_name(&self) -> &'static str {
        "generator"
    }
}
