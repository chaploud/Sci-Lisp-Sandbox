use std::sync::Arc;

use crate::rust::types::any::Any;

pub trait Iterable: Any + Iterator<Item = Arc<dyn Any>> {
    fn type_name(&self) -> &'static str {
        "iterable"
    }
}
