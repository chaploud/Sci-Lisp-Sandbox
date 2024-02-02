use std::sync::Arc;

use crate::rust::types::any::Any;

pub trait Function {
    fn invoke(&self, args: Vec<Arc<dyn Any>>) -> Any;
}
