use std::any::TypeId;

use crate::rust::types::any::Any;
use crate::rust::types::number::Number;

impl Any for f64 {
    fn type_id(&self) -> std::any::TypeId {
        TypeId::of::<f64>()
    }
}
impl Number for f64 {}
