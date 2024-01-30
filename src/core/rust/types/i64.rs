use std::any::TypeId;

use crate::rust::types::any::Any;
use crate::rust::types::number::Number;

impl Any for i64 {
    fn type_id(&self) -> std::any::TypeId {
        TypeId::of::<i64>()
    }
}
impl Number for i64 {}
