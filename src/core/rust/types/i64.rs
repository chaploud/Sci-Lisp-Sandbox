use crate::rust::types::any::Any;
use crate::rust::types::number::Number;

impl Any for i64 {
    fn type_name(&self) -> &'static str {
        "i64"
    }
}
impl Number for i64 {}
