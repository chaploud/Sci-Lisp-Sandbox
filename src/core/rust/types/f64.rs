use crate::rust::types::any::Any;
use crate::rust::types::number::Number;

impl Any for f64 {
    fn type_name(&self) -> &'static str {
        "f64"
    }
}
impl Number for f64 {}
