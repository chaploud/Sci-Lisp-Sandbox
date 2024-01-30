use std::any::TypeId;

use num::complex::Complex64;

use crate::rust::types::any::Any;
use crate::rust::types::number::Number;

#[allow(non_camel_case_types)]
pub type c64 = Complex64;

impl Any for c64 {
    fn type_id(&self) -> std::any::TypeId {
        TypeId::of::<c64>()
    }
}
impl Number for c64 {}
