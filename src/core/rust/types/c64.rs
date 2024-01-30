use num::complex::Complex64;

use crate::rust::types::any::Any;
use crate::rust::types::number::Number;

#[allow(non_camel_case_types)]
pub type c64 = Complex64;

impl Any for c64 {
    fn type_name(&self) -> &'static str {
        "c64"
    }
}
impl Number for c64 {}
