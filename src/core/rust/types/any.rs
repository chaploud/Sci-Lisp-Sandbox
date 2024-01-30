use std::fmt::Debug;
use std::fmt::Display;

pub trait Any: Debug + Display {
    fn type_name(&self) -> &'static str {
        "any"
    }
}
