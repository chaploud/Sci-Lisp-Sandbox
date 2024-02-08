/* structures/errors.rs */

use std::fmt::{self, Debug};

#[derive(Debug)]
pub enum Error {
    // wrapped errors
    // self defined errors
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            _ => write!(f, "error")
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            _ => None,
        }
    }
}
