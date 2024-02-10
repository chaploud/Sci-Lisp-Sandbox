/* structures/errors.rs */

use std::fmt::{self, Debug};

use pest::error::Error as PestError;

use crate::structures::objects::Str;
use crate::parser::Rule;

#[derive(Debug)]
pub enum Error {
    // wrapped errors
    Parse(PestError<Rule>),
    // self defined errors
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Parse(err) => write!(f, "parse error: {}", err),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            Error::Parse(_) => None,
        }
    }
}
