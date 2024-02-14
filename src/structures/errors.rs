/* structures/errors.rs */

use std::fmt::{self, Debug};

use pest::error::Error as PestError;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use crate::parser::Rule;

#[derive(Debug)]
pub enum Error {
    // wrapped errors
    ParseError(Box<PestError<Rule>>),
    // self defined errors
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ParseError(err) => write!(f, "ParseError: {}", err),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            Error::ParseError(_) => None,
        }
    }
}
