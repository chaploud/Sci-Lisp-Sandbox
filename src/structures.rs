pub mod errors;
pub mod ast;
pub mod arena;
pub mod objects;
pub mod definitions;
pub mod span;
pub mod green;
pub mod tokens;

use errors::Error;

// use everywhere
pub type Result<T> = std::result::Result<T, Error>;
