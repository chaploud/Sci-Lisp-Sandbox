pub mod errors;
pub mod ast;
pub mod arena;
pub mod types;
pub mod definitions;
pub mod span;
pub mod tokens;
pub mod objects;

use errors::Error;

// use everywhere
pub type Result<T> = std::result::Result<T, Error>;
