pub mod errors;
pub mod ast;
pub mod arena;
pub mod objects;
pub mod definitions;

use errors::Error;

// use everywhere
pub type Result<T> = std::result::Result<T, Error>;
