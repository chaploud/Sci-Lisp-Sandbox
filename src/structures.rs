pub mod types;
pub mod errors;
pub mod ast;
pub mod arena;

use errors::Error;

// use everywhere
pub type Result<T> = std::result::Result<T, Error>;
