/* parse.rs */

use pest::iterators::Pair;
use pest::Parser as ParserTrait;
use pest_derive::Parser;

use crate::structures::Result;
use crate::structures::errors::Error;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct Parser;
