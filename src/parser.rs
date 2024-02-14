/* parse.rs */

use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser as ParserTrait;
use pest_derive::Parser;

use crate::structures::ast;
use crate::structures::Result;
use crate::structures::errors::Error::*;
use crate::parser::parse2pairs::pest_parse_to_pairs;
use crate::parser::pairs2ast::pairs_to_ast;

mod parse2pairs;
mod pairs2ast;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct Parser;

pub fn parse(code: &str) -> Result<()> {
    let pairs = pest_parse_to_pairs(code)?;
    let ast = pairs_to_ast(pairs);
    Ok(())
}
