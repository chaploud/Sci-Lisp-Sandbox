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

pub fn parse(code: &str) -> Result<ast::AST> {
    let pairs = pest_parse_to_pairs(code)?;

    let mut ast = ast::AST {
        next_node_id: 0,
        nodes: Vec::new(),
        spans: Vec::new(),
        errors: Vec::new(),
    };

    pairs_to_ast(pairs);
    Ok(ast)
}
