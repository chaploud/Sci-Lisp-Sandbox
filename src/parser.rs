/* parse.rs */

use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser as ParserTrait;
use pest_derive::Parser;

use crate::structures::Result;
use crate::structures::errors::Error;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct Parser;

pub fn parse(code: &str) {
    let result = Parser::parse(Rule::scilisp, code);
    match result {
        Ok(pairs) => {
            println!("{:#?}", pairs);
        }
        Err(e) => {
            println!("{:#?}", e);
        }
    }
}

pub fn pest_parse(code: &str) -> Result<Pairs<Rule>> {
    match Parser::parse(Rule::scilisp, code) {
        Ok(pairs) => Ok(pairs),
        Err(e) => Err(Error::Parse(e)),
    }
}
