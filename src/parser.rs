/* parse.rs */

use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser as ParserTrait;
use pest_derive::Parser;

use crate::structures::Result;
use crate::structures::errors::Error::*;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct Parser;

pub fn parse(code: &str) {
    println!("{:?}", pest_parse_to_pairs(code));
}

fn pest_parse_to_pairs(code: &str) -> Result<Pairs<Rule>> {
    let toplevel = Parser::parse(Rule::scilisp, code);
    let inners = match toplevel {
        Ok(mut pairs) => pairs.next().unwrap().into_inner().next().unwrap().into_inner(),
        Err(err) => Err(ParseError(Box::new(err)))?,
    };
    Ok(inners)
}

#[cfg(test)]
mod tests {
    use super::pest_parse_to_pairs;
    use super::Rule;

    #[test]
    fn test_pest_parse_01_string() {
        let code = r##"
        "abc\n"
        "##;

        let parsed = pest_parse_to_pairs(code).unwrap().next().unwrap().as_rule();

        match parsed {
            Rule::string => (),
            _ => panic!("{}", code)
        }
    }
}
