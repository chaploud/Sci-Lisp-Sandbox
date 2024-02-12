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

        for (i, pairs) in pest_parse_to_pairs(code).unwrap().enumerate() {
            match pairs.as_rule() {
                Rule::string => (),
                _ => panic!("{0}\n => {1}", code, i)
            }
        }
    }

    #[test]
    fn test_pest_parse_02_regex() {
        let code = r##"
        #"[0-9]+"
        "##;

        for (i, pairs) in pest_parse_to_pairs(code).unwrap().enumerate() {
            match pairs.as_rule() {
                Rule::regex => (),
                _ => panic!("{0}\n => {1}", code, i)
            }
        }
    }

    #[test]
    fn test_pest_parse_03_bool() {
        let code = r##"
        false
        true
        "##;

        for (i, pairs) in pest_parse_to_pairs(code).unwrap().enumerate() {
            match pairs.as_rule() {
                Rule::bool => (),
                _ => panic!("{0}\n => {1}", code, i)
            }
        }
    }

    #[test]
    fn test_pest_parse_04_nil() {
        let code = r##"
        nil
        "##;

        for (i, pairs) in pest_parse_to_pairs(code).unwrap().enumerate() {
            match pairs.as_rule() {
                Rule::nil => (),
                _ => panic!("{0}\n => {1}", code, i)
            }
        }
    }

    #[test]
    fn test_pest_parse_05_i64() {
        let code = r##"
        -999
        -1
        12345
        +23
        0
        0b101
        0o777
        0xffa
        "##;

        for (i, pairs) in pest_parse_to_pairs(code).unwrap().enumerate() {
            match pairs.as_rule() {
                Rule::i64 => (),
                _ => panic!("{0}\n => {1}", code, i)
            }
        }
    }

    #[test]
    fn test_pest_parse_06_f64() {
        let code = r##"
        0.1
        1.0
        1.
        .2
        -1.
        -.2
        3.14e15
        3.14e-15
        3.14e+15
        -3.14e-15
        -3.e+15
        -.2e-15
        nan
        inf
        -inf
        -0.0
        "##;

        for (i, pairs) in pest_parse_to_pairs(code).unwrap().enumerate() {
            match pairs.as_rule() {
                Rule::f64 => (),
                _ => panic!("{0}\n => {1}", code, i)
            }
        }
    }

    #[test]
    fn test_pest_parse_07_c64() {
        let code = r##"
        0.1j
        1.0j
        1.j
        .2j
        -1.j
        -.2j
        3.14e15j
        3.14e-15j
        3.14e+15j
        -3.14e-15j
        -3.e+15j
        -.2e-15j
        -0.0j
        -999j
        -1j
        12345j
        +23j
        1+1j
        1-1j
        -1+1j
        -1-1j
        0.1+1.0j
        1.0+1.0j
        1.+1.0j
        .2+1.0j
        -1.+1.0j
        -.2+1.0j
        3.14e15+1.0j
        3.14e-15+1.0j
        3.14e+15+1.0j
        -3.14e-15+1.0j
        -3.e+15+1.0j
        -.2e-15+1.0j
        "##;

        for (i, pairs) in pest_parse_to_pairs(code).unwrap().enumerate() {
            match pairs.as_rule() {
                Rule::c64 => (),
                _ => panic!("{0}\n => {1}", code, i)
            }
        }
    }
}
