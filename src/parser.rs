/* parse.rs */

use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser as ParserTrait;
use pest_derive::Parser;

use crate::structures::ast;
use crate::structures::Result;
use crate::structures::errors::Error::*;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct Parser;

pub fn parse(code: &str) -> Result<ast::AST> {
    let pairs = pest_parse_to_pairs(code)?;
    let mut ast = pairs_to_ast(pairs);
}

fn pest_parse_to_pairs(code: &str) -> Result<Pairs<Rule>> {
    let toplevel = Parser::parse(Rule::scilisp, code);
    let inners = match toplevel {
        Ok(mut pairs) => pairs.next().unwrap().into_inner().next().unwrap().into_inner(),
        Err(err) => Err(ParseError(Box::new(err)))?,
    };
    Ok(inners)
}

fn pairs_to_ast(pairs: Pairs<Rule>) {
    for pair in pairs {
        match pair.as_rule() {
            Rule::string => (),
            Rule::regex => (),
            Rule::i64 => (),
            Rule::f64 => (),
            Rule::c64 => (),
            Rule::bool => (),
            Rule::nil => (),
            Rule::keyword => (),
            Rule::symbol => (),
            Rule::list => (),
            Rule::vector => (),
            Rule::map => (),
            Rule::set => (),
            Rule::array => (),
            Rule::slice => (),
            Rule::def_kw => (),
            Rule::const_kw => (),
            Rule::let_kw => (),
            Rule::sete_kw => (),
            Rule::defn_kw => (),
            Rule::return_kw => (),
            Rule::fn_kw => (),
            Rule::when_kw => (),
            Rule::do_kw => (),
            Rule::cond_kw => (),
            Rule::switch_kw => (),
            Rule::for_kw => (),
            Rule::while_kw => (),
            Rule::break_kw => (),
            Rule::continue_kw => (),
            Rule::enum_kw => (),
            Rule::struct_kw => (),
            Rule::method_kw => (),
            Rule::self_kw => (),
            Rule::macro_kw => (),
            Rule::try_kw => (),
            Rule::throw_kw => (),
            Rule::catch_kw => (),
            Rule::finally_kw => (),
            Rule::typedef_kw => (),
            Rule::import_kw => (),
            Rule::export_kw => (),
            Rule::dot => (),
            Rule::slash => (),
            Rule::and => (),
            Rule::quote => (),
            Rule::syntax_quote => (),
            Rule::unquote => (),
            Rule::unquote_splicing => (),
            Rule::splicing => (),
            Rule::type_annotation => (),
            Rule::EOI => (),
            _ => (),
        }
    }
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

    #[test]
    fn test_pest_parse_08_keyword() {
        let code = r##"
        :keyword
        :nil
        "##;
        for (i, pairs) in pest_parse_to_pairs(code).unwrap().enumerate() {
            match pairs.as_rule() {
                Rule::keyword => (),
                _ => panic!("{0}\n => {1}", code, i)
            }
        }
    }

    #[test]
    fn test_pest_parse_09_symbol() {
        let code = r##"
        sym
        symbol
        a12!$-=^+*<>?_\
        "##;
        for (i, pairs) in pest_parse_to_pairs(code).unwrap().enumerate() {
            match pairs.as_rule() {
                Rule::symbol => (),
                _ => panic!("{0}\n => {1}", code, i)
            }
        }
    }

    #[test]
    fn test_pest_parse_10_collection() {
        let code = r##"
        (1, "a", :b, sym)
        [1, "a", :b, sym]
        {1, "a", :b, sym}
        #{1, "a", :b, sym}
        #[1, 2, 3]
        "##;
        for (i, pairs) in pest_parse_to_pairs(code).unwrap().enumerate() {
            match pairs.as_rule() {
                Rule::list => if i == 0 { } else { panic!("{0}\n => {1}", code, i) },
                Rule::vector => if i == 1 { } else { panic!("{0}\n => {1}", code, i) },
                Rule::map => if i == 2 { } else { panic!("{0}\n => {1}", code, i) },
                Rule::set => if i == 3 { } else { panic!("{0}\n => {1}", code, i) },
                Rule::array => if i == 4 { } else { panic!("{0}\n => {1}", code, i) },
                _ => panic!("{0}\n => {1}", code, i)
            }
        }
    }
}
