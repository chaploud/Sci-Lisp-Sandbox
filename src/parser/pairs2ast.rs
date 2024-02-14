
use pest::iterators::Pairs;
use pest::iterators::Pair;
use crate::parser::Rule;

use crate::structures::tokens::TokenKind::*;

pub fn pairs_to_ast(pairs: Pairs<Rule>) {
    for pair in pairs {
        match pair.as_rule() {
            Rule::string => ,
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
            Rule::right_arrow => (),
            Rule::auto_gensym => (),
            Rule::module => (),
            Rule::member => (),
            Rule::EOI => (),
            _ => (),
        }
    }
}

fn parse_string(pair: Pair<Rule>) {
}

fn parse_regex(pair: Pair<Rule>) {
}

fn parse_i64(pair: Pair<Rule>) {
}

fn parse_f64(pair: Pair<Rule>) {
}

fn parse_c64(pair: Pair<Rule>) {
}

fn parse_bool(pair: Pair<Rule>) {
}

fn parse_nil(pair: Pair<Rule>) {
}

fn parse_keyword(pair: Pair<Rule>) {
}

fn parse_symbol(pair: Pair<Rule>) {
}

fn parse_list(pair: Pair<Rule>) {
}

fn parse_vector(pair: Pair<Rule>) {
}

fn parse_map(pair: Pair<Rule>) {
}

fn parse_set(pair: Pair<Rule>) {
}

fn parse_array(pair: Pair<Rule>) {
}

fn parse_slice(pair: Pair<Rule>) {
}

fn parse_def_kw(pair: Pair<Rule>) {
}

fn parse_const_kw(pair: Pair<Rule>) {
}

fn parse_let_kw(pair: Pair<Rule>) {
}

fn parse_sete_kw(pair: Pair<Rule>) {
}

fn parse_defn_kw(pair: Pair<Rule>) {
}

fn parse_return_kw(pair: Pair<Rule>) {
}

fn parse_fn_kw(pair: Pair<Rule>) {
}

fn parse_when_kw(pair: Pair<Rule>) {
}

fn parse_do_kw(pair: Pair<Rule>) {
}

fn parse_cond_kw(pair: Pair<Rule>) {
}

fn parse_switch_kw(pair: Pair<Rule>) {
}

fn parse_for_kw(pair: Pair<Rule>) {
}

fn parse_while_kw(pair: Pair<Rule>) {
}

fn parse_break_kw(pair: Pair<Rule>) {
}

fn parse_continue_kw(pair: Pair<Rule>) {
}

fn parse_enum_kw(pair: Pair<Rule>) {
}

fn parse_struct_kw(pair: Pair<Rule>) {
}

fn parse_method_kw(pair: Pair<Rule>) {
}

fn parse_self_kw(pair: Pair<Rule>) {
}

fn parse_macro_kw(pair: Pair<Rule>) {
}

fn parse_try_kw(pair: Pair<Rule>) {
}

fn parse_throw_kw(pair: Pair<Rule>) {
}

fn parse_catch_kw(pair: Pair<Rule>) {
}

fn parse_finally_kw(pair: Pair<Rule>) {
}

fn parse_typedef_kw(pair: Pair<Rule>) {
}

fn parse_import_kw(pair: Pair<Rule>) {
}

fn parse_export_kw(pair: Pair<Rule>) {
}

fn parse_dot(pair: Pair<Rule>) {
}

fn parse_slash(pair: Pair<Rule>) {
}

fn parse_and(pair: Pair<Rule>) {
}

fn parse_quote(pair: Pair<Rule>) {
}

fn parse_syntax_quote(pair: Pair<Rule>) {
}

fn parse_unquote(pair: Pair<Rule>) {
}

fn parse_unquote_splicing(pair: Pair<Rule>) {
}

fn parse_splicing(pair: Pair<Rule>) {
}

fn parse_type_annotation(pair: Pair<Rule>) {
}

fn parse_right_arrow(pair: Pair<Rule>) {
}

fn parse_auto_gensym(pair: Pair<Rule>) {
}

fn parse_module(pair: Pair<Rule>) {
}

fn parse_member(pair: Pair<Rule>) {
}


