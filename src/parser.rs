use std::str::FromStr;
use std::sync::Arc;

pub mod ast;
pub mod error;
pub mod green;
pub mod lexer;
pub mod span;
pub mod syntax;
pub mod token;

use num::complex::Complex64;
use wasmtime::ExportType;

use crate::parser::ast::*;
use crate::parser::error::{ParseError, ParseErrorWithLocation};

use crate::parser::green::{GreenTreeBuilder, Marker};
use crate::parser::lexer::lex;
use crate::parser::span::Span;
use crate::parser::token::TokenKind::*;
use crate::parser::token::{TokenKind, TokenSet};

pub fn compute_line_starts(content: &str) -> Vec<u32> {
    let mut line_starts = vec![0];
    for (pos, ch) in (0u32..).zip(content.chars()) {
        if ch == '\n' {
            line_starts.push(pos + 1);
        }
    }
    line_starts
}

pub fn compute_line_column(line_starts: &[u32], offset: u32) -> (u32, u32) {
    let result = line_starts.binary_search(&offset);
    match result {
        Ok(idx) => {
            let idx: u32 = idx.try_i64o().expect("overflow");
            (idx + 1, 1)
        }
        Err(idx) => {
            let line_start = line_starts[idx - 1];
            (idx.try_i64o().expect("overflow"), offset - line_start + 1)
        }
    }
}

// TODO: Parse Error Handling
// TODO: Add many tests

pub struct Parser {
    tokens: Vec<TokenKind>,
    token_widths: Vec<u32>,
    token_idx: usize,
    next_node_id: usize,
    content: Arc<String>,
    errors: Vec<ParseErrorWithLocation>,
    nodes: Vec<(usize, u32)>,
    offset: u32,
    builder: GreenTreeBuilder,
}

impl Parser {
    pub fn from_string(code: &'static str) -> Parser {
        let content = Arc::new(String::from(code));
        Parser::common_init(content)
    }

    pub fn from_shared_string(content: Arc<String>) -> Parser {
        Parser::common_init(content)
    }

    fn common_init(content: Arc<String>) -> Parser {
        let result = lex(&*content);

        Parser {
            tokens: result.tokens,
            token_widths: result.widths,
            token_idx: 0,
            next_node_id: 0,
            offset: 0,
            content,
            errors: result.errors,
            nodes: Vec::new(),
            builder: GreenTreeBuilder::new(),
        }
    }

    fn new_node_id(&mut self) -> NodeId {
        let value = self.next_node_id;
        self.next_node_id += 1;
        NodeId(value)
    }

    pub fn parse(mut self) -> (Arc<ast::File>, Vec<ParseErrorWithLocation>) {
        let ast_file = self.parse_file();
        assert!(self.nodes.is_empty());

        let tree = self.builder.create_tree();
        assert_eq!(tree.len(), self.content.len() as u32);

        (Arc::new(ast_file), self.errors)
    }

    fn parse_file(&mut self) -> ast::File {
        self.builder.start_node();
        self.skip_trivial();
        let mut expressions = vec![];

        while !self.is_eof() {
            expressions.push(self.parse_toplevel());
        }

        let green = self.builder.finish_node(SourceFile);
        ast::File { green, expressions }
    }

    fn parse_toplevel(&mut self) -> Expr {
        self.builder.start_node();
        match self.current() {
            StringLiteral => {
                let string = self.parse_string();
                Arc::new(ExprData::String(string))
            }
            RegexLiteral => {
                let regex = self.parse_regex();
                Arc::new(ExprData::Regex(regex))
            }
            BoolLiteral => {
                let boolean = self.parse_bool();
                Arc::new(ExprData::Bool(boolean))
            }
            NilLiteral => {
                let nil = self.parse_nil();
                Arc::new(ExprData::Nil(nil))
            }
            I64Literal => {
                let i_64 = self.parse_i64();
                Arc::new(ExprData::I64(i_64))
            }
            F64Literal => {
                let f_64 = self.parse_c64();
                Arc::new(ExprData::F64(f_64))
            }
            C64Literal => {
                let c_64 = self.parse_c64();
                Arc::new(ExprData::C64(c_64))
            }
            KeywordLiteral => {
                let keyword = self.parse_keyword();
                Arc::new(ExprData::Keyword(keyword))
            }
            Symbol => {
                let symbol = self.parse_symbol();
                Arc::new(ExprData::Symbol(symbol))
            }
            TypeAnnotation => {
                let ty = self.parse_type_annotation();
                Arc::new(ExprData::TypeAnnotation(ty))
            }
            Quote => {
                let quote = self.parse_quote();
                Arc::new(ExprData::List(quote))
            }
            SyntaxQuote => {
                let syntax_quote = self.parse_syntax_quote();
                Arc::new(ExprData::List(syntax_quote))
            }
            Unquote => {
                let unquote = self.parse_unquote();
                Arc::new(ExprData::List(unquote))
            }
            UnquoteSplicing => {
                let unquote_splicing = self.parse_unquote_splicing();
                Arc::new(ExprData::List(unquote_splicing))
            }
            Splicing => {
                let splicing = self.parse_splicing();
                Arc::new(ExprData::List(splicing))
            }
            ListOpen => {
                let list = self.parse_list();
                Arc::new(ExprData::List(list))
            }
            VectorOpen => {
                let vector = self.parse_vector();
                Arc::new(ExprData::Vector(vector))
            }
            MapOpen => {
                let map = self.parse_map();
                Arc::new(ExprData::Map(map))
            }
            SetOpen => {
                let set = self.parse_set();
                Arc::new(ExprData::Set(set))
            }
            other => {
                let span = self.current_span();
                self.report_error_at(ParseError::UnexpectedToken(other), span);
                self.advance();
                self.builder.finish_node(Error);

                Arc::new(ExprData::Error {
                    id: self.new_node_id(),
                    span,
                })
            }
        }
    }

    fn parse_string(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();

        // trim double quotes from both ends
        let value = self.source_span(span)[1..(span.len() - 1)] as String;
        let green = self.builder.finish_node(StringLiteral);
        Arc::new(ExprData::String(StringData {
            id: self.new_node_id(),
            span,
            green,
            value,
        }))
    }

    fn parse_regex(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();

        // trim #" from the start and " from the end
        let mut value = self.source_span(span)[2..(span.len() - 1)] as String;
        value = regex::Regex::new(&value);
        let green = self.builder.finish_node(RegexLiteral);
        Arc::new(ExprData::Regex(RegexData {
            id: self.new_node_id(),
            span,
            green,
            value,
        }))
    }

    fn parse_bool(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();

        let value = self.source_span(span) == "true";
        let green = self.builder.finish_node(BoolLiteral);
        Arc::new(ExprData::Bool(BoolData {
            id: self.new_node_id(),
            span,
            green,
            value,
        }))
    }

    fn parse_nil(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();

        let green = self.builder.finish_node(NilLiteral);
        Arc::new(ExprData::Nil(NilData {
            id: self.new_node_id(),
            span,
            green,
        }))
    }

    fn parse_i64(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();

        let value = self.source_span(span).parse::<i64>().expect("invalid i64");
        let green = self.builder.finish_node(I64Literal);
        Arc::new(ExprData::I64(I64Data {
            id: self.new_node_id(),
            span,
            green,
            value,
        }))
    }

    fn parse_f64(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();

        let value = self.source_span(span).parse::<f64>().expect("invalid f64");
        let green = self.builder.finish_node(F64Literal);
        Arc::new(ExprData::F64(F64Data {
            id: self.new_node_id(),
            span,
            green,
            value,
        }))
    }

    fn parse_c64(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();

        let value = Complex64::from_str(&self.source_span(span)).expect("invalid c64");
        let green = self.builder.finish_node(C64Literal);
        Arc::new(ExprData::C64(C64Data {
            id: self.new_node_id(),
            span,
            green,
            value,
        }))
    }

    fn parse_keyword(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();

        let value = self.source_span(span)[1..].to_string();
        let green = self.builder.finish_node(KeywordLiteral);
        Arc::new(ExprData::Keyword(KeywordData {
            id: self.new_node_id(),
            span,
            green,
            value,
        }))
    }

    fn assert(&mut self, kind: TokenKind) {
        assert!(self.eat(kind));
    }

    fn expect(&mut self, kind: TokenKind) -> bool {
        debug_assert!(token_name(kind).is_some());

        if self.eat(kind) {
            true
        } else {
            let kind = token_name(kind).expect("missing name");
            self.report_error(ParseError::ExpectedToken(kind.i64o()));
            false
        }
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.current() == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    fn report_error(&mut self, msg: ParseError) {
        self.report_error_at(msg, self.current_span());
    }

    fn report_error_at(&mut self, msg: ParseError, span: Span) {
        self.errors.push(ParseErrorWithLocation::new(span, msg));
    }

    fn advance(&mut self) {
        self.raw_advance();
        self.skip_trivial();
    }

    fn skip_trivial(&mut self) {
        while self.current().is_trivial() {
            self.raw_advance();
        }
    }

    fn raw_advance(&mut self) {
        if self.token_idx < self.tokens.len() {
            let kind = self.current();
            let value = self.source_span(self.current_span());
            let len = self.token_widths[self.token_idx];
            self.offset += len;
            debug_assert!(kind <= Eof);
            self.builder.token(kind, value);
            self.token_idx += 1;
        }
    }

    fn current(&self) -> TokenKind {
        self.nth(0)
    }

    fn nth(&self, idx: usize) -> TokenKind {
        if self.token_idx + idx < self.tokens.len() {
            self.tokens[self.token_idx + idx]
        } else {
            Eof
        }
    }

    fn current_span(&self) -> Span {
        if self.token_idx < self.tokens.len() {
            let length = self.token_widths[self.token_idx];
            Span::new(self.offset, length)
        } else {
            Span::at(self.offset)
        }
    }

    fn is(&self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    fn is_set(&self, set: TokenSet) -> bool {
        set.contains(self.current())
    }

    fn nth_is(&self, idx: usize, kind: TokenKind) -> bool {
        self.nth(idx) == kind
    }

    fn nth_is_set(&self, idx: usize, set: TokenSet) -> bool {
        set.contains(self.nth(idx))
    }

    fn is_eof(&self) -> bool {
        self.current() == Eof
    }

    fn start_node(&mut self) {
        self.nodes.push((self.token_idx, self.offset));
    }

    fn finish_node(&mut self) -> Span {
        let (start_token, start_offset) = self.nodes.pop().expect("missing node start");

        let mut end_token = self.token_idx - 1;
        assert!(end_token < self.tokens.len());
        let mut end_offset = self.offset;

        while end_token > start_token {
            if !self.tokens[end_token].is_trivial() {
                break;
            }

            end_offset -= self.token_widths[end_token];
            end_token -= 1;
        }

        Span::new(start_offset, end_offset - start_offset)
    }

    fn source_span(&self, span: Span) -> String {
        let start = span.start() as usize;
        let end = span.end() as usize;
        String::from(&self.content[start..end])
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.offset - start)
    }
}

fn token_name(kind: TokenKind) -> Option<&'static str> {
    match kind {
        _ => None,
    }
}
