use std::collections::HashMap;

use crate::parser::error::{ParseError, ParseErrorWithLocation};
use crate::parser::span::Span;
use crate::parser::token::TokenKind;
use crate::parser::token::TokenKind::*;

pub struct LexerResult {
    pub tokens: Vec<TokenKind>,
    pub widths: Vec<u32>,
    pub errors: Vec<ParseErrorWithLocation>,
}

pub fn lex(content: &str) -> LexerResult {
    let mut lexer = Lexer::new(content);
    let mut tokens = Vec::new();
    let mut widths = Vec::new();

    while !lexer.is_eof() {
        let start = lexer.offset();
        let token = lexer.read_token();
        assert!(token < TokenKind::Eof);
        let end = lexer.offset();
        tokens.push(token);
        widths.push(end - start);
    }

    LexerResult {
        tokens,
        widths,
        errors: lexer.errors,
    }
}

struct Lexer<'a> {
    content: &'a str,
    offset: usize,
    keywords: HashMap<&'static str, TokenKind>,
    errors: Vec<ParseErrorWithLocation>,
}

impl<'a> Lexer<'a> {
    fn new(content: &str) -> Lexer {
        let keywords = keywords_in_map();

        Lexer {
            offset: 0,
            content,
            keywords,
            errors: Vec::new(),
        }
    }

    fn read_token(&mut self) -> TokenKind {
        let ch = self.curr().expect("end of file reached");
        let ch = Some(ch);

        if is_whitespace(ch) {
            self.read_white_space()
        } else if is_semicolon(ch) {
            self.read_line_comment()
        } else if is_digit_or_minus(ch) {
            self.read_number(false, self.offset())
        } else if is_sharp(ch) {
            self.read_sharp()
        } else if is_open(ch) || is_close(ch) {
            self.read_bracket()
        } else if is_symbol_start(ch) {
            self.read_symbol()
        } else if is_colon(ch) {
            self.read_keyword()
        } else if is_double_quote(ch) {
            self.read_string()
        } else if is_quote(ch) {
            self.read_quote()
        } else if is_syntax_quote(ch) {
            self.read_syntax_quote()
        } else if is_unquote(ch) {
            self.read_unquote()
        } else if is_splicing(ch) {
            self.read_splicing()
        } else if is_dot(ch) {
            self.read_dot()
        } else if is_pipe(ch) {
            self.read_pipe() // TODO: need semantical analysis (slice 2|-1|1)
        } else if is_slash(ch) {
            self.read_slash()
        } else if is_and(ch) {
            self.read_and()
        } else {
            self.read_unknown_char()
        }
    }

    fn read_unknown_char(&mut self) -> TokenKind {
        let start = self.offset();
        let ch = self.curr().expect("missing char");
        self.eat_char();
        let span = self.span_from(start);
        self.report_error_at(ParseError::UnknownChar(ch), span);
        Unknown
    }

    fn read_white_space(&mut self) -> TokenKind {
        while is_whitespace(self.curr()) {
            self.eat_char();
        }

        Whitespace
    }

    fn read_line_comment(&mut self) -> TokenKind {
        while !self.curr().is_none() && !is_newline(self.curr()) {
            self.eat_char();
        }

        LineComment
    }

    fn read_sharp(&mut self) -> TokenKind {
        let start = self.offset();
        let ch = self.curr().expect("missing char");
        self.eat_char();
        let next = self.lookahead();

        match next {
            Some('"') => self.read_regex(start),
            Some('{') => self.read_set_start(),
            _ => {
                if is_symbol_start(next) {
                    self.read_type_annotation()
                } else {
                    self.report_error_at(
                        ParseError::ExpectedToken("'\"', '{' or symbol".to_string()),
                        self.span_from(start),
                    );
                    Error
                }
            }
        }
    }

    fn read_bracket(&mut self) -> TokenKind {
        let ch = self.curr().expect("missing char");
        self.eat_char();

        match ch {
            '(' => ListOpen,
            '[' => VectorOpen,
            '{' => MapOpen,
            ')' => ListClose,
            ']' => VectorClose,
            '}' => RBrace, // TODO: need semantical analysis (map or set)
            _ => {
                unreachable!()
            }
        }
    }

    fn read_symbol(&mut self) -> TokenKind {
        let value = self.read_symbol_as_string();
        if value == "nan" || value == "inf" || value == "-inf" {
            return F64Literal;
        }

        let lookup = self.keywords.get(&value[..]).cloned();

        if let Some(token_type) = lookup {
            token_type
        } else if value == "_" {
            Underscore
        } else if value == "=>" {
            RightArrow
        } else {
            Symbol
        }
    }

    fn read_type_annotation(&mut self) -> TokenKind {
        let value = self.read_symbol_as_string();
        TypeAnnotation
    }

    fn read_keyword(&mut self) -> TokenKind {
        self.eat_char();
        let value = self.read_symbol_as_string();
        KeywordLiteral
    }

    fn read_symbol_as_string(&mut self) -> String {
        let mut value = String::new();

        while is_symbol(self.curr()) {
            let ch = self.curr().unwrap();
            self.eat_char();
            value.push(ch);
        }

        value
    }

    fn read_escaped_char(&mut self) {
        if self.eat_char() == Some('\\') {
            self.eat_char();
        }
    }

    fn read_string(&mut self) -> TokenKind {
        let mut start = self.offset();

        self.eat_char();

        while self.curr().is_some() && !is_double_quote(self.curr()) {
            self.read_escaped_char();
        }

        if is_double_quote(self.curr()) {
            self.eat_char();
        } else {
            let span = self.span_from(start);
            self.report_error_at(ParseError::UnclosedString, span);
        }

        StringLiteral
    }

    fn read_set_start(&mut self) -> TokenKind {
        self.eat_char();
        SetOpen
    }

    fn read_regex(&mut self, start: u32) -> TokenKind {
        self.eat_char();

        while self.curr().is_some() && !is_double_quote(self.curr()) {
            self.read_escaped_char();
        }

        if is_double_quote(self.curr()) {
            self.eat_char();
        } else {
            let span = self.span_from(start);
            self.report_error_at(ParseError::UnclosedRegex, span);
        }

        RegexLiteral
    }

    fn read_number(&mut self, second_time: bool, start: u32) -> TokenKind {
        if self.curr() == Some('-') {
            let next = self.lookahead();
            if is_digit(next) {
                self.eat_char();
            } else if is_symbol_start(next) | is_whitespace(next) | is_close(next) {
                return self.read_symbol();
            }
        }

        let mut base = if self.curr() == Some('0') {
            let next = self.lookahead();

            match next {
                Some('x') => {
                    self.eat_char();
                    self.eat_char();

                    16
                }

                Some('o') => {
                    self.eat_char();
                    self.eat_char();

                    8
                }

                Some('b') => {
                    self.eat_char();
                    self.eat_char();

                    2
                }

                _ => 10,
            }
        } else {
            10
        };

        self.read_digits(base);

        // example: 12j
        if base == 10 && self.curr() == Some('j') {
            self.eat_char();
            return C64Literal;
        }

        // example: 12., 12.3, 12e3,...
        if base == 10
            && (self.curr() == Some('.') || self.curr() == Some('e') || self.curr() == Some('E'))
        {
            return self.read_number_as_float(second_time, start);
        }

        // example: 12+3j, 12-3.0j, 12+3.2e+01j...
        if !second_time
            && base == 10
            && (self.curr() == Some('+') || self.curr() == Some('-'))
            && is_digit(self.lookahead())
        {
            return self.read_number_as_complex(start);
        }

        if !is_whitespace(self.curr()) || !is_close(self.curr()) || !self.is_eof() {
            let span = self.span_from(start);
            self.report_error_at(ParseError::InvalidNumber, span);
        }

        I64Literal
    }

    fn read_number_as_float(&mut self, second_time: bool, start: u32) -> TokenKind {
        self.eat_char();

        self.read_digits(10);

        if self.curr() == Some('e') || self.curr() == Some('E') {
            self.eat_char();

            if self.curr() == Some('+') || self.curr() == Some('-') {
                self.eat_char();
            }

            self.read_digits(10);
        }

        // example: 12.3j, 12.j, 12.3-e+1j,...
        if self.curr() == Some('j') {
            self.eat_char();
            return C64Literal;
        }

        // example: 12.+3j, 12.3-3.2j, 12.3e-3+3.2j,...
        if !second_time && (self.curr() == Some('+') || self.curr() == Some('-')) {
            return self.read_number_as_complex(start);
        }

        if !is_whitespace(self.curr()) || !is_close(self.curr()) || !self.is_eof() {
            let span = self.span_from(start);
            self.report_error_at(ParseError::InvalidNumber, span);
        }

        F64Literal
    }

    fn read_number_as_complex(&mut self, start: u32) -> TokenKind {
        self.eat_char(); // consume '+' or '-'
        self.read_number(true, start)
    }

    fn read_digits(&mut self, base: u32) {
        while is_digit_or_underscore(self.curr(), base) {
            self.eat_char();
        }
    }

    fn read_quote(&mut self) -> TokenKind {
        self.eat_char();
        Quote
    }

    fn read_syntax_quote(&mut self) -> TokenKind {
        self.eat_char();
        SyntaxQuote
    }

    fn read_unquote(&mut self) -> TokenKind {
        self.eat_char();
        let next = self.lookahead();
        if let Some('@') = next {
            self.eat_char();
            return UnquoteSplicing;
        }
        Unquote
    }

    fn read_splicing(&mut self) -> TokenKind {
        self.eat_char();
        Splicing
    }

    fn read_dot(&mut self) -> TokenKind {
        self.eat_char();
        Dot
    }

    fn read_pipe(&mut self) -> TokenKind {
        self.eat_char();
        Pipe
    }

    fn read_slash(&mut self) -> TokenKind {
        self.eat_char();
        Slash
    }

    fn read_and(&mut self) -> TokenKind {
        self.eat_char();
        And
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.offset() - start)
    }

    fn offset(&self) -> u32 {
        self.offset.try_into().expect("overflow")
    }

    fn eat_char(&mut self) -> Option<char> {
        let curr = self.curr();

        if let Some(ch) = curr {
            self.offset += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    fn is_eof(&self) -> bool {
        self.offset == self.content.len()
    }

    fn curr(&self) -> Option<char> {
        if self.offset < self.content.len() {
            self.content[self.offset..].chars().next()
        } else {
            None
        }
    }

    fn lookahead(&self) -> Option<char> {
        if self.offset < self.content.len() {
            let mut it = self.content[self.offset..].chars();
            it.next();
            it.next()
        } else {
            None
        }
    }

    fn report_error_at(&mut self, msg: ParseError, span: Span) {
        self.errors.push(ParseErrorWithLocation::new(span, msg));
    }

    fn is_line_comment(&self) -> bool {
        self.curr() == Some(';')
    }
}

fn is_digit(ch: Option<char>) -> bool {
    ch.map(|ch| ch.is_digit(10)).unwrap_or(false)
}

fn is_digit_or_minus(ch: Option<char>) -> bool {
    ch.map(|ch| ch.is_digit(10) || ch == '-').unwrap_or(false)
}

fn is_digit_or_underscore(ch: Option<char>, base: u32) -> bool {
    ch.map(|ch| ch.is_digit(base) || ch == '_').unwrap_or(false)
}

fn is_whitespace(ch: Option<char>) -> bool {
    ch.map(|ch| ch.is_whitespace() || ch == ',' || ch == '\t' || ch == '\r' || ch == '\n')
        .unwrap_or(false)
}

fn is_symbol_start(ch: Option<char>) -> bool {
    match ch {
        Some(ch) => {
            (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || "!$%-=^+*<>?_\\#".contains(ch)
        }
        _ => false,
    }
}

fn is_symbol(ch: Option<char>) -> bool {
    is_symbol_start(ch) || is_digit(ch)
}

fn is_semicolon(ch: Option<char>) -> bool {
    ch == Some(';')
}

fn is_double_quote(ch: Option<char>) -> bool {
    ch == Some('\"')
}

fn is_newline(ch: Option<char>) -> bool {
    ch == Some('\n')
}

fn is_colon(ch: Option<char>) -> bool {
    ch == Some(':')
}

fn is_sharp(ch: Option<char>) -> bool {
    ch == Some('#')
}

fn is_quote(ch: Option<char>) -> bool {
    ch == Some('\'')
}

fn is_syntax_quote(ch: Option<char>) -> bool {
    ch == Some('`')
}

fn is_unquote(ch: Option<char>) -> bool {
    ch == Some('~')
}

fn is_splicing(ch: Option<char>) -> bool {
    ch == Some('@')
}

fn is_dot(ch: Option<char>) -> bool {
    ch == Some('.')
}

fn is_pipe(ch: Option<char>) -> bool {
    ch == Some('|')
}

fn is_slash(ch: Option<char>) -> bool {
    ch == Some('/')
}

fn is_and(ch: Option<char>) -> bool {
    ch == Some('&')
}

fn is_close(ch: Option<char>) -> bool {
    ch == Some(')') || ch == Some(']') || ch == Some('}')
}

fn is_open(ch: Option<char>) -> bool {
    ch == Some('(') || ch == Some('[') || ch == Some('{')
}

fn keywords_in_map() -> HashMap<&'static str, TokenKind> {
    let mut keywords = HashMap::with_capacity(30);

    keywords.insert("true", BoolLiteral);
    keywords.insert("false", BoolLiteral);
    keywords.insert("nil", NilLiteral);
    keywords.insert("def", DefKw);
    keywords.insert("const", ConstKw);
    keywords.insert("let", LetKw);
    keywords.insert("set!", SeteKw);
    keywords.insert("defn", DefnKw);
    keywords.insert("fn", FnKw);
    keywords.insert("return", ReturnKw);
    keywords.insert("if", IfKw);
    keywords.insert("when", WhenKw);
    keywords.insert("cond", CondKw);
    keywords.insert("do", DoKw);
    keywords.insert("switch", SwitchKw);
    keywords.insert("for", ForKw);
    keywords.insert("while", WhileKw);
    keywords.insert("break", BreakKw);
    keywords.insert("continue", ContinueKw);
    keywords.insert("enum", EnumKw);
    keywords.insert("struct", StructKw);
    keywords.insert("method", MethodKw);
    keywords.insert("self", SelfKw);
    keywords.insert("macro", MacroKw);
    keywords.insert("try", TryKw);
    keywords.insert("throw", ThrowKw);
    keywords.insert("catch", CatchKw);
    keywords.insert("finally", FinallyKw);
    keywords.insert("import", ImportKw);

    keywords
}

#[cfg(test)]
mod tests {
    use crate::parser::error::{ParseError, ParseErrorWithLocation};
    use crate::parser::lexer::lex;
    use crate::parser::token::TokenKind;
    use crate::parser::token::TokenKind::*;

    fn lex_success(content: &str) -> Vec<(TokenKind, u32)> {
        let result = lex(content);
        assert!(result.errors.is_empty());

        result
            .tokens
            .iter()
            .zip(result.widths.iter())
            .map(|(t, w)| (t.to_owned(), w.to_owned()))
            .collect()
    }

    fn lex_test(content: &str) -> (Vec<(TokenKind, u32)>, Vec<ParseErrorWithLocation>) {
        let result = lex(content);
        let token_with_widths = result
            .tokens
            .iter()
            .zip(result.widths.iter())
            .map(|(t, w)| (t.to_owned(), w.to_owned()))
            .collect();
        (token_with_widths, result.errors)
    }

    fn dump_tokens(tokens: Vec<(TokenKind, u32)>) -> String {
        let mut content = String::new();
        let mut start: u32 = 0;

        for (token, length) in tokens {
            content.push_str(&format!("{:?}@{}..{}\n", token, start, start + length));
            start += length;
        }

        content
    }

    fn assert_err(errors: Vec<ParseErrorWithLocation>, msg: ParseError, start: u32, count: u32) {
        assert_eq!(errors.len(), 1);
        let error = &errors[0];
        assert_eq!(msg, error.error);
        assert_eq!(start, error.span.start());
        assert_eq!(count, error.span.len());
    }

    #[test]
    fn test_read_empty_file() {
        let tokens = lex_success("");
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_read_numbers() {
        let tokens = lex_success("1 2\n0123 1_000_000");
        assert_eq!(
            tokens,
            vec![
                (I64Literal, 1),
                (Whitespace, 1),
                (I64Literal, 1),
                (Whitespace, 1),
                (I64Literal, 4),
                (Whitespace, 1),
                (I64Literal, 9),
            ]
        );
    }

    #[test]
    fn test_skip_single_line_comment() {
        let tokens = lex_success("; test\n1");
        assert_eq!(
            tokens,
            vec![(LineComment, 6), (Whitespace, 1), (I64Literal, 1)]
        );
    }

    #[test]
    fn test_unfinished_line_comment() {
        let tokens = lex_success(";;abc");
        assert_eq!(tokens, &[(LineComment, 5)]);
    }

    #[test]
    fn test_read_symbol() {
        let tokens = lex_success("abc ident test");
        assert_eq!(
            tokens,
            vec![
                (Symbol, 3),
                (Whitespace, 1),
                (Symbol, 5),
                (Whitespace, 1),
                (Symbol, 4),
            ]
        );
    }

    #[test]
    fn test_float_numbers() {
        let tokens = lex_success("1. 2. 3.0 4.5 0.6789 -1.2 -0.0");
        assert_eq!(
            tokens,
            vec![
                (F64Literal, 2),
                (Whitespace, 1),
                (F64Literal, 2),
                (Whitespace, 1),
                (F64Literal, 3),
                (Whitespace, 1),
                (F64Literal, 3),
                (Whitespace, 1),
                (F64Literal, 6),
                (Whitespace, 1),
                (F64Literal, 4),
                (Whitespace, 1),
                (F64Literal, 4),
            ]
        );
    }

    #[test]
    fn test_float_scientific_notation() {
        let tokens = lex_success("1.e1 1.0e1 1.0E1 -1.0e+1 1.0e-1");
        assert_eq!(
            tokens,
            vec![
                (F64Literal, 4),
                (Whitespace, 1),
                (F64Literal, 5),
                (Whitespace, 1),
                (F64Literal, 5),
                (Whitespace, 1),
                (F64Literal, 7),
                (Whitespace, 1),
                (F64Literal, 6),
            ]
        );
    }

    #[test]
    fn test_float_nan_inf() {
        let tokens = lex_success("nan inf -inf");
        assert_eq!(
            tokens,
            vec![
                (F64Literal, 3),
                (Whitespace, 1),
                (F64Literal, 3),
                (Whitespace, 1),
                (F64Literal, 4),
            ]
        );
    }

    #[test]
    fn test_hex_numbers() {
        let tokens = lex_success("0x1 0x2 0xABCDEF 0xB1");
        assert_eq!(
            tokens,
            vec![
                (I64Literal, 3),
                (Whitespace, 1),
                (I64Literal, 6),
                (Whitespace, 1),
                (I64Literal, 8),
                (Whitespace, 1),
                (I64Literal, 7),
            ]
        );
    }

    #[test]
    fn test_code_with_tabs() {
        let tokens = lex_success("1\t2\t3");
        assert_eq!(
            tokens,
            vec![
                (I64Literal, 1),
                (Whitespace, 1),
                (I64Literal, 1),
                (Whitespace, 1),
                (I64Literal, 1)
            ]
        );
    }

    #[test]
    fn test_string_with_newline() {
        let tokens = lex_success("\"abc\ndef\"");
        assert_eq!(tokens, vec![(StringLiteral, 9),]);
    }

    #[test]
    fn test_escape_sequences() {
        let tokens = lex_success("\"\\\"\"");
        assert_eq!(tokens, vec![(StringLiteral, 4)]);

        let tokens = lex_success("\"\\$\"");
        assert_eq!(tokens, &[(StringLiteral, 4)]);

        let tokens = lex_success("\"\\\'\"");
        assert_eq!(tokens, &[(StringLiteral, 4)]);

        let tokens = lex_success("\"\\t\"");
        assert_eq!(tokens, &[(StringLiteral, 4)]);

        let tokens = lex_success("\"\\n\"");
        assert_eq!(tokens, &[(StringLiteral, 4)]);

        let tokens = lex_success("\"\\r\"");
        assert_eq!(tokens, &[(StringLiteral, 4)]);

        let tokens = lex_success("\"\\\\\"");
        assert_eq!(tokens, &[(StringLiteral, 4)]);

        let (tokens, errors) = lex_test("\"\\");
        assert_eq!(tokens, vec![(StringLiteral, 2)]);
        assert_err(errors, ParseError::UnclosedString, 0, 2);
    }

    #[test]
    fn test_unclosed_string() {
        let (tokens, errors) = lex_test("\"abc");
        assert_eq!(tokens, &[(StringLiteral, 4)]);
        assert_err(errors, ParseError::UnclosedString, 0, 4);
    }

    #[test]
    fn test_string() {
        let tokens = lex_success("\"abc\"");
        assert_eq!(tokens, &[(StringLiteral, 5)]);
    }

    #[test]
    fn test_keywords() {
        let tokens = lex_success(
            "true \
            false \
            nil \
            def \
            const \
            let \
            set! \
            defn \
            fn \
            return \
            if \
            when \
            cond \
            do \
            switch \
            for \
            while \
            break \
            continue \
            enum \
            struct \
            method \
            self \
            macro \
            try \
            throw \
            catch \
            finally \
            import",
        );
        assert_eq!(
            tokens,
            vec![
                (BoolLiteral, 4),
                (Whitespace, 1),
                (BoolLiteral, 5),
                (Whitespace, 1),
                (NilLiteral, 3),
                (Whitespace, 1),
                (DefKw, 3),
                (Whitespace, 1),
                (ConstKw, 5),
                (Whitespace, 1),
                (LetKw, 3),
                (Whitespace, 1),
                (SeteKw, 4),
                (Whitespace, 1),
                (DefnKw, 4),
                (Whitespace, 1),
                (FnKw, 2),
                (Whitespace, 1),
                (ReturnKw, 6),
                (Whitespace, 1),
                (IfKw, 2),
                (Whitespace, 1),
                (WhenKw, 4),
                (Whitespace, 1),
                (CondKw, 4),
                (Whitespace, 1),
                (DoKw, 2),
                (Whitespace, 1),
                (SwitchKw, 6),
                (Whitespace, 1),
                (ForKw, 3),
                (Whitespace, 1),
                (WhileKw, 5),
                (Whitespace, 1),
                (BreakKw, 5),
                (Whitespace, 1),
                (ContinueKw, 8),
                (Whitespace, 1),
                (EnumKw, 4),
                (Whitespace, 1),
                (StructKw, 6),
                (Whitespace, 1),
                (MethodKw, 6),
                (Whitespace, 1),
                (SelfKw, 4),
                (Whitespace, 1),
                (MacroKw, 5),
                (Whitespace, 1),
                (TryKw, 3),
                (Whitespace, 1),
                (ThrowKw, 5),
                (Whitespace, 1),
                (CatchKw, 5),
                (Whitespace, 1),
                (FinallyKw, 7),
                (Whitespace, 1),
                (ImportKw, 6),
            ]
        );
    }

    #[test]
    fn test_invalid_char() {
        let (tokens, errors) = lex_test("a☕b");
        assert_eq!(tokens, vec![(Symbol, 1), (Unknown, 3), (Symbol, 1)]);
        assert_err(errors, ParseError::UnknownChar('☕'), 1, 3);
    }
}
