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
        assert!(token < TokenKind::EOF);
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
        } else if is_digit_or_minus(ch) {
            self.read_number(false)
        } else if self.is_line_comment() {
            self.read_line_comment()
        } else if is_sharp(ch) {
            self.read_start_with_sharp()
        } else if is_symbol_start(ch) {
            self.read_symbol()
        } else if is_keyword_start(ch) {
            self.read_keyword()
        } else if is_double_quote(ch) {
            self.read_string()
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

    fn read_start_with_sharp(&mut self) -> TokenKind {
        let start = self.offset();
        let ch = self.curr().expect("missing char");
        self.eat_char();
        let span = self.span_from(start);
        let next = self.lookahead();

        match next {
            Some('"') => self.read_regex(),
            Some('{') => self.read_map(),
            _ => {
                if is_symbol(next) {
                    self.read_type_id()
                } else {
                    self.report_error_at(
                        ParseError::ExpectedToken("'\"', '{' or symbol".to_string()),
                        span,
                    );
                    Error
                }
            }
        }
    }

    fn read_symbol(&mut self) -> TokenKind {
        let value = self.read_symbol_as_string();
        if value == "nan" || value == "inf" || value == "-inf" {
            return F64Literal;
        }

        let lookup = self.keywords.get(&value[..]).cloned();

        if let Some(tok_type) = lookup {
            tok_type
        } else if value == "_" {
            Underscore
        } else {
            Symbol
        }
    }

    fn read_type_id(&mut self) -> TokenKind {
        let value = self.read_symbol_as_string();
        TypeId
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

    fn read_regex(&mut self) -> TokenKind {
        let mut start = self.offset();

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

    // TODO:
    // '
    // keyword
    // quoted symbol
    // quoted vector
    // quoted map
    // quoted set
    // TODO:
    fn read_quote(&mut self) -> TokenKind {}

    // TODO:
    fn read_operator(&mut self) -> TokenKind {
        let ch = self.curr().unwrap();
        self.eat_char();

        let nch = self.curr().unwrap_or('x');
        let nnch = self.lookahead().unwrap_or('x');

        match ch {
            '(' => LParen,
            ')' => RParen,
            '[' => LBracket,
            ']' => RBracket,
            '{' => LBrace,
            '}' => RBrace,
            '&' => And,
            ';' => LineComment,
            '.' => Dot,
            _ => {
                unreachable!()
            }
        }
    }

    fn read_number(&mut self, second_time: bool) -> TokenKind {
        let minus = if self.curr() == Some('-') {
            let next = self.lookahead();
            if is_digit(next) {
                self.eat_char();
                true
            } else {
                return self.read_symbol();
            }
        } else {
            false
        };

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

        if base == 10 && self.curr() == Some('j') {
            self.eat_char();
            return C64Literal;
        }

        if base == 10 && self.curr() == Some('.') && is_digit(self.lookahead()) {
            return self.read_number_as_float(second_time);
        }

        if base == 10
            && (self.curr() == Some('+') || self.curr() == Some('-'))
            && is_digit(self.lookahead())
        {
            return self.read_number_as_complex(second_time);
        }

        if second_time {
            let span = self.span_from(self.offset());
            self.report_error_at(ParseError::InvalidNumber, span);
            return I64Literal;
        }

        I64Literal
    }

    fn read_number_as_float(&mut self, second_time: bool) -> TokenKind {
        self.eat_char();

        self.read_digits(10);

        if self.curr() == Some('e') || self.curr() == Some('E') {
            self.eat_char();

            if self.curr() == Some('+') || self.curr() == Some('-') {
                self.eat_char();
            }

            self.read_digits(10);
        }

        if second_time && self.curr() != Some('j') {
            let span = self.span_from(self.offset());
            self.report_error_at(ParseError::InvalidNumber, span);
            return F64Literal;
        }

        if self.curr() == Some('j') || self.curr() == Some('+') || self.curr() == Some('-') {
            return self.read_number_as_complex(second_time);
        }

        F64Literal
    }

    fn read_number_as_complex(&mut self, second_time: bool) -> TokenKind {
        if self.curr() == Some('j') {
            self.eat_char();
            return C64Literal;
        }

        if !second_time {
            self.eat_char();
            self.read_number(true);
            return C64Literal;
        }

        C64Literal
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.offset() - start)
    }

    fn read_digits(&mut self, base: u32) {
        while is_digit_or_underscore(self.curr(), base) {
            self.eat_char();
        }
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
        .unwrap_or(false)
}

fn is_newline(ch: Option<char>) -> bool {
    ch == Some('\n')
}

fn is_double_quote(ch: Option<char>) -> bool {
    ch == Some('\"')
}

fn is_symbol_start(ch: Option<char>) -> bool {
    match ch {
        Some(ch) => {
            (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || "!$%-=^+*<>?_\\#".contains(ch)
        }
        _ => false,
    }
}

fn is_keyword_start(ch: Option<char>) -> bool {
    ch == Some(':')
}

fn is_symbol(ch: Option<char>) -> bool {
    is_symbol_start(ch) || is_digit(ch)
}

fn is_sharp(ch: Option<char>) -> bool {
    ch == Some('#')
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

fn keywords_in_map() -> HashMap<&'static str, TokenKind> {
    let mut keywords = HashMap::with_capacity(30);

    keywords.insert("true", True);
    keywords.insert("false", False);
    keywords.insert("nil", Nil);
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
        let tokens = lex_success("1 2\n0123 10");
        assert_eq!(
            tokens,
            vec![
                (INT_LITERAL, 1),
                (WHITESPACE, 1),
                (INT_LITERAL, 1),
                (WHITESPACE, 1),
                (INT_LITERAL, 4),
                (WHITESPACE, 1),
                (INT_LITERAL, 2),
            ]
        );

        let tokens = lex_success("12u8 300u8 1_000 1__1");
        assert_eq!(
            tokens,
            vec![
                (INT_LITERAL, 4),
                (WHITESPACE, 1),
                (INT_LITERAL, 5),
                (WHITESPACE, 1),
                (INT_LITERAL, 5),
                (WHITESPACE, 1),
                (INT_LITERAL, 4),
            ]
        );
    }

    #[test]
    fn test_read_numbers_with_suffix() {
        let tokens = lex_success("1i32 2u8 3i64");
        assert_eq!(
            tokens,
            vec![
                (INT_LITERAL, 4),
                (WHITESPACE, 1),
                (INT_LITERAL, 3),
                (WHITESPACE, 1),
                (INT_LITERAL, 4),
            ]
        );
    }

    #[test]
    fn test_skip_single_line_comment() {
        let tokens = lex_success("//test\n1");
        assert_eq!(
            tokens,
            vec![(LINE_COMMENT, 6), (WHITESPACE, 1), (INT_LITERAL, 1)]
        );
    }

    #[test]
    fn test_unfinished_line_comment() {
        let tokens = lex_success("//abc");
        assert_eq!(tokens, &[(LINE_COMMENT, 5)]);
    }

    #[test]
    fn test_skip_multi_comment() {
        let tokens = lex_success("/*test*/1");
        assert_eq!(tokens, &[(MULTILINE_COMMENT, 8), (INT_LITERAL, 1)]);
    }

    #[test]
    fn test_unfinished_multi_comment() {
        let (tokens, errors) = lex_test("/*test");
        assert_eq!(tokens, &[(MULTILINE_COMMENT, 6)]);
        assert_err(errors, ParseError::UnclosedComment, 0, 6);

        let (tokens, errors) = lex_test("1/*test");
        assert_eq!(tokens, &[(INT_LITERAL, 1), (MULTILINE_COMMENT, 6)]);
        assert_err(errors, ParseError::UnclosedComment, 1, 6);
    }

    #[test]
    fn test_read_identifier() {
        let tokens = lex_success("abc ident test");
        assert_eq!(
            tokens,
            vec![
                (IDENTIFIER, 3),
                (WHITESPACE, 1),
                (IDENTIFIER, 5),
                (WHITESPACE, 1),
                (IDENTIFIER, 4),
            ]
        );
    }

    #[test]
    fn test_code_with_spaces() {
        let tokens = lex_success("1 2 3");
        assert_eq!(
            tokens,
            vec![
                (INT_LITERAL, 1),
                (WHITESPACE, 1),
                (INT_LITERAL, 1),
                (WHITESPACE, 1),
                (INT_LITERAL, 1)
            ]
        );
    }

    #[test]
    fn test_float_numbers() {
        let tokens = lex_success("1f32 1.0 0.1f32 1.3f64 4f64");
        assert_eq!(
            tokens,
            vec![
                (INT_LITERAL, 4),
                (WHITESPACE, 1),
                (FLOAT_LITERAL, 3),
                (WHITESPACE, 1),
                (FLOAT_LITERAL, 6),
                (WHITESPACE, 1),
                (FLOAT_LITERAL, 6),
                (WHITESPACE, 1),
                (INT_LITERAL, 4),
            ]
        );
    }

    #[test]
    fn test_float_scientific_notation() {
        let tokens = lex_success("1.0e1 1.0E1 1.0e+1 1.0e-1");
        assert_eq!(
            tokens,
            vec![
                (FLOAT_LITERAL, 5),
                (WHITESPACE, 1),
                (FLOAT_LITERAL, 5),
                (WHITESPACE, 1),
                (FLOAT_LITERAL, 6),
                (WHITESPACE, 1),
                (FLOAT_LITERAL, 6),
            ]
        );
    }

    #[test]
    fn test_hex_numbers() {
        let tokens = lex_success("0x1 0x2i64 0xABCDEF 0xB1i64");
        assert_eq!(
            tokens,
            vec![
                (INT_LITERAL, 3),
                (WHITESPACE, 1),
                (INT_LITERAL, 6),
                (WHITESPACE, 1),
                (INT_LITERAL, 8),
                (WHITESPACE, 1),
                (INT_LITERAL, 7),
            ]
        );
    }

    #[test]
    fn test_code_with_newlines() {
        let tokens = lex_success("1\n2\n3");
        assert_eq!(
            tokens,
            vec![
                (INT_LITERAL, 1),
                (WHITESPACE, 1),
                (INT_LITERAL, 1),
                (WHITESPACE, 1),
                (INT_LITERAL, 1)
            ]
        );
    }

    #[test]
    fn test_code_with_tabs() {
        let tokens = lex_success("1\t2\t3");
        assert_eq!(
            tokens,
            vec![
                (INT_LITERAL, 1),
                (WHITESPACE, 1),
                (INT_LITERAL, 1),
                (WHITESPACE, 1),
                (INT_LITERAL, 1)
            ]
        );
    }

    #[test]
    fn test_string_with_newline() {
        let tokens = lex_success("\"abc\ndef\"");
        assert_eq!(tokens, vec![(STRING_LITERAL, 9),]);
    }

    #[test]
    fn test_escape_sequences() {
        let tokens = lex_success("\"\\\"\"");
        assert_eq!(tokens, vec![(STRING_LITERAL, 4)]);

        let tokens = lex_success("\"\\$\"");
        assert_eq!(tokens, &[(STRING_LITERAL, 4)]);

        let tokens = lex_success("\"\\\'\"");
        assert_eq!(tokens, &[(STRING_LITERAL, 4)]);

        let tokens = lex_success("\"\\t\"");
        assert_eq!(tokens, &[(STRING_LITERAL, 4)]);

        let tokens = lex_success("\"\\n\"");
        assert_eq!(tokens, &[(STRING_LITERAL, 4)]);

        let tokens = lex_success("\"\\r\"");
        assert_eq!(tokens, &[(STRING_LITERAL, 4)]);

        let tokens = lex_success("\"\\\\\"");
        assert_eq!(tokens, &[(STRING_LITERAL, 4)]);

        let (tokens, errors) = lex_test("\"\\");
        assert_eq!(tokens, vec![(STRING_LITERAL, 2)]);
        assert_err(errors, ParseError::UnclosedString, 0, 2);
    }

    #[test]
    fn test_unclosed_string() {
        let (tokens, errors) = lex_test("\"abc");
        assert_eq!(tokens, &[(STRING_LITERAL, 4)]);
        assert_err(errors, ParseError::UnclosedString, 0, 4);
    }

    #[test]
    fn test_unclosed_char() {
        let (tokens, errors) = lex_test("'a");
        assert_eq!(tokens, &[(CHAR_LITERAL, 2)]);
        assert_err(errors, ParseError::UnclosedChar, 0, 2);

        let (tokens, errors) = lex_test("'\\");
        assert_eq!(tokens, &[(CHAR_LITERAL, 2)]);
        assert_err(errors, ParseError::UnclosedChar, 0, 2);

        let (tokens, errors) = lex_test("'\\n");
        assert_eq!(tokens, &[(CHAR_LITERAL, 3)]);
        assert_err(errors, ParseError::UnclosedChar, 0, 3);

        let tokens = lex_success("'ab'");
        assert_eq!(tokens, &[(CHAR_LITERAL, 4)]);

        let (tokens, errors) = lex_test("'");
        assert_eq!(tokens, &[(CHAR_LITERAL, 1)]);
        assert_err(errors, ParseError::UnclosedChar, 0, 1);
    }

    #[test]
    fn test_string() {
        let tokens = lex_success("\"abc\"");
        assert_eq!(tokens, &[(STRING_LITERAL, 5)]);
    }

    #[test]
    fn test_keywords() {
        let tokens = lex_success("fn let while if else match");
        assert_eq!(
            tokens,
            vec![
                (FN_KW, 2),
                (WHITESPACE, 1),
                (LET_KW, 3),
                (WHITESPACE, 1),
                (WHILE_KW, 5),
                (WHITESPACE, 1),
                (IF_KW, 2),
                (WHITESPACE, 1),
                (ELSE_KW, 4),
                (WHITESPACE, 1),
                (MATCH_KW, 5),
            ]
        );

        let tokens = lex_success("self class super mod");
        assert_eq!(
            tokens,
            vec![
                (SELF_KW, 4),
                (WHITESPACE, 1),
                (CLASS_KW, 5),
                (WHITESPACE, 1),
                (SUPER_KW, 5),
                (WHITESPACE, 1),
                (MOD_KW, 3),
            ]
        );

        let tokens = lex_success("break continue return");
        assert_eq!(
            tokens,
            vec![
                (BREAK_KW, 5),
                (WHITESPACE, 1),
                (CONTINUE_KW, 8),
                (WHITESPACE, 1),
                (RETURN_KW, 6),
            ]
        );

        let tokens = lex_success("type struct enum trait const");
        assert_eq!(
            tokens,
            vec![
                (TYPE_KW, 4),
                (WHITESPACE, 1),
                (STRUCT_KW, 6),
                (WHITESPACE, 1),
                (ENUM_KW, 4),
                (WHITESPACE, 1),
                (TRAIT_KW, 5),
                (WHITESPACE, 1),
                (CONST_KW, 5),
            ]
        );

        let tokens = lex_success("for in impl Self mut");
        assert_eq!(
            tokens,
            vec![
                (FOR_KW, 3),
                (WHITESPACE, 1),
                (IN_KW, 2),
                (WHITESPACE, 1),
                (IMPL_KW, 4),
                (WHITESPACE, 1),
                (UPCASE_SELF_KW, 4),
                (WHITESPACE, 1),
                (MUT_KW, 3),
            ]
        );
    }

    #[test]
    fn test_operators() {
        let tokens = lex_success("==-*/%.@...,");
        assert_eq!(
            tokens,
            vec![
                (EQ_EQ, 2),
                (SUB, 1),
                (MUL, 1),
                (DIV, 1),
                (MODULO, 1),
                (DOT, 1),
                (AT, 1),
                (DOT_DOT_DOT, 3),
                (COMMA, 1),
            ]
        );

        let tokens = lex_success("<=<>=><");
        assert_eq!(tokens, vec![(LE, 2), (LT, 1), (GE, 2), (GT, 1), (LT, 1),]);

        let tokens = lex_success("!=====!");
        assert_eq!(tokens, vec![(NOT_EQ_EQ, 3), (EQ_EQ_EQ, 3), (NOT, 1),]);

        let tokens = lex_success("!=!");
        assert_eq!(tokens, vec![(NOT_EQ, 2), (NOT, 1),]);

        let tokens = lex_success("=>->");
        assert_eq!(tokens, vec![(DOUBLE_ARROW, 2), (ARROW, 2),]);

        let tokens = lex_success(">><<>>>_::");
        assert_eq!(
            tokens,
            vec![
                (GT_GT, 2),
                (LT_LT, 2),
                (GT_GT_GT, 3),
                (UNDERSCORE, 1),
                (COLON_COLON, 2),
            ]
        );
    }

    #[test]
    fn test_invalid_char() {
        let (tokens, errors) = lex_test("a☕b");
        assert_eq!(tokens, vec![(IDENTIFIER, 1), (UNKNOWN, 3), (IDENTIFIER, 1)]);
        assert_err(errors, ParseError::UnknownChar('☕'), 1, 3);
    }

    #[test]
    fn test_string_template() {
        let tokens = lex_success(r#""1${a}2${b}3"{}"#);
        assert_eq!(
            dump_tokens(tokens),
            r#"TEMPLATE_LITERAL@0..4
IDENTIFIER@4..5
TEMPLATE_LITERAL@5..9
IDENTIFIER@9..10
TEMPLATE_END_LITERAL@10..13
L_BRACE@13..14
R_BRACE@14..15
"#
        );
    }
}
