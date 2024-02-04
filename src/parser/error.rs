use crate::parser::span::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseError {
    // Lexer errors
    UnknownChar(char),
    UnclosedString,
    UnclosedRegex,
    InvalidEscapeSequence,
    InvalidSlice,
    InvalidNumber,

    // Parser errors
    ExpectedToken(String),
    UnexpectedToken(String),
}

impl ParseError {
    pub fn message(&self) -> String {
        match self {
            ParseError::UnknownChar(ch) => {
                format!("unknown character {} (codepoint {}).", ch, *ch as usize)
            }
            ParseError::UnclosedString => "unclosed string.".into(),
            ParseError::UnclosedRegex => "unclosed regex.".into(),
            ParseError::InvalidEscapeSequence => "unknown escape sequence.".into(),
            ParseError::InvalidSlice => "invalid slice.".into(),
            ParseError::InvalidNumber => "invalid number.".into(),

            // Parser errors
            ParseError::ExpectedToken(ref exp) => {
                format!("expected `{}`.", exp)
            }
            ParseError::UnexpectedToken(ref exp) => {
                format!("unexpected `{}`.", exp)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseErrorWithLocation {
    pub span: Span,
    pub error: ParseError,
}

impl ParseErrorWithLocation {
    pub fn new(span: Span, error: ParseError) -> ParseErrorWithLocation {
        ParseErrorWithLocation { span, error }
    }
}
