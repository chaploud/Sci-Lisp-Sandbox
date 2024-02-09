#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum TokenKind {
    // literals
    STRING_LITERAL,
    I64_Literal,
    F64_Literal,
    C64_Literal,
    BoolLiteral,
    KeywordLiteral,
    Symbol,
    List,
    Vector,
}
