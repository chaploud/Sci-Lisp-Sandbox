#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
#[repr(u8)]
pub enum TokenKind {
    // literals
    StringLiteral,  // "string"
    RegexLiteral,   // #"[a-z]+"
    True,           // true
    False,          // false
    Nil,            // nil
    I64Literal,     // -999
    F64Literal,     // -3.14e-15, nan, inf, -inf, -0.0
    C64Literal,     // -1.2+3.4j, 1j, -10+5j
    KeywordLiteral, // :keyword
    Symbol,         // identifier

    // type identifier
    TypeId, // #i64, #f64, #str,...

    // keywords
    // define variable
    DefKw,   // def
    ConstKw, // const
    LetKw,   // let
    SeteKw,  // set!

    // define function
    DefnKw,   // defn
    FnKw,     // fn
    ReturnKw, // return

    // control flow
    IfKw,       // if
    WhenKw,     // when
    CondKw,     // cond
    DoKw,       // do
    SwitchKw,   // switch
    ForKw,      // for
    WhileKw,    // while
    BreakKw,    // break
    ContinueKw, // continue

    // use defined type
    EnumKw,   // enum
    StructKw, // struct
    MethodKw, // method
    SelfKw,   // self
    MacroKw,  // macro

    // exception handling
    TryKw,     // try
    ThrowKw,   // throw
    CatchKw,   // catch
    FinallyKw, // finally

    // module
    ImportKw,

    // Mark
    Underscore,      // _
    Quote,           // '
    SyntaxQuote,     // `
    Unquote,         // ~
    UnquoteSplicing, // ~@
    Splicing,        // @
    Sharp,           // #
    Dot,             // .
    Pipe,            // |
    Slash,           // /
    And,             // &
    RightArrow,      // =>

    // brackets
    ListClose,   // )
    VectorClose, // ]
    RBrace,      // }
    MapClose,    // }
    SetClose,    // }

    // collections
    ListOpen,   // (
    VectorOpen, // [
    MapOpen,    // {
    SetOpen,    // #{

    // trivial
    Whitespace,  // , is treated as whitespace
    LineComment, // ; comment

    // Unknown Character
    Unknown,

    // End of File
    Eof,

    // Syntax tree nodes
    SourceFile,

    // Expressions
    BoolLiteralExpr,
    NilLiteralExpr,
    I64LiteralExpr,
    F64LiteralExpr,
    C64LiteralExpr,
    StringLiteralExpr,
    RegexLiteralExpr,
    KeywordLiteralExpr,
    SymbolExpr,
    TypeIdExpr,
    UnderScoreExpr,
    QuoteExpr,
    SyntaxQuoteExpr,
    UnquoteExpr,
    UnquoteSplicingExpr,
    SplicingExpr,
    DotExpr,
    PipeExpr,
    SlashExpr,
    AndExpr,
    RightArrowExpr,
    ListExpr,
    VectorExpr,
    MapExpr,
    SetExpr,
    DefExpr,
    ConstExpr,
    LetExpr,
    SeteExpr,
    DefnExpr,
    FnExpr,
    ReturnExpr,
    IfExpr,
    WhenExpr,
    CondExpr,
    DoExpr,
    SwitchExpr,
    ForExpr,
    WhileExpr,
    BreakExpr,
    ContinueExpr,
    EnumExpr,
    StructExpr,
    MethodExpr,
    SelfExpr,
    MacroExpr,
    TryExpr,
    ThrowExpr,
    CatchExpr,
    FinallyExpr,
    ImportExpr,

    Error,
}

pub const LAST_TOKEN: TokenKind = TokenKind::Eof;

impl TokenKind {
    pub fn is_trivial(self) -> bool {
        matches!(self, TokenKind::Whitespace | TokenKind::LineComment)
    }

    pub fn is_eof(self) -> bool {
        self == TokenKind::Eof
    }
}

#[derive(Debug, Clone)]
pub struct TokenSet(u128);

impl TokenSet {
    pub const fn new(kinds: &[TokenKind]) -> TokenSet {
        let mut value = 0;
        let mut i = 0;

        while i < kinds.len() {
            i += 1;
        }

        TokenSet(value)
    }

    pub const fn union(&self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    pub fn contains(&self, kind: TokenKind) -> bool {
        (self.0 & (1 << kind as u8)) != 0
    }
}
