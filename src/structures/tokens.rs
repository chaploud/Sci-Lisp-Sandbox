/* structures/tokens.rs */

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum TokenKind {
    // Literals
    STRING,
    REGEX,
    I64,
    F64,
    C64,
    BOOL,
    KEYWORD,
    SYMBOL,
    LIST,
    VECTOR,
    MAP,
    SET,
    ARRAY,
    SLICE,
    // Spceial Forms
    DEF_KW,
    CONST_KW,
    LET_KW,
    SETE_KW,
    DEFN_KW,
    RETURN_KW,
    FN_KW,
    WHEN_KW,
    DO_KW,
    COND_KW,
    SWITCH_KW,
    FOR_KW,
    WHILE_KW,
    BREAK_KW,
    CONTINUE_KW,
    ENUM_KW,
    STRUCT_KW,
    METHOD_KW,
    SELF_KW,
    MACRO_KW,
    TRY_KW,
    THROW_KW,
    CATCH_KW,
    FINALLY_KW,
    TYPEDEF_KW,
    IMPORT_KW,
    EXPORT_KW,
    // Special Marks
    DOT,
    SLASH,
    AND,
    // Reader Macros
    QUOTE,
    SYNTAX_QUOTE,
    UNQUOTE,
    UNQUOTE_SPLICING,
    SPLICING,
    // Type Annotation
    TYPE_ANNOTATION,
    RIGHT_ARROW,
    // Auto GenSym
    AUTO_GENSYM,
    // Semantic
    CALL,
    MODULE_PATH,
    INSTANCE,
}
