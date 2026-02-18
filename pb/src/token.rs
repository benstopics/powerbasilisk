#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    // Identifiers and constants
    Identifier(String),       // e.g. "Cash", "CoName$", "i&"
    PercentConstant(String),  // e.g. "%IDYES", "%TRUE"

    // Keywords
    And,
    As,
    Byref,
    Byval,
    Call,
    Case,
    Close,
    Cur,       // CUR type
    Decr,
    Dim,
    Do,
    Double,
    Dword,
    Else,
    ElseIf,
    End,
    Exit,
    Ext,
    For,
    Function,
    Global,
    GoSub,
    GoTo,
    If,
    Incr,
    Integer,
    Is,
    Iterate,
    Lbound,
    Local,
    Long,
    Loop,
    Mod,
    Next,
    Not,
    On,
    Open,
    Optional,
    Or,
    Output,
    Append,
    Input, // the keyword INPUT (for OPEN FOR INPUT)
    Print,
    Quad,
    Redim,
    Register,
    Resume,
    Return,
    Select,
    Single,
    Static,
    Step,
    String_,
    Sub,
    Then,
    To,
    Type,
    Ubound,
    Until,
    Wend,
    While,
    Xor,

    // Preprocessor directives (kept as tokens after preprocessing)
    // These are mostly consumed by the preprocessor, but INCLUDE may appear
    HashInclude,
    HashCompile,
    HashDim,
    HashIf,
    HashElseIf,
    HashElse,
    HashEndIf,
    HashRegister,
    HashDebug,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Backslash, // integer division
    Caret,     // exponent
    Eq,        // =
    Neq,       // <>
    Lt,
    Gt,
    Lte,       // <=
    Gte,       // >=
    Ampersand, // & (string concat)

    // Delimiters
    LParen,
    RParen,
    Comma,
    Semicolon,
    Colon,     // : (label marker / statement separator)
    Hash,      // # (file number prefix)
    Dot,
    DotDot,    // .. (range in SELECT CASE)

    // Special
    Underscore, // line continuation (shouldn't appear in final tokens)
    Eol,        // end of logical line
    Eof,

    // Declare keyword
    Declare,

    // DLL export/import keywords
    Export,
    Alias,
    Lib,
    Callback,

    // Additional keywords for completeness
    Error,
    Macro,
    EndMacro,
    Replace,   // REPLACE statement
    Remove,    // REMOVE$ or REMOVE
    Shared,
    ReDimPreserve, // not a single token, handled in parser

    // FREEFILE
    FreeFile,
}

impl Token {
    /// Try to match a keyword (case-insensitive). Returns None if not a keyword.
    pub fn from_keyword(word: &str) -> Option<Token> {
        match word.to_uppercase().as_str() {
            "ALIAS" => Some(Token::Alias),
            "AND" => Some(Token::And),
            "AS" => Some(Token::As),
            "BYREF" => Some(Token::Byref),
            "BYVAL" => Some(Token::Byval),
            "CALL" => Some(Token::Call),
            "CASE" => Some(Token::Case),
            "CLOSE" => Some(Token::Close),
            "CUR" => Some(Token::Cur),
            "DECR" => Some(Token::Decr),
            "DIM" => Some(Token::Dim),
            "CALLBACK" => Some(Token::Callback),
            "DECLARE" => Some(Token::Declare),
            "DO" => Some(Token::Do),
            "DOUBLE" => Some(Token::Double),
            "DWORD" => Some(Token::Dword),
            "ELSE" => Some(Token::Else),
            "ELSEIF" => Some(Token::ElseIf),
            "END" => Some(Token::End),
            "ERROR" => Some(Token::Error),
            "EXIT" => Some(Token::Exit),
            "EXPORT" => Some(Token::Export),
            "EXT" => Some(Token::Ext),
            "FOR" => Some(Token::For),
            "FREEFILE" => Some(Token::FreeFile),
            "FUNCTION" => Some(Token::Function),
            "GLOBAL" => Some(Token::Global),
            "GOSUB" => Some(Token::GoSub),
            "GOTO" => Some(Token::GoTo),
            "IF" => Some(Token::If),
            "INCR" => Some(Token::Incr),
            "INPUT" => Some(Token::Input),
            "INTEGER" => Some(Token::Integer),
            "IS" => Some(Token::Is),
            "ITERATE" => Some(Token::Iterate),
            "LBOUND" => Some(Token::Lbound),
            "LIB" => Some(Token::Lib),
            "LOCAL" => Some(Token::Local),
            "LONG" => Some(Token::Long),
            "LOOP" => Some(Token::Loop),
            "MACRO" => Some(Token::Macro),
            "MOD" => Some(Token::Mod),
            "NEXT" => Some(Token::Next),
            "NOT" => Some(Token::Not),
            "ON" => Some(Token::On),
            "OPEN" => Some(Token::Open),
            "OPTIONAL" => Some(Token::Optional),
            "OR" => Some(Token::Or),
            "OUTPUT" => Some(Token::Output),
            "APPEND" => Some(Token::Append),
            "PRINT" => Some(Token::Print),
            "QUAD" => Some(Token::Quad),
            "REDIM" => Some(Token::Redim),
            "REGISTER" => Some(Token::Register),
            "REMOVE" => Some(Token::Remove),
            "REPLACE" => Some(Token::Replace),
            "RESUME" => Some(Token::Resume),
            "RETURN" => Some(Token::Return),
            "SELECT" => Some(Token::Select),
            "SHARED" => Some(Token::Shared),
            "SINGLE" => Some(Token::Single),
            "STATIC" => Some(Token::Static),
            "STEP" => Some(Token::Step),
            "STRING" => Some(Token::String_),
            "SUB" => Some(Token::Sub),
            "THEN" => Some(Token::Then),
            "TO" => Some(Token::To),
            "TYPE" => Some(Token::Type),
            "UBOUND" => Some(Token::Ubound),
            "UNTIL" => Some(Token::Until),
            "WEND" => Some(Token::Wend),
            "WHILE" => Some(Token::While),
            "XOR" => Some(Token::Xor),
            _ => None,
        }
    }

    pub fn is_type_keyword(&self) -> bool {
        matches!(
            self,
            Token::Long
                | Token::Integer
                | Token::Double
                | Token::Single
                | Token::String_
                | Token::Quad
                | Token::Dword
                | Token::Ext
                | Token::Cur
        )
    }
}

/// A token with source location info
#[derive(Debug, Clone)]
pub struct Located {
    pub token: Token,
    pub line: usize,
    pub col: usize,
    pub file: Option<String>,
}
