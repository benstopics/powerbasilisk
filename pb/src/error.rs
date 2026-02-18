use std::fmt;

#[derive(Debug, Clone)]
pub struct PbError {
    pub kind: ErrorKind,
    pub message: String,
    pub file: Option<String>,
    pub line: Option<usize>,
    pub col: Option<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    Lexer,
    Preprocessor,
    Parser,
    Runtime,
    Io,
}

impl PbError {
    pub fn lexer(msg: impl Into<String>, file: Option<&str>, line: usize, col: usize) -> Self {
        PbError {
            kind: ErrorKind::Lexer,
            message: msg.into(),
            file: file.map(|s| s.to_string()),
            line: Some(line),
            col: Some(col),
        }
    }

    pub fn preprocessor(msg: impl Into<String>, file: Option<&str>, line: usize) -> Self {
        PbError {
            kind: ErrorKind::Preprocessor,
            message: msg.into(),
            file: file.map(|s| s.to_string()),
            line: Some(line),
            col: None,
        }
    }

    pub fn parser(msg: impl Into<String>, file: Option<&str>, line: usize) -> Self {
        PbError {
            kind: ErrorKind::Parser,
            message: msg.into(),
            file: file.map(|s| s.to_string()),
            line: Some(line),
            col: None,
        }
    }

    pub fn runtime(msg: impl Into<String>) -> Self {
        PbError {
            kind: ErrorKind::Runtime,
            message: msg.into(),
            file: None,
            line: None,
            col: None,
        }
    }

    pub fn io(msg: impl Into<String>) -> Self {
        PbError {
            kind: ErrorKind::Io,
            message: msg.into(),
            file: None,
            line: None,
            col: None,
        }
    }
}

impl fmt::Display for PbError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind_str = match self.kind {
            ErrorKind::Lexer => "Lexer error",
            ErrorKind::Preprocessor => "Preprocessor error",
            ErrorKind::Parser => "Parse error",
            ErrorKind::Runtime => "Runtime error",
            ErrorKind::Io => "I/O error",
        };
        write!(f, "{}", kind_str)?;
        if let Some(ref file) = self.file {
            write!(f, " in {}", file)?;
        }
        if let Some(line) = self.line {
            write!(f, " at line {}", line)?;
            if let Some(col) = self.col {
                write!(f, ":{}", col)?;
            }
        }
        write!(f, ": {}", self.message)
    }
}

impl std::error::Error for PbError {}

pub type PbResult<T> = Result<T, PbError>;
