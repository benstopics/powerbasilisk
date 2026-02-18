/// PowerBASIC type names
#[derive(Debug, Clone, PartialEq)]
pub enum PbType {
    Long,
    Integer,
    Double,
    Single,
    String,
    FixedString(usize), // STRING * N
    Quad,
    Dword,
    Ext,
    Cur,       // CURRENCY
    Byte,
    Word,
    UserDefined(String), // TYPE name
    Variant,   // untyped / default
}

impl PbType {
    pub fn suffix(&self) -> &str {
        match self {
            PbType::Long => "&",
            PbType::Integer => "%",
            PbType::Double => "#",
            PbType::Single => "!",
            PbType::String | PbType::FixedString(_) => "$",
            PbType::Quad => "&&",
            PbType::Dword => "???",
            PbType::Ext => "##",
            PbType::Cur => "@",
            _ => "",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<TopLevel>,
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    GlobalDecl(VarDecl),
    GlobalDeclList(Vec<VarDecl>),
    DimDecl(DimStatement),
    ConstDecl(ConstDecl),
    TypeDecl(TypeDecl),
    SubDecl(SubDecl),
    FunctionDecl(FunctionDecl),
    DeclareStmt(DeclareStmt), // DECLARE SUB/FUNCTION — skip at runtime
    Statement(Statement),     // top-level statements (before PBMAIN)
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,        // canonical uppercase with suffix
    pub pb_type: PbType,
    pub is_array: bool,      // declared with ()
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct DimStatement {
    pub scope: DimScope,
    pub name: String,
    pub pb_type: PbType,
    pub bounds: Vec<DimBound>,  // one per dimension
    pub line: usize,
    pub is_redim: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DimScope {
    Local,
    Global,
    Static,
    Dim,  // plain DIM inside a sub/function
}

#[derive(Debug, Clone)]
pub struct DimBound {
    pub lower: Expr,
    pub upper: Expr,
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub name: String,
    pub value: Expr,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: String,
    pub fields: Vec<TypeField>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct TypeField {
    pub name: String,
    pub pb_type: PbType,
}

#[derive(Debug, Clone)]
pub struct SubDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Vec<Statement>,
    pub alias: Option<String>,
    pub export: bool,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: PbType,
    pub body: Vec<Statement>,
    pub alias: Option<String>,
    pub export: bool,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub pb_type: PbType,
    pub is_byval: bool,
    pub is_optional: bool,
    pub is_array: bool,
}

#[derive(Debug, Clone)]
pub struct DeclareStmt {
    pub name: String,
    pub is_function: bool,       // true=FUNCTION, false=SUB
    pub params: Vec<Param>,
    pub return_type: PbType,     // Void for SUB
    pub lib: Option<String>,     // LIB "dll.dll"
    pub alias: Option<String>,   // ALIAS "ExternalName"
    pub line: usize,
}

// Statements
#[derive(Debug, Clone)]
pub enum Statement {
    Assign(AssignStmt),
    Call(CallStmt),
    If(IfStmt),
    For(ForStmt),
    DoLoop(DoLoopStmt),
    WhileWend(WhileWendStmt),
    SelectCase(SelectCaseStmt),
    Print(PrintStmt),
    PrintFile(PrintFileStmt),
    Open(OpenStmt),
    Close(CloseStmt),
    Dim(DimStatement),
    Redim(DimStatement),
    Incr(IncrDecrStmt),
    Decr(IncrDecrStmt),
    ExitSub,
    ExitFunction,
    ExitFor,
    ExitDo,
    ExitSelect,
    FunctionReturn(FunctionReturnStmt),
    GoSub(String),
    Return,
    GoTo(String),
    Label(String),
    OnErrorGoto(String),
    OnErrorGotoZero,
    ResumeNext,
    Iterate(IterateTarget),
    InputFile(InputFileStmt),
    LineInputFile(LineInputFileStmt),
    Kill(Expr),
    Block(Vec<Statement>), // multiple statements from one parse (e.g. LOCAL a, b)
    Noop, // for empty stubs, no-ops
}

#[derive(Debug, Clone)]
pub enum IterateTarget {
    For,
    Do,
}

#[derive(Debug, Clone)]
pub struct AssignStmt {
    pub target: Expr, // variable, array element, or type member
    pub value: Expr,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct CallStmt {
    pub name: String,
    pub args: Vec<Expr>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_body: Vec<Statement>,
    pub elseif_clauses: Vec<ElseIfClause>,
    pub else_body: Vec<Statement>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct ElseIfClause {
    pub condition: Expr,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub var: String,
    pub start: Expr,
    pub end: Expr,
    pub step: Option<Expr>,
    pub body: Vec<Statement>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct DoLoopStmt {
    pub condition: Option<Expr>,
    pub is_while: bool, // true=WHILE, false=UNTIL
    pub is_pre_test: bool, // condition at DO vs LOOP
    pub body: Vec<Statement>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct WhileWendStmt {
    pub condition: Expr,
    pub body: Vec<Statement>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct SelectCaseStmt {
    pub expr: Expr,
    pub cases: Vec<CaseClause>,
    pub else_body: Vec<Statement>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct CaseClause {
    pub patterns: Vec<CasePattern>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum CasePattern {
    Value(Expr),
    Range(Expr, Expr),
    Is(CaseOp, Expr),
}

#[derive(Debug, Clone)]
pub enum CaseOp {
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,
}

#[derive(Debug, Clone)]
pub struct PrintStmt {
    pub args: Vec<Expr>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct PrintFileStmt {
    pub file_num: Expr,
    pub args: Vec<Expr>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct OpenStmt {
    pub filename: Expr,
    pub mode: OpenMode,
    pub file_num: Expr,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub enum OpenMode {
    Output,
    Append,
    Input,
}

#[derive(Debug, Clone)]
pub struct CloseStmt {
    pub file_num: Expr,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct InputFileStmt {
    pub file_num: Expr,
    pub vars: Vec<Expr>,  // variables to read into (Variable or ArrayAccess)
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct LineInputFileStmt {
    pub file_num: Expr,
    pub var: Expr,  // string variable to read into
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct IncrDecrStmt {
    pub target: Expr,
    pub amount: Option<Expr>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct FunctionReturnStmt {
    pub value: Expr,
    pub line: usize,
}

// Expressions
#[derive(Debug, Clone)]
pub enum Expr {
    IntegerLit(i64),
    FloatLit(f64),
    StringLit(String),
    Variable(String),
    ArrayAccess(String, Vec<Expr>),
    TypeMember(Box<Expr>, String),
    FunctionCall(String, Vec<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    PercentConst(String),
    Negate(Box<Expr>),
    Varptr(Box<Expr>),  // VARPTR(variable) or VARPTR(array(idx))
    ByvalOverride(Box<Expr>),  // BYVAL expr at call site — pass value, not address
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Mod,
    Pow,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
    Xor,
    StrConcat, // &
}
