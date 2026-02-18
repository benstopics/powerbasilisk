use crate::ast::*;
use crate::error::{PbError, PbResult};
use crate::token::{Located, Token};

#[derive(Debug, Clone, Copy, PartialEq)]
enum BodyEnd {
    EndSub,
    EndFunction,
    EndIf,
    Else,
    ElseIf,
    Next,
    Loop,
    Wend,
    EndSelect,
    Case,
    CaseElse,
}

pub struct Parser {
    tokens: Vec<Located>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Located>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .map(|t| &t.token)
            .unwrap_or(&Token::Eof)
    }

    fn peek_located(&self) -> &Located {
        static EOF: Located = Located {
            token: Token::Eof,
            line: 0,
            col: 0,
            file: None,
        };
        self.tokens.get(self.pos).unwrap_or(&EOF)
    }

    fn current_line(&self) -> usize {
        self.peek_located().line
    }

    fn current_file(&self) -> Option<&str> {
        self.peek_located().file.as_deref()
    }

    fn advance(&mut self) -> &Token {
        let tok = self
            .tokens
            .get(self.pos)
            .map(|t| &t.token)
            .unwrap_or(&Token::Eof);
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        tok
    }

    fn expect(&mut self, expected: &Token) -> PbResult<()> {
        if self.peek() == expected {
            self.advance();
            Ok(())
        } else {
            Err(PbError::parser(
                format!("Expected {:?}, got {:?}", expected, self.peek()),
                self.current_file(),
                self.current_line(),
            ))
        }
    }

    fn skip_eol(&mut self) {
        while matches!(self.peek(), Token::Eol | Token::Colon) {
            self.advance();
        }
    }

    fn at_end(&self) -> bool {
        matches!(self.peek(), Token::Eof)
    }

    fn at_eol_or_eof(&self) -> bool {
        matches!(self.peek(), Token::Eol | Token::Eof | Token::Colon)
    }

    fn consume_to_eol(&mut self) {
        while !matches!(
            self.peek(),
            Token::Eol | Token::Eof | Token::Colon | Token::Else | Token::ElseIf
        ) {
            self.advance();
        }
        // Consume Eol but NOT Colon/Else/ElseIf — these are statement-level
        // boundaries that callers need to detect
        // (e.g., single-line IF: IF cond THEN stmt ELSE stmt)
        if self.peek() == &Token::Eol {
            self.advance();
        }
    }

    #[allow(dead_code)]
    fn is_identifier(&self, name: &str) -> bool {
        matches!(self.peek(), Token::Identifier(ref s) if s == name)
    }

    #[allow(dead_code)]
    fn peek_identifier(&self) -> Option<&str> {
        match self.peek() {
            Token::Identifier(s) => Some(s.as_str()),
            _ => None,
        }
    }

    fn consume_identifier(&mut self) -> PbResult<String> {
        match self.peek().clone() {
            Token::Identifier(s) => {
                self.advance();
                Ok(s)
            }
            _ => Err(PbError::parser(
                format!("Expected identifier, got {:?}", self.peek()),
                self.current_file(),
                self.current_line(),
            )),
        }
    }

    pub fn parse(&mut self) -> PbResult<Program> {
        let mut items = Vec::new();
        self.skip_eol();

        while !self.at_end() {
            let saved_pos = self.pos;
            match self.parse_top_level() {
                Ok(Some(item)) => items.push(item),
                Ok(None) => {} // consumed but no item (e.g., comment)
                Err(e) => {
                    eprintln!("Parse warning: {}", e);
                    self.consume_to_eol();
                }
            }
            self.skip_eol();
            // Safety: prevent infinite loop at top level
            if self.pos == saved_pos && !self.at_end() {
                self.advance();
            }
        }

        Ok(Program { items })
    }

    fn parse_top_level(&mut self) -> PbResult<Option<TopLevel>> {
        self.skip_eol();
        if self.at_end() {
            return Ok(None);
        }

        let line = self.current_line();

        match self.peek().clone() {
            Token::Global => self.parse_global_decl(),
            Token::Dim => {
                let stmt = self.parse_dim_statement(DimScope::Dim)?;
                Ok(Some(TopLevel::DimDecl(stmt)))
            }
            Token::Redim => {
                let stmt = self.parse_redim_statement()?;
                Ok(Some(TopLevel::DimDecl(stmt)))
            }
            Token::PercentConstant(_) => {
                let c = self.parse_percent_const_decl(line)?;
                Ok(Some(TopLevel::ConstDecl(c)))
            }
            Token::Type => {
                let td = self.parse_type_decl(line)?;
                Ok(Some(TopLevel::TypeDecl(td)))
            }
            Token::Sub => {
                let sd = self.parse_sub_decl()?;
                Ok(Some(TopLevel::SubDecl(sd)))
            }
            Token::Function => {
                let fd = self.parse_function_decl()?;
                Ok(Some(TopLevel::FunctionDecl(fd)))
            }
            Token::Declare => match self.parse_declare_stmt(line)? {
                Some(ds) => Ok(Some(TopLevel::DeclareStmt(ds))),
                None => Ok(None),
            },
            Token::HashInclude
            | Token::HashCompile
            | Token::HashDim
            | Token::HashRegister
            | Token::HashDebug => {
                self.consume_to_eol();
                Ok(None)
            }
            Token::Macro => {
                // MACRO name = body : END MACRO — skip to END MACRO
                self.advance();
                loop {
                    match self.peek() {
                        Token::EndMacro | Token::Eof => {
                            self.advance();
                            break;
                        }
                        _ => {
                            self.advance();
                        }
                    }
                }
                self.consume_to_eol();
                Ok(None)
            }
            Token::Callback => {
                self.advance();
                match self.peek() {
                    Token::Function => {
                        let fd = self.parse_function_decl()?;
                        Ok(Some(TopLevel::FunctionDecl(fd)))
                    }
                    Token::Sub => {
                        let sd = self.parse_sub_decl()?;
                        Ok(Some(TopLevel::SubDecl(sd)))
                    }
                    _ => {
                        self.consume_to_eol();
                        Ok(None)
                    }
                }
            }
            Token::HashIf => {
                // Should have been handled by preprocessor, but skip if present
                self.skip_conditional_block();
                Ok(None)
            }
            _ => {
                // Could be a top-level statement (before PBMAIN), skip it
                self.consume_to_eol();
                Ok(None)
            }
        }
    }

    fn skip_conditional_block(&mut self) {
        let mut depth = 0;
        loop {
            match self.peek() {
                Token::HashIf => {
                    depth += 1;
                    self.advance();
                }
                Token::HashEndIf => {
                    self.advance();
                    depth -= 1;
                    if depth <= 0 {
                        self.consume_to_eol();
                        break;
                    }
                }
                Token::Eof => break,
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn parse_global_decl(&mut self) -> PbResult<Option<TopLevel>> {
        let line = self.current_line();
        self.advance(); // consume GLOBAL

        // Parse first (and possibly only) variable
        let decls = self.parse_global_var_list(line)?;
        self.consume_to_eol();

        if decls.len() == 1 {
            return Ok(Some(TopLevel::GlobalDecl(
                decls.into_iter().next().unwrap(),
            )));
        }
        // Multiple globals on one line: emit as GlobalDeclList
        Ok(Some(TopLevel::GlobalDeclList(decls)))
    }

    fn parse_global_var_list(&mut self, line: usize) -> PbResult<Vec<VarDecl>> {
        let mut decls = Vec::new();
        loop {
            let name = self.consume_identifier()?;

            // Check for array declaration: Name() [AS Type]
            if self.peek() == &Token::LParen {
                self.advance(); // (
                self.expect(&Token::RParen)?; // )
                let pb_type = if self.peek() == &Token::As {
                    self.advance();
                    self.parse_type()?
                } else {
                    type_from_suffix(&name)
                };
                decls.push(VarDecl {
                    name,
                    pb_type,
                    is_array: true,
                    line,
                });
            } else if self.peek() == &Token::As {
                self.advance();
                let pb_type = self.parse_type()?;
                decls.push(VarDecl {
                    name,
                    pb_type,
                    is_array: false,
                    line,
                });
            } else {
                // GLOBAL name (no type, infer from suffix)
                let pb_type = type_from_suffix(&name);
                decls.push(VarDecl {
                    name,
                    pb_type,
                    is_array: false,
                    line,
                });
            }

            // Check for comma-separated additional variables
            if self.peek() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(decls)
    }

    fn parse_dim_statement(&mut self, scope: DimScope) -> PbResult<DimStatement> {
        let line = self.current_line();
        self.advance(); // consume DIM/LOCAL/STATIC

        // DIM SHARED -> treat as global
        let actual_scope = if self.peek() == &Token::Shared {
            self.advance();
            DimScope::Global
        } else {
            scope
        };

        let name = self.consume_identifier()?;

        // Array bounds
        let mut bounds = Vec::new();
        if self.peek() == &Token::LParen {
            self.advance();
            if self.peek() != &Token::RParen {
                bounds = self.parse_dim_bounds()?;
            }
            self.expect(&Token::RParen)?;
        }

        // AS Type (optional — if missing, infer from suffix)
        let pb_type = if self.peek() == &Token::As {
            self.advance();
            self.parse_type()?
        } else {
            type_from_suffix(&name)
        };

        self.consume_to_eol();
        Ok(DimStatement {
            scope: actual_scope,
            name,
            pb_type,
            bounds,
            line,
            is_redim: false,
        })
    }

    fn parse_redim_statement(&mut self) -> PbResult<DimStatement> {
        let line = self.current_line();
        self.advance(); // consume REDIM

        let name = self.consume_identifier()?;

        let mut bounds = Vec::new();
        if self.peek() == &Token::LParen {
            self.advance();
            if self.peek() != &Token::RParen {
                bounds = self.parse_dim_bounds()?;
            }
            self.expect(&Token::RParen)?;
        }

        let pb_type = if self.peek() == &Token::As {
            self.advance();
            self.parse_type()?
        } else {
            type_from_suffix(&name)
        };

        self.consume_to_eol();
        Ok(DimStatement {
            scope: DimScope::Dim,
            name,
            pb_type,
            bounds,
            line,
            is_redim: true,
        })
    }

    fn parse_dim_bounds(&mut self) -> PbResult<Vec<DimBound>> {
        let mut bounds = Vec::new();
        loop {
            let first = self.parse_expression()?;
            // PB supports both TO and : as range separator: DIM arr(1 TO 10) or DIM arr(1:10)
            if self.peek() == &Token::To || self.peek() == &Token::Colon {
                self.advance();
                let upper = self.parse_expression()?;
                bounds.push(DimBound {
                    lower: first,
                    upper,
                });
            } else {
                // Just upper bound, lower is 0
                bounds.push(DimBound {
                    lower: Expr::IntegerLit(0),
                    upper: first,
                });
            }
            if self.peek() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(bounds)
    }

    fn parse_type(&mut self) -> PbResult<PbType> {
        // Skip LOCAL keyword if present (PB allows DIM arr() AS LOCAL STRING)
        if self.peek() == &Token::Local {
            self.advance();
        }
        match self.peek().clone() {
            Token::Long => {
                self.advance();
                Ok(PbType::Long)
            }
            Token::Integer => {
                self.advance();
                Ok(PbType::Integer)
            }
            Token::Double => {
                self.advance();
                Ok(PbType::Double)
            }
            Token::Single => {
                self.advance();
                Ok(PbType::Single)
            }
            Token::String_ => {
                self.advance();
                // Check for STRING * N (fixed string)
                if self.peek() == &Token::Star {
                    self.advance();
                    if let Token::IntegerLiteral(n) = self.peek().clone() {
                        self.advance();
                        return Ok(PbType::FixedString(n as usize));
                    }
                }
                Ok(PbType::String)
            }
            Token::Quad => {
                self.advance();
                Ok(PbType::Quad)
            }
            Token::Dword => {
                self.advance();
                Ok(PbType::Dword)
            }
            Token::Ext => {
                self.advance();
                Ok(PbType::Ext)
            }
            Token::Cur => {
                self.advance();
                Ok(PbType::Cur)
            }
            Token::Identifier(ref s) => {
                let upper = s.to_uppercase();
                match upper.as_str() {
                    "BYTE" => {
                        self.advance();
                        Ok(PbType::Byte)
                    }
                    "WORD" => {
                        self.advance();
                        Ok(PbType::Word)
                    }
                    "ASCIIZ" | "ASCIZ" => {
                        self.advance();
                        // ASCIIZ * N — fixed-size null-terminated string
                        if self.peek() == &Token::Star {
                            self.advance();
                            if let Token::IntegerLiteral(n) = self.peek().clone() {
                                self.advance();
                                return Ok(PbType::FixedString(n as usize));
                            }
                        }
                        Ok(PbType::FixedString(260)) // default ASCIIZ size
                    }
                    _ => {
                        let name = s.clone();
                        self.advance();
                        Ok(PbType::UserDefined(name))
                    }
                }
            }
            _ => {
                // Default
                Ok(PbType::Variant)
            }
        }
    }

    fn parse_percent_const_decl(&mut self, line: usize) -> PbResult<ConstDecl> {
        let name = match self.peek().clone() {
            Token::PercentConstant(s) => {
                self.advance();
                s
            }
            _ => {
                return Err(PbError::parser(
                    "Expected %CONSTANT",
                    self.current_file(),
                    line,
                ))
            }
        };
        self.expect(&Token::Eq)?;
        let value = self.parse_expression()?;
        self.consume_to_eol();
        Ok(ConstDecl { name, value, line })
    }

    fn parse_type_decl(&mut self, line: usize) -> PbResult<TypeDecl> {
        self.advance(); // TYPE
        let name = self.consume_identifier()?;
        self.consume_to_eol();
        self.skip_eol();

        let mut fields = Vec::new();
        loop {
            if self.peek() == &Token::End {
                self.advance();
                // END TYPE
                if self.peek() == &Token::Type {
                    self.advance();
                }
                self.consume_to_eol();
                break;
            }
            if self.at_end() {
                break;
            }

            // Parse field: name AS type
            let fname = self.consume_identifier()?;
            self.expect(&Token::As)?;
            let ftype = self.parse_type()?;
            fields.push(TypeField {
                name: fname,
                pb_type: ftype,
            });
            self.consume_to_eol();
            self.skip_eol();
        }

        Ok(TypeDecl { name, fields, line })
    }

    fn parse_sub_decl(&mut self) -> PbResult<SubDecl> {
        let line = self.current_line();
        self.advance(); // SUB
        let name = self.consume_identifier()?;

        // Optional: CALLBACK keyword after name (treat as decoration)
        if self.peek() == &Token::Callback {
            self.advance();
        }

        // Optional: CDECL/STDCALL calling convention (skip)
        while matches!(self.peek(), Token::Identifier(ref s) if {
            let u = s.to_uppercase();
            u == "CDECL" || u == "STDCALL" || u == "BDECL"
        }) {
            self.advance();
        }

        // Optional ALIAS "ExternalName"
        let alias = if self.peek() == &Token::Alias {
            self.advance();
            match self.peek().clone() {
                Token::StringLiteral(s) => {
                    self.advance();
                    Some(s)
                }
                _ => None,
            }
        } else {
            None
        };

        let params = if self.peek() == &Token::LParen {
            self.advance();
            let p = self.parse_params()?;
            self.expect(&Token::RParen)?;
            p
        } else {
            Vec::new()
        };

        // Optional EXPORT keyword
        let export = if self.peek() == &Token::Export {
            self.advance();
            true
        } else {
            false
        };

        self.consume_to_eol();
        let body = self.parse_body(&[BodyEnd::EndSub])?;

        Ok(SubDecl {
            name,
            params,
            body,
            alias,
            export,
            line,
        })
    }

    fn parse_function_decl(&mut self) -> PbResult<FunctionDecl> {
        let line = self.current_line();
        self.advance(); // FUNCTION
        let name = self.consume_identifier()?;

        // Optional: CDECL/STDCALL calling convention (skip)
        while matches!(self.peek(), Token::Identifier(ref s) if {
            let u = s.to_uppercase();
            u == "CDECL" || u == "STDCALL" || u == "BDECL"
        }) {
            self.advance();
        }

        // Optional ALIAS "ExternalName"
        let alias = if self.peek() == &Token::Alias {
            self.advance();
            match self.peek().clone() {
                Token::StringLiteral(s) => {
                    self.advance();
                    Some(s)
                }
                _ => None,
            }
        } else {
            None
        };

        let params = if self.peek() == &Token::LParen {
            self.advance();
            let p = if self.peek() == &Token::RParen {
                Vec::new()
            } else {
                self.parse_params()?
            };
            self.expect(&Token::RParen)?;
            p
        } else {
            Vec::new()
        };

        let return_type = if self.peek() == &Token::As {
            self.advance();
            self.parse_type()?
        } else {
            type_from_suffix(&name)
        };

        // Optional EXPORT keyword
        let export = if self.peek() == &Token::Export {
            self.advance();
            true
        } else {
            false
        };

        self.consume_to_eol();
        let body = self.parse_body(&[BodyEnd::EndFunction])?;

        Ok(FunctionDecl {
            name,
            params,
            return_type,
            body,
            alias,
            export,
            line,
        })
    }

    fn parse_params(&mut self) -> PbResult<Vec<Param>> {
        let mut params = Vec::new();
        loop {
            if self.peek() == &Token::RParen || self.at_end() {
                break;
            }

            let mut is_byval = false;
            let mut is_optional = false;

            // OPTIONAL, BYVAL, BYREF modifiers
            loop {
                match self.peek() {
                    Token::Optional => {
                        is_optional = true;
                        self.advance();
                    }
                    Token::Byval => {
                        is_byval = true;
                        self.advance();
                    }
                    Token::Byref => {
                        self.advance(); // BYREF is default
                    }
                    _ => break,
                }
            }

            let name = self.consume_identifier()?;
            let is_array = if self.peek() == &Token::LParen {
                self.advance();
                self.expect(&Token::RParen)?;
                true
            } else {
                false
            };

            let pb_type = if self.peek() == &Token::As {
                self.advance();
                self.parse_type()?
            } else {
                type_from_suffix(&name)
            };

            params.push(Param {
                name,
                pb_type,
                is_byval,
                is_optional,
                is_array,
            });

            if self.peek() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(params)
    }

    fn parse_declare_stmt(&mut self, line: usize) -> PbResult<Option<DeclareStmt>> {
        self.advance(); // consume DECLARE

        // Optional CALLBACK keyword (treat as decoration)
        if self.peek() == &Token::Callback {
            self.advance();
        }

        // SUB or FUNCTION
        let is_function = match self.peek() {
            Token::Function => {
                self.advance();
                true
            }
            Token::Sub => {
                self.advance();
                false
            }
            _ => {
                self.consume_to_eol();
                return Ok(None);
            }
        };

        let name = self.consume_identifier()?;

        // Optional: CDECL, STDCALL, etc. — skip any identifier that's a calling convention
        while matches!(self.peek(), Token::Identifier(ref s) if {
            let u = s.to_uppercase();
            u == "CDECL" || u == "STDCALL" || u == "BDECL"
        }) {
            self.advance();
        }

        // Optional LIB "dll.dll"
        let lib = if self.peek() == &Token::Lib {
            self.advance();
            match self.peek().clone() {
                Token::StringLiteral(s) => {
                    self.advance();
                    Some(s)
                }
                _ => None,
            }
        } else {
            None
        };

        // Optional ALIAS "ExternalName"
        let alias = if self.peek() == &Token::Alias {
            self.advance();
            match self.peek().clone() {
                Token::StringLiteral(s) => {
                    self.advance();
                    Some(s)
                }
                _ => None,
            }
        } else {
            None
        };

        // Optional parameter list
        let params = if self.peek() == &Token::LParen {
            self.advance();
            let p = if self.peek() == &Token::RParen {
                Vec::new()
            } else {
                self.parse_params()?
            };
            self.expect(&Token::RParen)?;
            p
        } else {
            Vec::new()
        };

        // Optional AS type (for FUNCTION)
        let return_type = if is_function && self.peek() == &Token::As {
            self.advance();
            self.parse_type()?
        } else if is_function {
            type_from_suffix(&name)
        } else {
            PbType::Variant // SUB has no return type
        };

        self.consume_to_eol();

        Ok(Some(DeclareStmt {
            name,
            is_function,
            params,
            return_type,
            lib,
            alias,
            line,
        }))
    }

    // ===== Body parsing (inside SUB/FUNCTION) =====

    fn parse_body(&mut self, terminators: &[BodyEnd]) -> PbResult<Vec<Statement>> {
        self.parse_body_with_terminator(terminators)
            .map(|(stmts, _)| stmts)
    }

    /// Like parse_body but also returns which terminator was matched (if any).
    fn parse_body_with_terminator(
        &mut self,
        terminators: &[BodyEnd],
    ) -> PbResult<(Vec<Statement>, Option<BodyEnd>)> {
        let mut stmts = Vec::new();
        self.skip_eol();

        let mut matched_terminator = None;

        loop {
            if self.at_end() {
                break;
            }

            // Check for terminators
            if let Some(t) = self.which_terminator(terminators) {
                self.consume_terminator(terminators);
                matched_terminator = Some(t);
                break;
            }

            self.skip_eol();
            if self.at_end() {
                break;
            }
            if let Some(t) = self.which_terminator(terminators) {
                self.consume_terminator(terminators);
                matched_terminator = Some(t);
                break;
            }

            // Safety: break on END SUB/END FUNCTION even if not in our terminator
            // list — this indicates we've overrun into a parent scope (e.g., a CASE
            // body encountering the enclosing function's END SUB).
            if self.peek() == &Token::End {
                match self.peek_at(1) {
                    Some(&Token::Sub) | Some(&Token::Function) => {
                        if !terminators
                            .iter()
                            .any(|t| matches!(t, BodyEnd::EndSub | BodyEnd::EndFunction))
                        {
                            break; // don't consume, let parent handle it
                        }
                    }
                    _ => {}
                }
            }

            // Safety: if we see a top-level declaration keyword that shouldn't
            // be inside a body, break out to avoid consuming the rest of the file.
            if self.is_top_level_keyword() {
                break;
            }

            let saved_pos = self.pos;
            match self.parse_statement() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    eprintln!("Parse warning (body): {}", e);
                    self.consume_to_eol();
                }
            }

            self.skip_eol();

            // Safety: if no progress was made, force advance to prevent infinite loops.
            if self.pos == saved_pos {
                self.advance();
            }
        }

        Ok((stmts, matched_terminator))
    }

    /// Check if current position looks like a top-level declaration that shouldn't be inside a body
    fn is_top_level_keyword(&self) -> bool {
        match self.peek() {
            Token::Sub => {
                // SUB followed by identifier = new sub declaration
                matches!(self.peek_at(1), Some(Token::Identifier(_)))
            }
            Token::Function => {
                // FUNCTION followed by identifier (not =) = new function declaration
                match self.peek_at(1) {
                    Some(Token::Eq) => false,           // FUNCTION = expr (return value)
                    Some(Token::Identifier(_)) => true, // New function declaration
                    _ => false,
                }
            }
            Token::Declare => true,
            Token::Type => {
                // TYPE at start of line (new type decl, not inside body)
                // Check if next is an identifier (type name), not a keyword
                matches!(self.peek_at(1), Some(Token::Identifier(_)))
            }
            _ => false,
        }
    }

    fn which_terminator(&self, terminators: &[BodyEnd]) -> Option<BodyEnd> {
        for t in terminators {
            if self.matches_terminator(t) {
                return Some(*t);
            }
        }
        None
    }

    fn matches_terminator(&self, term: &BodyEnd) -> bool {
        match term {
            BodyEnd::EndSub => self.peek() == &Token::End && self.peek_at(1) == Some(&Token::Sub),
            BodyEnd::EndFunction => {
                self.peek() == &Token::End && self.peek_at(1) == Some(&Token::Function)
            }
            BodyEnd::EndIf => self.peek() == &Token::End && self.peek_at(1) == Some(&Token::If),
            BodyEnd::Else => self.peek() == &Token::Else,
            BodyEnd::ElseIf => self.peek() == &Token::ElseIf,
            BodyEnd::Next => self.peek() == &Token::Next,
            BodyEnd::Loop => self.peek() == &Token::Loop,
            BodyEnd::Wend => self.peek() == &Token::Wend,
            BodyEnd::EndSelect => {
                self.peek() == &Token::End && self.peek_at(1) == Some(&Token::Select)
            }
            BodyEnd::Case => self.peek() == &Token::Case,
            BodyEnd::CaseElse => {
                self.peek() == &Token::Case && self.peek_at(1) == Some(&Token::Else)
            }
        }
    }

    fn consume_terminator(&mut self, terminators: &[BodyEnd]) {
        for t in terminators {
            if self.matches_terminator(t) {
                match t {
                    BodyEnd::EndSub | BodyEnd::EndFunction | BodyEnd::EndIf => {
                        self.advance(); // END
                        self.advance(); // SUB/FUNCTION/IF
                        self.consume_to_eol();
                    }
                    BodyEnd::EndSelect
                    | BodyEnd::Else
                    | BodyEnd::ElseIf
                    | BodyEnd::Next
                    | BodyEnd::Loop
                    | BodyEnd::Wend
                    | BodyEnd::Case
                    | BodyEnd::CaseElse => {
                        // Don't consume — caller needs to see it
                    }
                }
                return;
            }
        }
    }

    fn peek_at(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.pos + offset).map(|t| &t.token)
    }

    fn parse_statement(&mut self) -> PbResult<Statement> {
        self.skip_eol();
        let line = self.current_line();

        match self.peek().clone() {
            Token::If => self.parse_if_statement(),
            Token::For => self.parse_for_statement(),
            Token::Do => self.parse_do_loop(),
            Token::While => self.parse_while_wend(),
            Token::Select => self.parse_select_case(),
            Token::Print => self.parse_print_statement(),
            Token::Input => {
                self.advance(); // consume INPUT
                                // INPUT #filenum, var1, var2, ...
                if self.peek() == &Token::Hash {
                    self.advance(); // consume #
                    let file_num = self.parse_expression()?;
                    self.expect(&Token::Comma)?;
                    let mut vars = Vec::new();
                    vars.push(self.parse_expression()?);
                    while self.peek() == &Token::Comma {
                        self.advance();
                        vars.push(self.parse_expression()?);
                    }
                    self.consume_to_eol();
                    return Ok(Statement::InputFile(InputFileStmt {
                        file_num,
                        vars,
                        line,
                    }));
                }
                // Plain INPUT (console) — not implemented, consume
                self.consume_to_eol();
                Ok(Statement::Noop)
            }
            Token::Open => self.parse_open_statement(),
            Token::Close => self.parse_close_statement(),
            Token::Call => {
                self.advance();
                let name = self.consume_identifier()?;
                let args = if self.peek() == &Token::LParen {
                    self.advance();
                    let a = self.parse_arg_list()?;
                    self.expect(&Token::RParen)?;
                    a
                } else {
                    Vec::new()
                };
                self.consume_to_eol();
                Ok(Statement::Call(CallStmt { name, args, line }))
            }
            Token::Incr => {
                self.advance();
                let target = self.parse_expression()?;
                let amount = if self.peek() == &Token::Comma {
                    self.advance();
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.consume_to_eol();
                Ok(Statement::Incr(IncrDecrStmt {
                    target,
                    amount,
                    line,
                }))
            }
            Token::Decr => {
                self.advance();
                let target = self.parse_expression()?;
                let amount = if self.peek() == &Token::Comma {
                    self.advance();
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.consume_to_eol();
                Ok(Statement::Decr(IncrDecrStmt {
                    target,
                    amount,
                    line,
                }))
            }
            Token::Exit => {
                self.advance();
                let stmt = match self.peek() {
                    Token::Sub => {
                        self.advance();
                        Statement::ExitSub
                    }
                    Token::Function => {
                        self.advance();
                        Statement::ExitFunction
                    }
                    Token::For => {
                        self.advance();
                        Statement::ExitFor
                    }
                    Token::Do => {
                        self.advance();
                        Statement::ExitDo
                    }
                    Token::Select => {
                        self.advance();
                        Statement::ExitSelect
                    }
                    _ => Statement::ExitSub,
                };
                self.consume_to_eol();
                Ok(stmt)
            }
            Token::Function => {
                // FUNCTION = expr (return value)
                self.advance();
                self.expect(&Token::Eq)?;
                let value = self.parse_expression()?;
                self.consume_to_eol();
                Ok(Statement::FunctionReturn(FunctionReturnStmt {
                    value,
                    line,
                }))
            }
            Token::Local => {
                let dims = self.parse_local_decl()?;
                if dims.len() == 1 {
                    Ok(Statement::Dim(dims.into_iter().next().unwrap()))
                } else {
                    Ok(Statement::Block(
                        dims.into_iter().map(Statement::Dim).collect(),
                    ))
                }
            }
            Token::Dim => {
                let dim = self.parse_dim_statement(DimScope::Dim)?;
                Ok(Statement::Dim(dim))
            }
            Token::Redim => {
                let dim = self.parse_redim_statement()?;
                Ok(Statement::Redim(dim))
            }
            Token::Static => {
                let dim = self.parse_dim_statement(DimScope::Static)?;
                Ok(Statement::Dim(dim))
            }
            Token::Global => {
                // GLOBAL inside a sub (shouldn't happen but handle gracefully)
                let dim = self.parse_dim_statement(DimScope::Global)?;
                Ok(Statement::Dim(dim))
            }
            Token::On => {
                self.advance();
                // ON ERROR GOTO label
                if self.peek() == &Token::Error {
                    self.advance();
                    if self.peek() == &Token::GoTo {
                        self.advance();
                        if let Token::IntegerLiteral(0) = self.peek() {
                            self.advance();
                            self.consume_to_eol();
                            return Ok(Statement::OnErrorGotoZero);
                        }
                        let label = match self.peek().clone() {
                            Token::Identifier(s) => {
                                self.advance();
                                s
                            }
                            Token::IntegerLiteral(n) => {
                                self.advance();
                                n.to_string()
                            }
                            _ => {
                                self.consume_to_eol();
                                return Ok(Statement::Noop);
                            }
                        };
                        self.consume_to_eol();
                        return Ok(Statement::OnErrorGoto(label));
                    }
                }
                self.consume_to_eol();
                Ok(Statement::Noop)
            }
            Token::Replace => {
                // REPLACE old$ WITH new$ IN target$
                self.advance();
                self.consume_to_eol();
                Ok(Statement::Noop)
            }
            Token::Remove => {
                // REMOVE$ is usually a function but REMOVE can be a statement
                self.advance();
                self.consume_to_eol();
                Ok(Statement::Noop)
            }
            Token::Resume => {
                self.advance();
                // RESUME NEXT
                self.consume_to_eol();
                Ok(Statement::ResumeNext)
            }
            Token::GoSub => {
                self.advance();
                let label = match self.peek().clone() {
                    Token::Identifier(s) => {
                        self.advance();
                        s
                    }
                    Token::IntegerLiteral(n) => {
                        self.advance();
                        n.to_string()
                    }
                    _ => {
                        return Err(PbError::parser(
                            format!("Expected label after GOSUB, got {:?}", self.peek()),
                            self.current_file(),
                            line,
                        ))
                    }
                };
                self.consume_to_eol();
                Ok(Statement::GoSub(label))
            }
            Token::GoTo => {
                self.advance();
                let label = match self.peek().clone() {
                    Token::Identifier(s) => {
                        self.advance();
                        s
                    }
                    Token::IntegerLiteral(n) => {
                        self.advance();
                        n.to_string()
                    }
                    _ => {
                        return Err(PbError::parser(
                            format!("Expected label after GOTO, got {:?}", self.peek()),
                            self.current_file(),
                            line,
                        ))
                    }
                };
                self.consume_to_eol();
                Ok(Statement::GoTo(label))
            }
            Token::Return => {
                self.advance();
                self.consume_to_eol();
                Ok(Statement::Return)
            }
            Token::Iterate => {
                self.advance();
                let target = match self.peek() {
                    Token::For => {
                        self.advance();
                        IterateTarget::For
                    }
                    Token::Do => {
                        self.advance();
                        IterateTarget::Do
                    }
                    _ => IterateTarget::For,
                };
                self.consume_to_eol();
                Ok(Statement::Iterate(target))
            }
            Token::HashInclude => {
                // #INCLUDE inside a SUB (e.g., SetupDims including DIMFILE.INC)
                // We need to treat the included content as statements
                // For now, skip — the preprocessor should have resolved this
                self.consume_to_eol();
                Ok(Statement::Noop)
            }
            Token::PercentConstant(_) => {
                // %CONSTANT = value inside a sub
                self.consume_to_eol();
                Ok(Statement::Noop)
            }
            Token::End => {
                // Check what follows END
                match self.peek_at(1) {
                    Some(Token::Sub) | Some(Token::Function) => {
                        // END SUB/FUNCTION should be handled as body terminators,
                        // not consumed here. Return error to let body parser deal with it.
                        Err(PbError::parser(
                            "Unexpected END SUB/FUNCTION inside body",
                            None,
                            self.current_line(),
                        ))
                    }
                    Some(Token::If) | Some(Token::Select) | Some(Token::Type) => {
                        // Mismatched block-end — consume and skip
                        self.consume_to_eol();
                        Ok(Statement::Noop)
                    }
                    _ => {
                        // Standalone END (program termination)
                        self.advance(); // consume END
                        self.consume_to_eol();
                        Ok(Statement::Noop)
                    }
                }
            }
            Token::IntegerLiteral(n) => {
                // Numeric label: 30300 followed by anything (used with GOSUB/GOTO)
                let label = n.to_string();
                self.advance(); // consume the number
                Ok(Statement::Label(label))
            }
            Token::Identifier(ref name) => {
                let name_upper = name.to_uppercase();

                // LINE INPUT #filenum, var$
                if name_upper == "LINE" {
                    // Peek ahead: LINE INPUT #...
                    if let Some(Token::Input) = self.peek_at(1) {
                        self.advance(); // consume LINE
                        self.advance(); // consume INPUT
                        if self.peek() == &Token::Hash {
                            self.advance(); // consume #
                            let file_num = self.parse_expression()?;
                            self.expect(&Token::Comma)?;
                            let var = self.parse_expression()?;
                            self.consume_to_eol();
                            return Ok(Statement::LineInputFile(LineInputFileStmt {
                                file_num,
                                var,
                                line,
                            }));
                        }
                        // LINE INPUT without # — console, not implemented
                        self.consume_to_eol();
                        return Ok(Statement::Noop);
                    }
                    // LINE (not INPUT) — DDT drawing or other, consume
                    self.advance();
                    self.consume_to_eol();
                    return Ok(Statement::Noop);
                }

                // KILL filename$
                if name_upper == "KILL" {
                    self.advance(); // consume KILL
                    let filename = self.parse_expression()?;
                    self.consume_to_eol();
                    return Ok(Statement::Kill(filename));
                }

                // TRY ... CATCH ... END TRY
                if name_upper == "TRY" {
                    self.advance(); // consume TRY
                    self.skip_eol();
                    // Parse try body until CATCH or END TRY
                    let mut try_body = Vec::new();
                    loop {
                        if let Token::Identifier(ref kw) = self.peek().clone() {
                            let kw_upper = kw.to_uppercase();
                            if kw_upper == "CATCH" {
                                self.advance(); // consume CATCH
                                self.consume_to_eol();
                                self.skip_eol();
                                break;
                            }
                        }
                        if let Token::End = self.peek() {
                            // Could be END TRY
                            if let Some(Token::Identifier(ref kw)) = self.peek_at(1) {
                                if kw.eq_ignore_ascii_case("TRY") {
                                    self.advance(); // consume END
                                    self.advance(); // consume TRY
                                    self.skip_eol();
                                    return Ok(Statement::Block(try_body));
                                }
                            }
                        }
                        if self.peek() == &Token::Eof {
                            break;
                        }
                        try_body.push(self.parse_statement()?);
                        self.skip_eol();
                    }
                    // Skip catch body until END TRY
                    loop {
                        if let Token::End = self.peek() {
                            if let Some(Token::Identifier(ref kw)) = self.peek_at(1) {
                                if kw.eq_ignore_ascii_case("TRY") {
                                    self.advance(); // consume END
                                    self.advance(); // consume TRY
                                    self.skip_eol();
                                    break;
                                }
                            }
                        }
                        if self.peek() == &Token::Eof {
                            break;
                        }
                        self.advance();
                        if self.at_eol_or_eof() {
                            self.skip_eol();
                        }
                    }
                    return Ok(Statement::Block(try_body));
                }

                // DDT statements: DIALOG, CONTROL, MENU, TOOLBAR, STATUSBAR
                // COMBOBOX, LISTBOX, TREEVIEW, LISTVIEW, XPRINT — DDT UI verbs
                // SLEEP, RANDOMIZE, MKDIR — parse arg, emit as function call
                if matches!(
                    name_upper.as_str(),
                    "SLEEP" | "RANDOMIZE" | "MKDIR" | "RMDIR" | "CHDIR"
                ) {
                    let call_name = name_upper.clone();
                    self.advance();
                    let args = if !self.at_eol_or_eof() {
                        vec![self.parse_expression()?]
                    } else {
                        Vec::new()
                    };
                    self.consume_to_eol();
                    return Ok(Statement::Call(CallStmt {
                        name: call_name,
                        args,
                        line,
                    }));
                }

                if matches!(
                    name_upper.as_str(),
                    "DIALOG"
                        | "CONTROL"
                        | "MENU"
                        | "TOOLBAR"
                        | "STATUSBAR"
                        | "COMBOBOX"
                        | "LISTBOX"
                        | "TREEVIEW"
                        | "LISTVIEW"
                        | "XPRINT"
                        | "WRITE"
                        | "LSET"
                        | "RSET"
                        | "RESET"
                        | "ERASE"
                        | "FLUSH"
                        | "NAME"
                        | "SEEK"
                        | "LOCK"
                        | "UNLOCK"
                ) {
                    self.advance();
                    self.consume_to_eol();
                    return Ok(Statement::Noop);
                }

                let name_clone = name.clone();

                // Check for label: IDENTIFIER:
                if self.peek_at(1) == Some(&Token::Colon) {
                    self.advance(); // consume identifier
                    self.advance(); // consume colon
                    return Ok(Statement::Label(name_clone));
                }

                // Or assignment: IDENTIFIER = expr
                // Or sub call: IDENTIFIER args
                // Or array assignment: IDENTIFIER(idx) = expr

                // Peek ahead to decide
                if self.peek_at(1) == Some(&Token::Eq) {
                    // Simple assignment: name = expr
                    self.advance(); // consume name
                    self.advance(); // consume =
                    let value = self.parse_expression()?;
                    self.consume_to_eol();
                    return Ok(Statement::Assign(AssignStmt {
                        target: Expr::Variable(name_clone),
                        value,
                        line,
                    }));
                }

                if self.peek_at(1) == Some(&Token::Dot) {
                    // TYPE member assignment: name.member = expr
                    let primary = self.parse_primary()?;
                    if self.peek() == &Token::Eq {
                        self.advance(); // consume =
                        let value = self.parse_expression()?;
                        self.consume_to_eol();
                        return Ok(Statement::Assign(AssignStmt {
                            target: primary,
                            value,
                            line,
                        }));
                    }
                    self.consume_to_eol();
                    return Ok(Statement::Noop);
                }

                if self.peek_at(1) == Some(&Token::LParen) {
                    // Could be: arr(idx) = expr  (array assignment)
                    // Or:       Sub(args)        (sub call)
                    // Or:       arr(idx).member = expr (UDT array member)
                    // Parse just the primary expression (no comparison operators)
                    let primary = self.parse_primary()?;

                    if self.peek() == &Token::Eq {
                        // Array assignment: arr(idx) = expr
                        self.advance(); // consume =
                        let value = self.parse_expression()?;
                        self.consume_to_eol();
                        return Ok(Statement::Assign(AssignStmt {
                            target: primary,
                            value,
                            line,
                        }));
                    }

                    // It's a sub call with parenthesized args
                    if let Expr::FunctionCall(func_name, func_args) = primary {
                        self.consume_to_eol();
                        return Ok(Statement::Call(CallStmt {
                            name: func_name,
                            args: func_args,
                            line,
                        }));
                    }

                    self.consume_to_eol();
                    return Ok(Statement::Noop);
                }

                // No LParen after identifier — could be a SUB call: SubName arg1, arg2
                // Or a label target in single-line IF: IF cond THEN LabelName ELSE ...
                self.advance(); // consume identifier
                let mut args = Vec::new();
                if !self.at_eol_or_eof() && !matches!(self.peek(), Token::Else | Token::ElseIf) {
                    args.push(self.parse_expression()?);
                    while self.peek() == &Token::Comma {
                        self.advance();
                        args.push(self.parse_expression()?);
                    }
                }
                self.consume_to_eol();
                Ok(Statement::Call(CallStmt {
                    name: name_clone,
                    args,
                    line,
                }))
            }
            _ => {
                self.consume_to_eol();
                Ok(Statement::Noop)
            }
        }
    }

    fn parse_local_decl(&mut self) -> PbResult<Vec<DimStatement>> {
        let line = self.current_line();
        self.advance(); // LOCAL

        let mut dims = Vec::new();

        loop {
            let name = self.consume_identifier()?;

            let mut bounds = Vec::new();
            if self.peek() == &Token::LParen {
                self.advance();
                if self.peek() != &Token::RParen {
                    bounds = self.parse_dim_bounds()?;
                }
                self.expect(&Token::RParen)?;
            }

            let pb_type = if self.peek() == &Token::As {
                self.advance();
                self.parse_type()?
            } else {
                type_from_suffix(&name)
            };

            dims.push(DimStatement {
                scope: DimScope::Local,
                name,
                pb_type,
                bounds,
                line,
                is_redim: false,
            });

            if self.peek() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        self.consume_to_eol();
        Ok(dims)
    }

    fn parse_if_statement(&mut self) -> PbResult<Statement> {
        let line = self.current_line();
        self.advance(); // IF
        let condition = self.parse_expression()?;
        self.expect(&Token::Then)?;

        // Check for single-line IF: IF cond THEN stmt1[:stmt2...] [ELSE stmt3[:stmt4...]]
        if !self.at_eol_or_eof() {
            // Single-line IF — parse one or more colon-separated THEN statements
            let if_line = self.current_line();
            let mut then_body = Vec::new();
            loop {
                let stmt = self.parse_statement()?;
                then_body.push(stmt);
                // Continue with more colon-separated statements on the same line
                if self.peek() == &Token::Colon && self.current_line() == if_line {
                    self.advance(); // consume :
                                    // But check if next token is ELSE (IF cond THEN stmt : ELSE stmt)
                    if self.peek() == &Token::Else {
                        break;
                    }
                } else {
                    break;
                }
            }

            let mut else_body = Vec::new();

            // Check for ELSE on the SAME line only.
            // After parse_statement, we may have crossed a line boundary
            // (e.g. PRINT calls consume_to_eol). Only match ELSE if still
            // on the same line to avoid stealing an outer block IF's ELSE.
            if self.peek() == &Token::Else && self.current_line() == if_line {
                self.advance(); // consume ELSE
                loop {
                    let stmt = self.parse_statement()?;
                    else_body.push(stmt);
                    if self.peek() == &Token::Colon && self.current_line() == if_line {
                        self.advance(); // consume :
                    } else {
                        break;
                    }
                }
            }

            return Ok(Statement::If(IfStmt {
                condition,
                then_body,
                elseif_clauses: Vec::new(),
                else_body,
                line,
            }));
        }

        self.skip_eol();

        // Multi-line IF
        let (then_body, matched) =
            self.parse_body_with_terminator(&[BodyEnd::EndIf, BodyEnd::Else, BodyEnd::ElseIf])?;

        let mut elseif_clauses = Vec::new();
        let mut else_body = Vec::new();

        // Only check for ELSEIF/ELSE if the then_body wasn't terminated by END IF.
        // When a nested IF...END IF is inside an ELSEIF body, the END IF belongs
        // to the nested IF, and the next ELSEIF/ELSE belongs to the OUTER IF.
        if matched != Some(BodyEnd::EndIf) {
            // Check for ELSEIF
            while self.peek() == &Token::ElseIf {
                self.advance();
                let elif_cond = self.parse_expression()?;
                self.expect(&Token::Then)?;
                self.consume_to_eol();
                let (elif_body, elif_matched) = self.parse_body_with_terminator(&[
                    BodyEnd::EndIf,
                    BodyEnd::Else,
                    BodyEnd::ElseIf,
                ])?;
                elseif_clauses.push(ElseIfClause {
                    condition: elif_cond,
                    body: elif_body,
                });
                // If this ELSEIF body ended with END IF, the IF statement is complete
                if elif_matched == Some(BodyEnd::EndIf) {
                    break;
                }
            }

            // Check for ELSE (only if not already terminated by END IF)
            if self.peek() == &Token::Else {
                self.advance();
                self.consume_to_eol();
                else_body = self.parse_body(&[BodyEnd::EndIf])?;
            }
        }

        Ok(Statement::If(IfStmt {
            condition,
            then_body,
            elseif_clauses,
            else_body,
            line,
        }))
    }

    fn parse_for_statement(&mut self) -> PbResult<Statement> {
        let line = self.current_line();
        self.advance(); // FOR
        let var = self.consume_identifier()?;
        self.expect(&Token::Eq)?;
        let start = self.parse_expression()?;
        self.expect(&Token::To)?;
        let end = self.parse_expression()?;
        let step = if self.peek() == &Token::Step {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.consume_to_eol();

        let body = self.parse_body(&[BodyEnd::Next])?;

        // NEXT [var]
        if self.peek() == &Token::Next {
            self.advance();
            // Optionally consume the variable name
            if let Token::Identifier(_) = self.peek() {
                self.advance();
            }
            self.consume_to_eol();
        }

        Ok(Statement::For(ForStmt {
            var,
            start,
            end,
            step,
            body,
            line,
        }))
    }

    fn parse_do_loop(&mut self) -> PbResult<Statement> {
        let line = self.current_line();
        self.advance(); // DO

        let mut condition = None;
        let mut is_while = true;
        let mut is_pre_test = false;

        // DO WHILE/UNTIL cond
        if self.peek() == &Token::While {
            self.advance();
            condition = Some(self.parse_expression()?);
            is_while = true;
            is_pre_test = true;
        } else if self.peek() == &Token::Until {
            self.advance();
            condition = Some(self.parse_expression()?);
            is_while = false;
            is_pre_test = true;
        }
        self.consume_to_eol();

        let body = self.parse_body(&[BodyEnd::Loop])?;

        // LOOP [WHILE/UNTIL cond]
        if self.peek() == &Token::Loop {
            self.advance();
            if condition.is_none() {
                if self.peek() == &Token::While {
                    self.advance();
                    condition = Some(self.parse_expression()?);
                    is_while = true;
                } else if self.peek() == &Token::Until {
                    self.advance();
                    condition = Some(self.parse_expression()?);
                    is_while = false;
                }
            }
            self.consume_to_eol();
        }

        Ok(Statement::DoLoop(DoLoopStmt {
            condition,
            is_while,
            is_pre_test,
            body,
            line,
        }))
    }

    fn parse_while_wend(&mut self) -> PbResult<Statement> {
        let line = self.current_line();
        self.advance(); // WHILE
        let condition = self.parse_expression()?;
        self.consume_to_eol();

        let body = self.parse_body(&[BodyEnd::Wend])?;

        if self.peek() == &Token::Wend {
            self.advance();
            self.consume_to_eol();
        }

        Ok(Statement::WhileWend(WhileWendStmt {
            condition,
            body,
            line,
        }))
    }

    fn parse_select_case(&mut self) -> PbResult<Statement> {
        let line = self.current_line();
        self.advance(); // SELECT
        self.expect(&Token::Case)?;
        let expr = self.parse_expression()?;
        self.consume_to_eol();
        self.skip_eol();

        let mut cases = Vec::new();
        let mut else_body = Vec::new();

        loop {
            if self.at_end() {
                break;
            }
            if self.peek() == &Token::End {
                if self.peek_at(1) == Some(&Token::Select) {
                    self.advance(); // END
                    self.advance(); // SELECT
                    self.consume_to_eol();
                    break;
                }
                // Break on END SUB/FUNCTION — we've overrun into parent scope
                if matches!(self.peek_at(1), Some(&Token::Sub) | Some(&Token::Function)) {
                    break;
                }
            }
            // Break on top-level keywords (new SUB/FUNCTION declaration)
            if self.is_top_level_keyword() {
                break;
            }
            if self.peek() == &Token::Case {
                self.advance();
                if self.peek() == &Token::Else {
                    self.advance();
                    self.consume_to_eol();
                    else_body = self.parse_body(&[BodyEnd::EndSelect, BodyEnd::Case])?;
                    continue;
                }
                // Parse case patterns
                let patterns = self.parse_case_patterns()?;
                self.consume_to_eol();
                let body =
                    self.parse_body(&[BodyEnd::EndSelect, BodyEnd::Case, BodyEnd::CaseElse])?;
                cases.push(CaseClause { patterns, body });
            } else {
                let saved = self.pos;
                self.consume_to_eol();
                self.skip_eol();
                // Safety: prevent infinite loop if consume_to_eol makes no progress
                // (e.g., stuck on Token::Else which consume_to_eol won't consume)
                if self.pos == saved {
                    self.advance();
                }
            }
        }

        Ok(Statement::SelectCase(SelectCaseStmt {
            expr,
            cases,
            else_body,
            line,
        }))
    }

    fn parse_case_patterns(&mut self) -> PbResult<Vec<CasePattern>> {
        let mut patterns = Vec::new();
        loop {
            if self.at_eol_or_eof() {
                break;
            }
            // CASE IS <op> expr
            if self.peek() == &Token::Is {
                self.advance();
                let op = match self.peek() {
                    Token::Lt => CaseOp::Lt,
                    Token::Gt => CaseOp::Gt,
                    Token::Lte => CaseOp::Lte,
                    Token::Gte => CaseOp::Gte,
                    Token::Eq => CaseOp::Eq,
                    Token::Neq => CaseOp::Neq,
                    _ => CaseOp::Eq,
                };
                self.advance();
                let expr = self.parse_expression()?;
                patterns.push(CasePattern::Is(op, expr));
            // CASE < expr, CASE > expr, CASE <= expr, CASE >= expr (without IS keyword)
            } else if matches!(self.peek(), Token::Lt | Token::Gt | Token::Lte | Token::Gte) {
                let op = match self.peek() {
                    Token::Lt => CaseOp::Lt,
                    Token::Gt => CaseOp::Gt,
                    Token::Lte => CaseOp::Lte,
                    Token::Gte => CaseOp::Gte,
                    _ => unreachable!(),
                };
                self.advance();
                let expr = self.parse_expression()?;
                patterns.push(CasePattern::Is(op, expr));
            } else {
                let expr = self.parse_expression()?;
                // Check for range: expr TO expr
                if self.peek() == &Token::To {
                    self.advance();
                    let upper = self.parse_expression()?;
                    patterns.push(CasePattern::Range(expr, upper));
                } else {
                    patterns.push(CasePattern::Value(expr));
                }
            }
            if self.peek() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(patterns)
    }

    fn parse_print_statement(&mut self) -> PbResult<Statement> {
        let line = self.current_line();
        self.advance(); // PRINT

        // PRINT #filenum, args
        if self.peek() == &Token::Hash {
            self.advance();
            let file_num = self.parse_expression()?;
            self.expect(&Token::Comma)?;
            let args = self.parse_print_args()?;
            self.consume_to_eol();
            return Ok(Statement::PrintFile(PrintFileStmt {
                file_num,
                args,
                line,
            }));
        }

        let args = self.parse_print_args()?;
        self.consume_to_eol();
        Ok(Statement::Print(PrintStmt { args, line }))
    }

    fn parse_print_args(&mut self) -> PbResult<Vec<Expr>> {
        let mut args = Vec::new();
        while !self.at_eol_or_eof() {
            args.push(self.parse_expression()?);
            // PRINT uses ; or , for concatenation
            match self.peek() {
                Token::Semicolon | Token::Comma => {
                    self.advance();
                }
                _ => break,
            }
        }
        Ok(args)
    }

    fn parse_open_statement(&mut self) -> PbResult<Statement> {
        let line = self.current_line();
        self.advance(); // OPEN

        let filename = self.parse_expression()?;

        self.expect(&Token::For)?;

        let mode = match self.peek() {
            Token::Output => {
                self.advance();
                OpenMode::Output
            }
            Token::Append => {
                self.advance();
                OpenMode::Append
            }
            Token::Input => {
                self.advance();
                OpenMode::Input
            }
            _ => {
                self.consume_to_eol();
                return Ok(Statement::Noop);
            }
        };

        self.expect(&Token::As)?;
        // # is optional: OPEN file FOR OUTPUT AS #num  or  AS num
        if self.peek() == &Token::Hash {
            self.advance();
        }
        let file_num = self.parse_expression()?;

        self.consume_to_eol();
        Ok(Statement::Open(OpenStmt {
            filename,
            mode,
            file_num,
            line,
        }))
    }

    fn parse_close_statement(&mut self) -> PbResult<Statement> {
        let line = self.current_line();
        self.advance(); // CLOSE
                        // # is optional in PowerBASIC: CLOSE #filenum or CLOSE filenum
        if self.peek() == &Token::Hash {
            self.advance();
        }
        if self.at_eol_or_eof() {
            // Bare CLOSE with no file number — close all files
            self.consume_to_eol();
            return Ok(Statement::Noop);
        }
        let file_num = self.parse_expression()?;
        self.consume_to_eol();
        Ok(Statement::Close(CloseStmt { file_num, line }))
    }

    // ===== Expression parsing with precedence climbing =====

    fn parse_expression(&mut self) -> PbResult<Expr> {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> PbResult<Expr> {
        let mut left = self.parse_xor_expr()?;
        while self.peek() == &Token::Or {
            self.advance();
            let right = self.parse_xor_expr()?;
            left = Expr::BinaryOp(BinaryOp::Or, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_xor_expr(&mut self) -> PbResult<Expr> {
        let mut left = self.parse_and_expr()?;
        while self.peek() == &Token::Xor {
            self.advance();
            let right = self.parse_and_expr()?;
            left = Expr::BinaryOp(BinaryOp::Xor, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_and_expr(&mut self) -> PbResult<Expr> {
        let mut left = self.parse_not_expr()?;
        while self.peek() == &Token::And {
            self.advance();
            let right = self.parse_not_expr()?;
            left = Expr::BinaryOp(BinaryOp::And, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_not_expr(&mut self) -> PbResult<Expr> {
        if self.peek() == &Token::Not {
            self.advance();
            let expr = self.parse_comparison()?;
            return Ok(Expr::UnaryOp(UnaryOp::Not, Box::new(expr)));
        }
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> PbResult<Expr> {
        let mut left = self.parse_concat_expr()?;
        loop {
            let op = match self.peek() {
                Token::Eq => {
                    // PB allows =< and => as alternatives to <= and >=
                    match self.peek_at(1) {
                        Some(Token::Lt) => {
                            self.advance(); // consume =
                            BinaryOp::Lte
                        }
                        Some(Token::Gt) => {
                            self.advance(); // consume =
                            BinaryOp::Gte
                        }
                        _ => BinaryOp::Eq,
                    }
                }
                Token::Neq => BinaryOp::Neq,
                Token::Lt => BinaryOp::Lt,
                Token::Gt => BinaryOp::Gt,
                Token::Lte => BinaryOp::Lte,
                Token::Gte => BinaryOp::Gte,
                _ => break,
            };
            self.advance();
            let right = self.parse_concat_expr()?;
            left = Expr::BinaryOp(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_concat_expr(&mut self) -> PbResult<Expr> {
        let mut left = self.parse_add_sub()?;
        while self.peek() == &Token::Ampersand {
            self.advance();
            let right = self.parse_add_sub()?;
            left = Expr::BinaryOp(BinaryOp::StrConcat, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_add_sub(&mut self) -> PbResult<Expr> {
        let mut left = self.parse_mul_div()?;
        loop {
            let op = match self.peek() {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_mul_div()?;
            left = Expr::BinaryOp(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_mul_div(&mut self) -> PbResult<Expr> {
        let mut left = self.parse_int_div()?;
        loop {
            let op = match self.peek() {
                Token::Star => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                _ => break,
            };
            self.advance();
            let right = self.parse_int_div()?;
            left = Expr::BinaryOp(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_int_div(&mut self) -> PbResult<Expr> {
        let mut left = self.parse_mod_expr()?;
        while self.peek() == &Token::Backslash {
            self.advance();
            let right = self.parse_mod_expr()?;
            left = Expr::BinaryOp(BinaryOp::IntDiv, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_mod_expr(&mut self) -> PbResult<Expr> {
        let mut left = self.parse_power()?;
        while self.peek() == &Token::Mod {
            self.advance();
            let right = self.parse_power()?;
            left = Expr::BinaryOp(BinaryOp::Mod, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_power(&mut self) -> PbResult<Expr> {
        let base = self.parse_unary()?;
        if self.peek() == &Token::Caret {
            self.advance();
            let exp = self.parse_unary()?;
            return Ok(Expr::BinaryOp(BinaryOp::Pow, Box::new(base), Box::new(exp)));
        }
        Ok(base)
    }

    fn parse_unary(&mut self) -> PbResult<Expr> {
        if self.peek() == &Token::Minus {
            self.advance();
            let expr = self.parse_primary()?;
            return Ok(Expr::Negate(Box::new(expr)));
        }
        if self.peek() == &Token::Plus {
            self.advance();
            return self.parse_primary();
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> PbResult<Expr> {
        match self.peek().clone() {
            Token::IntegerLiteral(n) => {
                self.advance();
                Ok(Expr::IntegerLit(n))
            }
            Token::FloatLiteral(f) => {
                self.advance();
                Ok(Expr::FloatLit(f))
            }
            Token::StringLiteral(s) => {
                self.advance();
                Ok(Expr::StringLit(s))
            }
            Token::PercentConstant(name) => {
                self.advance();
                Ok(Expr::PercentConst(name))
            }
            Token::FreeFile => {
                self.advance();
                Ok(Expr::FunctionCall("FREEFILE".to_string(), Vec::new()))
            }
            Token::Ubound | Token::Lbound => {
                let name = if matches!(self.peek(), Token::Ubound) {
                    "UBOUND"
                } else {
                    "LBOUND"
                };
                let name = name.to_string();
                self.advance();
                if self.peek() == &Token::LParen {
                    self.advance();
                    let args = self.parse_arg_list()?;
                    self.expect(&Token::RParen)?;
                    Ok(Expr::FunctionCall(name, args))
                } else {
                    Ok(Expr::FunctionCall(name, Vec::new()))
                }
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            Token::Identifier(name) if name.eq_ignore_ascii_case("VARPTR") => {
                self.advance();
                self.expect(&Token::LParen)?;
                let inner = self.parse_expression()?;
                self.expect(&Token::RParen)?;
                Ok(Expr::Varptr(Box::new(inner)))
            }
            Token::Identifier(name) => {
                self.advance();
                // Check for function call or array access
                let mut expr = if self.peek() == &Token::LParen {
                    self.advance();
                    let args = if self.peek() == &Token::RParen {
                        Vec::new()
                    } else {
                        self.parse_arg_list()?
                    };
                    self.expect(&Token::RParen)?;
                    // Determine if this is a function call or array access
                    // We can't tell at parse time — interpreter will resolve
                    Expr::FunctionCall(name, args)
                } else {
                    Expr::Variable(name)
                };
                // Chained .member access: obj.field1.field2...
                while self.peek() == &Token::Dot {
                    self.advance();
                    let member = self.consume_identifier()?;
                    expr = Expr::TypeMember(Box::new(expr), member);
                }
                Ok(expr)
            }
            _ => Err(PbError::parser(
                format!("Unexpected token in expression: {:?}", self.peek()),
                self.current_file(),
                self.current_line(),
            )),
        }
    }

    fn parse_arg_list(&mut self) -> PbResult<Vec<Expr>> {
        let mut args = Vec::new();
        loop {
            if self.peek() == &Token::RParen || self.at_end() {
                break;
            }
            // Skip ANY keyword (used in PARSE$ calls: PARSE$(str$, ANY "|"))
            if matches!(self.peek(), Token::Identifier(ref s) if s.eq_ignore_ascii_case("ANY"))
                && matches!(self.peek_at(1), Some(Token::StringLiteral(_)))
            {
                self.advance(); // skip ANY
            }
            // BYVAL/BYCOPY modifier at call site: Foo(BYVAL x) or Foo(BYCOPY s$)
            // Both override BYREF parameter to pass value directly
            let mut has_byval = matches!(self.peek(), Token::Byval);
            if has_byval {
                self.advance(); // consume BYVAL
            }
            if matches!(self.peek(), Token::Identifier(ref s) if s.eq_ignore_ascii_case("BYCOPY")) {
                has_byval = true;
                self.advance(); // consume BYCOPY
            }
            let expr = self.parse_expression()?;
            args.push(if has_byval {
                Expr::ByvalOverride(Box::new(expr))
            } else {
                expr
            });
            if self.peek() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(args)
    }
}

/// Infer PB type from variable name suffix
fn type_from_suffix(name: &str) -> PbType {
    if name.ends_with("##") {
        PbType::Ext
    } else if name.ends_with('#') {
        PbType::Double
    } else if name.ends_with("&&") {
        PbType::Quad
    } else if name.ends_with('&') {
        PbType::Long
    } else if name.ends_with("%%") {
        PbType::Word
    } else if name.ends_with('%') {
        PbType::Integer
    } else if name.ends_with('!') {
        PbType::Single
    } else if name.ends_with("@@") || name.ends_with('@') {
        PbType::Cur
    } else if name.ends_with('$') {
        PbType::String
    } else {
        PbType::Long // PB default numeric type
    }
}
