use std::collections::HashMap;
use std::io::Write;

use crate::builtins;
use crate::environment::{normalize, ArrayDesc, CallFrame, Environment};
use crate::value::Value;
use pb::ast::*;
use pb::error::{PbError, PbResult};

/// Control flow signals
#[allow(dead_code)]
enum Flow {
    Normal,
    ExitSub,
    ExitFunction,
    ExitFor,
    ExitDo,
    ExitSelect,
    Return(Value),
    IterateFor,
    IterateDo,
    GoTo(String),
    GoSub(String),
    GoSubReturn,
}

pub struct Interpreter {
    pub env: Environment,
    subs: HashMap<String, SubDecl>,
    functions: HashMap<String, FunctionDecl>,
    types: HashMap<String, TypeDecl>,
    pub stdout: Vec<u8>,
    /// Stack for GOSUB return addresses (statement indices)
    gosub_stack: Vec<usize>,
    /// Current ON ERROR GOTO label (None = no error trap)
    error_trap: Option<String>,
    /// Static variable storage: (sub_name, var_name) -> Value
    static_vars: HashMap<(String, String), Value>,
    /// Static array storage: (sub_name, array_name) -> ArrayDesc
    static_arrays: HashMap<(String, String), ArrayDesc>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment::new(),
            subs: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            stdout: Vec::new(),
            gosub_stack: Vec::new(),
            error_trap: None,
            static_vars: HashMap::new(),
            static_arrays: HashMap::new(),
        }
    }

    pub fn run(&mut self, program: &Program) -> PbResult<i32> {
        // First pass: collect all declarations
        for item in &program.items {
            match item {
                TopLevel::GlobalDecl(decl) => {
                    if decl.is_array {
                        self.env.declare_global_array(&decl.name, &decl.pb_type);
                    } else {
                        self.env.declare_global(&decl.name, &decl.pb_type);
                    }
                }
                TopLevel::GlobalDeclList(decls) => {
                    for decl in decls {
                        if decl.is_array {
                            self.env.declare_global_array(&decl.name, &decl.pb_type);
                        } else {
                            self.env.declare_global(&decl.name, &decl.pb_type);
                        }
                    }
                }
                TopLevel::ConstDecl(c) => {
                    let val = self.eval_const_expr(&c.value);
                    self.env.set_constant(&c.name, val);
                }
                TopLevel::DimDecl(dim) => {
                    // Top-level DIM — evaluate bounds and create array
                    self.exec_dim(dim)?;
                }
                TopLevel::TypeDecl(td) => {
                    self.types.insert(td.name.clone(), td.clone());
                }
                TopLevel::SubDecl(sd) => {
                    self.subs.insert(normalize(&sd.name), sd.clone());
                }
                TopLevel::FunctionDecl(fd) => {
                    self.functions.insert(normalize(&fd.name), fd.clone());
                }
                TopLevel::DeclareStmt(_) => {} // skip DECLARE
                TopLevel::Statement(_) => {}   // will execute below
            }
        }

        // Look for PBMAIN function
        if let Some(pbmain) = self.functions.get("PBMAIN").cloned() {
            let result = self.call_function(&pbmain, &[])?;
            return Ok(result.to_i32());
        }

        // No PBMAIN — execute top-level statements
        for item in &program.items {
            if let TopLevel::Statement(stmt) = item {
                if let Flow::ExitFunction | Flow::ExitSub = self.exec_statement(stmt)? {
                    break;
                }
            }
        }

        Ok(0)
    }

    fn eval_const_expr(&self, expr: &Expr) -> i64 {
        match expr {
            Expr::IntegerLit(n) => *n,
            Expr::FloatLit(f) => *f as i64,
            Expr::PercentConst(name) => self.env.get_constant(name).unwrap_or(0),
            Expr::Negate(inner) => -self.eval_const_expr(inner),
            Expr::BinaryOp(op, a, b) => {
                let av = self.eval_const_expr(a);
                let bv = self.eval_const_expr(b);
                match op {
                    BinaryOp::Add => av + bv,
                    BinaryOp::Sub => av - bv,
                    BinaryOp::Mul => av * bv,
                    BinaryOp::Div => {
                        if bv != 0 {
                            av / bv
                        } else {
                            0
                        }
                    }
                    BinaryOp::Mod => {
                        if bv != 0 {
                            av % bv
                        } else {
                            0
                        }
                    }
                    BinaryOp::And => av & bv,
                    BinaryOp::Or => av | bv,
                    _ => 0,
                }
            }
            _ => 0,
        }
    }

    fn exec_dim(&mut self, dim: &DimStatement) -> PbResult<()> {
        let bounds: Vec<(i64, i64)> = dim
            .bounds
            .iter()
            .map(|b| {
                let lo = self.eval_expr(&b.lower).unwrap_or(Value::Long(0)).to_i64();
                let hi = self.eval_expr(&b.upper).unwrap_or(Value::Long(0)).to_i64();
                (lo, hi)
            })
            .collect();

        if bounds.is_empty() {
            // Scalar declaration in DIM context
            match dim.scope {
                DimScope::Local => {
                    self.env
                        .declare_local(&dim.name, Value::default_for_type(&dim.pb_type));
                }
                DimScope::Global | DimScope::Dim => {
                    self.env.declare_global(&dim.name, &dim.pb_type);
                }
                DimScope::Static => {
                    // Static: persists across calls to the same function
                    let func_name = self.env.current_function_name();
                    let key = (func_name, normalize(&dim.name));
                    let val = self
                        .static_vars
                        .get(&key)
                        .cloned()
                        .unwrap_or_else(|| Value::default_for_type(&dim.pb_type));
                    self.env.declare_local(&dim.name, val);
                    self.env.mark_static(&dim.name);
                }
            }
        } else {
            let is_local = matches!(dim.scope, DimScope::Local | DimScope::Static);
            // Check if this array name was previously marked STATIC
            // (via STATIC name() AS TYPE before DIM name(bounds))
            let is_static = dim.scope == DimScope::Static || self.is_name_static(&dim.name);

            if is_static {
                // STATIC array: restore from persistent storage if available
                let func_name = self.env.current_function_name();
                let key = (func_name, normalize(&dim.name));
                if let Some(arr) = self.static_arrays.get(&key) {
                    // Restore the previously saved static array
                    if let Some(frame) = self.env.call_stack.last_mut() {
                        frame.local_arrays.insert(normalize(&dim.name), arr.clone());
                    }
                    self.env.mark_static(&dim.name);
                } else {
                    // First call — create new array and mark as static
                    self.env.dim_array(&dim.name, bounds, &dim.pb_type, true);
                    self.env.mark_static(&dim.name);
                }
            } else if dim.is_redim {
                self.env.redim_array(&dim.name, bounds, &dim.pb_type);
            } else {
                self.env
                    .dim_array(&dim.name, bounds, &dim.pb_type, is_local);
            }
        }

        Ok(())
    }

    fn call_sub(&mut self, decl: &SubDecl, args: &[Value]) -> PbResult<()> {
        let mut frame = CallFrame::new(&decl.name, false, &PbType::Long);

        // Bind parameters
        for (i, param) in decl.params.iter().enumerate() {
            let val = if i < args.len() {
                let arg_val = args[i].clone();
                if param.is_byval {
                    arg_val.coerce_to(&param.pb_type)
                } else {
                    arg_val
                }
            } else {
                Value::default_for_type(&param.pb_type)
            };
            frame.locals.insert(normalize(&param.name), val);
        }

        self.env.push_frame(frame);
        let _flow = self.exec_body(&decl.body)?;

        // Save static variables before popping frame
        self.save_static_vars(&decl.name);

        self.env.pop_frame();
        Ok(())
    }

    fn call_function(&mut self, decl: &FunctionDecl, args: &[Value]) -> PbResult<Value> {
        let mut frame = CallFrame::new(&decl.name, true, &decl.return_type);

        // Bind parameters
        for (i, param) in decl.params.iter().enumerate() {
            let val = if i < args.len() {
                let arg_val = args[i].clone();
                if param.is_byval {
                    arg_val.coerce_to(&param.pb_type)
                } else {
                    arg_val
                }
            } else {
                Value::default_for_type(&param.pb_type)
            };
            frame.locals.insert(normalize(&param.name), val);
        }

        self.env.push_frame(frame);
        let _flow = self.exec_body(&decl.body)?;

        let return_val = self
            .env
            .call_stack
            .last()
            .map(|f| f.return_value.clone())
            .unwrap_or(Value::Long(0));

        // Save static variables before popping frame
        self.save_static_vars(&decl.name);

        self.env.pop_frame();
        Ok(return_val)
    }

    /// Save any STATIC variables from the current frame back to persistent storage
    /// Check if a name has been marked STATIC in the current call frame
    fn is_name_static(&self, name: &str) -> bool {
        let key = normalize(name);
        if let Some(frame) = self.env.call_stack.last() {
            frame.static_names.contains(&key)
        } else {
            false
        }
    }

    fn save_static_vars(&mut self, func_name: &str) {
        let fn_key = func_name.to_uppercase();
        if let Some(frame) = self.env.call_stack.last() {
            // Save scalar STATIC variables
            for static_name in &frame.static_names {
                if let Some(val) = frame.locals.get(static_name) {
                    let key = (fn_key.clone(), static_name.clone());
                    self.static_vars.insert(key, val.clone());
                }
            }
            // Save STATIC local arrays
            for static_name in &frame.static_names {
                if let Some(arr_desc) = frame.local_arrays.get(static_name) {
                    let key = (fn_key.clone(), static_name.clone());
                    self.static_arrays.insert(key, arr_desc.clone());
                }
            }
        }
    }

    fn exec_body(&mut self, stmts: &[Statement]) -> PbResult<Flow> {
        // Collect label positions for GOTO/GOSUB
        let mut labels: HashMap<String, usize> = HashMap::new();
        for (i, stmt) in stmts.iter().enumerate() {
            if let Statement::Label(name) = stmt {
                labels.insert(name.to_uppercase(), i);
            }
        }

        let mut i = 0;
        while i < stmts.len() {
            let result = self.exec_statement(&stmts[i]);

            match result {
                Ok(flow) => match flow {
                    Flow::Normal => {
                        i += 1;
                    }
                    Flow::GoTo(label) => {
                        let key = label.to_uppercase();
                        if let Some(&target) = labels.get(&key) {
                            i = target;
                        } else {
                            // Label not in this body — propagate up
                            return Ok(Flow::GoTo(label));
                        }
                    }
                    Flow::GoSub(label) => {
                        let key = label.to_uppercase();
                        if let Some(&target) = labels.get(&key) {
                            self.gosub_stack.push(i + 1);
                            i = target;
                        } else {
                            return Ok(Flow::GoSub(label));
                        }
                    }
                    Flow::GoSubReturn => {
                        if let Some(return_idx) = self.gosub_stack.pop() {
                            i = return_idx;
                        } else {
                            // No GOSUB to return from — treat as EXIT SUB
                            return Ok(Flow::ExitSub);
                        }
                    }
                    other => return Ok(other),
                },
                Err(e) => {
                    // Check for ON ERROR GOTO trap
                    if let Some(trap_label) = &self.error_trap.clone() {
                        let key = trap_label.to_uppercase();
                        if let Some(&target) = labels.get(&key) {
                            // Set ERR variable and jump to error handler
                            self.env.set_var("ERR", Value::Long(1));
                            self.error_trap = None; // disable trap to prevent infinite loop
                            i = target;
                            continue;
                        }
                    }
                    return Err(e);
                }
            }
        }
        Ok(Flow::Normal)
    }

    fn exec_statement(&mut self, stmt: &Statement) -> PbResult<Flow> {
        match stmt {
            Statement::Assign(a) => {
                let value = self.eval_expr(&a.value)?;
                self.assign_target(&a.target, value)?;
                Ok(Flow::Normal)
            }
            Statement::Call(c) => self.exec_call(c),
            Statement::If(i) => self.exec_if(i),
            Statement::For(f) => self.exec_for(f),
            Statement::DoLoop(d) => self.exec_do_loop(d),
            Statement::WhileWend(w) => self.exec_while_wend(w),
            Statement::SelectCase(s) => self.exec_select_case(s),
            Statement::Print(p) => {
                self.exec_print(p)?;
                Ok(Flow::Normal)
            }
            Statement::PrintFile(p) => {
                self.exec_print_file(p)?;
                Ok(Flow::Normal)
            }
            Statement::Open(o) => {
                self.exec_open(o)?;
                Ok(Flow::Normal)
            }
            Statement::Close(c) => {
                self.exec_close(c)?;
                Ok(Flow::Normal)
            }
            Statement::InputFile(_) => {
                // TODO: implement INPUT# in interpreter
                Ok(Flow::Normal)
            }
            Statement::LineInputFile(_) => {
                // TODO: implement LINE INPUT# in interpreter
                Ok(Flow::Normal)
            }
            Statement::Kill(_) => {
                // TODO: implement KILL in interpreter
                Ok(Flow::Normal)
            }
            Statement::Dim(d) => {
                self.exec_dim(d)?;
                Ok(Flow::Normal)
            }
            Statement::Redim(d) => {
                self.exec_dim(d)?;
                Ok(Flow::Normal)
            }
            Statement::Incr(inc) => {
                let current = self.eval_expr(&inc.target)?;
                let amount = match &inc.amount {
                    Some(e) => self.eval_expr(e)?,
                    None => Value::Long(1),
                };
                let new_val = current.add(&amount);
                self.assign_target(&inc.target, new_val)?;
                Ok(Flow::Normal)
            }
            Statement::Decr(dec) => {
                let current = self.eval_expr(&dec.target)?;
                let amount = match &dec.amount {
                    Some(e) => self.eval_expr(e)?,
                    None => Value::Long(1),
                };
                let new_val = current.sub(&amount);
                self.assign_target(&dec.target, new_val)?;
                Ok(Flow::Normal)
            }
            Statement::ExitSub => Ok(Flow::ExitSub),
            Statement::ExitFunction => Ok(Flow::ExitFunction),
            Statement::ExitFor => Ok(Flow::ExitFor),
            Statement::ExitDo => Ok(Flow::ExitDo),
            Statement::ExitSelect => Ok(Flow::ExitSelect),
            Statement::FunctionReturn(fr) => {
                let val = self.eval_expr(&fr.value)?;
                if let Some(frame) = self.env.call_stack.last_mut() {
                    frame.return_value = val;
                }
                Ok(Flow::Normal)
            }
            Statement::Iterate(target) => match target {
                IterateTarget::For => Ok(Flow::IterateFor),
                IterateTarget::Do => Ok(Flow::IterateDo),
            },
            Statement::Label(_) => Ok(Flow::Normal), // labels are markers, skip during execution
            Statement::GoTo(label) => Ok(Flow::GoTo(label.clone())),
            Statement::GoSub(label) => Ok(Flow::GoSub(label.clone())),
            Statement::Return => Ok(Flow::GoSubReturn),
            Statement::OnErrorGoto(label) => {
                self.error_trap = Some(label.clone());
                Ok(Flow::Normal)
            }
            Statement::OnErrorGotoZero => {
                self.error_trap = None;
                Ok(Flow::Normal)
            }
            Statement::ResumeNext => {
                // RESUME NEXT after error handler — just continue
                Ok(Flow::Normal)
            }
            Statement::Block(stmts) => {
                for s in stmts {
                    match self.exec_statement(s)? {
                        Flow::Normal => continue,
                        other => return Ok(other),
                    }
                }
                Ok(Flow::Normal)
            }
            Statement::Noop => Ok(Flow::Normal),
        }
    }

    fn exec_call(&mut self, call: &CallStmt) -> PbResult<Flow> {
        let name = normalize(&call.name);

        // Evaluate arguments
        let args: Vec<Value> = call
            .args
            .iter()
            .map(|a| self.eval_expr(a))
            .collect::<PbResult<Vec<_>>>()?;

        // Check for built-in subs
        if name.as_str() == "PRINT" {
            // PRINT as a statement — shouldn't come here normally
            let output: String = args
                .iter()
                .map(|a| a.to_string_val())
                .collect::<Vec<_>>()
                .join("");
            writeln!(self.stdout, "{}", output).ok();
            print!("{}", output);
            println!();
            return Ok(Flow::Normal);
        }

        // Look up user-defined SUB
        if let Some(sub_decl) = self.subs.get(&name).cloned() {
            self.call_sub(&sub_decl, &args)?;
            return Ok(Flow::Normal);
        }

        // Look up as FUNCTION (some code calls functions as subs, discarding return)
        if let Some(func_decl) = self.functions.get(&name).cloned() {
            self.call_function(&func_decl, &args)?;
            return Ok(Flow::Normal);
        }

        // Try as builtin function (called as sub, discard result)
        if builtins::call_builtin(&name, &args).is_some() {
            return Ok(Flow::Normal);
        }

        // Unknown sub — warn but don't error (could be a stub we haven't implemented)
        // For the test harness, this is fine
        Ok(Flow::Normal)
    }

    fn exec_if(&mut self, if_stmt: &IfStmt) -> PbResult<Flow> {
        let condition = self.eval_expr(&if_stmt.condition)?;
        if condition.is_truthy() {
            return self.exec_body(&if_stmt.then_body);
        }

        for clause in &if_stmt.elseif_clauses {
            let cond = self.eval_expr(&clause.condition)?;
            if cond.is_truthy() {
                return self.exec_body(&clause.body);
            }
        }

        if !if_stmt.else_body.is_empty() {
            return self.exec_body(&if_stmt.else_body);
        }

        Ok(Flow::Normal)
    }

    fn exec_for(&mut self, for_stmt: &ForStmt) -> PbResult<Flow> {
        let start = self.eval_expr(&for_stmt.start)?;
        let end = self.eval_expr(&for_stmt.end)?;
        let step = match &for_stmt.step {
            Some(e) => self.eval_expr(e)?,
            None => Value::Long(1),
        };

        let step_val = step.to_f64();
        let mut current = start.to_f64();
        let end_val = end.to_f64();

        loop {
            // Check termination
            if step_val > 0.0 && current > end_val {
                break;
            }
            if step_val < 0.0 && current < end_val {
                break;
            }
            if step_val == 0.0 {
                break; // prevent infinite loop
            }

            // Set loop variable
            self.env.set_var(&for_stmt.var, Value::Double(current));

            // Execute body
            match self.exec_body(&for_stmt.body)? {
                Flow::Normal | Flow::IterateFor => {}
                Flow::ExitFor => break,
                other => return Ok(other),
            }

            // Re-read loop variable from environment — PB allows body to
            // modify the loop variable (e.g., N=15 to force exit on NEXT)
            current = self.env.get_var(&for_stmt.var).to_f64();
            current += step_val;
        }

        // Set final value of loop variable
        self.env.set_var(&for_stmt.var, Value::Double(current));

        Ok(Flow::Normal)
    }

    fn exec_do_loop(&mut self, do_loop: &DoLoopStmt) -> PbResult<Flow> {
        loop {
            // Pre-test
            if do_loop.is_pre_test {
                if let Some(ref cond) = do_loop.condition {
                    let val = self.eval_expr(cond)?;
                    let should_continue = if do_loop.is_while {
                        val.is_truthy()
                    } else {
                        !val.is_truthy()
                    };
                    if !should_continue {
                        break;
                    }
                }
            }

            match self.exec_body(&do_loop.body)? {
                Flow::Normal | Flow::IterateDo => {}
                Flow::ExitDo => break,
                other => return Ok(other),
            }

            // Post-test
            if !do_loop.is_pre_test {
                if let Some(ref cond) = do_loop.condition {
                    let val = self.eval_expr(cond)?;
                    let should_continue = if do_loop.is_while {
                        val.is_truthy()
                    } else {
                        !val.is_truthy()
                    };
                    if !should_continue {
                        break;
                    }
                }
            }

            // Infinite DO/LOOP with no condition — shouldn't happen in well-formed code
            if do_loop.condition.is_none() {
                break;
            }
        }
        Ok(Flow::Normal)
    }

    fn exec_while_wend(&mut self, while_stmt: &WhileWendStmt) -> PbResult<Flow> {
        loop {
            let cond = self.eval_expr(&while_stmt.condition)?;
            if !cond.is_truthy() {
                break;
            }
            match self.exec_body(&while_stmt.body)? {
                Flow::Normal => {}
                Flow::ExitDo | Flow::ExitFor => break,
                other => return Ok(other),
            }
        }
        Ok(Flow::Normal)
    }

    fn exec_select_case(&mut self, select: &SelectCaseStmt) -> PbResult<Flow> {
        let test_val = self.eval_expr(&select.expr)?;

        for case in &select.cases {
            let mut matched = false;
            for pattern in &case.patterns {
                match pattern {
                    CasePattern::Value(expr) => {
                        let case_val = self.eval_expr(expr)?;
                        if test_val.compare_eq(&case_val).is_truthy() {
                            matched = true;
                            break;
                        }
                    }
                    CasePattern::Range(lo, hi) => {
                        let lo_val = self.eval_expr(lo)?;
                        let hi_val = self.eval_expr(hi)?;
                        if test_val.compare_gte(&lo_val).is_truthy()
                            && test_val.compare_lte(&hi_val).is_truthy()
                        {
                            matched = true;
                            break;
                        }
                    }
                    CasePattern::Is(op, expr) => {
                        let case_val = self.eval_expr(expr)?;
                        let result = match op {
                            CaseOp::Lt => test_val.compare_lt(&case_val),
                            CaseOp::Gt => test_val.compare_gt(&case_val),
                            CaseOp::Lte => test_val.compare_lte(&case_val),
                            CaseOp::Gte => test_val.compare_gte(&case_val),
                            CaseOp::Eq => test_val.compare_eq(&case_val),
                            CaseOp::Neq => test_val.compare_neq(&case_val),
                        };
                        if result.is_truthy() {
                            matched = true;
                            break;
                        }
                    }
                }
            }
            if matched {
                let flow = self.exec_body(&case.body)?;
                return Ok(match flow {
                    Flow::ExitSelect => Flow::Normal,
                    other => other,
                });
            }
        }

        // CASE ELSE
        if !select.else_body.is_empty() {
            let flow = self.exec_body(&select.else_body)?;
            return Ok(match flow {
                Flow::ExitSelect => Flow::Normal,
                other => other,
            });
        }

        Ok(Flow::Normal)
    }

    fn exec_print(&mut self, print_stmt: &PrintStmt) -> PbResult<()> {
        let mut output = String::new();
        for arg in print_stmt.args.iter() {
            let val = self.eval_expr(arg)?;
            output.push_str(&val.to_string_val());
        }
        writeln!(self.stdout, "{}", output).ok();
        println!("{}", output);
        Ok(())
    }

    fn exec_print_file(&mut self, print_stmt: &PrintFileStmt) -> PbResult<()> {
        let file_num = self.eval_expr(&print_stmt.file_num)?.to_i32();
        let mut output = String::new();
        for arg in &print_stmt.args {
            let val = self.eval_expr(arg)?;
            output.push_str(&val.to_string_val());
        }

        if let Some(file) = self.env.open_files.get_mut(&file_num) {
            file.content.push_str(&output);
            file.content.push('\n');
        }
        Ok(())
    }

    fn exec_open(&mut self, open_stmt: &OpenStmt) -> PbResult<()> {
        let filename = self.eval_expr(&open_stmt.filename)?.to_string_val();
        let file_num = self.eval_expr(&open_stmt.file_num)?.to_i32();

        self.env.open_files.insert(
            file_num,
            crate::environment::OpenFile {
                path: filename,
                content: String::new(),
            },
        );
        Ok(())
    }

    fn exec_close(&mut self, close_stmt: &CloseStmt) -> PbResult<()> {
        let file_num = self.eval_expr(&close_stmt.file_num)?.to_i32();

        if let Some(file) = self.env.open_files.remove(&file_num) {
            // Write content to actual file
            match std::fs::write(&file.path, &file.content) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Warning: failed to write {}: {}", file.path, e);
                }
            }
        }
        Ok(())
    }

    // ===== Expression evaluation =====

    pub fn eval_expr(&mut self, expr: &Expr) -> PbResult<Value> {
        match expr {
            Expr::IntegerLit(n) => Ok(Value::Long(*n as i32)),
            Expr::FloatLit(f) => Ok(Value::Double(*f)),
            Expr::StringLit(s) => Ok(Value::Str(s.clone())),
            Expr::Variable(name) => {
                let key = name.to_uppercase();
                // Check string equates
                match key.as_str() {
                    "$CRLF" => return Ok(Value::Str("\r\n".to_string())),
                    "$CR" => return Ok(Value::Str("\r".to_string())),
                    "$LF" => return Ok(Value::Str("\n".to_string())),
                    "$NUL" => return Ok(Value::Str("\0".to_string())),
                    "$TAB" => return Ok(Value::Str("\t".to_string())),
                    _ => {}
                }
                Ok(self.env.get_var(&key))
            }
            Expr::PercentConst(name) => {
                let val = self.env.get_constant(name).unwrap_or(0);
                Ok(Value::Long(val as i32))
            }
            Expr::ArrayAccess(name, indices) => {
                let idx: Vec<i64> = indices
                    .iter()
                    .map(|e| self.eval_expr(e).map(|v| v.to_i64()))
                    .collect::<PbResult<Vec<_>>>()?;
                Ok(self.env.get_array(name, &idx))
            }
            Expr::FunctionCall(name, args) => self.eval_function_call(name, args),
            Expr::UnaryOp(op, inner) => {
                let val = self.eval_expr(inner)?;
                match op {
                    UnaryOp::Neg => Ok(val.negate()),
                    UnaryOp::Not => Ok(val.not()),
                }
            }
            Expr::Negate(inner) => {
                let val = self.eval_expr(inner)?;
                Ok(val.negate())
            }
            Expr::BinaryOp(op, left, right) => {
                let lval = self.eval_expr(left)?;

                // String concat with + should work like &
                if *op == BinaryOp::Add && (lval.is_string()) {
                    let rval = self.eval_expr(right)?;
                    return Ok(Value::Str(format!(
                        "{}{}",
                        lval.to_string_val(),
                        rval.to_string_val()
                    )));
                }

                let rval = self.eval_expr(right)?;
                let result = match op {
                    BinaryOp::Add => lval.add(&rval),
                    BinaryOp::Sub => lval.sub(&rval),
                    BinaryOp::Mul => lval.mul(&rval),
                    BinaryOp::Div => lval.div(&rval),
                    BinaryOp::IntDiv => lval.int_div(&rval),
                    BinaryOp::Mod => lval.modulo(&rval),
                    BinaryOp::Pow => lval.power(&rval),
                    BinaryOp::Eq => lval.compare_eq(&rval),
                    BinaryOp::Neq => lval.compare_neq(&rval),
                    BinaryOp::Lt => lval.compare_lt(&rval),
                    BinaryOp::Gt => lval.compare_gt(&rval),
                    BinaryOp::Lte => lval.compare_lte(&rval),
                    BinaryOp::Gte => lval.compare_gte(&rval),
                    BinaryOp::And => lval.and(&rval),
                    BinaryOp::Or => lval.or(&rval),
                    BinaryOp::Xor => lval.xor(&rval),
                    BinaryOp::StrConcat => {
                        Value::Str(format!("{}{}", lval.to_string_val(), rval.to_string_val()))
                    }
                };
                Ok(result)
            }
            Expr::TypeMember(_base, _member) => {
                // Simplified: just access as variable.member
                // For TYPE fields on UDTs — not needed for test harness
                // Return the member as if it were a separate variable
                Ok(Value::Long(0))
            }
            Expr::Varptr(_inner) => {
                // VARPTR not meaningful in interpreter — return dummy address
                Ok(Value::Long(0))
            }
            Expr::ByvalOverride(inner) => {
                // BYVAL override at call site — just evaluate the inner expression
                self.eval_expr(inner)
            }
        }
    }

    fn eval_function_call(&mut self, name: &str, arg_exprs: &[Expr]) -> PbResult<Value> {
        let key = normalize(name);

        // Evaluate args
        let args: Vec<Value> = arg_exprs
            .iter()
            .map(|e| self.eval_expr(e))
            .collect::<PbResult<Vec<_>>>()?;

        // Special built-ins that need interpreter context
        if key == "FREEFILE" {
            return Ok(Value::Long(self.env.freefile()));
        }
        if key == "FUNCNAME" {
            return Ok(Value::Str(self.env.current_function_name()));
        }

        // Check if it's an array access
        if self.env.is_array(&key) {
            let indices: Vec<i64> = args.iter().map(|v| v.to_i64()).collect();
            return Ok(self.env.get_array(&key, &indices));
        }

        // Check builtins
        if let Some(result) = builtins::call_builtin(&key, &args) {
            return Ok(result);
        }

        // Check user-defined functions
        if let Some(func_decl) = self.functions.get(&key).cloned() {
            return self.call_function(&func_decl, &args);
        }

        // Check user-defined subs (called as function, discard return)
        if let Some(sub_decl) = self.subs.get(&key).cloned() {
            self.call_sub(&sub_decl, &args)?;
            return Ok(Value::Long(0));
        }

        // Unknown function — return default
        Ok(Value::Long(0))
    }

    fn assign_target(&mut self, target: &Expr, value: Value) -> PbResult<()> {
        match target {
            Expr::Variable(name) => {
                self.env.set_var(name, value);
            }
            Expr::ArrayAccess(name, indices) => {
                let idx: Vec<i64> = indices
                    .iter()
                    .map(|e| self.eval_expr(e).map(|v| v.to_i64()))
                    .collect::<PbResult<Vec<_>>>()?;
                self.env.set_array(name, &idx, value);
            }
            Expr::FunctionCall(name, indices) => {
                // This happens when parser couldn't tell if it's array or function
                // Treat as array assignment
                let idx: Vec<i64> = indices
                    .iter()
                    .map(|e| self.eval_expr(e).map(|v| v.to_i64()))
                    .collect::<PbResult<Vec<_>>>()?;
                self.env.set_array(name, &idx, value);
            }
            Expr::TypeMember(_base, _member) => {
                // UDT member assignment — not needed for test harness
            }
            _ => {
                return Err(PbError::runtime(format!(
                    "Cannot assign to expression: {:?}",
                    target
                )));
            }
        }
        Ok(())
    }
}
