//! Type-safe LLVM IR text builder.
//!
//! Generates LLVM IR (.ll) text that can be compiled by `clang`.
//! This avoids requiring LLVM development libraries (llvm-sys/inkwell)
//! while producing identical native code output.

use std::collections::HashSet;
use std::fmt::Write;

/// LLVM IR types
#[derive(Debug, Clone, PartialEq)]
pub enum IrType {
    I1,
    I8,
    I16,
    I32,
    I64,
    Float,
    Double,
    Void,
    Ptr,                       // opaque pointer (LLVM 17+ default)
    Array(usize, Box<IrType>), // [N x T]
    Struct(String),            // named struct reference (%MyType)
}

impl std::fmt::Display for IrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrType::I1 => write!(f, "i1"),
            IrType::I8 => write!(f, "i8"),
            IrType::I16 => write!(f, "i16"),
            IrType::I32 => write!(f, "i32"),
            IrType::I64 => write!(f, "i64"),
            IrType::Float => write!(f, "float"),
            IrType::Double => write!(f, "double"),
            IrType::Void => write!(f, "void"),
            IrType::Ptr => write!(f, "ptr"),
            IrType::Array(count, elem_ty) => write!(f, "[{} x {}]", count, elem_ty),
            IrType::Struct(name) => write!(f, "%{}", name),
        }
    }
}

impl IrType {
    pub fn is_float(&self) -> bool {
        matches!(self, IrType::Float | IrType::Double)
    }

    pub fn is_int(&self) -> bool {
        matches!(
            self,
            IrType::I1 | IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64
        )
    }

    #[allow(dead_code)]
    pub fn is_array(&self) -> bool {
        matches!(self, IrType::Array(..))
    }

    #[allow(dead_code)]
    pub fn is_struct(&self) -> bool {
        matches!(self, IrType::Struct(..))
    }

    pub fn bit_width(&self) -> u32 {
        match self {
            IrType::I1 => 1,
            IrType::I8 => 8,
            IrType::I16 => 16,
            IrType::I32 => 32,
            IrType::I64 => 64,
            IrType::Float => 32,
            IrType::Double => 64,
            _ => 0,
        }
    }

    pub fn zero_literal(&self) -> String {
        match self {
            IrType::I1 | IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64 => "0".to_string(),
            IrType::Ptr => "null".to_string(),
            IrType::Float => "0.0".to_string(),
            IrType::Double => "0.0".to_string(),
            _ => "zeroinitializer".to_string(),
        }
    }
}

/// A named SSA value with its type.
#[derive(Debug, Clone)]
pub struct Val {
    pub name: String,
    pub ty: IrType,
}

impl Val {
    pub fn new(name: impl Into<String>, ty: IrType) -> Self {
        Val {
            name: name.into(),
            ty,
        }
    }

    /// Format as typed operand: `i32 %x`
    pub fn typed(&self) -> String {
        format!("{} {}", self.ty, self.name)
    }
}

/// Builds LLVM IR module text.
pub struct ModuleBuilder {
    /// Target triple
    target_triple: String,
    /// Global declarations (appear at top of module)
    globals: String,
    /// Function definitions
    functions: String,
    /// String constant counter
    str_counter: usize,
    /// Track declared external functions to avoid duplicates
    declared: HashSet<String>,
}

impl ModuleBuilder {
    pub fn new(target_triple: &str) -> Self {
        ModuleBuilder {
            target_triple: target_triple.to_string(),
            globals: String::new(),
            declared: HashSet::new(),
            functions: String::new(),
            str_counter: 0,
        }
    }

    /// Whether this module targets a 32-bit architecture.
    pub fn is_32bit(&self) -> bool {
        self.target_triple.starts_with("i686") || self.target_triple.starts_with("i386")
    }

    /// Declare an external function.
    pub fn declare_function(
        &mut self,
        name: &str,
        ret: &IrType,
        params: &[IrType],
        variadic: bool,
    ) {
        if !self.declared.insert(name.to_string()) {
            return; // already declared
        }
        let params_str: Vec<String> = params.iter().map(|t| t.to_string()).collect();
        let mut sig = params_str.join(", ");
        if variadic {
            if !sig.is_empty() {
                sig.push_str(", ");
            }
            sig.push_str("...");
        }
        writeln!(self.globals, "declare {} @{}({})", ret, name, sig).unwrap();
    }

    /// Declare an external global variable.
    pub fn declare_external_global(&mut self, name: &str, ty: &IrType) {
        writeln!(self.globals, "@{} = external global {}", name, ty).unwrap();
    }

    /// Declare a dllimport function (from an external DLL).
    /// On 32-bit targets, uses x86_stdcallcc (Windows API convention).
    pub fn declare_dllimport(&mut self, name: &str, ret: &IrType, params: &[IrType]) {
        if !self.declared.insert(name.to_string()) {
            return; // already declared
        }
        let params_str: Vec<String> = params.iter().map(|t| t.to_string()).collect();
        if self.is_32bit() {
            writeln!(
                self.globals,
                "declare dllimport x86_stdcallcc {} @{}({})",
                ret,
                name,
                params_str.join(", ")
            )
            .unwrap();
        } else {
            writeln!(
                self.globals,
                "declare dllimport {} @{}({})",
                ret,
                name,
                params_str.join(", ")
            )
            .unwrap();
        }
    }

    /// Declare a dllimport function with cdecl convention (for C runtime and ui.dll functions).
    pub fn declare_dllimport_cdecl(&mut self, name: &str, ret: &IrType, params: &[IrType]) {
        if !self.declared.insert(name.to_string()) {
            return; // already declared
        }
        let params_str: Vec<String> = params.iter().map(|t| t.to_string()).collect();
        writeln!(
            self.globals,
            "declare dllimport {} @{}({})",
            ret,
            name,
            params_str.join(", ")
        )
        .unwrap();
    }

    /// Declare an LLVM intrinsic.
    pub fn declare_intrinsic(&mut self, name: &str, ret: &IrType, params: &[IrType]) {
        self.declare_function(name, ret, params, false);
    }

    /// Define a named struct type: `%name = type { field_types... }`
    pub fn define_struct_type(&mut self, name: &str, field_types: &[IrType]) {
        let fields_str: Vec<String> = field_types.iter().map(|t| t.to_string()).collect();
        writeln!(
            self.globals,
            "%{} = type {{ {} }}",
            name,
            fields_str.join(", ")
        )
        .unwrap();
    }

    /// Add a global variable.
    pub fn add_global(&mut self, name: &str, ty: &IrType, init: &str) {
        writeln!(self.globals, "@{} = global {} {}", name, ty, init).unwrap();
    }

    /// Add a global variable with dllexport attribute.
    pub fn add_global_exported(&mut self, name: &str, ty: &IrType, init: &str) {
        writeln!(self.globals, "@{} = dllexport global {} {}", name, ty, init).unwrap();
    }

    /// Add a global string constant. Returns the global name.
    pub fn add_string_constant(&mut self, value: &str) -> (String, usize) {
        let name = format!(".str.{}", self.str_counter);
        self.str_counter += 1;
        let len = value.len() + 1; // +1 for null terminator
                                   // Escape special characters for LLVM IR string
        let escaped = llvm_escape_string(value);
        writeln!(
            self.globals,
            "@{} = private unnamed_addr constant [{} x i8] c\"{}\\00\"",
            name, len, escaped
        )
        .unwrap();
        (format!("@{}", name), len)
    }

    /// Create a new function builder. Call `finish()` and pass result to `add_function_body()`.
    pub fn create_function_builder(
        &self,
        name: &str,
        ret: &IrType,
        params: &[(String, IrType)],
    ) -> FunctionBuilder {
        FunctionBuilder::new(name, ret, params, false)
    }

    /// Create a function builder with optional dllexport attribute.
    pub fn create_function_builder_ex(
        &self,
        name: &str,
        ret: &IrType,
        params: &[(String, IrType)],
        export: bool,
    ) -> FunctionBuilder {
        FunctionBuilder::new(name, ret, params, export)
    }

    /// Add a completed function body to the module.
    pub fn add_function_body(&mut self, body: String) {
        self.functions.push_str(&body);
    }

    /// Emit the complete module as LLVM IR text.
    pub fn emit(&self) -> String {
        let mut out = String::new();
        writeln!(out, "; Generated by pbcompiler").unwrap();
        if self.is_32bit() {
            writeln!(out, "target datalayout = \"e-m:x-p:32:32-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32-a:0:32-S32\"").unwrap();
        }
        writeln!(out, "target triple = \"{}\"", self.target_triple).unwrap();
        writeln!(out).unwrap();
        out.push_str(&self.globals);
        writeln!(out).unwrap();
        out.push_str(&self.functions);
        out
    }
}

/// Builds a single LLVM IR function.
pub struct FunctionBuilder {
    body: String,
    /// Alloca instructions — always emitted in the entry block
    allocas: String,
    /// Byte offset in `body` right after "entry:\n" — where allocas get inserted
    entry_end_pos: usize,
    /// SSA register counter
    reg_counter: usize,
    /// Basic block label counter
    label_counter: usize,
    /// Current basic block label (for tracking)
    current_label: String,
    /// Whether the current block has been terminated
    terminated: bool,
}

impl FunctionBuilder {
    fn new(name: &str, ret: &IrType, params: &[(String, IrType)], export: bool) -> Self {
        let params_str: Vec<String> = params
            .iter()
            .map(|(n, t)| format!("{} %{}", t, n))
            .collect();
        let export_attr = if export { "dllexport " } else { "" };
        let header = format!(
            "define {}{} @{}({}) {{\n",
            export_attr,
            ret,
            name,
            params_str.join(", ")
        );
        let mut fb = FunctionBuilder {
            body: header,
            allocas: String::new(),
            entry_end_pos: 0,
            reg_counter: 0,
            label_counter: 0,
            current_label: "entry".to_string(),
            terminated: false,
        };
        writeln!(fb.body, "entry:").unwrap();
        fb.entry_end_pos = fb.body.len();
        fb
    }

    /// Finish building and return the function IR text.
    /// Inserts all alloca instructions into the entry block.
    pub fn finish(mut self) -> String {
        if !self.allocas.is_empty() {
            self.body.insert_str(self.entry_end_pos, &self.allocas);
        }
        self.body.push_str("}\n\n");
        self.body
    }

    /// Get a fresh SSA register name.
    pub fn next_reg(&mut self) -> String {
        let r = format!("%r{}", self.reg_counter);
        self.reg_counter += 1;
        r
    }

    /// Get a fresh label name.
    pub fn next_label(&mut self, prefix: &str) -> String {
        let l = format!("{}.{}", prefix, self.label_counter);
        self.label_counter += 1;
        l
    }

    /// Whether the current block has a terminator.
    pub fn is_terminated(&self) -> bool {
        self.terminated
    }

    /// Emit a label (starts a new basic block).
    pub fn label(&mut self, name: &str) {
        writeln!(self.body, "{}:", name).unwrap();
        self.current_label = name.to_string();
        self.terminated = false;
    }

    // ===== Memory =====

    /// `%r = alloca ty` — always emitted in the entry block.
    pub fn alloca(&mut self, ty: &IrType) -> Val {
        let r = self.next_reg();
        writeln!(self.allocas, "  {} = alloca {}", r, ty).unwrap();
        Val::new(r, IrType::Ptr)
    }

    /// `store ty %val, ptr %ptr`
    pub fn store(&mut self, val: &Val, ptr: &Val) {
        writeln!(self.body, "  store {}, ptr {}", val.typed(), ptr.name).unwrap();
    }

    /// `%r = load ty, ptr %ptr`
    pub fn load(&mut self, ty: &IrType, ptr: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = load {}, ptr {}", r, ty, ptr.name).unwrap();
        Val::new(r, ty.clone())
    }

    /// `%r = getelementptr inbounds [N x T], ptr %base, i32 0, i32 %idx`
    pub fn gep_array(&mut self, array_ty: &IrType, base: &Val, index: &Val) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = getelementptr inbounds {}, ptr {}, i32 0, i32 {}",
            r, array_ty, base.name, index.name
        )
        .unwrap();
        Val::new(r, IrType::Ptr)
    }

    /// `%r = getelementptr inbounds %StructName, ptr %base, i32 0, i32 field_idx`
    pub fn gep_struct(&mut self, struct_name: &str, base: &Val, field_idx: usize) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = getelementptr inbounds %{}, ptr {}, i32 0, i32 {}",
            r, struct_name, base.name, field_idx
        )
        .unwrap();
        Val::new(r, IrType::Ptr)
    }

    /// `%r = getelementptr i8, ptr %base, i32 %offset`
    pub fn gep_byte(&mut self, base: &Val, offset: &Val) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = getelementptr i8, ptr {}, i32 {}",
            r, base.name, offset.name
        )
        .unwrap();
        Val::new(r, IrType::Ptr)
    }

    /// `%r = ptrtoint ptr %val to i32` (32-bit target)
    pub fn ptrtoint(&mut self, val: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = ptrtoint ptr {} to i32", r, val.name).unwrap();
        Val::new(r, IrType::I32)
    }

    /// `%r = inttoptr i32 %val to ptr` (32-bit target)
    pub fn inttoptr(&mut self, val: &Val) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = inttoptr {} {} to ptr",
            r, val.ty, val.name
        )
        .unwrap();
        Val::new(r, IrType::Ptr)
    }

    // ===== Integer arithmetic =====

    pub fn add(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = add {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn sub(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = sub {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn mul(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = mul {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn sdiv(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = sdiv {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn srem(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = srem {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn and(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = and {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn or(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = or {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn shl(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = shl {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn lshr(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = lshr {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn xor(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = xor {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    /// Integer negation: `sub 0, %val`
    pub fn neg(&mut self, val: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = sub {} 0, {}", r, val.ty, val.name).unwrap();
        Val::new(r, val.ty.clone())
    }

    /// Bitwise NOT: `xor %val, -1`
    pub fn not(&mut self, val: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = xor {} {}, -1", r, val.ty, val.name).unwrap();
        Val::new(r, val.ty.clone())
    }

    // ===== Float arithmetic =====

    pub fn fadd(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = fadd {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn fsub(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = fsub {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn fmul(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = fmul {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn fdiv(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = fdiv {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn frem(&mut self, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = frem {} {}, {}", r, a.ty, a.name, b.name).unwrap();
        Val::new(r, a.ty.clone())
    }

    pub fn fneg(&mut self, val: &Val) -> Val {
        let r = self.next_reg();
        writeln!(self.body, "  {} = fneg {} {}", r, val.ty, val.name).unwrap();
        Val::new(r, val.ty.clone())
    }

    // ===== Comparisons =====

    /// Integer comparison. Returns i1.
    pub fn icmp(&mut self, pred: &str, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = icmp {} {} {}, {}",
            r, pred, a.ty, a.name, b.name
        )
        .unwrap();
        Val::new(r, IrType::I1)
    }

    /// Float comparison. Returns i1.
    pub fn fcmp(&mut self, pred: &str, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = fcmp {} {} {}, {}",
            r, pred, a.ty, a.name, b.name
        )
        .unwrap();
        Val::new(r, IrType::I1)
    }

    // ===== Conversions =====

    /// Sign-extend integer to wider type.
    pub fn sext(&mut self, val: &Val, to: &IrType) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = sext {} {} to {}",
            r, val.ty, val.name, to
        )
        .unwrap();
        Val::new(r, to.clone())
    }

    /// Truncate integer to narrower type.
    pub fn trunc(&mut self, val: &Val, to: &IrType) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = trunc {} {} to {}",
            r, val.ty, val.name, to
        )
        .unwrap();
        Val::new(r, to.clone())
    }

    /// Float extend (e.g. float → double).
    pub fn fpext(&mut self, val: &Val, to: &IrType) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = fpext {} {} to {}",
            r, val.ty, val.name, to
        )
        .unwrap();
        Val::new(r, to.clone())
    }

    /// Float truncate (e.g. double → float).
    pub fn fptrunc(&mut self, val: &Val, to: &IrType) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = fptrunc {} {} to {}",
            r, val.ty, val.name, to
        )
        .unwrap();
        Val::new(r, to.clone())
    }

    /// Signed int to float.
    pub fn sitofp(&mut self, val: &Val, to: &IrType) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = sitofp {} {} to {}",
            r, val.ty, val.name, to
        )
        .unwrap();
        Val::new(r, to.clone())
    }

    /// Float to signed int.
    pub fn fptosi(&mut self, val: &Val, to: &IrType) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = fptosi {} {} to {}",
            r, val.ty, val.name, to
        )
        .unwrap();
        Val::new(r, to.clone())
    }

    /// Zero-extend i1 to wider integer.
    pub fn zext(&mut self, val: &Val, to: &IrType) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = zext {} {} to {}",
            r, val.ty, val.name, to
        )
        .unwrap();
        Val::new(r, to.clone())
    }

    // ===== Select =====

    /// `%r = select i1 %cond, ty %a, ty %b`
    pub fn select(&mut self, cond: &Val, a: &Val, b: &Val) -> Val {
        let r = self.next_reg();
        writeln!(
            self.body,
            "  {} = select i1 {}, {} {}, {} {}",
            r, cond.name, a.ty, a.name, b.ty, b.name
        )
        .unwrap();
        Val::new(r, a.ty.clone())
    }

    // ===== Control flow =====

    /// Unconditional branch.
    pub fn br(&mut self, label: &str) {
        if !self.terminated {
            writeln!(self.body, "  br label %{}", label).unwrap();
            self.terminated = true;
        }
    }

    /// Conditional branch.
    pub fn condbr(&mut self, cond: &Val, then_label: &str, else_label: &str) {
        if !self.terminated {
            writeln!(
                self.body,
                "  br i1 {}, label %{}, label %{}",
                cond.name, then_label, else_label
            )
            .unwrap();
            self.terminated = true;
        }
    }

    /// Return void.
    pub fn ret_void(&mut self) {
        if !self.terminated {
            writeln!(self.body, "  ret void").unwrap();
            self.terminated = true;
        }
    }

    /// Return a value.
    pub fn ret(&mut self, val: &Val) {
        if !self.terminated {
            writeln!(self.body, "  ret {} {}", val.ty, val.name).unwrap();
            self.terminated = true;
        }
    }

    /// Switch dispatch: branch to one of several labels based on an integer value.
    /// `switch i32 %val, label %default [ i32 1, label %L1  i32 2, label %L2 ... ]`
    pub fn switch(&mut self, val: &Val, default_label: &str, cases: &[(i32, String)]) {
        if !self.terminated {
            let cases_str: Vec<String> = cases
                .iter()
                .map(|(n, lbl)| format!("    i32 {}, label %{}", n, lbl))
                .collect();
            writeln!(
                self.body,
                "  switch i32 {}, label %{} [\n{}\n  ]",
                val.name,
                default_label,
                cases_str.join("\n")
            )
            .unwrap();
            self.terminated = true;
        }
    }

    // ===== Function calls =====

    /// Call a void function.
    pub fn call_void(&mut self, name: &str, args: &[Val]) {
        let args_str: Vec<String> = args.iter().map(|a| a.typed()).collect();
        writeln!(self.body, "  call void @{}({})", name, args_str.join(", ")).unwrap();
    }

    /// Call a void function with x86_stdcallcc convention (Win32 API on 32-bit).
    pub fn call_void_stdcall(&mut self, name: &str, args: &[Val]) {
        let args_str: Vec<String> = args.iter().map(|a| a.typed()).collect();
        writeln!(
            self.body,
            "  call x86_stdcallcc void @{}({})",
            name,
            args_str.join(", ")
        )
        .unwrap();
    }

    /// Call a function that returns a value.
    pub fn call(&mut self, ret_ty: &IrType, name: &str, args: &[Val]) -> Val {
        let r = self.next_reg();
        let args_str: Vec<String> = args.iter().map(|a| a.typed()).collect();
        writeln!(
            self.body,
            "  {} = call {} @{}({})",
            r,
            ret_ty,
            name,
            args_str.join(", ")
        )
        .unwrap();
        Val::new(r, ret_ty.clone())
    }

    /// Call a function with x86_stdcallcc convention that returns a value.
    pub fn call_stdcall(&mut self, ret_ty: &IrType, name: &str, args: &[Val]) -> Val {
        let r = self.next_reg();
        let args_str: Vec<String> = args.iter().map(|a| a.typed()).collect();
        writeln!(
            self.body,
            "  {} = call x86_stdcallcc {} @{}({})",
            r,
            ret_ty,
            name,
            args_str.join(", ")
        )
        .unwrap();
        Val::new(r, ret_ty.clone())
    }

    /// Call a variadic function (like printf, snprintf).
    /// `fixed_param_types` are the types before `...` in the signature.
    pub fn call_variadic(&mut self, ret_ty: &IrType, name: &str, args: &[Val]) -> Val {
        self.call_variadic_with_sig(ret_ty, name, args, &[IrType::Ptr])
    }

    /// Call a variadic function with explicit fixed parameter types.
    pub fn call_variadic_with_sig(
        &mut self,
        ret_ty: &IrType,
        name: &str,
        args: &[Val],
        fixed_types: &[IrType],
    ) -> Val {
        let r = self.next_reg();
        let args_str: Vec<String> = args.iter().map(|a| a.typed()).collect();
        let fixed_str: Vec<String> = fixed_types.iter().map(|t| t.to_string()).collect();
        writeln!(
            self.body,
            "  {} = call {} ({}, ...) @{}({})",
            r,
            ret_ty,
            fixed_str.join(", "),
            name,
            args_str.join(", ")
        )
        .unwrap();
        Val::new(r, ret_ty.clone())
    }

    // ===== Constants =====

    pub fn const_i32(&self, val: i32) -> Val {
        Val::new(val.to_string(), IrType::I32)
    }

    #[allow(dead_code)]
    pub fn const_i64(&self, val: i64) -> Val {
        Val::new(val.to_string(), IrType::I64)
    }

    #[allow(dead_code)]
    pub fn const_i16(&self, val: i16) -> Val {
        Val::new(val.to_string(), IrType::I16)
    }

    pub fn const_i1(&self, val: bool) -> Val {
        Val::new(if val { "1" } else { "0" }, IrType::I1)
    }

    pub fn const_f64(&self, val: f64) -> Val {
        // LLVM IR requires hex float for exact representation
        Val::new(format_f64_hex(val), IrType::Double)
    }

    #[allow(dead_code)]
    pub fn const_f32(&self, val: f32) -> Val {
        Val::new(format_f64_hex(val as f64), IrType::Float)
    }

    pub fn const_i8(&self, val: u8) -> Val {
        Val::new(val.to_string(), IrType::I8)
    }

    pub fn const_null_ptr(&self) -> Val {
        Val::new("null", IrType::Ptr)
    }

    // ===== Inline comment =====

    #[allow(dead_code)]
    pub fn comment(&mut self, text: &str) {
        writeln!(self.body, "  ; {}", text).unwrap();
    }
}

/// Format f64 as LLVM IR hex float literal.
/// LLVM IR uses `0xHHHHHHHHHHHHHHHH` format for exact IEEE 754 representation.
fn format_f64_hex(val: f64) -> String {
    if val == 0.0 {
        return "0.0".to_string();
    }
    // For simple values, use decimal
    if val == val.floor() && val.abs() < 1e15 {
        return format!("{:.1}", val);
    }
    // Use hex representation for exact floats
    let bits = val.to_bits();
    format!("0x{:016X}", bits)
}

/// Escape a string for LLVM IR constant.
fn llvm_escape_string(s: &str) -> String {
    let mut out = String::new();
    for b in s.bytes() {
        match b {
            b'\\' => out.push_str("\\5C"),
            b'"' => out.push_str("\\22"),
            b'\n' => out.push_str("\\0A"),
            b'\r' => out.push_str("\\0D"),
            b'\t' => out.push_str("\\09"),
            0x20..=0x7E => out.push(b as char),
            _ => {
                write!(out, "\\{:02X}", b).unwrap();
            }
        }
    }
    out
}
