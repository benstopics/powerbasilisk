use std::collections::{HashMap, HashSet};
use std::path::Path;

use pb::ast::*;
use pb::error::{PbError, PbResult};

use crate::llvm_ir::{FunctionBuilder, IrType, ModuleBuilder, Val};
use crate::symbols::{ArrayInfo, SymbolTable};

/// Options for the compilation pipeline.
pub const DEFAULT_TARGET: &str = "i686-pc-windows-msvc";

pub struct CompileOptions {
    pub dll_mode: bool,
    pub exe_mode: bool,
    pub session_mode: bool,
    pub emit_llvm: bool,
    pub debug_mode: bool, // enable verbose debug logging (modal text, assertions)
    pub runtime_lib: Option<String>, // path to pb_runtime.obj
    pub lib_dir: Option<String>, // path to directory containing import libraries (.lib)
    pub split_threshold: usize, // split functions exceeding this many IR lines (0 = disabled)
    pub target: String, // LLVM target triple (e.g. "i686-pc-windows-msvc", "x86_64-pc-windows-msvc")
}

/// Compile a parsed PB program to LLVM IR, then optionally to object code via clang.
pub fn compile(
    program: &Program,
    output_path: &Path,
    opts: &CompileOptions,
    pp_constants: &HashMap<String, i64>,
) -> PbResult<()> {
    let mut compiler = Compiler::with_target(&opts.target);
    compiler.session_mode = opts.session_mode;
    compiler.debug_mode = opts.debug_mode;
    compiler.pp_constants = pp_constants.clone();
    compiler.compile_program(program)?;

    let ir = compiler.module.emit();

    // Write .ll file
    let ll_path = output_path.with_extension("ll");
    std::fs::write(&ll_path, &ir)
        .map_err(|e| PbError::io(format!("Failed to write .ll: {}", e)))?;
    eprintln!(
        "[pbcompiler] Wrote LLVM IR: {} ({} lines)",
        ll_path.display(),
        ir.lines().count()
    );

    if opts.emit_llvm {
        return Ok(());
    }

    // Split large functions if threshold is set
    let threshold = if opts.split_threshold > 0 {
        opts.split_threshold
    } else {
        20000
    };
    let (main_ir, split_ir) = split_large_functions(&ir, threshold);

    let mut obj_paths: Vec<std::path::PathBuf> = Vec::new();

    if let Some(ref split_ir_content) = split_ir {
        // Write the split main .ll (large functions removed)
        let main_ll_path = output_path.with_extension("main.ll");
        std::fs::write(&main_ll_path, &main_ir)
            .map_err(|e| PbError::io(format!("Failed to write main .ll: {}", e)))?;

        let main_obj_path = output_path.with_extension("main.obj");
        // Use -O0 to avoid clang 17.0.1 crashes (Register Coalescer, DAG ISel)
        compile_with_clang(&main_ll_path, &main_obj_path, "-O0", &opts.target)?;
        eprintln!(
            "[pbcompiler] Wrote main object: {}",
            main_obj_path.display()
        );
        obj_paths.push(main_obj_path);

        // Write and compile the split .ll (large functions only)
        let split_ll_path = output_path.with_extension("split.ll");
        std::fs::write(&split_ll_path, split_ir_content)
            .map_err(|e| PbError::io(format!("Failed to write split .ll: {}", e)))?;
        eprintln!(
            "[pbcompiler] Split large functions to: {}",
            split_ll_path.display()
        );

        let split_obj_path = output_path.with_extension("split.obj");
        compile_split_module(&split_ll_path, &split_obj_path, &opts.target)?;
        eprintln!(
            "[pbcompiler] Wrote split object: {}",
            split_obj_path.display()
        );
        obj_paths.push(split_obj_path);
    } else {
        // No split needed — compile as one unit
        let obj_path = output_path.with_extension("obj");
        compile_with_clang(&ll_path, &obj_path, "-O2", &opts.target)?;
        eprintln!("[pbcompiler] Wrote object: {}", obj_path.display());
        obj_paths.push(obj_path);
    }

    // Link if requested
    if opts.dll_mode {
        let dll_path = output_path.with_extension("dll");
        link_dll(&obj_paths[0], &dll_path, &opts.target)?;
        eprintln!("[pbcompiler] Linked DLL: {}", dll_path.display());
    }

    if opts.exe_mode {
        let exe_path = output_path.with_extension("exe");
        let obj_refs: Vec<&Path> = obj_paths.iter().map(|p| p.as_path()).collect();
        link_exe(&obj_refs, &exe_path, opts)?;
        eprintln!("[pbcompiler] Linked EXE: {}", exe_path.display());
    }

    Ok(())
}

fn compile_with_clang(
    ll_path: &Path,
    obj_path: &Path,
    opt_level: &str,
    target: &str,
) -> PbResult<()> {
    let target_flag = format!("--target={}", target);
    let ll_str = ll_path.to_string_lossy();
    let obj_str = obj_path.to_string_lossy();
    let output = std::process::Command::new("clang")
        .args([
            "-c",
            opt_level,
            &target_flag,
            ll_str.as_ref(),
            "-o",
            obj_str.as_ref(),
        ])
        .output()
        .map_err(|e| PbError::io(format!("Failed to run clang: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(PbError::io(format!("clang failed:\n{}", stderr)));
    }

    Ok(())
}

/// Compile a split module containing large functions.
/// Tries multiple strategies since these functions crash clang's default ISel.
/// If all strategies fail, generates stubs for uncompilable functions.
fn compile_split_module(ll_path: &Path, obj_path: &Path, target: &str) -> PbResult<()> {
    let target_flag = format!("--target={}", target);
    let ll_str = ll_path.to_string_lossy();
    let obj_str = obj_path.to_string_lossy();

    // Strategy 1: Try -O1 (lighter optimization, may avoid DAG explosion)
    eprintln!("[pbcompiler] Compiling split module with -O1...");
    let result = std::process::Command::new("clang")
        .args([
            "-c",
            "-O1",
            &target_flag,
            ll_str.as_ref(),
            "-o",
            obj_str.as_ref(),
        ])
        .output();

    if let Ok(output) = &result {
        if output.status.success() {
            return Ok(());
        }
    }

    // Strategy 2: -O0 with fast-isel
    eprintln!("[pbcompiler] -O1 failed, trying -O0 -mllvm -fast-isel...");
    let result = std::process::Command::new("clang")
        .args([
            "-c",
            "-O0",
            "-mllvm",
            "-fast-isel",
            &target_flag,
            ll_str.as_ref(),
            "-o",
            obj_str.as_ref(),
        ])
        .output();

    if let Ok(output) = &result {
        if output.status.success() {
            return Ok(());
        }
    }

    // Strategy 3: Plain -O0
    eprintln!("[pbcompiler] fast-isel failed, trying plain -O0...");
    let result = std::process::Command::new("clang")
        .args([
            "-c",
            "-O0",
            &target_flag,
            ll_str.as_ref(),
            "-o",
            obj_str.as_ref(),
        ])
        .output();

    if let Ok(output) = &result {
        if output.status.success() {
            return Ok(());
        }
    }

    // Strategy 4: Generate stub module for all functions
    // BIGLOOP and similar functions crash clang at ALL optimization levels.
    // Emit stub functions that return default values so linking can proceed.
    eprintln!("[pbcompiler] All compilation strategies failed for split module.");
    eprintln!("[pbcompiler] Generating stub functions for uncompilable functions...");
    generate_stub_module(ll_path, obj_path, target)
}

/// Generate a stub .ll module where each function just returns a default value.
/// This allows linking to succeed even when clang can't compile the full function.
fn generate_stub_module(original_ll: &Path, obj_path: &Path, target: &str) -> PbResult<()> {
    let content = std::fs::read_to_string(original_ll)
        .map_err(|e| PbError::io(format!("Failed to read split .ll: {}", e)))?;

    let mut stub = String::new();
    use std::fmt::Write;

    // Copy module header
    for line in content.lines() {
        if line.starts_with("; Generated") || line.starts_with("target ") || line.is_empty() {
            writeln!(stub, "{}", line).unwrap();
        } else {
            break;
        }
    }

    // Copy type definitions
    for line in content.lines() {
        if line.starts_with('%') && line.contains(" = type ") {
            writeln!(stub, "{}", line).unwrap();
        }
    }

    // Copy declare statements
    for line in content.lines() {
        if line.starts_with("declare ") {
            writeln!(stub, "{}", line).unwrap();
        }
    }

    // Copy external global declarations
    for line in content.lines() {
        if line.starts_with('@') && line.contains(" = external global ") {
            writeln!(stub, "{}", line).unwrap();
        }
    }

    writeln!(stub).unwrap();

    // For each define, emit a stub that returns default
    for line in content.lines() {
        if line.starts_with("define ") {
            // Extract return type and generate stub
            let rest = line.strip_prefix("define ").unwrap();
            let rest = rest.strip_prefix("dllexport ").unwrap_or(rest);
            // rest is "TYPE @NAME(PARAMS) {"
            let ret_type = rest.split_whitespace().next().unwrap_or("void");

            // Extract function name
            let name = if let Some(at_pos) = rest.find('@') {
                let after_at = &rest[at_pos + 1..];
                if let Some(paren_pos) = after_at.find('(') {
                    &after_at[..paren_pos]
                } else {
                    after_at
                }
            } else {
                "unknown"
            };

            eprintln!(
                "[pbcompiler] Stubbing function: {} (returns {})",
                name, ret_type
            );

            // Emit stub: strip param names for the define
            let export = if line.contains("dllexport") {
                "dllexport "
            } else {
                ""
            };

            // Get the parameter part
            if let (Some(open), Some(close)) = (line.rfind('('), line.rfind(')')) {
                let params = &line[open..=close];
                writeln!(stub, "define {}{} @{}{} {{", export, ret_type, name, params).unwrap();
            } else {
                writeln!(stub, "define {}{} @{}() {{", export, ret_type, name).unwrap();
            }
            writeln!(stub, "entry:").unwrap();
            match ret_type {
                "void" => writeln!(stub, "  ret void").unwrap(),
                "i32" => writeln!(stub, "  ret i32 0").unwrap(),
                "i16" => writeln!(stub, "  ret i16 0").unwrap(),
                "i64" => writeln!(stub, "  ret i64 0").unwrap(),
                "double" => writeln!(stub, "  ret double 0.0").unwrap(),
                "float" => writeln!(stub, "  ret float 0.0").unwrap(),
                "ptr" => writeln!(stub, "  ret ptr null").unwrap(),
                _ => writeln!(stub, "  ret {} zeroinitializer", ret_type).unwrap(),
            }
            writeln!(stub, "}}").unwrap();
            writeln!(stub).unwrap();
        }
    }

    // Write and compile the stub module
    let stub_ll = original_ll.with_extension("stub.ll");
    std::fs::write(&stub_ll, &stub)
        .map_err(|e| PbError::io(format!("Failed to write stub .ll: {}", e)))?;

    compile_with_clang(&stub_ll, obj_path, "-O0", target)
}

fn link_dll(obj_path: &Path, dll_path: &Path, target: &str) -> PbResult<()> {
    let target_flag = format!("--target={}", target);
    let obj_str = obj_path.to_string_lossy();
    let dll_str = dll_path.to_string_lossy();
    let output = std::process::Command::new("clang")
        .args([
            "-shared",
            &target_flag,
            obj_str.as_ref(),
            "-loleaut32",
            "-o",
            dll_str.as_ref(),
        ])
        .output()
        .map_err(|e| PbError::io(format!("Failed to run clang for DLL linking: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(PbError::io(format!("clang DLL link failed:\n{}", stderr)));
    }

    Ok(())
}

/// Link object files into an executable.
fn link_exe(obj_paths: &[&Path], exe_path: &Path, opts: &CompileOptions) -> PbResult<()> {
    let mut args: Vec<String> = vec![format!("--target={}", opts.target)];

    // Add all object files
    for obj in obj_paths {
        args.push(obj.to_string_lossy().to_string());
    }

    // Add runtime library if specified
    if let Some(ref runtime) = opts.runtime_lib {
        args.push(runtime.clone());
    }

    // Output path
    args.push("-o".to_string());
    args.push(exe_path.to_string_lossy().to_string());

    // Always link oleaut32 (needed by pb_runtime for BSTR/SysAllocString)
    args.push("-loleaut32".to_string());

    // Link additional Windows libraries only when --lib-dir is provided
    if let Some(ref lib_dir) = opts.lib_dir {
        args.push(format!("-L{}", lib_dir));
        args.extend([
            "-lui".to_string(),
            "-lkernel32".to_string(),
            "-luser32".to_string(),
            "-lgdi32".to_string(),
            "-lshell32".to_string(),
            "-lcomctl32".to_string(),
            "-lcomdlg32".to_string(),
            "-ladvapi32".to_string(),
            "-lole32".to_string(),
            "-lwinmm".to_string(),
        ]);
    }

    let output = std::process::Command::new("clang")
        .args(&args)
        .output()
        .map_err(|e| PbError::io(format!("Failed to run clang for EXE linking: {}", e)))?;

    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.is_empty() {
        // Print warnings but don't fail on them
        for line in stderr.lines() {
            if line.contains("error") && !line.contains("warning") {
                return Err(PbError::io(format!("clang EXE link failed:\n{}", stderr)));
            }
        }
        // Print any warnings
        for line in stderr.lines() {
            if line.contains("warning") {
                eprintln!("[pbcompiler] Link warning: {}", line);
            }
        }
    }

    if !output.status.success() {
        return Err(PbError::io(format!("clang EXE link failed:\n{}", stderr)));
    }

    Ok(())
}

/// Split large functions from the main IR module into a separate module.
///
/// Functions exceeding `threshold` lines are moved to a split module.
/// The main module gets `declare` stubs for those functions.
/// The split module gets the module header, all type definitions, global
/// declarations (as `external`), and `declare` stubs for all other functions.
///
/// Returns (main_ir, Some(split_ir)) if functions were split, or (ir, None) if not.
fn split_large_functions(ir: &str, threshold: usize) -> (String, Option<String>) {
    let lines: Vec<&str> = ir.lines().collect();

    // Find all function definition boundaries: (start_line, end_line, signature_line)
    // A function definition starts with "define " and ends with "}"
    struct FnRange {
        start: usize,
        end: usize,       // inclusive (the "}" line)
        sig_line: String, // the "define ..." line
        name: String,     // @FunctionName
    }

    let mut fn_ranges: Vec<FnRange> = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        if lines[i].starts_with("define ") {
            let start = i;
            let sig_line = lines[i].to_string();

            // Extract function name from "define ... @NAME(..."
            let name = if let Some(at_pos) = sig_line.find('@') {
                let after_at = &sig_line[at_pos + 1..];
                if let Some(paren_pos) = after_at.find('(') {
                    after_at[..paren_pos].to_string()
                } else {
                    after_at.to_string()
                }
            } else {
                String::new()
            };

            // Find closing brace (at column 0)
            i += 1;
            while i < lines.len() && lines[i] != "}" {
                i += 1;
            }
            let end = i; // the "}" line

            fn_ranges.push(FnRange {
                start,
                end,
                sig_line,
                name,
            });
        }
        i += 1;
    }

    // Find functions exceeding threshold
    let large_fn_indices: Vec<usize> = fn_ranges
        .iter()
        .enumerate()
        .filter(|(_, fr)| fr.end - fr.start > threshold)
        .map(|(idx, _)| idx)
        .collect();

    if large_fn_indices.is_empty() {
        return (ir.to_string(), None);
    }

    let large_names: Vec<&str> = large_fn_indices
        .iter()
        .map(|&idx| fn_ranges[idx].name.as_str())
        .collect();
    eprintln!(
        "[pbcompiler] Splitting {} large function(s): {}",
        large_names.len(),
        large_names.join(", ")
    );

    // Build a set of line ranges to exclude from the main module
    let mut exclude_ranges: Vec<(usize, usize)> = Vec::new();
    for &idx in &large_fn_indices {
        let fr = &fn_ranges[idx];
        exclude_ranges.push((fr.start, fr.end));
    }

    // --- Build main module ---
    // Everything except large function bodies, with `declare` stubs added
    let mut main_lines: Vec<String> = Vec::new();
    let mut declare_stubs: Vec<String> = Vec::new();

    // Generate declare stubs for large functions
    for &idx in &large_fn_indices {
        let fr = &fn_ranges[idx];
        // Convert "define [dllexport] TYPE @NAME(PARAMS) {" to "declare TYPE @NAME(PARAMS)"
        let stub = fn_define_to_declare(&fr.sig_line);
        declare_stubs.push(stub);
    }

    let mut skip_until: Option<usize> = None;
    for (line_idx, line) in lines.iter().enumerate() {
        if let Some(skip_end) = skip_until {
            if line_idx <= skip_end {
                continue;
            }
            skip_until = None;
        }

        // Check if this line starts an excluded function
        let mut excluded = false;
        for &(start, end) in &exclude_ranges {
            if line_idx == start {
                skip_until = Some(end);
                excluded = true;
                break;
            }
        }

        if excluded {
            // Skip the next blank line after "}" too
            continue;
        }

        main_lines.push(line.to_string());
    }

    // Insert declare stubs after the globals section (before first define)
    let first_define_pos = main_lines.iter().position(|l| l.starts_with("define "));
    if let Some(pos) = first_define_pos {
        for (i, stub) in declare_stubs.iter().enumerate() {
            main_lines.insert(pos + i, stub.clone());
        }
        main_lines.insert(pos + declare_stubs.len(), String::new());
    }

    // --- Build split module ---
    let mut split_lines: Vec<String> = Vec::new();

    // Copy module header (target triple, datalayout)
    for line in &lines {
        if line.starts_with("; Generated") || line.starts_with("target ") || line.is_empty() {
            split_lines.push(line.to_string());
        } else {
            break;
        }
    }
    split_lines.push(String::new());

    // Copy type definitions
    for line in &lines {
        if line.starts_with('%') && line.contains(" = type ") {
            split_lines.push(line.to_string());
        }
    }

    // Add external declarations for all global variables
    for line in &lines {
        if line.starts_with('@') && line.contains(" = global ") {
            // Convert "@NAME = global TYPE init" to "@NAME = external global TYPE"
            if let Some(decl) = global_to_external(line) {
                split_lines.push(decl);
            }
        } else if line.starts_with('@') && line.contains(" = dllexport global ") {
            if let Some(decl) = global_to_external(line) {
                split_lines.push(decl);
            }
        } else if line.starts_with('@') && line.contains(" = external global ") {
            // Already in external form (e.g., @pb_debug_line = external global i32)
            split_lines.push(line.to_string());
        }
    }

    // Add external declarations for string constants referenced by large functions
    // (private unnamed_addr constants) - these need to be in the split module
    // Actually, string constants are private so they can't be cross-referenced.
    // We need to copy them into the split module too.
    let large_fn_text: Vec<&str> = large_fn_indices
        .iter()
        .flat_map(|&idx| {
            let fr = &fn_ranges[idx];
            &lines[fr.start..=fr.end]
        })
        .copied()
        .collect();

    let large_fn_text_joined = large_fn_text.join("\n");
    for line in &lines {
        if line.starts_with('@') && line.contains("private unnamed_addr constant") {
            // Check if this constant is referenced in any large function
            let const_name = if let Some(eq_pos) = line.find(" = ") {
                &line[..eq_pos]
            } else {
                continue;
            };
            if large_fn_text_joined.contains(const_name) {
                split_lines.push(line.to_string());
            }
        }
    }

    // Copy all declare statements (external functions and dllimport)
    for line in &lines {
        if line.starts_with("declare ") {
            split_lines.push(line.to_string());
        }
    }

    // Add declare stubs for all non-large defined functions
    for (idx, fr) in fn_ranges.iter().enumerate() {
        if !large_fn_indices.contains(&idx) {
            let stub = fn_define_to_declare(&fr.sig_line);
            split_lines.push(stub);
        }
    }

    split_lines.push(String::new());

    // Copy large function definitions
    for &idx in &large_fn_indices {
        let fr = &fn_ranges[idx];
        for line in &lines[fr.start..=fr.end] {
            split_lines.push(line.to_string());
        }
        split_lines.push(String::new());
    }

    let main_ir = main_lines.join("\n");
    let split_ir = split_lines.join("\n");

    (main_ir, Some(split_ir))
}

/// Convert a `define` line to a `declare` stub.
/// "define dllexport i32 @FOO(i32 %x, ptr %y) {" → "declare i32 @FOO(i32, ptr)"
fn fn_define_to_declare(define_line: &str) -> String {
    // Remove "define " prefix and optional "dllexport "
    let rest = define_line.strip_prefix("define ").unwrap_or(define_line);
    let rest = rest.strip_prefix("dllexport ").unwrap_or(rest);

    // Find the opening "{" and remove it
    let rest = if let Some(brace_pos) = rest.rfind('{') {
        rest[..brace_pos].trim()
    } else {
        rest.trim()
    };

    // Strip parameter names from the signature: "i32 %x" → "i32"
    // Find params between ( and )
    if let (Some(open), Some(close)) = (rest.find('('), rest.rfind(')')) {
        let before_params = &rest[..open];
        let params_str = &rest[open + 1..close];

        // Strip parameter names
        let stripped_params: Vec<&str> = params_str
            .split(',')
            .map(|p| {
                let p = p.trim();
                if p.is_empty() {
                    return p;
                }
                // "i32 %name" → "i32", "ptr %name" → "ptr", "..." → "..."
                if p == "..." {
                    return p;
                }
                // Take just the type (first word)
                if let Some(space_pos) = p.find(' ') {
                    p[..space_pos].trim()
                } else {
                    p
                }
            })
            .collect();

        format!("declare {}({})", before_params, stripped_params.join(", "))
    } else {
        format!("declare {}", rest)
    }
}

/// Convert a global definition to an external declaration.
/// "@NAME = global i32 0" → "@NAME = external global i32"
/// "@NAME = dllexport global %TYPE zeroinitializer" → "@NAME = external global %TYPE"
fn global_to_external(line: &str) -> Option<String> {
    // Find "@NAME = "
    let eq_pos = line.find(" = ")?;
    let name = &line[..eq_pos];

    // Find "global TYPE ..."
    let global_pos = line.find(" global ")?;
    let after_global = &line[global_pos + 8..]; // skip " global "

    // Type is everything up to the initializer (first space after type, or end)
    // Types can be: i32, double, ptr, [N x T], %TYPENAME, { ... }
    let ir_type = extract_ir_type(after_global);

    Some(format!("{} = external global {}", name, ir_type))
}

/// Extract the IR type from the start of a string like "i32 0" or "[1600 x i32] zeroinitializer"
fn extract_ir_type(s: &str) -> &str {
    let s = s.trim();

    // Handle array types: [N x T]
    if s.starts_with('[') {
        if let Some(bracket_end) = s.find(']') {
            return &s[..bracket_end + 1];
        }
    }

    // Handle struct types: %TYPENAME
    if s.starts_with('%') {
        // %TYPENAME followed by space
        if let Some(space) = s.find(' ') {
            return &s[..space];
        }
        return s;
    }

    // Handle simple types: i32, double, ptr, etc.
    if let Some(space) = s.find(' ') {
        return &s[..space];
    }
    s
}

struct Compiler {
    module: ModuleBuilder,
    symbols: SymbolTable,

    // Function/sub declarations collected during first pass
    functions: HashMap<String, FuncInfo>,
    subs: HashMap<String, FuncInfo>,

    // Current function context
    current_fn_name: Option<String>,
    current_fn_return_type: Option<(PbType, IrType)>,
    current_fn_retval_ptr: Option<Val>,

    // Loop control: stack of (continue_label, exit_label, is_for)
    loop_stack: Vec<LoopInfo>,

    // GLOBAL arr() AS TYPE awaiting DIM with bounds
    pending_global_arrays: HashMap<String, PbType>,

    // Name of the global empty string constant (e.g., "@.str.empty")
    empty_string_name: String,

    // User-defined TYPE layouts
    type_layouts: HashMap<String, TypeLayout>,

    // GOSUB/RETURN context for the current function being compiled
    gosub_context: Option<GosubContext>,

    // Session struct mode: wrap all globals in a single struct
    session_mode: bool,
    session_fields: Vec<SessionFieldInfo>,

    // Debug mode: emit extra logging calls (modal text, state transitions)
    debug_mode: bool,

    // Preprocessor %CONSTANTS (name → value)
    pp_constants: HashMap<String, i64>,
}

/// Tracks a global variable or array that will become a session struct field.
struct SessionFieldInfo {
    name: String,    // normalized global name
    ir_type: IrType, // field IR type (scalar or array type)
    pb_type: PbType, // PB type
    is_array: bool,
    elem_ir_type: Option<IrType>,        // for arrays: element type
    array_dims: Option<Vec<(i64, i64)>>, // for arrays: (lower_bound, count) per dim
    total_elements: Option<usize>,       // for arrays: total element count
}

/// Tracks GOSUB/RETURN info for one function's compilation.
struct GosubContext {
    /// Map from label name → LLVM block name
    label_blocks: HashMap<String, String>,
    /// Map from GOSUB site ID → (return-point label name)
    return_points: Vec<String>,
    /// Alloca for the gosub return address (i32)
    ret_addr_ptr: Val,
    /// Labels already emitted (to prevent duplicate LLVM blocks)
    emitted_labels: HashSet<String>,
}

#[allow(dead_code)]
#[derive(Clone)]
struct FuncInfo {
    ir_name: String,
    ret_type: IrType,
    params: Vec<ParamInfo>,
    export: bool,
    is_external: bool, // true if DECLARE LIB (dllimport)
    is_stdcall: bool,  // true if Win32 API (stdcall on 32-bit), false for cdecl
}

#[derive(Clone)]
struct ParamInfo {
    name: String,
    pb_type: PbType,
    ir_type: IrType,
    is_byval: bool,
    is_optional: bool,
}

struct LoopInfo {
    continue_label: String,
    exit_label: String,
    is_for: bool,
}

/// Layout of a user-defined TYPE (struct).
#[allow(dead_code)]
struct TypeLayout {
    ir_name: String,                   // normalized TYPE name
    fields: Vec<FieldInfo>,            // ordered fields
    field_map: HashMap<String, usize>, // normalized field name → index
}

#[allow(dead_code)]
struct FieldInfo {
    name: String,
    pb_type: PbType,
    ir_type: IrType,
}

impl Compiler {
    /// Returns true if the DLL is stubbed in pb_runtime.c (not available at link time).
    fn is_stubbed_dll(lib_name: &str) -> bool {
        let name = lib_name.trim_end_matches(".DLL").trim_end_matches(".dll");
        matches!(name, "VARICHEK" | "REMARKSLIB")
    }

    /// Returns true if the DLL name is a known Win32 API library that uses stdcall calling convention.
    fn is_win32_stdcall_dll(lib_name: &str) -> bool {
        // Strip .DLL extension if present
        let name = lib_name.trim_end_matches(".DLL").trim_end_matches(".dll");
        matches!(
            name,
            "KERNEL32"
                | "USER32"
                | "GDI32"
                | "COMCTL32"
                | "COMDLG32"
                | "SHELL32"
                | "OLEAUT32"
                | "OLE32"
                | "ADVAPI32"
                | "WINMM"
                | "WS2_32"
                | "WSOCK32"
                | "NETAPI32"
                | "SHLWAPI"
                | "VERSION"
                | "WININET"
                | "WINSPOOL"
                | "MAPI32"
                | "PSAPI"
                | "SETUPAPI"
                | "IPHLPAPI"
                | "SECUR32"
                | "CRYPT32"
                | "MPR"
                | "DBGHELP"
        )
    }

    fn with_target(target: &str) -> Self {
        Compiler {
            module: ModuleBuilder::new(target),
            symbols: SymbolTable::new(),
            functions: HashMap::new(),
            subs: HashMap::new(),
            current_fn_name: None,
            current_fn_return_type: None,
            current_fn_retval_ptr: None,
            loop_stack: Vec::new(),
            pending_global_arrays: HashMap::new(),
            empty_string_name: String::new(),
            type_layouts: HashMap::new(),
            gosub_context: None,
            session_mode: false,
            session_fields: Vec::new(),
            debug_mode: false,
            pp_constants: HashMap::new(),
        }
    }

    // ========== Type mapping ==========

    fn ir_type_for(pb_type: &PbType) -> IrType {
        match pb_type {
            PbType::Long | PbType::Dword => IrType::I32,
            PbType::Integer | PbType::Word => IrType::I16,
            PbType::Byte => IrType::I8,
            PbType::Quad => IrType::I64,
            PbType::Double | PbType::Ext | PbType::Cur => IrType::Double,
            PbType::Single => IrType::Float,
            PbType::String | PbType::FixedString(_) => IrType::Ptr,
            PbType::UserDefined(name) => IrType::Struct(normalize_name(name)),
            PbType::Variant => IrType::I32, // default to LONG
        }
    }

    /// IR type for a struct field — FixedString becomes [N x i8] instead of ptr
    fn ir_type_for_field(pb_type: &PbType) -> IrType {
        match pb_type {
            PbType::FixedString(n) => IrType::Array(*n, Box::new(IrType::I8)),
            other => Self::ir_type_for(other),
        }
    }

    fn is_string_pb(pb_type: &PbType) -> bool {
        matches!(pb_type, PbType::String | PbType::FixedString(_))
    }

    fn is_string_name(name: &str) -> bool {
        name.ends_with('$')
    }

    fn is_float_pb(pb_type: &PbType) -> bool {
        matches!(
            pb_type,
            PbType::Double | PbType::Single | PbType::Ext | PbType::Cur
        )
    }

    // ========== Top-level compilation ==========

    fn compile_program(&mut self, program: &Program) -> PbResult<()> {
        // Declare printf
        self.module
            .declare_function("printf", &IrType::I32, &[IrType::Ptr], true);

        // LLVM intrinsics for math
        self.module.declare_intrinsic(
            "llvm.pow.f64",
            &IrType::Double,
            &[IrType::Double, IrType::Double],
        );
        self.module
            .declare_intrinsic("llvm.fabs.f64", &IrType::Double, &[IrType::Double]);
        self.module
            .declare_intrinsic("llvm.floor.f64", &IrType::Double, &[IrType::Double]);
        self.module
            .declare_intrinsic("llvm.ceil.f64", &IrType::Double, &[IrType::Double]);
        self.module
            .declare_intrinsic("llvm.sqrt.f64", &IrType::Double, &[IrType::Double]);
        self.module
            .declare_intrinsic("llvm.round.f64", &IrType::Double, &[IrType::Double]);
        self.module
            .declare_intrinsic("llvm.log.f64", &IrType::Double, &[IrType::Double]);
        self.module
            .declare_intrinsic("llvm.exp.f64", &IrType::Double, &[IrType::Double]);
        self.module
            .declare_intrinsic("llvm.sin.f64", &IrType::Double, &[IrType::Double]);
        self.module
            .declare_intrinsic("llvm.cos.f64", &IrType::Double, &[IrType::Double]);

        // C math library (no LLVM intrinsic)
        self.module
            .declare_function("tan", &IrType::Double, &[IrType::Double], false);
        self.module
            .declare_function("atan", &IrType::Double, &[IrType::Double], false);

        // C stdlib for RND
        self.module
            .declare_function("rand", &IrType::I32, &[], false);

        // PB runtime library (pb_runtime.c)
        self.module.declare_function(
            "pb_format",
            &IrType::Ptr,
            &[IrType::Double, IrType::Ptr],
            false,
        );
        self.module.declare_function(
            "pb_parse",
            &IrType::Ptr,
            &[IrType::Ptr, IrType::Ptr, IrType::I32],
            false,
        );
        self.module.declare_function(
            "pb_parsecount",
            &IrType::I32,
            &[IrType::Ptr, IrType::Ptr],
            false,
        );
        self.module.declare_function(
            "pb_replace",
            &IrType::Void,
            &[IrType::Ptr, IrType::Ptr, IrType::Ptr],
            false,
        );
        self.module.declare_function(
            "pb_remove",
            &IrType::Ptr,
            &[IrType::Ptr, IrType::Ptr],
            false,
        );
        self.module.declare_function(
            "pb_using",
            &IrType::Ptr,
            &[IrType::Ptr, IrType::Double],
            false,
        );

        // oleaut32: BSTR allocation for bridge compatibility
        self.module.declare_function(
            "pb_bstr_alloc",
            &IrType::Ptr,
            &[IrType::Ptr, IrType::I32],
            false,
        );
        self.module
            .declare_function("pb_bstr_free", &IrType::Void, &[IrType::Ptr], false);
        self.module.declare_function(
            "pb_str_concat",
            &IrType::Ptr,
            &[IrType::Ptr, IrType::Ptr],
            false,
        );

        // File I/O runtime
        self.module
            .declare_function("pb_freefile", &IrType::I32, &[], false);
        self.module.declare_function(
            "pb_open",
            &IrType::I32,
            &[IrType::Ptr, IrType::I32, IrType::I32],
            false,
        );
        self.module
            .declare_function("pb_close", &IrType::Void, &[IrType::I32], false);
        self.module.declare_function(
            "pb_print_file",
            &IrType::Void,
            &[IrType::I32, IrType::Ptr],
            false,
        );
        self.module.declare_function(
            "pb_print_file_newline",
            &IrType::Void,
            &[IrType::I32],
            false,
        );
        self.module
            .declare_function("pb_line_input", &IrType::Ptr, &[IrType::I32], false);
        self.module
            .declare_function("pb_eof", &IrType::I32, &[IrType::I32], false);
        self.module.declare_function(
            "pb_input_file_str",
            &IrType::Void,
            &[IrType::I32, IrType::Ptr],
            false,
        );
        self.module.declare_function(
            "pb_input_file_int",
            &IrType::Void,
            &[IrType::I32, IrType::Ptr],
            false,
        );
        self.module.declare_function(
            "pb_input_file_dbl",
            &IrType::Void,
            &[IrType::I32, IrType::Ptr],
            false,
        );
        self.module
            .declare_function("pb_kill", &IrType::Void, &[IrType::Ptr], false);

        // C string library (size_t = i32 on 32-bit)
        self.module
            .declare_function("strlen", &IrType::I32, &[IrType::Ptr], false);
        self.module
            .declare_function("strcmp", &IrType::I32, &[IrType::Ptr, IrType::Ptr], false);
        self.module
            .declare_function("strcpy", &IrType::Ptr, &[IrType::Ptr, IrType::Ptr], false);
        self.module.declare_function(
            "strncpy",
            &IrType::Ptr,
            &[IrType::Ptr, IrType::Ptr, IrType::I32],
            false,
        );
        self.module
            .declare_function("strcat", &IrType::Ptr, &[IrType::Ptr, IrType::Ptr], false);
        self.module
            .declare_function("strstr", &IrType::Ptr, &[IrType::Ptr, IrType::Ptr], false);
        self.module
            .declare_function("malloc", &IrType::Ptr, &[IrType::I32], false);
        self.module
            .declare_function("free", &IrType::Void, &[IrType::Ptr], false);
        self.module.declare_function(
            "snprintf",
            &IrType::I32,
            &[IrType::Ptr, IrType::I32, IrType::Ptr],
            true,
        );
        self.module
            .declare_function("atof", &IrType::Double, &[IrType::Ptr], false);
        self.module.declare_function(
            "memcpy",
            &IrType::Ptr,
            &[IrType::Ptr, IrType::Ptr, IrType::I32],
            false,
        );
        self.module
            .declare_function("toupper", &IrType::I32, &[IrType::I32], false);
        self.module
            .declare_function("tolower", &IrType::I32, &[IrType::I32], false);

        // System functions
        self.module
            .declare_function("time", &IrType::I32, &[IrType::Ptr], false);
        self.module
            .declare_function("pb_date", &IrType::Ptr, &[], false);
        self.module
            .declare_function("pb_time", &IrType::Ptr, &[], false);
        self.module
            .declare_function("pb_environ", &IrType::Ptr, &[IrType::Ptr], false);
        self.module
            .declare_function("pb_exe_path", &IrType::Ptr, &[], false);
        self.module
            .declare_function("pb_exe_name", &IrType::Ptr, &[], false);
        self.module
            .declare_function("srand", &IrType::Void, &[IrType::I32], false);
        self.module
            .declare_function("rand", &IrType::I32, &[], false);
        self.module
            .declare_function("_mkdir", &IrType::I32, &[IrType::Ptr], false);
        self.module
            .declare_function("_rmdir", &IrType::I32, &[IrType::Ptr], false);
        self.module
            .declare_function("_chdir", &IrType::I32, &[IrType::Ptr], false);
        self.module
            .declare_function("_access", &IrType::I32, &[IrType::Ptr, IrType::I32], false);
        self.module
            .declare_dllimport("Sleep", &IrType::Void, &[IrType::I32]);
        self.module
            .declare_dllimport("GetCommandLineA", &IrType::Ptr, &[]);

        // Empty string constant
        let (empty_name, _) = self.module.add_string_constant("");
        self.empty_string_name = empty_name;

        // First pass: collect TYPE declarations, function/sub signatures, and globals

        // Pre-scan: find all names that will be DIMmed as arrays, so we don't
        // emit them as scalars when we see GLOBAL name (without parens) first.
        // Also scan function/sub bodies since common PB pattern puts DIM inside SetupDims().
        let mut future_arrays: HashSet<String> = HashSet::new();
        for item in &program.items {
            match item {
                TopLevel::DimDecl(dim) => {
                    if !dim.bounds.is_empty() {
                        future_arrays.insert(normalize_name(&dim.name));
                    }
                }
                TopLevel::SubDecl(sd) => {
                    Self::scan_body_for_array_names(&sd.body, &mut future_arrays);
                }
                TopLevel::FunctionDecl(fd) => {
                    Self::scan_body_for_array_names(&fd.body, &mut future_arrays);
                }
                _ => {}
            }
        }

        for item in &program.items {
            if let TopLevel::TypeDecl(td) = item {
                self.register_type(td);
            }
        }

        for item in &program.items {
            match item {
                TopLevel::FunctionDecl(fd) => {
                    self.register_function(fd);
                }
                TopLevel::SubDecl(sd) => {
                    self.register_sub(sd);
                }
                TopLevel::DeclareStmt(ds) => {
                    self.register_declare(ds);
                }
                TopLevel::GlobalDecl(vd) => {
                    if vd.is_array {
                        // GLOBAL arr() AS TYPE — remember for later DIM with bounds
                        self.pending_global_arrays
                            .insert(normalize_name(&vd.name), vd.pb_type.clone());
                    } else if future_arrays.contains(&normalize_name(&vd.name)) {
                        // Will be DIMmed as array later — treat as pending array
                        self.pending_global_arrays
                            .insert(normalize_name(&vd.name), vd.pb_type.clone());
                    } else {
                        self.declare_global(vd);
                    }
                }
                TopLevel::GlobalDeclList(decls) => {
                    for vd in decls {
                        if vd.is_array || future_arrays.contains(&normalize_name(&vd.name)) {
                            self.pending_global_arrays
                                .insert(normalize_name(&vd.name), vd.pb_type.clone());
                        } else {
                            self.declare_global(vd);
                        }
                    }
                }
                TopLevel::DimDecl(dim) => {
                    let name = normalize_name(&dim.name);
                    if !dim.bounds.is_empty() {
                        // Top-level DIM with bounds — declare as global array
                        let pb_type = if let Some(pt) = self.pending_global_arrays.get(&name) {
                            pt.clone()
                        } else {
                            dim.pb_type.clone()
                        };
                        self.declare_global_array(&name, &pb_type, &dim.bounds);
                    } else if dim.scope == DimScope::Global {
                        let vd = VarDecl {
                            name: dim.name.clone(),
                            pb_type: dim.pb_type.clone(),
                            is_array: false,
                            line: dim.line,
                        };
                        self.declare_global(&vd);
                    }
                }
                _ => {}
            }
        }

        // In session mode, also scan function/sub bodies for pending global DIMs
        // and GLOBAL scalar declarations inside SUBs (e.g., GLOBAL ForceSync in SetupDims)
        if self.session_mode {
            for item in &program.items {
                match item {
                    TopLevel::FunctionDecl(fd) => self.scan_body_for_global_dims(&fd.body),
                    TopLevel::SubDecl(sd) => self.scan_body_for_global_dims(&sd.body),
                    _ => {}
                }
            }
        }

        // In session mode, emit the struct type and global instance
        if self.session_mode {
            self.emit_session_struct();
        }

        // Second pass: compile function/sub bodies
        let mut codegen_errors = 0usize;
        for item in &program.items {
            match item {
                TopLevel::FunctionDecl(fd) => {
                    if let Err(e) = self.compile_function(fd) {
                        eprintln!("Codegen warning: FUNCTION {} — {}", fd.name, e);
                        codegen_errors += 1;
                        // Emit stub declaration so call sites don't fail
                        let name = normalize_name(&fd.name);
                        if let Some(fi) = self.functions.get(&name) {
                            let ir_name = fi.ir_name.clone();
                            let ret_type = fi.ret_type.clone();
                            let param_types: Vec<IrType> =
                                fi.params.iter().map(|p| p.ir_type.clone()).collect();
                            self.module
                                .declare_function(&ir_name, &ret_type, &param_types, false);
                        }
                    }
                }
                TopLevel::SubDecl(sd) => {
                    if let Err(e) = self.compile_sub(sd) {
                        eprintln!("Codegen warning: SUB {} — {}", sd.name, e);
                        codegen_errors += 1;
                        // Emit stub declaration so call sites don't fail
                        let name = normalize_name(&sd.name);
                        if let Some(fi) = self.subs.get(&name) {
                            let ir_name = fi.ir_name.clone();
                            let param_types: Vec<IrType> =
                                fi.params.iter().map(|p| p.ir_type.clone()).collect();
                            self.module.declare_function(
                                &ir_name,
                                &IrType::Void,
                                &param_types,
                                false,
                            );
                        }
                    }
                }
                _ => {}
            }
        }
        if codegen_errors > 0 {
            eprintln!(
                "[pbcompiler] {} codegen errors (functions skipped)",
                codegen_errors
            );
        }

        // Emit main() entry point that calls PBMAIN() if it exists
        if let Some(pbmain) = self.functions.get("PBMAIN") {
            let pbmain_ir_name = pbmain.ir_name.clone();
            // Declare debug/crash facilities (from pb_runtime.c)
            self.module
                .declare_function("pb_install_crash_handler", &IrType::Void, &[], false);
            self.module
                .declare_function("pb_debug_enter", &IrType::Void, &[IrType::Ptr], false);
            self.module.declare_function(
                "pb_debug_log_msg",
                &IrType::Void,
                &[IrType::Ptr, IrType::Ptr],
                false,
            );
            self.module.declare_function(
                "pb_debug_modal",
                &IrType::Void,
                &[IrType::Ptr, IrType::Ptr, IrType::Ptr],
                false,
            );
            self.module
                .declare_external_global("pb_debug_line", &IrType::I32);
            let mut fb = self
                .module
                .create_function_builder("main", &IrType::I32, &[]);
            fb.call_void("pb_install_crash_handler", &[]);
            if self.debug_mode {
                let (cat, _) = self.module.add_string_constant("BUILD");
                let (msg, _) = self
                    .module
                    .add_string_constant("Debug mode enabled — modal and state logging active");
                fb.call_void(
                    "pb_debug_log_msg",
                    &[Val::new(cat, IrType::Ptr), Val::new(msg, IrType::Ptr)],
                );
            }
            let ret = fb.call(&IrType::I32, &pbmain_ir_name, &[]);
            fb.ret(&ret);
            self.module.add_function_body(fb.finish());
        }

        // In session mode, emit GetSession() export
        if self.session_mode {
            self.emit_get_session();
        }

        Ok(())
    }

    fn register_type(&mut self, td: &TypeDecl) {
        let name = normalize_name(&td.name);
        let mut fields = Vec::new();
        let mut field_map = HashMap::new();
        let mut ir_types = Vec::new();

        for (i, field) in td.fields.iter().enumerate() {
            let field_name = normalize_name(&field.name);
            let ir_type = Self::ir_type_for_field(&field.pb_type);
            ir_types.push(ir_type.clone());
            field_map.insert(field_name.clone(), i);
            fields.push(FieldInfo {
                name: field_name,
                pb_type: field.pb_type.clone(),
                ir_type,
            });
        }

        self.module.define_struct_type(&name, &ir_types);
        self.type_layouts.insert(
            name.clone(),
            TypeLayout {
                ir_name: name,
                fields,
                field_map,
            },
        );
    }

    fn register_function(&mut self, fd: &FunctionDecl) {
        let name = normalize_name(&fd.name);
        let ir_name = fd.alias.clone().unwrap_or_else(|| name.clone());
        let ret_type = Self::ir_type_for(&fd.return_type);
        let params: Vec<ParamInfo> = fd
            .params
            .iter()
            .map(|p| {
                let ir_type = if p.is_byval {
                    Self::ir_type_for(&p.pb_type)
                } else {
                    IrType::Ptr
                };
                ParamInfo {
                    name: normalize_name(&p.name),
                    pb_type: p.pb_type.clone(),
                    ir_type,
                    is_byval: p.is_byval,
                    is_optional: p.is_optional,
                }
            })
            .collect();

        self.functions.insert(
            name.clone(),
            FuncInfo {
                ir_name,
                ret_type,
                params,
                export: fd.export,
                is_external: false,
                is_stdcall: false,
            },
        );
    }

    fn register_sub(&mut self, sd: &SubDecl) {
        let name = normalize_name(&sd.name);
        let ir_name = sd.alias.clone().unwrap_or_else(|| name.clone());
        let params: Vec<ParamInfo> = sd
            .params
            .iter()
            .map(|p| {
                let ir_type = if p.is_byval {
                    Self::ir_type_for(&p.pb_type)
                } else {
                    IrType::Ptr
                };
                ParamInfo {
                    name: normalize_name(&p.name),
                    pb_type: p.pb_type.clone(),
                    ir_type,
                    is_byval: p.is_byval,
                    is_optional: p.is_optional,
                }
            })
            .collect();

        self.subs.insert(
            name.clone(),
            FuncInfo {
                ir_name,
                ret_type: IrType::Void,
                params,
                export: sd.export,
                is_external: false,
                is_stdcall: false,
            },
        );
    }

    fn register_declare(&mut self, ds: &DeclareStmt) {
        let name = normalize_name(&ds.name);

        if ds.lib.is_none() {
            // Forward declaration within same compilation unit — skip.
            // The actual definition will be registered from FunctionDecl/SubDecl.
            return;
        }

        // External DLL import: emit dllimport declaration
        let ir_name = ds.alias.clone().unwrap_or_else(|| name.clone());
        let params: Vec<ParamInfo> = ds
            .params
            .iter()
            .map(|p| {
                let ir_type = if p.is_byval {
                    Self::ir_type_for(&p.pb_type)
                } else {
                    IrType::Ptr
                };
                ParamInfo {
                    name: normalize_name(&p.name),
                    pb_type: p.pb_type.clone(),
                    ir_type,
                    is_byval: p.is_byval,
                    is_optional: p.is_optional,
                }
            })
            .collect();

        let ir_param_types: Vec<IrType> = params.iter().map(|p| p.ir_type.clone()).collect();

        // Determine calling convention and linkage:
        // - Win32 API DLLs: dllimport + stdcall on 32-bit
        // - Stubbed DLLs: regular extern (stubs provided in pb_runtime.c)
        // - Other DLLs: dllimport + cdecl
        let lib_name = ds.lib.as_deref().unwrap_or("").to_uppercase();
        let is_stdcall = self.module.is_32bit() && Self::is_win32_stdcall_dll(&lib_name);
        let is_stubbed = Self::is_stubbed_dll(&lib_name);

        if ds.is_function {
            let ret_type = Self::ir_type_for(&ds.return_type);
            if is_stubbed {
                // Stubbed DLLs: declare as regular external (resolved by pb_runtime.obj)
                self.module
                    .declare_function(&ir_name, &ret_type, &ir_param_types, false);
            } else if is_stdcall {
                self.module
                    .declare_dllimport(&ir_name, &ret_type, &ir_param_types);
            } else {
                self.module
                    .declare_dllimport_cdecl(&ir_name, &ret_type, &ir_param_types);
            }
            self.functions.insert(
                name,
                FuncInfo {
                    ir_name,
                    ret_type,
                    params,
                    export: false,
                    is_external: !is_stubbed,
                    is_stdcall: is_stdcall && !is_stubbed,
                },
            );
        } else {
            if is_stubbed {
                self.module
                    .declare_function(&ir_name, &IrType::Void, &ir_param_types, false);
            } else if is_stdcall {
                self.module
                    .declare_dllimport(&ir_name, &IrType::Void, &ir_param_types);
            } else {
                self.module
                    .declare_dllimport_cdecl(&ir_name, &IrType::Void, &ir_param_types);
            }
            self.subs.insert(
                name,
                FuncInfo {
                    ir_name,
                    ret_type: IrType::Void,
                    params,
                    export: false,
                    is_external: !is_stubbed,
                    is_stdcall: is_stdcall && !is_stubbed,
                },
            );
        }
    }

    fn declare_global(&mut self, vd: &VarDecl) {
        let name = normalize_name(&vd.name);
        let ir_type = Self::ir_type_for(&vd.pb_type);
        if self.session_mode {
            // Defer: collect into session_fields for struct emission later
            if !self.session_fields.iter().any(|f| f.name == name) {
                self.session_fields.push(SessionFieldInfo {
                    name,
                    ir_type,
                    pb_type: vd.pb_type.clone(),
                    is_array: false,
                    elem_ir_type: None,
                    array_dims: None,
                    total_elements: None,
                });
            }
        } else {
            if Self::is_string_pb(&vd.pb_type) {
                // String globals init to empty string pointer
                self.module
                    .add_global(&name, &ir_type, &self.empty_string_name.clone());
            } else {
                self.module
                    .add_global(&name, &ir_type, &ir_type.zero_literal());
            }
            self.symbols
                .insert_global(name, ir_type, vd.pb_type.clone());
        }
    }

    // ========== Function/Sub compilation ==========

    /// Returns true if the function/sub should be nooped (compiled as an empty stub).
    /// EZLIB/EZGUI functions are part of the old Win32 UI — the Electron frontend handles all UI now.
    fn should_noop_function(name: &str) -> bool {
        name.starts_with("EZLIB_") || name.starts_with("EZGUI_")
    }

    fn compile_function(&mut self, fd: &FunctionDecl) -> PbResult<()> {
        let name = normalize_name(&fd.name);
        let info = self.functions.get(&name).unwrap().clone();

        let ir_params: Vec<(String, IrType)> = info
            .params
            .iter()
            .map(|p| (p.name.clone(), p.ir_type.clone()))
            .collect();

        let mut fb = self.module.create_function_builder_ex(
            &info.ir_name,
            &info.ret_type,
            &ir_params,
            info.export,
        );

        // Debug: record current function name
        let (fn_name_str, _) = self.module.add_string_constant(&name);
        fb.call_void("pb_debug_enter", &[Val::new(fn_name_str, IrType::Ptr)]);

        // Noop EZLIB/EZGUI functions — old Win32 UI, replaced by Electron
        if Self::should_noop_function(&name) {
            let ret_ir = Self::ir_type_for(&fd.return_type);
            if ret_ir == IrType::Ptr {
                fb.ret(&Val::new(self.empty_string_name.clone(), IrType::Ptr));
            } else {
                fb.ret(&Val::new(ret_ir.zero_literal(), ret_ir.clone()));
            }
            self.module.add_function_body(fb.finish());
            return Ok(());
        }

        self.current_fn_name = Some(name.clone());
        let ret_ir = Self::ir_type_for(&fd.return_type);
        self.current_fn_return_type = Some((fd.return_type.clone(), ret_ir.clone()));
        self.symbols.clear_locals();

        // Allocate return value storage
        let retval_ptr = fb.alloca(&ret_ir);
        if ret_ir == IrType::Ptr {
            fb.store(
                &Val::new(self.empty_string_name.clone(), IrType::Ptr),
                &retval_ptr,
            );
        } else {
            fb.store(
                &Val::new(ret_ir.zero_literal(), ret_ir.clone()),
                &retval_ptr,
            );
        }
        self.current_fn_retval_ptr = Some(retval_ptr);

        // Bind parameters
        self.bind_params(&mut fb, &info.params);

        // Set up GOSUB context (must happen before compile_body)
        self.setup_gosub_context(&mut fb, &fd.body);

        // Compile body
        self.compile_body(&mut fb, &fd.body)?;

        // Emit RETURN dispatch if we had GOSUBs
        self.emit_return_dispatch(&mut fb);

        // Return
        if !fb.is_terminated() {
            let retval_ptr = self.current_fn_retval_ptr.clone().unwrap();
            let ret_val = fb.load(&ret_ir, &retval_ptr);
            fb.ret(&ret_val);
        }

        self.current_fn_name = None;
        self.current_fn_return_type = None;
        self.current_fn_retval_ptr = None;
        self.gosub_context = None;

        self.module.add_function_body(fb.finish());
        Ok(())
    }

    fn compile_sub(&mut self, sd: &SubDecl) -> PbResult<()> {
        let name = normalize_name(&sd.name);
        let info = self.subs.get(&name).unwrap().clone();

        let ir_params: Vec<(String, IrType)> = info
            .params
            .iter()
            .map(|p| (p.name.clone(), p.ir_type.clone()))
            .collect();

        let mut fb = self.module.create_function_builder_ex(
            &info.ir_name,
            &IrType::Void,
            &ir_params,
            info.export,
        );

        // Debug: record current function name
        let (fn_name_str, _) = self.module.add_string_constant(&name);
        fb.call_void("pb_debug_enter", &[Val::new(fn_name_str, IrType::Ptr)]);

        // Noop EZLIB/EZGUI subs — old Win32 UI, replaced by Electron
        if Self::should_noop_function(&name) {
            fb.ret_void();
            self.module.add_function_body(fb.finish());
            return Ok(());
        }

        self.current_fn_name = Some(name.clone());
        self.current_fn_return_type = None;
        self.current_fn_retval_ptr = None;
        self.symbols.clear_locals();

        // Bind parameters
        self.bind_params(&mut fb, &info.params);

        // Set up GOSUB context
        self.setup_gosub_context(&mut fb, &sd.body);

        // Compile body
        self.compile_body(&mut fb, &sd.body)?;

        // Emit RETURN dispatch if we had GOSUBs
        self.emit_return_dispatch(&mut fb);

        // Return void
        if !fb.is_terminated() {
            fb.ret_void();
        }

        self.current_fn_name = None;
        self.gosub_context = None;

        self.module.add_function_body(fb.finish());
        Ok(())
    }

    fn bind_params(&mut self, fb: &mut FunctionBuilder, params: &[ParamInfo]) {
        for p in params {
            if p.is_byval {
                // Allocate local storage and copy
                let ptr = fb.alloca(&Self::ir_type_for(&p.pb_type));
                let param_val = Val::new(format!("%{}", p.name), p.ir_type.clone());
                fb.store(&param_val, &ptr);
                self.symbols.insert_local(
                    p.name.clone(),
                    ptr.name.clone(),
                    Self::ir_type_for(&p.pb_type),
                    p.pb_type.clone(),
                );
            } else {
                // BYREF: parameter IS a pointer
                self.symbols.insert_local(
                    p.name.clone(),
                    format!("%{}", p.name),
                    Self::ir_type_for(&p.pb_type),
                    p.pb_type.clone(),
                );
            }
        }
    }

    // ========== GOSUB/RETURN setup ==========

    /// Scan a function body for all Label and GoSub statements.
    /// Returns (set of all label names, set of labels that are GOSUB targets).
    fn collect_labels(stmts: &[Statement]) -> (HashSet<String>, HashSet<String>) {
        let mut labels = HashSet::new();
        let mut gosub_targets = HashSet::new();
        Self::collect_labels_recursive(stmts, &mut labels, &mut gosub_targets);
        (labels, gosub_targets)
    }

    fn collect_labels_recursive(
        stmts: &[Statement],
        labels: &mut HashSet<String>,
        gosub_targets: &mut HashSet<String>,
    ) {
        for stmt in stmts {
            match stmt {
                Statement::Label(name) => {
                    labels.insert(name.clone());
                }
                Statement::GoSub(name) => {
                    gosub_targets.insert(name.clone());
                }
                Statement::GoTo(_) => {}
                Statement::If(if_stmt) => {
                    Self::collect_labels_recursive(&if_stmt.then_body, labels, gosub_targets);
                    for clause in &if_stmt.elseif_clauses {
                        Self::collect_labels_recursive(&clause.body, labels, gosub_targets);
                    }
                    Self::collect_labels_recursive(&if_stmt.else_body, labels, gosub_targets);
                }
                Statement::For(for_stmt) => {
                    Self::collect_labels_recursive(&for_stmt.body, labels, gosub_targets);
                }
                Statement::DoLoop(do_stmt) => {
                    Self::collect_labels_recursive(&do_stmt.body, labels, gosub_targets);
                }
                Statement::WhileWend(while_stmt) => {
                    Self::collect_labels_recursive(&while_stmt.body, labels, gosub_targets);
                }
                Statement::SelectCase(select_stmt) => {
                    for case in &select_stmt.cases {
                        Self::collect_labels_recursive(&case.body, labels, gosub_targets);
                    }
                    Self::collect_labels_recursive(&select_stmt.else_body, labels, gosub_targets);
                }
                Statement::Block(inner) => {
                    Self::collect_labels_recursive(inner, labels, gosub_targets);
                }
                _ => {}
            }
        }
    }

    /// Set up GOSUB context for a function body (if it has any labels/gosubs).
    fn setup_gosub_context(&mut self, fb: &mut FunctionBuilder, body: &[Statement]) {
        let (labels, gosub_targets) = Self::collect_labels(body);
        // Filter out labels that are actually known SUB/FUNCTION names (parser ambiguity)
        let labels: HashSet<String> = labels
            .into_iter()
            .filter(|name| !self.functions.contains_key(&normalize_name(name)))
            .collect();
        if labels.is_empty() && gosub_targets.is_empty() {
            self.gosub_context = None;
            return;
        }

        // Allocate return address variable
        let ret_addr_ptr = fb.alloca(&IrType::I32);
        fb.store(&Val::new("0", IrType::I32), &ret_addr_ptr);

        // Create block names for all labels
        let mut label_blocks = HashMap::new();
        for label_name in &labels {
            let block_name = fb.next_label(&format!("label.{}", label_name));
            label_blocks.insert(label_name.clone(), block_name);
        }
        // Also create blocks for gosub targets that might not have been in `labels`
        // (shouldn't happen but be safe)
        for target in &gosub_targets {
            if !label_blocks.contains_key(target) {
                let block_name = fb.next_label(&format!("label.{}", target));
                label_blocks.insert(target.clone(), block_name);
            }
        }

        self.gosub_context = Some(GosubContext {
            label_blocks,
            return_points: Vec::new(),
            ret_addr_ptr,
            emitted_labels: HashSet::new(),
        });
    }

    // ========== Body / Statement compilation ==========

    fn compile_body(&mut self, fb: &mut FunctionBuilder, stmts: &[Statement]) -> PbResult<()> {
        for stmt in stmts {
            if fb.is_terminated() {
                // If terminated but we hit a label, we still need to emit it
                // (labels create new basic blocks and reset the terminated state)
                match stmt {
                    Statement::Label(_) => {
                        // Fall through: compile the label (it creates a new block)
                    }
                    _ => continue,
                }
            }
            self.compile_statement(fb, stmt)?;
        }
        Ok(())
    }

    fn emit_debug_line(&self, fb: &mut FunctionBuilder, line: usize) {
        if line > 0 {
            let line_val = fb.const_i32(line as i32);
            let debug_ptr = Val::new("@pb_debug_line".to_string(), IrType::Ptr);
            fb.store(&line_val, &debug_ptr);
        }
    }

    fn compile_statement(&mut self, fb: &mut FunctionBuilder, stmt: &Statement) -> PbResult<()> {
        // Emit debug line number for crash reporting
        let line = match stmt {
            Statement::Assign(a) => a.line,
            Statement::Call(c) => c.line,
            Statement::If(i) => i.line,
            Statement::For(f) => f.line,
            Statement::DoLoop(d) => d.line,
            Statement::SelectCase(s) => s.line,
            Statement::Dim(d) => d.line,
            _ => 0,
        };
        self.emit_debug_line(fb, line);

        match stmt {
            Statement::Assign(a) => self.compile_assign(fb, a),
            Statement::Call(c) => self.compile_call_stmt(fb, c),
            Statement::If(i) => self.compile_if(fb, i),
            Statement::For(f) => self.compile_for(fb, f),
            Statement::DoLoop(d) => self.compile_do_loop(fb, d),
            Statement::WhileWend(w) => self.compile_while_wend(fb, w),
            Statement::SelectCase(s) => self.compile_select_case(fb, s),
            Statement::Print(p) => self.compile_print(fb, p),
            Statement::Dim(d) => self.compile_dim(fb, d),
            Statement::FunctionReturn(fr) => self.compile_function_return(fb, fr),
            Statement::ExitSub => {
                // In a FUNCTION context, EXIT SUB should still return the return value
                if let Some((_, ref ret_ir)) = self.current_fn_return_type {
                    let ret_ir = ret_ir.clone();
                    let retval_ptr = self.current_fn_retval_ptr.clone().unwrap();
                    let val = fb.load(&ret_ir, &retval_ptr);
                    fb.ret(&val);
                } else {
                    fb.ret_void();
                }
                Ok(())
            }
            Statement::ExitFunction => {
                if let Some((_, ref ret_ir)) = self.current_fn_return_type {
                    let ret_ir = ret_ir.clone();
                    let retval_ptr = self.current_fn_retval_ptr.clone().unwrap();
                    let val = fb.load(&ret_ir, &retval_ptr);
                    fb.ret(&val);
                }
                Ok(())
            }
            Statement::ExitSelect => {
                // EXIT SELECT — in compiled code, CASE blocks don't fall through,
                // so this is effectively a no-op (control goes to end of select)
                Ok(())
            }
            Statement::ExitFor | Statement::ExitDo => {
                if let Some(info) = self.loop_stack.last() {
                    let exit = info.exit_label.clone();
                    fb.br(&exit);
                }
                Ok(())
            }
            Statement::Iterate(target) => {
                let is_for = matches!(target, IterateTarget::For);
                for info in self.loop_stack.iter().rev() {
                    if info.is_for == is_for {
                        let cont = info.continue_label.clone();
                        fb.br(&cont);
                        break;
                    }
                }
                Ok(())
            }
            Statement::Incr(inc) => self.compile_incr_decr(fb, inc, true),
            Statement::Decr(dec) => self.compile_incr_decr(fb, dec, false),
            Statement::Redim(d) => self.compile_dim(fb, d),
            Statement::Block(stmts) => self.compile_body(fb, stmts),
            // GOSUB/RETURN/Label/GoTo
            Statement::Label(name) => self.compile_label(fb, name),
            Statement::GoSub(label) => self.compile_gosub(fb, label),
            Statement::Return => self.compile_return(fb),
            Statement::GoTo(label) => self.compile_goto(fb, label),
            // Stubs: statements that compile to no-ops
            Statement::OnErrorGoto(_) | Statement::OnErrorGotoZero | Statement::ResumeNext => {
                Ok(())
            }
            Statement::Open(o) => self.compile_open(fb, o),
            Statement::Close(c) => self.compile_close(fb, c),
            Statement::PrintFile(p) => self.compile_print_file(fb, p),
            Statement::InputFile(inp) => self.compile_input_file(fb, inp),
            Statement::LineInputFile(li) => self.compile_line_input_file(fb, li),
            Statement::Kill(expr) => {
                let path = self.compile_expr(fb, expr)?;
                fb.call_void("pb_kill", &[path]);
                Ok(())
            }
            Statement::Noop => Ok(()),
        }
    }

    // ========== Assign ==========

    fn compile_assign(&mut self, fb: &mut FunctionBuilder, assign: &AssignStmt) -> PbResult<()> {
        let value = self.compile_expr(fb, &assign.value)?;
        match &assign.target {
            Expr::Variable(orig_name) => {
                let name = normalize_name(orig_name);
                if let Some(info) = self.symbols.lookup(&name) {
                    let ptr_name = info.ptr_name.clone();
                    let target_ir = info.ir_type.clone();
                    let target_pb = info.pb_type.clone();
                    // FixedString (ASCIIZ*N): use strncpy instead of store
                    if let PbType::FixedString(n) = &target_pb {
                        let buf_ptr = Val::new(ptr_name, IrType::Ptr);
                        let src = self.compile_expr(fb, &assign.value)?;
                        let size = fb.const_i32((*n as i32) - 1);
                        fb.call_void("strncpy", &[buf_ptr.clone(), src, size]);
                        // Null-terminate at position N-1
                        let term_idx = fb.const_i32((*n as i32) - 1);
                        let term_ptr = fb.gep_byte(&buf_ptr, &term_idx);
                        fb.store(&Val::new("0".to_string(), IrType::I8), &term_ptr);
                    } else {
                        let converted = self.convert_value(fb, &value, &target_ir, &target_pb);
                        let ptr = Val::new(ptr_name, IrType::Ptr);
                        fb.store(&converted, &ptr);
                    }
                } else {
                    // Auto-declare local — use original name for type inference
                    let pb_type = infer_type_from_name(orig_name);
                    let ir_type = Self::ir_type_for(&pb_type);
                    let ptr = fb.alloca(&ir_type);
                    let converted = self.convert_value(fb, &value, &ir_type, &pb_type);
                    fb.store(&converted, &ptr);
                    self.symbols.insert_local(name, ptr.name, ir_type, pb_type);
                }
            }
            Expr::ArrayAccess(arr_name, indices) | Expr::FunctionCall(arr_name, indices) => {
                let name = normalize_name(arr_name);
                if let Some(arr_info) = self.symbols.lookup_array(&name).cloned() {
                    let mut compiled_indices = Vec::new();
                    for idx in indices {
                        compiled_indices.push(self.compile_expr(fb, idx)?);
                    }
                    let flat_idx = self.compute_flat_index(fb, &compiled_indices, &arr_info);
                    let base = Val::new(arr_info.ptr_name.clone(), IrType::Ptr);
                    let elem_ptr = fb.gep_array(&arr_info.array_ir_type, &base, &flat_idx);
                    let converted =
                        self.convert_value(fb, &value, &arr_info.elem_ir_type, &arr_info.pb_type);
                    fb.store(&converted, &elem_ptr);
                }
            }
            Expr::TypeMember(_, _) => {
                let (field_ptr, field_pb) = self.compile_lvalue_ptr(fb, &assign.target)?;
                let field_ir = Self::ir_type_for(&field_pb);
                let converted = self.convert_value(fb, &value, &field_ir, &field_pb);
                fb.store(&converted, &field_ptr);
            }
            _ => {}
        }
        Ok(())
    }

    // ========== Lvalue pointer ==========

    /// Return a pointer to an lvalue expression (without loading it).
    fn compile_lvalue_ptr(
        &mut self,
        fb: &mut FunctionBuilder,
        expr: &Expr,
    ) -> PbResult<(Val, PbType)> {
        match expr {
            Expr::Variable(orig_name) => {
                let name = normalize_name(orig_name);
                if let Some(info) = self.symbols.lookup(&name) {
                    Ok((
                        Val::new(info.ptr_name.clone(), IrType::Ptr),
                        info.pb_type.clone(),
                    ))
                } else {
                    // Auto-declare
                    let ptr_name = self.ensure_variable_ptr(fb, &name, orig_name);
                    let info = self.symbols.lookup(&name).unwrap();
                    let pb_type = info.pb_type.clone();
                    Ok((Val::new(ptr_name, IrType::Ptr), pb_type))
                }
            }
            Expr::ArrayAccess(arr_name, indices) | Expr::FunctionCall(arr_name, indices) => {
                let name = normalize_name(arr_name);
                if let Some(arr_info) = self.symbols.lookup_array(&name).cloned() {
                    let mut compiled_indices = Vec::new();
                    for idx in indices {
                        compiled_indices.push(self.compile_expr(fb, idx)?);
                    }
                    let flat_idx = self.compute_flat_index(fb, &compiled_indices, &arr_info);
                    let base = Val::new(arr_info.ptr_name.clone(), IrType::Ptr);
                    let elem_ptr = fb.gep_array(&arr_info.array_ir_type, &base, &flat_idx);
                    Ok((elem_ptr, arr_info.pb_type.clone()))
                } else {
                    Err(PbError::runtime(format!("Unknown array: {}", name)))
                }
            }
            Expr::TypeMember(base_expr, member) => {
                let (base_ptr, base_pb) = self.compile_lvalue_ptr(fb, base_expr)?;
                if let PbType::UserDefined(type_name) = &base_pb {
                    let type_name = normalize_name(type_name);
                    let layout = self
                        .type_layouts
                        .get(&type_name)
                        .ok_or_else(|| PbError::runtime(format!("Unknown TYPE: {}", type_name)))?;
                    let member_norm = normalize_name(member);
                    let field_idx = *layout.field_map.get(&member_norm).ok_or_else(|| {
                        PbError::runtime(format!("Unknown field: {}.{}", type_name, member_norm))
                    })?;
                    let field_pb = layout.fields[field_idx].pb_type.clone();
                    let field_ptr = fb.gep_struct(&type_name, &base_ptr, field_idx);
                    Ok((field_ptr, field_pb))
                } else {
                    Err(PbError::runtime(
                        "TypeMember on non-TYPE variable".to_string(),
                    ))
                }
            }
            _ => Err(PbError::runtime("Not an lvalue".to_string())),
        }
    }

    // ========== DIM ==========

    fn compile_dim(&mut self, fb: &mut FunctionBuilder, dim: &DimStatement) -> PbResult<()> {
        let name = normalize_name(&dim.name);

        if !dim.bounds.is_empty() {
            // Check if this is a pending global array (GLOBAL arr() + DIM arr(bounds) inside fn)
            if self.pending_global_arrays.contains_key(&name) {
                let pb_type = self.pending_global_arrays.get(&name).unwrap().clone();
                self.declare_global_array(&name, &pb_type, &dim.bounds);
                return Ok(());
            }
            // Already declared as global array — skip
            if self.symbols.lookup_array(&name).is_some() {
                return Ok(());
            }
            // Local array
            let elem_ir = Self::ir_type_for(&dim.pb_type);
            let mut dims = Vec::new();
            let mut total = 1usize;
            for bound in &dim.bounds {
                let lower = Self::eval_const_expr(&bound.lower).unwrap_or(0);
                let upper = Self::eval_const_expr(&bound.upper).unwrap_or(0);
                let count = (upper - lower + 1) as usize;
                dims.push((lower, count as i64));
                total *= count;
            }
            let array_ir = IrType::Array(total, Box::new(elem_ir.clone()));
            let ptr = fb.alloca(&array_ir);
            fb.store(&Val::new("zeroinitializer", array_ir.clone()), &ptr);
            self.symbols.insert_local_array(
                name,
                ArrayInfo {
                    ptr_name: ptr.name,
                    array_ir_type: array_ir,
                    elem_ir_type: elem_ir,
                    pb_type: dim.pb_type.clone(),
                    dims,
                    total_elements: total,
                },
            );
            return Ok(());
        }

        // Scalar DIM
        // FixedString(N) / ASCIIZ*N: allocate [N x i8] buffer on stack
        if let PbType::FixedString(n) = &dim.pb_type {
            let buf_ir = IrType::Array(*n, Box::new(IrType::I8));
            let ptr = fb.alloca(&buf_ir);
            fb.store(&Val::new("zeroinitializer", buf_ir.clone()), &ptr);
            // Register as Ptr type so it can be passed to functions expecting char*
            self.symbols
                .insert_local(name, ptr.name, IrType::Ptr, dim.pb_type.clone());
            return Ok(());
        }
        let ir_type = Self::ir_type_for(&dim.pb_type);
        let ptr = fb.alloca(&ir_type);
        if Self::is_string_pb(&dim.pb_type) {
            fb.store(&Val::new(self.empty_string_name.clone(), IrType::Ptr), &ptr);
        } else {
            fb.store(&Val::new(ir_type.zero_literal(), ir_type.clone()), &ptr);
        }
        self.symbols
            .insert_local(name, ptr.name, ir_type, dim.pb_type.clone());
        Ok(())
    }

    // ========== FUNCTION = expr ==========

    fn compile_function_return(
        &mut self,
        fb: &mut FunctionBuilder,
        fr: &FunctionReturnStmt,
    ) -> PbResult<()> {
        if let Some(retval_ptr) = self.current_fn_retval_ptr.clone() {
            let (ref pb_type, ref ir_type) = self
                .current_fn_return_type
                .clone()
                .unwrap_or((PbType::Long, IrType::I32));
            let value = self.compile_expr(fb, &fr.value)?;
            let converted = self.convert_value(fb, &value, ir_type, pb_type);
            let ptr = Val::new(retval_ptr.name, IrType::Ptr);
            fb.store(&converted, &ptr);
        }
        Ok(())
    }

    // ========== CALL ==========

    fn compile_call_stmt(&mut self, fb: &mut FunctionBuilder, call: &CallStmt) -> PbResult<()> {
        let name = normalize_name(&call.name);

        // System statement-builtins
        match name.as_str() {
            "SLEEP" => {
                if !call.args.is_empty() {
                    let ms = self.compile_expr(fb, &call.args[0])?;
                    let ms_i32 = self.to_i32(fb, &ms);
                    if self.module.is_32bit() {
                        fb.call_void_stdcall("Sleep", &[ms_i32]);
                    } else {
                        fb.call_void("Sleep", &[ms_i32]);
                    }
                }
                return Ok(());
            }
            "RANDOMIZE" => {
                if !call.args.is_empty() {
                    let seed = self.compile_expr(fb, &call.args[0])?;
                    let seed_i32 = self.to_i32(fb, &seed);
                    fb.call_void("srand", &[seed_i32]);
                } else {
                    let null = Val::new("null".to_string(), IrType::Ptr);
                    let t = fb.call(&IrType::I32, "time", &[null]);
                    fb.call_void("srand", &[t]);
                }
                return Ok(());
            }
            "MKDIR" => {
                if !call.args.is_empty() {
                    let path = self.compile_expr(fb, &call.args[0])?;
                    fb.call(&IrType::I32, "_mkdir", &[path]);
                }
                return Ok(());
            }
            "RMDIR" => {
                if !call.args.is_empty() {
                    let path = self.compile_expr(fb, &call.args[0])?;
                    fb.call(&IrType::I32, "_rmdir", &[path]);
                }
                return Ok(());
            }
            "CHDIR" => {
                if !call.args.is_empty() {
                    let path = self.compile_expr(fb, &call.args[0])?;
                    fb.call(&IrType::I32, "_chdir", &[path]);
                }
                return Ok(());
            }
            _ => {}
        }

        if let Some(info) = self.functions.get(&name).cloned() {
            let args = self.compile_call_args(fb, &call.args, &info)?;
            if info.is_stdcall {
                fb.call_stdcall(&info.ret_type, &info.ir_name, &args);
            } else {
                fb.call(&info.ret_type, &info.ir_name, &args);
            }
            return Ok(());
        }
        if let Some(info) = self.subs.get(&name).cloned() {
            let args = self.compile_call_args(fb, &call.args, &info)?;
            if info.is_stdcall {
                fb.call_void_stdcall(&info.ir_name, &args);
            } else {
                fb.call_void(&info.ir_name, &args);
            }
            return Ok(());
        }

        Ok(()) // Unknown sub — skip
    }

    fn compile_call_args(
        &mut self,
        fb: &mut FunctionBuilder,
        args: &[Expr],
        func_info: &FuncInfo,
    ) -> PbResult<Vec<Val>> {
        let mut compiled = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            if i < func_info.params.len() {
                let param = &func_info.params[i];
                // BYVAL override at call site: pass value directly even if param is BYREF
                let (is_byval_override, inner_arg) = match arg {
                    Expr::ByvalOverride(inner) => (true, inner.as_ref()),
                    _ => (false, arg),
                };
                if param.is_byval || is_byval_override {
                    let val = self.compile_expr(fb, inner_arg)?;
                    // BYVAL override with integer value for a ptr param (e.g. BYVAL STRPTR(x$)):
                    // convert i32 to ptr via inttoptr
                    if is_byval_override && val.ty == IrType::I32 && param.ir_type == IrType::Ptr {
                        let ptr_val = fb.inttoptr(&val);
                        compiled.push(ptr_val);
                    } else {
                        compiled.push(val);
                    }
                } else {
                    // BYREF: pass address
                    match inner_arg {
                        Expr::Variable(vname) => {
                            let norm = normalize_name(vname);
                            let ptr_name = self.ensure_variable_ptr(fb, &norm, vname).clone();
                            compiled.push(Val::new(ptr_name, IrType::Ptr));
                        }
                        _ => {
                            // Expression to BYREF: allocate temp
                            let val = self.compile_expr(fb, inner_arg)?;
                            let tmp = fb.alloca(&val.ty);
                            fb.store(&val, &tmp);
                            compiled.push(tmp);
                        }
                    }
                }
            } else {
                let val = self.compile_expr(fb, arg)?;
                compiled.push(val);
            }
        }

        // Fill in default values for missing OPTIONAL parameters
        for i in args.len()..func_info.params.len() {
            let param = &func_info.params[i];
            if !param.is_optional {
                break; // Non-optional param missing — stop (caller error, handled at runtime)
            }
            let default_val = if Self::is_string_pb(&param.pb_type) {
                Val::new(self.empty_string_name.clone(), IrType::Ptr)
            } else {
                let ir_type = Self::ir_type_for(&param.pb_type);
                Val::new(ir_type.zero_literal(), ir_type)
            };
            if param.is_byval {
                compiled.push(default_val);
            } else {
                // BYREF: allocate temp with default value and pass its address
                let tmp = fb.alloca(&default_val.ty);
                fb.store(&default_val, &tmp);
                compiled.push(tmp);
            }
        }

        Ok(compiled)
    }

    // ========== IF ==========

    fn compile_if(&mut self, fb: &mut FunctionBuilder, if_stmt: &IfStmt) -> PbResult<()> {
        let cond = self.compile_expr_as_bool(fb, &if_stmt.condition)?;
        let merge = fb.next_label("if.merge");

        let has_else = !if_stmt.elseif_clauses.is_empty() || !if_stmt.else_body.is_empty();

        if has_else {
            let then_label = fb.next_label("if.then");
            let else_label = fb.next_label("if.else");

            fb.condbr(&cond, &then_label, &else_label);

            // Then
            fb.label(&then_label);
            self.compile_body(fb, &if_stmt.then_body)?;
            if !fb.is_terminated() {
                fb.br(&merge);
            }

            // Elseif chain
            let mut current_else = else_label;
            for (i, clause) in if_stmt.elseif_clauses.iter().enumerate() {
                fb.label(&current_else);
                let elif_cond = self.compile_expr_as_bool(fb, &clause.condition)?;
                let elif_then = fb.next_label(&format!("elif.then.{}", i));
                let elif_else = fb.next_label(&format!("elif.else.{}", i));
                fb.condbr(&elif_cond, &elif_then, &elif_else);

                fb.label(&elif_then);
                self.compile_body(fb, &clause.body)?;
                if !fb.is_terminated() {
                    fb.br(&merge);
                }

                current_else = elif_else;
            }

            // Else
            fb.label(&current_else);
            if !if_stmt.else_body.is_empty() {
                self.compile_body(fb, &if_stmt.else_body)?;
            }
            if !fb.is_terminated() {
                fb.br(&merge);
            }
        } else {
            let then_label = fb.next_label("if.then");
            fb.condbr(&cond, &then_label, &merge);
            fb.label(&then_label);
            self.compile_body(fb, &if_stmt.then_body)?;
            if !fb.is_terminated() {
                fb.br(&merge);
            }
        }

        fb.label(&merge);
        Ok(())
    }

    // ========== FOR ==========

    fn compile_for(&mut self, fb: &mut FunctionBuilder, for_stmt: &ForStmt) -> PbResult<()> {
        let var_name = normalize_name(&for_stmt.var);
        let var_ptr_name = self.ensure_variable_ptr(fb, &var_name, &for_stmt.var);

        // Compute and store start
        let start = self.compile_expr(fb, &for_stmt.start)?;
        let start_i32 = self.to_i32(fb, &start);
        let var_ptr = Val::new(var_ptr_name.clone(), IrType::Ptr);
        fb.store(&start_i32, &var_ptr);

        let end_val = self.compile_expr(fb, &for_stmt.end)?;
        let end_i32 = self.to_i32(fb, &end_val);

        let step_val = if let Some(ref step) = for_stmt.step {
            let s = self.compile_expr(fb, step)?;
            self.to_i32(fb, &s)
        } else {
            fb.const_i32(1)
        };

        // Store end and step in locals so they survive across iterations
        let end_ptr = fb.alloca(&IrType::I32);
        fb.store(&end_i32, &end_ptr);
        let step_ptr = fb.alloca(&IrType::I32);
        fb.store(&step_val, &step_ptr);

        let cond_label = fb.next_label("for.cond");
        let body_label = fb.next_label("for.body");
        let incr_label = fb.next_label("for.incr");
        let exit_label = fb.next_label("for.exit");

        fb.br(&cond_label);

        // Condition
        fb.label(&cond_label);
        let cur = fb.load(&IrType::I32, &var_ptr);
        let end = fb.load(&IrType::I32, &end_ptr);
        let step = fb.load(&IrType::I32, &step_ptr);
        let zero = fb.const_i32(0);
        let step_pos = fb.icmp("sgt", &step, &zero);
        let cond_le = fb.icmp("sle", &cur, &end);
        let cond_ge = fb.icmp("sge", &cur, &end);
        let cond = fb.select(&step_pos, &cond_le, &cond_ge);
        // select returns the type of the selected values (i1), convert to i1 for branch
        let cond_bool = if cond.ty != IrType::I1 {
            fb.icmp("ne", &cond, &fb.const_i32(0))
        } else {
            cond
        };
        fb.condbr(&cond_bool, &body_label, &exit_label);

        // Body
        fb.label(&body_label);
        self.loop_stack.push(LoopInfo {
            continue_label: incr_label.clone(),
            exit_label: exit_label.clone(),
            is_for: true,
        });
        self.compile_body(fb, &for_stmt.body)?;
        self.loop_stack.pop();
        if !fb.is_terminated() {
            fb.br(&incr_label);
        }

        // Increment
        fb.label(&incr_label);
        let cur2 = fb.load(&IrType::I32, &var_ptr);
        let step2 = fb.load(&IrType::I32, &step_ptr);
        let next = fb.add(&cur2, &step2);
        fb.store(&next, &var_ptr);
        fb.br(&cond_label);

        fb.label(&exit_label);
        Ok(())
    }

    // ========== DO/LOOP ==========

    fn compile_do_loop(&mut self, fb: &mut FunctionBuilder, do_stmt: &DoLoopStmt) -> PbResult<()> {
        let cond_label = fb.next_label("do.cond");
        let body_label = fb.next_label("do.body");
        let exit_label = fb.next_label("do.exit");

        if let (true, Some(condition)) = (do_stmt.is_pre_test, do_stmt.condition.as_ref()) {
            fb.br(&cond_label);
            fb.label(&cond_label);
            let cond = self.compile_expr_as_bool(fb, condition)?;
            let cond = if !do_stmt.is_while {
                // UNTIL: negate
                let one = fb.const_i1(true);
                fb.xor(&cond, &one)
            } else {
                cond
            };
            fb.condbr(&cond, &body_label, &exit_label);
        } else {
            fb.br(&body_label);
        }

        // Body
        fb.label(&body_label);
        self.loop_stack.push(LoopInfo {
            continue_label: cond_label.clone(),
            exit_label: exit_label.clone(),
            is_for: false,
        });
        self.compile_body(fb, &do_stmt.body)?;
        self.loop_stack.pop();

        if !fb.is_terminated() {
            if do_stmt.is_pre_test {
                fb.br(&cond_label);
            } else if let Some(ref cond_expr) = do_stmt.condition {
                fb.br(&cond_label);
                fb.label(&cond_label);
                let cond = self.compile_expr_as_bool(fb, cond_expr)?;
                let cond = if !do_stmt.is_while {
                    let one = fb.const_i1(true);
                    fb.xor(&cond, &one)
                } else {
                    cond
                };
                fb.condbr(&cond, &body_label, &exit_label);
            } else {
                fb.br(&body_label); // Infinite loop
            }
        }

        fb.label(&exit_label);
        Ok(())
    }

    // ========== WHILE/WEND ==========

    fn compile_while_wend(
        &mut self,
        fb: &mut FunctionBuilder,
        while_stmt: &WhileWendStmt,
    ) -> PbResult<()> {
        let cond_label = fb.next_label("while.cond");
        let body_label = fb.next_label("while.body");
        let exit_label = fb.next_label("while.exit");

        fb.br(&cond_label);

        fb.label(&cond_label);
        let cond = self.compile_expr_as_bool(fb, &while_stmt.condition)?;
        fb.condbr(&cond, &body_label, &exit_label);

        fb.label(&body_label);
        self.loop_stack.push(LoopInfo {
            continue_label: cond_label.clone(),
            exit_label: exit_label.clone(),
            is_for: false,
        });
        self.compile_body(fb, &while_stmt.body)?;
        self.loop_stack.pop();
        if !fb.is_terminated() {
            fb.br(&cond_label);
        }

        fb.label(&exit_label);
        Ok(())
    }

    // ========== SELECT CASE ==========

    fn compile_select_case(
        &mut self,
        fb: &mut FunctionBuilder,
        select: &SelectCaseStmt,
    ) -> PbResult<()> {
        let test_val = self.compile_expr(fb, &select.expr)?;
        let merge_label = fb.next_label("select.merge");

        let else_label = if !select.else_body.is_empty() {
            fb.next_label("select.else")
        } else {
            merge_label.clone()
        };

        // Generate test+body labels for each case
        let case_labels: Vec<(String, String)> = (0..select.cases.len())
            .map(|i| {
                (
                    fb.next_label(&format!("case.test.{}", i)),
                    fb.next_label(&format!("case.body.{}", i)),
                )
            })
            .collect();

        // Branch to first case
        if !case_labels.is_empty() {
            fb.br(&case_labels[0].0);
        } else {
            fb.br(&else_label);
        }

        for (i, case) in select.cases.iter().enumerate() {
            let (ref test_l, ref body_l) = case_labels[i];
            let fallthrough = if i + 1 < case_labels.len() {
                case_labels[i + 1].0.clone()
            } else {
                else_label.clone()
            };

            // Test
            fb.label(test_l);
            let matched = self.compile_case_patterns(fb, &case.patterns, &test_val)?;
            fb.condbr(&matched, body_l, &fallthrough);

            // Body
            fb.label(body_l);

            // Debug: log which CASE branch matched
            if self.debug_mode {
                let case_desc = Self::describe_case_patterns(&case.patterns);
                let (cat, _) = self.module.add_string_constant("SELECT");
                let (msg, _) = self.module.add_string_constant(&case_desc);
                fb.call_void(
                    "pb_debug_log_msg",
                    &[Val::new(cat, IrType::Ptr), Val::new(msg, IrType::Ptr)],
                );
            }

            self.compile_body(fb, &case.body)?;
            if !fb.is_terminated() {
                fb.br(&merge_label);
            }
        }

        // Else
        if !select.else_body.is_empty() {
            fb.label(&else_label);
            self.compile_body(fb, &select.else_body)?;
            if !fb.is_terminated() {
                fb.br(&merge_label);
            }
        }

        fb.label(&merge_label);
        Ok(())
    }

    /// Build a human-readable description of CASE patterns for debug logging.
    /// e.g. "CASE 20" or "CASE 60, 70" or "CASE 1 TO 10" or "CASE IS > 5"
    fn describe_case_patterns(patterns: &[CasePattern]) -> String {
        let parts: Vec<String> = patterns
            .iter()
            .map(|p| match p {
                CasePattern::Value(Expr::IntegerLit(n)) => format!("{}", n),
                CasePattern::Value(Expr::FloatLit(f)) => format!("{}", f),
                CasePattern::Value(Expr::StringLit(s)) => format!("\"{}\"", s),
                CasePattern::Value(_) => "<expr>".to_string(),
                CasePattern::Range(Expr::IntegerLit(lo), Expr::IntegerLit(hi)) => {
                    format!("{} TO {}", lo, hi)
                }
                CasePattern::Range(_, _) => "<expr> TO <expr>".to_string(),
                CasePattern::Is(op, Expr::IntegerLit(n)) => {
                    let op_str = match op {
                        CaseOp::Lt => "<",
                        CaseOp::Lte => "<=",
                        CaseOp::Gt => ">",
                        CaseOp::Gte => ">=",
                        CaseOp::Eq => "=",
                        CaseOp::Neq => "<>",
                    };
                    format!("IS {} {}", op_str, n)
                }
                CasePattern::Is(_, _) => "IS <op> <expr>".to_string(),
            })
            .collect();
        format!("CASE {}", parts.join(", "))
    }

    fn compile_case_patterns(
        &mut self,
        fb: &mut FunctionBuilder,
        patterns: &[CasePattern],
        test_val: &Val,
    ) -> PbResult<Val> {
        let mut result = fb.const_i1(false);

        for pattern in patterns {
            let matched = match pattern {
                CasePattern::Value(expr) => {
                    let val = self.compile_expr(fb, expr)?;
                    self.compare_eq(fb, test_val, &val)
                }
                CasePattern::Range(lo_expr, hi_expr) => {
                    let lo = self.compile_expr(fb, lo_expr)?;
                    let hi = self.compile_expr(fb, hi_expr)?;
                    let ge = self.compare_ge(fb, test_val, &lo);
                    let le = self.compare_le(fb, test_val, &hi);
                    fb.and(&ge, &le)
                }
                CasePattern::Is(op, expr) => {
                    let val = self.compile_expr(fb, expr)?;
                    match op {
                        CaseOp::Lt => self.compare_lt(fb, test_val, &val),
                        CaseOp::Gt => self.compare_gt(fb, test_val, &val),
                        CaseOp::Lte => self.compare_le(fb, test_val, &val),
                        CaseOp::Gte => self.compare_ge(fb, test_val, &val),
                        CaseOp::Eq => self.compare_eq(fb, test_val, &val),
                        CaseOp::Neq => self.compare_ne(fb, test_val, &val),
                    }
                }
            };
            result = fb.or(&result, &matched);
        }

        Ok(result)
    }

    // ========== PRINT ==========

    fn compile_print(&mut self, fb: &mut FunctionBuilder, print_stmt: &PrintStmt) -> PbResult<()> {
        for arg in &print_stmt.args {
            let val = self.compile_expr(fb, arg)?;
            if val.ty == IrType::Ptr {
                let (str_name, _len) = self.module.add_string_constant("%s");
                let str_ptr = Val::new(str_name, IrType::Ptr);
                fb.call_variadic(&IrType::I32, "printf", &[str_ptr, val]);
            } else if val.ty.is_int() {
                let (str_name, _len) = self.module.add_string_constant("%d");
                let str_ptr = Val::new(str_name, IrType::Ptr);
                // Extend to i32 if needed
                let val_i32 = self.to_i32(fb, &val);
                fb.call_variadic(&IrType::I32, "printf", &[str_ptr, val_i32]);
            } else if val.ty.is_float() {
                let (str_name, _len) = self.module.add_string_constant("%.6g");
                let str_ptr = Val::new(str_name, IrType::Ptr);
                // Promote to f64 if needed
                let val_f64 = self.to_f64(fb, &val);
                fb.call_variadic(&IrType::I32, "printf", &[str_ptr, val_f64]);
            }
        }
        // Newline
        let (nl_name, _) = self.module.add_string_constant("\n");
        let nl_ptr = Val::new(nl_name, IrType::Ptr);
        fb.call_variadic(&IrType::I32, "printf", &[nl_ptr]);
        Ok(())
    }

    // ========== File I/O ==========

    fn compile_open(&mut self, fb: &mut FunctionBuilder, open: &OpenStmt) -> PbResult<()> {
        let filename = self.compile_expr(fb, &open.filename)?;
        let mode = match open.mode {
            OpenMode::Input => fb.const_i32(0),
            OpenMode::Output => fb.const_i32(1),
            OpenMode::Append => fb.const_i32(2),
        };
        let filenum = self.compile_expr(fb, &open.file_num)?;
        let filenum_i32 = self.to_i32(fb, &filenum);
        fb.call(&IrType::I32, "pb_open", &[filename, mode, filenum_i32]);
        Ok(())
    }

    fn compile_close(&mut self, fb: &mut FunctionBuilder, close: &CloseStmt) -> PbResult<()> {
        let filenum = self.compile_expr(fb, &close.file_num)?;
        let filenum_i32 = self.to_i32(fb, &filenum);
        fb.call_void("pb_close", &[filenum_i32]);
        Ok(())
    }

    fn compile_print_file(&mut self, fb: &mut FunctionBuilder, pf: &PrintFileStmt) -> PbResult<()> {
        let filenum = self.compile_expr(fb, &pf.file_num)?;
        let filenum_i32 = self.to_i32(fb, &filenum);
        for arg in &pf.args {
            let val = self.compile_expr(fb, arg)?;
            let s = self.val_to_string(fb, &val);
            fb.call_void("pb_print_file", &[filenum_i32.clone(), s]);
        }
        // Print newline after args
        fb.call_void("pb_print_file_newline", &[filenum_i32]);
        Ok(())
    }

    fn compile_input_file(
        &mut self,
        fb: &mut FunctionBuilder,
        inp: &InputFileStmt,
    ) -> PbResult<()> {
        let filenum = self.compile_expr(fb, &inp.file_num)?;
        let filenum_i32 = self.to_i32(fb, &filenum);
        for var_expr in &inp.vars {
            let (ptr, pb_type) = self.compile_lvalue_ptr(fb, var_expr)?;
            match pb_type {
                PbType::String | PbType::FixedString(_) => {
                    fb.call_void("pb_input_file_str", &[filenum_i32.clone(), ptr]);
                }
                PbType::Double | PbType::Single | PbType::Ext | PbType::Cur => {
                    fb.call_void("pb_input_file_dbl", &[filenum_i32.clone(), ptr]);
                }
                _ => {
                    // Integer types (Long, Integer, Dword, etc.)
                    fb.call_void("pb_input_file_int", &[filenum_i32.clone(), ptr]);
                }
            }
        }
        Ok(())
    }

    fn compile_line_input_file(
        &mut self,
        fb: &mut FunctionBuilder,
        li: &LineInputFileStmt,
    ) -> PbResult<()> {
        let filenum = self.compile_expr(fb, &li.file_num)?;
        let filenum_i32 = self.to_i32(fb, &filenum);
        let result = fb.call(&IrType::Ptr, "pb_line_input", &[filenum_i32]);
        let (ptr, _pb_type) = self.compile_lvalue_ptr(fb, &li.var)?;
        fb.store(&result, &ptr);
        Ok(())
    }

    // ========== INCR / DECR ==========

    fn compile_incr_decr(
        &mut self,
        fb: &mut FunctionBuilder,
        stmt: &IncrDecrStmt,
        is_incr: bool,
    ) -> PbResult<()> {
        match &stmt.target {
            Expr::Variable(ref orig_name) => {
                let name = normalize_name(orig_name);
                let ptr_name = self.ensure_variable_ptr(fb, &name, orig_name);
                let ptr = Val::new(ptr_name, IrType::Ptr);
                let current = fb.load(&IrType::I32, &ptr);
                let amount = if let Some(ref amt) = stmt.amount {
                    let v = self.compile_expr(fb, amt)?;
                    self.to_i32(fb, &v)
                } else {
                    fb.const_i32(1)
                };
                let result = if is_incr {
                    fb.add(&current, &amount)
                } else {
                    fb.sub(&current, &amount)
                };
                fb.store(&result, &ptr);
            }
            Expr::ArrayAccess(ref arr_name, ref indices)
            | Expr::FunctionCall(ref arr_name, ref indices) => {
                let name = normalize_name(arr_name);
                if let Some(arr_info) = self.symbols.lookup_array(&name).cloned() {
                    let mut compiled_indices = Vec::new();
                    for idx in indices {
                        compiled_indices.push(self.compile_expr(fb, idx)?);
                    }
                    let flat_idx = self.compute_flat_index(fb, &compiled_indices, &arr_info);
                    let base = Val::new(arr_info.ptr_name.clone(), IrType::Ptr);
                    let elem_ptr = fb.gep_array(&arr_info.array_ir_type, &base, &flat_idx);
                    let current = fb.load(&arr_info.elem_ir_type, &elem_ptr);
                    let cur_i32 = self.to_i32(fb, &current);
                    let amount = if let Some(ref amt) = stmt.amount {
                        let v = self.compile_expr(fb, amt)?;
                        self.to_i32(fb, &v)
                    } else {
                        fb.const_i32(1)
                    };
                    let result = if is_incr {
                        fb.add(&cur_i32, &amount)
                    } else {
                        fb.sub(&cur_i32, &amount)
                    };
                    let converted =
                        self.convert_value(fb, &result, &arr_info.elem_ir_type, &arr_info.pb_type);
                    fb.store(&converted, &elem_ptr);
                }
            }
            _ => {}
        }
        Ok(())
    }

    // ========== GOSUB / RETURN / GOTO / LABEL codegen ==========

    fn compile_label(&mut self, fb: &mut FunctionBuilder, name: &str) -> PbResult<()> {
        // If name matches a known SUB/FUNCTION, it was misparsed — emit a call instead
        let norm = normalize_name(name);
        if self.functions.contains_key(&norm) {
            let info = self.functions.get(&norm).unwrap().clone();
            if info.ret_type == IrType::Void {
                fb.call_void(&info.ir_name, &[]);
            } else {
                fb.call(&info.ret_type, &info.ir_name, &[]);
            }
            return Ok(());
        }

        if let Some(ref mut ctx) = self.gosub_context {
            if let Some(block_name) = ctx.label_blocks.get(name) {
                // Skip duplicate label emissions
                if ctx.emitted_labels.contains(name) {
                    return Ok(());
                }
                ctx.emitted_labels.insert(name.to_string());
                let block_name = block_name.clone();
                // Fallthrough: unconditional branch to the label block
                fb.br(&block_name);
                fb.label(&block_name);
            }
        }
        Ok(())
    }

    fn compile_gosub(&mut self, fb: &mut FunctionBuilder, label: &str) -> PbResult<()> {
        if let Some(ref mut ctx) = self.gosub_context {
            let return_id = (ctx.return_points.len() + 1) as i32;
            let return_label = fb.next_label(&format!("gosub.ret.{}", return_id));
            ctx.return_points.push(return_label.clone());

            // Store unique return ID
            let ret_addr_ptr = ctx.ret_addr_ptr.clone();
            let id_val = fb.const_i32(return_id);
            fb.store(&id_val, &ret_addr_ptr);

            // Branch to the label's block
            if let Some(target_block) = ctx.label_blocks.get(label) {
                let target_block = target_block.clone();
                fb.br(&target_block);
            }

            // Emit the return-point label (where RETURN comes back to)
            fb.label(&return_label);
        }
        Ok(())
    }

    fn compile_return(&mut self, fb: &mut FunctionBuilder) -> PbResult<()> {
        if let Some(ref ctx) = self.gosub_context {
            if !ctx.return_points.is_empty() {
                // Branch to the return dispatch block
                let dispatch_label = "gosub.dispatch".to_string();
                fb.br(&dispatch_label);
            }
        }
        Ok(())
    }

    fn compile_goto(&mut self, fb: &mut FunctionBuilder, label: &str) -> PbResult<()> {
        if let Some(ref ctx) = self.gosub_context {
            if let Some(block_name) = ctx.label_blocks.get(label) {
                let block_name = block_name.clone();
                fb.br(&block_name);
            }
        }
        Ok(())
    }

    /// Emit the RETURN dispatch block at the end of a function.
    /// This reads the gosub_ret_addr and switch-dispatches to the correct return point.
    fn emit_return_dispatch(&mut self, fb: &mut FunctionBuilder) {
        let ctx = match self.gosub_context.take() {
            Some(ctx) if !ctx.return_points.is_empty() => ctx,
            other => {
                self.gosub_context = other;
                return;
            }
        };

        // Build switch cases: return_id -> return-point label
        let cases: Vec<(i32, String)> = ctx
            .return_points
            .iter()
            .enumerate()
            .map(|(i, label)| ((i + 1) as i32, label.clone()))
            .collect();

        // Create a "bad return" label (should never be reached)
        let bad_label = fb.next_label("gosub.bad");

        // Emit the dispatch block
        fb.br("gosub.dispatch");
        fb.label("gosub.dispatch");
        let ret_addr = fb.load(&IrType::I32, &ctx.ret_addr_ptr);
        fb.switch(&ret_addr, &bad_label, &cases);

        // Bad label — unreachable but needed by LLVM
        fb.label(&bad_label);
        // Just fall through (will hit the function's normal return)

        self.gosub_context = None;
    }

    // ========== Expression compilation ==========

    fn compile_expr(&mut self, fb: &mut FunctionBuilder, expr: &Expr) -> PbResult<Val> {
        match expr {
            Expr::IntegerLit(n) => Ok(fb.const_i32(*n as i32)),
            Expr::FloatLit(f) => Ok(fb.const_f64(*f)),
            Expr::StringLit(s) => {
                let (str_name, _) = self.module.add_string_constant(s);
                Ok(Val::new(str_name, IrType::Ptr))
            }
            Expr::Variable(orig_name) => {
                let name = normalize_name(orig_name);
                // System variables that are actually function calls
                match name.as_str() {
                    "DATE" if orig_name.ends_with('$') => {
                        return Ok(fb.call(&IrType::Ptr, "pb_date", &[]));
                    }
                    "TIME" if orig_name.ends_with('$') => {
                        return Ok(fb.call(&IrType::Ptr, "pb_time", &[]));
                    }
                    _ => {}
                }
                let ptr_name = self.ensure_variable_ptr(fb, &name, orig_name);
                let info = self.symbols.lookup(&name).unwrap();
                // FixedString (ASCIIZ*N): alloca IS the buffer — create heap copy
                // (stack buffer would be dangling after function return)
                if matches!(info.pb_type, PbType::FixedString(_)) {
                    let buf_ptr = Val::new(ptr_name, IrType::Ptr);
                    let len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&buf_ptr));
                    return Ok(fb.call(&IrType::Ptr, "pb_bstr_alloc", &[buf_ptr, len]));
                }
                let ptr = Val::new(ptr_name, IrType::Ptr);
                let ir_type = info.ir_type.clone();
                let val = fb.load(&ir_type, &ptr);
                // For string variables, null-guard: replace null with empty string
                if ir_type == IrType::Ptr && Self::is_string_name(orig_name) {
                    let is_null = fb.icmp("eq", &val, &Val::new("null".to_string(), IrType::Ptr));
                    let empty = Val::new(self.empty_string_name.clone(), IrType::Ptr);
                    Ok(fb.select(&is_null, &empty, &val))
                } else {
                    Ok(val)
                }
            }
            Expr::PercentConst(name) => {
                let val = self
                    .pp_constants
                    .get(&name.to_uppercase())
                    .copied()
                    .unwrap_or(0);
                Ok(fb.const_i32(val as i32))
            }
            Expr::FunctionCall(name, args) => {
                let name = normalize_name(name);
                // Check if it's actually an array access (parser can't distinguish)
                if let Some(arr_info) = self.symbols.lookup_array(&name).cloned() {
                    let mut compiled_indices = Vec::new();
                    for idx in args {
                        compiled_indices.push(self.compile_expr(fb, idx)?);
                    }
                    let flat_idx = self.compute_flat_index(fb, &compiled_indices, &arr_info);
                    let base = Val::new(arr_info.ptr_name.clone(), IrType::Ptr);
                    let elem_ptr = fb.gep_array(&arr_info.array_ir_type, &base, &flat_idx);
                    let val = fb.load(&arr_info.elem_ir_type, &elem_ptr);
                    // Null-guard string array elements (zeroinitializer → null pointers)
                    if arr_info.elem_ir_type == IrType::Ptr {
                        Ok(self.null_guard_string(fb, &val))
                    } else {
                        Ok(val)
                    }
                } else if let Some(result) = self.compile_builtin(fb, &name, args) {
                    result
                } else if let Some(info) = self.functions.get(&name).cloned() {
                    let compiled_args = self.compile_call_args(fb, args, &info)?;
                    if info.is_stdcall {
                        Ok(fb.call_stdcall(&info.ret_type, &info.ir_name, &compiled_args))
                    } else {
                        Ok(fb.call(&info.ret_type, &info.ir_name, &compiled_args))
                    }
                } else {
                    Ok(fb.const_i32(0)) // Unknown function
                }
            }
            Expr::ArrayAccess(arr_name, indices) => {
                let name = normalize_name(arr_name);
                if let Some(arr_info) = self.symbols.lookup_array(&name).cloned() {
                    let mut compiled_indices = Vec::new();
                    for idx in indices {
                        compiled_indices.push(self.compile_expr(fb, idx)?);
                    }
                    let flat_idx = self.compute_flat_index(fb, &compiled_indices, &arr_info);
                    let base = Val::new(arr_info.ptr_name.clone(), IrType::Ptr);
                    let elem_ptr = fb.gep_array(&arr_info.array_ir_type, &base, &flat_idx);
                    let val = fb.load(&arr_info.elem_ir_type, &elem_ptr);
                    // Null-guard string array elements (zeroinitializer → null pointers)
                    if arr_info.elem_ir_type == IrType::Ptr {
                        Ok(self.null_guard_string(fb, &val))
                    } else {
                        Ok(val)
                    }
                } else {
                    Ok(fb.const_i32(0))
                }
            }
            Expr::TypeMember(base_expr, member) => {
                // Intercept EXE.PATH$ and EXE.NAME$
                if let Expr::Variable(ref base_name) = **base_expr {
                    let bn = normalize_name(base_name);
                    let mn = normalize_name(member);
                    if bn == "EXE" && mn == "PATH" {
                        return Ok(fb.call(&IrType::Ptr, "pb_exe_path", &[]));
                    }
                    if bn == "EXE" && (mn == "NAME" || mn == "FULL") {
                        return Ok(fb.call(&IrType::Ptr, "pb_exe_name", &[]));
                    }
                }
                let (field_ptr, field_pb) = self.compile_lvalue_ptr(fb, expr)?;
                // FixedString TYPE fields: the buffer address IS the string —
                // don't load from the buffer (that would read buffer content as a pointer)
                if matches!(field_pb, PbType::FixedString(_)) {
                    return Ok(field_ptr);
                }
                let field_ir = Self::ir_type_for(&field_pb);
                Ok(fb.load(&field_ir, &field_ptr))
            }
            Expr::Negate(inner) => {
                let val = self.compile_expr(fb, inner)?;
                if val.ty.is_float() {
                    Ok(fb.fneg(&val))
                } else {
                    Ok(fb.neg(&val))
                }
            }
            Expr::UnaryOp(UnaryOp::Not, inner) => {
                let val = self.compile_expr(fb, inner)?;
                Ok(fb.not(&val))
            }
            Expr::UnaryOp(UnaryOp::Neg, inner) => {
                let val = self.compile_expr(fb, inner)?;
                if val.ty.is_float() {
                    Ok(fb.fneg(&val))
                } else {
                    Ok(fb.neg(&val))
                }
            }
            Expr::Varptr(inner) => {
                // VARPTR(var) or VARPTR(arr(idx)) — return address as i32
                let (ptr, _pb_type) = self.compile_lvalue_ptr(fb, inner)?;
                Ok(fb.ptrtoint(&ptr))
            }
            Expr::ByvalOverride(inner) => {
                // BYVAL override outside of call args context — just compile inner expr
                self.compile_expr(fb, inner)
            }
            Expr::BinaryOp(op, left, right) => {
                let lhs = self.compile_expr(fb, left)?;
                let rhs = self.compile_expr(fb, right)?;
                self.compile_binary_op(fb, op, &lhs, &rhs)
            }
        }
    }

    fn compile_binary_op(
        &mut self,
        fb: &mut FunctionBuilder,
        op: &BinaryOp,
        lhs: &Val,
        rhs: &Val,
    ) -> PbResult<Val> {
        // String operations
        if lhs.ty == IrType::Ptr || rhs.ty == IrType::Ptr {
            return self.compile_string_binop(fb, op, lhs, rhs);
        }

        // Float promotion if either side is float
        if lhs.ty.is_float() || rhs.ty.is_float() {
            let l = self.to_f64(fb, lhs);
            let r = self.to_f64(fb, rhs);
            return self.compile_float_binop(fb, op, &l, &r);
        }

        // Integer: promote to same width
        let (l, r) = self.promote_ints(fb, lhs, rhs);

        match op {
            BinaryOp::Add => Ok(fb.add(&l, &r)),
            BinaryOp::Sub => Ok(fb.sub(&l, &r)),
            BinaryOp::Mul => Ok(fb.mul(&l, &r)),
            BinaryOp::Div => {
                // PB `/` produces float result
                let lf = fb.sitofp(&l, &IrType::Double);
                let rf = fb.sitofp(&r, &IrType::Double);
                Ok(fb.fdiv(&lf, &rf))
            }
            BinaryOp::IntDiv => Ok(fb.sdiv(&l, &r)),
            BinaryOp::Mod => Ok(fb.srem(&l, &r)),
            BinaryOp::Pow => {
                let lf = fb.sitofp(&l, &IrType::Double);
                let rf = fb.sitofp(&r, &IrType::Double);
                Ok(fb.call(&IrType::Double, "llvm.pow.f64", &[lf, rf]))
            }
            BinaryOp::And => Ok(fb.and(&l, &r)),
            BinaryOp::Or => Ok(fb.or(&l, &r)),
            BinaryOp::Xor => Ok(fb.xor(&l, &r)),
            BinaryOp::Eq => Ok(fb.icmp("eq", &l, &r)),
            BinaryOp::Neq => Ok(fb.icmp("ne", &l, &r)),
            BinaryOp::Lt => Ok(fb.icmp("slt", &l, &r)),
            BinaryOp::Gt => Ok(fb.icmp("sgt", &l, &r)),
            BinaryOp::Lte => Ok(fb.icmp("sle", &l, &r)),
            BinaryOp::Gte => Ok(fb.icmp("sge", &l, &r)),
            BinaryOp::StrConcat => {
                // & on non-string operands: convert both to strings and concat
                let ls = self.num_to_string(fb, &l);
                let rs = self.num_to_string(fb, &r);
                Ok(self.compile_str_concat(fb, &ls, &rs))
            }
        }
    }

    fn compile_float_binop(
        &mut self,
        fb: &mut FunctionBuilder,
        op: &BinaryOp,
        l: &Val,
        r: &Val,
    ) -> PbResult<Val> {
        match op {
            BinaryOp::Add => Ok(fb.fadd(l, r)),
            BinaryOp::Sub => Ok(fb.fsub(l, r)),
            BinaryOp::Mul => Ok(fb.fmul(l, r)),
            BinaryOp::Div => Ok(fb.fdiv(l, r)),
            BinaryOp::IntDiv => {
                let div = fb.fdiv(l, r);
                Ok(fb.fptosi(&div, &IrType::I32))
            }
            BinaryOp::Mod => Ok(fb.frem(l, r)),
            BinaryOp::Pow => Ok(fb.call(&IrType::Double, "llvm.pow.f64", &[l.clone(), r.clone()])),
            BinaryOp::Eq => Ok(fb.fcmp("oeq", l, r)),
            BinaryOp::Neq => Ok(fb.fcmp("one", l, r)),
            BinaryOp::Lt => Ok(fb.fcmp("olt", l, r)),
            BinaryOp::Gt => Ok(fb.fcmp("ogt", l, r)),
            BinaryOp::Lte => Ok(fb.fcmp("ole", l, r)),
            BinaryOp::Gte => Ok(fb.fcmp("oge", l, r)),
            BinaryOp::StrConcat => {
                let ls = self.num_to_string(fb, l);
                let rs = self.num_to_string(fb, r);
                Ok(self.compile_str_concat(fb, &ls, &rs))
            }
            _ => Ok(fb.const_f64(0.0)),
        }
    }

    // ========== String binary ops ==========

    fn compile_string_binop(
        &mut self,
        fb: &mut FunctionBuilder,
        op: &BinaryOp,
        lhs: &Val,
        rhs: &Val,
    ) -> PbResult<Val> {
        // Null-guard both operands for all string operations
        let safe_l = self.null_guard_string(fb, lhs);
        let safe_r = self.null_guard_string(fb, rhs);
        match op {
            BinaryOp::StrConcat => Ok(self.compile_str_concat(fb, &safe_l, &safe_r)),
            BinaryOp::Add => {
                // "+" on strings acts as concat in some contexts
                Ok(self.compile_str_concat(fb, &safe_l, &safe_r))
            }
            BinaryOp::Eq => {
                let cmp = fb.call(&IrType::I32, "strcmp", &[safe_l, safe_r]);
                let zero = fb.const_i32(0);
                Ok(fb.icmp("eq", &cmp, &zero))
            }
            BinaryOp::Neq => {
                let cmp = fb.call(&IrType::I32, "strcmp", &[safe_l, safe_r]);
                let zero = fb.const_i32(0);
                Ok(fb.icmp("ne", &cmp, &zero))
            }
            BinaryOp::Lt => {
                let cmp = fb.call(&IrType::I32, "strcmp", &[safe_l, safe_r]);
                let zero = fb.const_i32(0);
                Ok(fb.icmp("slt", &cmp, &zero))
            }
            BinaryOp::Gt => {
                let cmp = fb.call(&IrType::I32, "strcmp", &[safe_l, safe_r]);
                let zero = fb.const_i32(0);
                Ok(fb.icmp("sgt", &cmp, &zero))
            }
            BinaryOp::Lte => {
                let cmp = fb.call(&IrType::I32, "strcmp", &[safe_l, safe_r]);
                let zero = fb.const_i32(0);
                Ok(fb.icmp("sle", &cmp, &zero))
            }
            BinaryOp::Gte => {
                let cmp = fb.call(&IrType::I32, "strcmp", &[safe_l, safe_r]);
                let zero = fb.const_i32(0);
                Ok(fb.icmp("sge", &cmp, &zero))
            }
            _ => Ok(fb.const_i32(0)), // Other ops not meaningful on strings
        }
    }

    fn compile_str_concat(&mut self, fb: &mut FunctionBuilder, a: &Val, b: &Val) -> Val {
        // Use null-safe runtime concat (handles null pointers from uninitialized strings)
        fb.call(&IrType::Ptr, "pb_str_concat", &[a.clone(), b.clone()])
    }

    // ========== Boolean conversion ==========

    fn compile_expr_as_bool(&mut self, fb: &mut FunctionBuilder, expr: &Expr) -> PbResult<Val> {
        let val = self.compile_expr(fb, expr)?;
        if val.ty == IrType::I1 {
            return Ok(val);
        }
        if val.ty == IrType::Ptr {
            // String: non-empty = true (check first byte != 0)
            let safe = self.null_guard_string(fb, &val);
            let first_byte = fb.load(&IrType::I8, &safe);
            let zero = fb.const_i8(0);
            Ok(fb.icmp("ne", &first_byte, &zero))
        } else if val.ty.is_int() {
            let zero = Val::new(val.ty.zero_literal(), val.ty.clone());
            Ok(fb.icmp("ne", &val, &zero))
        } else if val.ty.is_float() {
            let zero = Val::new(val.ty.zero_literal(), val.ty.clone());
            Ok(fb.fcmp("one", &val, &zero))
        } else {
            Ok(fb.const_i1(false))
        }
    }

    // ========== Comparison helpers ==========

    fn string_compare(&self, fb: &mut FunctionBuilder, pred: &str, a: &Val, b: &Val) -> Val {
        // Null-guard both operands: null → empty string (prevents strcmp crash)
        let safe_a = self.null_guard_string(fb, a);
        let safe_b = self.null_guard_string(fb, b);
        let cmp = fb.call(&IrType::I32, "strcmp", &[safe_a, safe_b]);
        let zero = fb.const_i32(0);
        fb.icmp(pred, &cmp, &zero)
    }

    /// Replace null string pointer with empty string constant. No-op for non-pointer types.
    fn null_guard_string(&self, fb: &mut FunctionBuilder, val: &Val) -> Val {
        if val.ty != IrType::Ptr {
            return val.clone();
        }
        let is_null = fb.icmp("eq", val, &Val::new("null".to_string(), IrType::Ptr));
        let empty = Val::new(self.empty_string_name.clone(), IrType::Ptr);
        fb.select(&is_null, &empty, val)
    }

    fn compare_eq(&self, fb: &mut FunctionBuilder, a: &Val, b: &Val) -> Val {
        if a.ty == IrType::Ptr || b.ty == IrType::Ptr {
            self.string_compare(fb, "eq", a, b)
        } else if a.ty.is_int() && b.ty.is_int() {
            let (l, r) = self.promote_ints(fb, a, b);
            fb.icmp("eq", &l, &r)
        } else {
            let l = self.to_f64(fb, a);
            let r = self.to_f64(fb, b);
            fb.fcmp("oeq", &l, &r)
        }
    }

    fn compare_ne(&self, fb: &mut FunctionBuilder, a: &Val, b: &Val) -> Val {
        if a.ty == IrType::Ptr || b.ty == IrType::Ptr {
            self.string_compare(fb, "ne", a, b)
        } else if a.ty.is_int() && b.ty.is_int() {
            let (l, r) = self.promote_ints(fb, a, b);
            fb.icmp("ne", &l, &r)
        } else {
            let l = self.to_f64(fb, a);
            let r = self.to_f64(fb, b);
            fb.fcmp("one", &l, &r)
        }
    }

    fn compare_lt(&self, fb: &mut FunctionBuilder, a: &Val, b: &Val) -> Val {
        if a.ty == IrType::Ptr || b.ty == IrType::Ptr {
            self.string_compare(fb, "slt", a, b)
        } else if a.ty.is_int() && b.ty.is_int() {
            let (l, r) = self.promote_ints(fb, a, b);
            fb.icmp("slt", &l, &r)
        } else {
            let l = self.to_f64(fb, a);
            let r = self.to_f64(fb, b);
            fb.fcmp("olt", &l, &r)
        }
    }

    fn compare_gt(&self, fb: &mut FunctionBuilder, a: &Val, b: &Val) -> Val {
        if a.ty == IrType::Ptr || b.ty == IrType::Ptr {
            self.string_compare(fb, "sgt", a, b)
        } else if a.ty.is_int() && b.ty.is_int() {
            let (l, r) = self.promote_ints(fb, a, b);
            fb.icmp("sgt", &l, &r)
        } else {
            let l = self.to_f64(fb, a);
            let r = self.to_f64(fb, b);
            fb.fcmp("ogt", &l, &r)
        }
    }

    fn compare_le(&self, fb: &mut FunctionBuilder, a: &Val, b: &Val) -> Val {
        if a.ty == IrType::Ptr || b.ty == IrType::Ptr {
            self.string_compare(fb, "sle", a, b)
        } else if a.ty.is_int() && b.ty.is_int() {
            let (l, r) = self.promote_ints(fb, a, b);
            fb.icmp("sle", &l, &r)
        } else {
            let l = self.to_f64(fb, a);
            let r = self.to_f64(fb, b);
            fb.fcmp("ole", &l, &r)
        }
    }

    fn compare_ge(&self, fb: &mut FunctionBuilder, a: &Val, b: &Val) -> Val {
        if a.ty == IrType::Ptr || b.ty == IrType::Ptr {
            self.string_compare(fb, "sge", a, b)
        } else if a.ty.is_int() && b.ty.is_int() {
            let (l, r) = self.promote_ints(fb, a, b);
            fb.icmp("sge", &l, &r)
        } else {
            let l = self.to_f64(fb, a);
            let r = self.to_f64(fb, b);
            fb.fcmp("oge", &l, &r)
        }
    }

    /// Wrap a malloc'd C string buffer into a BSTR and free the temp buffer.
    fn bstr_from_buf(&mut self, fb: &mut FunctionBuilder, buf: &Val) -> Val {
        let len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(buf));
        let bstr = fb.call(&IrType::Ptr, "pb_bstr_alloc", &[buf.clone(), len]);
        fb.call_void("free", std::slice::from_ref(buf));
        bstr
    }

    // ========== Type conversion helpers ==========

    fn to_i32(&self, fb: &mut FunctionBuilder, val: &Val) -> Val {
        match &val.ty {
            IrType::I32 => val.clone(),
            IrType::I1 => fb.zext(val, &IrType::I32),
            IrType::I16 => fb.sext(val, &IrType::I32),
            IrType::I64 => fb.trunc(val, &IrType::I32),
            IrType::Double | IrType::Float => fb.fptosi(val, &IrType::I32),
            _ => val.clone(),
        }
    }

    fn to_f64(&self, fb: &mut FunctionBuilder, val: &Val) -> Val {
        match &val.ty {
            IrType::Double => val.clone(),
            IrType::Float => fb.fpext(val, &IrType::Double),
            IrType::I1 | IrType::I16 | IrType::I32 | IrType::I64 => fb.sitofp(val, &IrType::Double),
            _ => val.clone(),
        }
    }

    fn val_to_string(&mut self, fb: &mut FunctionBuilder, val: &Val) -> Val {
        if val.ty == IrType::Ptr {
            val.clone()
        } else {
            self.num_to_string(fb, val)
        }
    }

    fn promote_ints(&self, fb: &mut FunctionBuilder, a: &Val, b: &Val) -> (Val, Val) {
        let aw = a.ty.bit_width();
        let bw = b.ty.bit_width();
        if aw == bw {
            (a.clone(), b.clone())
        } else if aw > bw {
            let b_ext = fb.sext(b, &a.ty);
            (a.clone(), b_ext)
        } else {
            let a_ext = fb.sext(a, &b.ty);
            (a_ext, b.clone())
        }
    }

    fn convert_value(
        &self,
        fb: &mut FunctionBuilder,
        val: &Val,
        target_ir: &IrType,
        target_pb: &PbType,
    ) -> Val {
        if val.ty == *target_ir {
            return val.clone();
        }
        // Ptr→Ptr: no conversion needed (string to string)
        if val.ty == IrType::Ptr && *target_ir == IrType::Ptr {
            return val.clone();
        }
        if Self::is_float_pb(target_pb) {
            if val.ty.is_float() {
                if val.ty.bit_width() < target_ir.bit_width() {
                    fb.fpext(val, target_ir)
                } else {
                    fb.fptrunc(val, target_ir)
                }
            } else {
                fb.sitofp(val, target_ir)
            }
        } else if target_ir.is_int() {
            if val.ty.is_int() {
                if val.ty.bit_width() < target_ir.bit_width() {
                    fb.sext(val, target_ir)
                } else {
                    fb.trunc(val, target_ir)
                }
            } else {
                fb.fptosi(val, target_ir)
            }
        } else {
            val.clone()
        }
    }

    // ========== Builtin functions ==========

    /// Try to compile a builtin function call. Returns None if not a builtin.
    fn compile_builtin(
        &mut self,
        fb: &mut FunctionBuilder,
        name: &str,
        args: &[Expr],
    ) -> Option<PbResult<Val>> {
        match name {
            "ABS" => Some(self.builtin_abs(fb, args)),
            "SGN" => Some(self.builtin_sgn(fb, args)),
            "MIN" => Some(self.builtin_min_max(fb, args, false)),
            "MAX" => Some(self.builtin_min_max(fb, args, true)),
            "INT" => Some(self.builtin_int(fb, args)),
            "FIX" => Some(self.builtin_fix(fb, args)),
            "CEIL" => Some(self.builtin_ceil(fb, args)),
            "SQR" => Some(self.builtin_unary_math(fb, args, "llvm.sqrt.f64")),
            "LOG" => Some(self.builtin_unary_math(fb, args, "llvm.log.f64")),
            "EXP" => Some(self.builtin_unary_math(fb, args, "llvm.exp.f64")),
            "SIN" => Some(self.builtin_unary_math(fb, args, "llvm.sin.f64")),
            "COS" => Some(self.builtin_unary_math(fb, args, "llvm.cos.f64")),
            "TAN" => Some(self.builtin_unary_math(fb, args, "tan")),
            "ATN" => Some(self.builtin_unary_math(fb, args, "atan")),
            "CINT" | "CLNG" | "CDWD" => Some(self.builtin_to_i32(fb, args)),
            "CDBL" | "CVD" => Some(self.builtin_to_f64(fb, args)),
            "CSNG" | "CVS" => Some(self.builtin_to_f32(fb, args)),
            "RND" => Some(self.builtin_rnd(fb, args)),
            "ROUND" => Some(self.builtin_round(fb, args)),
            // String builtins
            "LEN" => Some(self.builtin_len(fb, args)),
            "CHR" => Some(self.builtin_chr(fb, args)),
            "ASC" => Some(self.builtin_asc(fb, args)),
            "STR" => Some(self.builtin_str(fb, args)),
            "VAL" => Some(self.builtin_val(fb, args)),
            "LEFT" => Some(self.builtin_left(fb, args)),
            "RIGHT" => Some(self.builtin_right(fb, args)),
            "MID" => Some(self.builtin_mid(fb, args)),
            "INSTR" => Some(self.builtin_instr(fb, args)),
            "UCASE" => Some(self.builtin_ucase(fb, args)),
            "LCASE" => Some(self.builtin_lcase(fb, args)),
            "TRIM" => Some(self.builtin_trim(fb, args)),
            "LTRIM" => Some(self.builtin_ltrim(fb, args)),
            "RTRIM" => Some(self.builtin_rtrim(fb, args)),
            "SPACE" => Some(self.builtin_space(fb, args)),
            "STRING" => Some(self.builtin_string_fn(fb, args)),
            // PB runtime builtins
            "FORMAT" => Some(self.builtin_format(fb, args)),
            "PARSE" => Some(self.builtin_parse(fb, args)),
            "PARSECOUNT" => Some(self.builtin_parsecount(fb, args)),
            "REMOVE" => Some(self.builtin_remove(fb, args)),
            "USING" => Some(self.builtin_using(fb, args)),
            // File I/O builtins
            "FREEFILE" => Some(Ok(fb.call(&IrType::I32, "pb_freefile", &[]))),
            "EOF" => {
                let filenum = self.compile_expr(fb, &args[0]);
                Some(filenum.map(|v| {
                    let v32 = self.to_i32(fb, &v);
                    fb.call(&IrType::I32, "pb_eof", &[v32])
                }))
            }
            // System builtins
            "TIMER" => {
                let null = Val::new("null".to_string(), IrType::Ptr);
                Some(Ok(fb.call(&IrType::I32, "time", &[null])))
            }
            "COMMAND" => {
                let cmd = if self.module.is_32bit() {
                    fb.call_stdcall(&IrType::Ptr, "GetCommandLineA", &[])
                } else {
                    fb.call(&IrType::Ptr, "GetCommandLineA", &[])
                };
                let len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&cmd));
                Some(Ok(fb.call(&IrType::Ptr, "pb_bstr_alloc", &[cmd, len])))
            }
            "EXIST" => {
                let path = self.compile_expr(fb, &args[0]);
                Some(path.map(|p| {
                    let zero = fb.const_i32(0);
                    let result = fb.call(&IrType::I32, "_access", &[p, zero.clone()]);
                    // _access returns 0 if file exists, -1 if not
                    // PB EXIST returns -1 (true) if exists, 0 if not
                    let exists = fb.icmp("eq", &result, &zero);
                    let neg_one = fb.const_i32(-1);
                    let zero2 = fb.const_i32(0);
                    fb.select(&exists, &neg_one, &zero2)
                }))
            }
            "SIZEOF" => {
                // SIZEOF(var) — return size in bytes
                // For ASCIIZ*N / FixedString(N): returns N
                // For TYPE variables: returns struct size
                // For scalar types: returns type size
                if let Some(arg) = args.first() {
                    let size = match arg {
                        Expr::Variable(vname) => {
                            let norm = normalize_name(vname);
                            if let Some(info) = self.symbols.lookup(&norm) {
                                match &info.pb_type {
                                    PbType::FixedString(n) => *n as i32,
                                    PbType::Long | PbType::Dword | PbType::Single => 4,
                                    PbType::Integer | PbType::Word => 2,
                                    PbType::Byte => 1,
                                    PbType::Double | PbType::Ext | PbType::Cur | PbType::Quad => 8,
                                    PbType::String => 4, // pointer size (32-bit)
                                    PbType::UserDefined(tn) => {
                                        let tn = normalize_name(tn);
                                        self.type_layouts
                                            .get(&tn)
                                            .map(|l| {
                                                l.fields
                                                    .iter()
                                                    .map(|f| match &f.pb_type {
                                                        PbType::Long
                                                        | PbType::Dword
                                                        | PbType::Single
                                                        | PbType::String => 4,
                                                        PbType::Integer | PbType::Word => 2,
                                                        PbType::Byte => 1,
                                                        PbType::Double
                                                        | PbType::Ext
                                                        | PbType::Cur
                                                        | PbType::Quad => 8,
                                                        PbType::FixedString(n) => *n as i32,
                                                        _ => 4,
                                                    })
                                                    .sum::<i32>()
                                            })
                                            .unwrap_or(0)
                                    }
                                    _ => 4,
                                }
                            } else {
                                0
                            }
                        }
                        _ => 0,
                    };
                    Some(Ok(fb.const_i32(size)))
                } else {
                    Some(Ok(fb.const_i32(0)))
                }
            }
            "ENVIRON" => {
                let var_name = self.compile_expr(fb, &args[0]);
                Some(var_name.map(|v| fb.call(&IrType::Ptr, "pb_environ", &[v])))
            }
            "EXE" => {
                // EXE.PATH$ and EXE.NAME$ — handled via the TypeMember path
                // This shouldn't normally be reached but as fallback return empty
                Some(Ok(fb.call(&IrType::Ptr, "pb_exe_path", &[])))
            }
            "STRPTR" => {
                // STRPTR(s$) — returns pointer to string data as integer
                let s = self.compile_expr(fb, &args[0]);
                Some(s.map(|s| {
                    // The string value IS the pointer to char data
                    fb.ptrtoint(&s)
                }))
            }
            "VARPTR" => {
                // VARPTR(v) — returns address of variable as integer
                match &args[0] {
                    Expr::Variable(orig_name) => {
                        let vname = normalize_name(orig_name);
                        let ptr_name = self.ensure_variable_ptr(fb, &vname, orig_name);
                        let ptr = Val::new(ptr_name, IrType::Ptr);
                        Some(Ok(fb.ptrtoint(&ptr)))
                    }
                    Expr::ArrayAccess(arr_name, indices) => {
                        let name = normalize_name(arr_name);
                        if let Some(arr_info) = self.symbols.lookup_array(&name).cloned() {
                            let mut compiled_indices = Vec::new();
                            for idx in indices {
                                match self.compile_expr(fb, idx) {
                                    Ok(v) => compiled_indices.push(v),
                                    Err(e) => return Some(Err(e)),
                                }
                            }
                            let flat_idx =
                                self.compute_flat_index(fb, &compiled_indices, &arr_info);
                            let base = Val::new(arr_info.ptr_name.clone(), IrType::Ptr);
                            let elem_ptr = fb.gep_array(&arr_info.array_ir_type, &base, &flat_idx);
                            Some(Ok(fb.ptrtoint(&elem_ptr)))
                        } else {
                            Some(Ok(fb.const_i32(0)))
                        }
                    }
                    _ => {
                        // Expression: allocate temp and return its address
                        match self.compile_expr(fb, &args[0]) {
                            Ok(val) => {
                                let tmp = fb.alloca(&val.ty);
                                fb.store(&val, &tmp);
                                Some(Ok(fb.ptrtoint(&tmp)))
                            }
                            Err(e) => Some(Err(e)),
                        }
                    }
                }
            }
            "RGB" => {
                if args.len() >= 3 {
                    let r = self.compile_expr(fb, &args[0]);
                    Some(r.and_then(|r| {
                        let g = self.compile_expr(fb, &args[1])?;
                        let b = self.compile_expr(fb, &args[2])?;
                        let r32 = self.to_i32(fb, &r);
                        let g32 = self.to_i32(fb, &g);
                        let b32 = self.to_i32(fb, &b);
                        // RGB = R | (G << 8) | (B << 16)
                        let eight = fb.const_i32(8);
                        let sixteen = fb.const_i32(16);
                        let g_shifted = fb.shl(&g32, &eight);
                        let b_shifted = fb.shl(&b32, &sixteen);
                        let rg = fb.or(&r32, &g_shifted);
                        Ok(fb.or(&rg, &b_shifted))
                    }))
                } else {
                    Some(Ok(fb.const_i32(0)))
                }
            }
            "LOBYT" | "LOWRD" => {
                let v = self.compile_expr(fb, &args[0]);
                Some(v.map(|v| {
                    let v32 = self.to_i32(fb, &v);
                    let mask = fb.const_i32(0xFF);
                    fb.and(&v32, &mask)
                }))
            }
            "HIBYT" | "HIWRD" => {
                let v = self.compile_expr(fb, &args[0]);
                Some(v.map(|v| {
                    let v32 = self.to_i32(fb, &v);
                    let eight = fb.const_i32(8);
                    let shifted = fb.lshr(&v32, &eight);
                    let mask = fb.const_i32(0xFF);
                    fb.and(&shifted, &mask)
                }))
            }
            "MAKDWD" | "MAK" => {
                if args.len() >= 2 {
                    let lo = self.compile_expr(fb, &args[0]);
                    Some(lo.and_then(|lo| {
                        let hi = self.compile_expr(fb, &args[1])?;
                        let lo32 = self.to_i32(fb, &lo);
                        let hi32 = self.to_i32(fb, &hi);
                        let sixteen = fb.const_i32(16);
                        let hi_shifted = fb.shl(&hi32, &sixteen);
                        Ok(fb.or(&lo32, &hi_shifted))
                    }))
                } else {
                    Some(Ok(fb.const_i32(0)))
                }
            }
            _ => None,
        }
    }

    fn builtin_abs(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        if val.ty.is_float() {
            let f64_val = self.to_f64(fb, &val);
            Ok(fb.call(&IrType::Double, "llvm.fabs.f64", &[f64_val]))
        } else {
            let i32_val = self.to_i32(fb, &val);
            let zero = fb.const_i32(0);
            let is_neg = fb.icmp("slt", &i32_val, &zero);
            let neg_val = fb.neg(&i32_val);
            Ok(fb.select(&is_neg, &neg_val, &i32_val))
        }
    }

    fn builtin_sgn(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        let pos_one = fb.const_i32(1);
        let neg_one = fb.const_i32(-1);
        let zero_i32 = fb.const_i32(0);
        if val.ty.is_float() {
            let f64_val = self.to_f64(fb, &val);
            let zero_f = fb.const_f64(0.0);
            let is_pos = fb.fcmp("ogt", &f64_val, &zero_f);
            let is_neg = fb.fcmp("olt", &f64_val, &zero_f);
            let tmp = fb.select(&is_neg, &neg_one, &zero_i32);
            Ok(fb.select(&is_pos, &pos_one, &tmp))
        } else {
            let i32_val = self.to_i32(fb, &val);
            let is_pos = fb.icmp("sgt", &i32_val, &zero_i32);
            let is_neg = fb.icmp("slt", &i32_val, &zero_i32);
            let tmp = fb.select(&is_neg, &neg_one, &zero_i32);
            Ok(fb.select(&is_pos, &pos_one, &tmp))
        }
    }

    fn builtin_min_max(
        &mut self,
        fb: &mut FunctionBuilder,
        args: &[Expr],
        is_max: bool,
    ) -> PbResult<Val> {
        let a = self.compile_expr(fb, &args[0])?;
        let b = self.compile_expr(fb, &args[1])?;
        if a.ty.is_float() || b.ty.is_float() {
            let fa = self.to_f64(fb, &a);
            let fb_val = self.to_f64(fb, &b);
            let cmp = if is_max {
                fb.fcmp("ogt", &fa, &fb_val)
            } else {
                fb.fcmp("olt", &fa, &fb_val)
            };
            Ok(fb.select(&cmp, &fa, &fb_val))
        } else {
            let (ia, ib) = self.promote_ints(fb, &a, &b);
            let cmp = if is_max {
                fb.icmp("sgt", &ia, &ib)
            } else {
                fb.icmp("slt", &ia, &ib)
            };
            Ok(fb.select(&cmp, &ia, &ib))
        }
    }

    fn builtin_int(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        if val.ty.is_float() {
            let f64_val = self.to_f64(fb, &val);
            let floored = fb.call(&IrType::Double, "llvm.floor.f64", &[f64_val]);
            Ok(fb.fptosi(&floored, &IrType::I32))
        } else {
            Ok(self.to_i32(fb, &val))
        }
    }

    fn builtin_fix(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        if val.ty.is_float() {
            // fptosi truncates toward zero (same as FIX)
            Ok(fb.fptosi(&val, &IrType::I32))
        } else {
            Ok(self.to_i32(fb, &val))
        }
    }

    fn builtin_ceil(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        let f64_val = self.to_f64(fb, &val);
        let ceiled = fb.call(&IrType::Double, "llvm.ceil.f64", &[f64_val]);
        Ok(fb.fptosi(&ceiled, &IrType::I32))
    }

    fn builtin_unary_math(
        &mut self,
        fb: &mut FunctionBuilder,
        args: &[Expr],
        func_name: &str,
    ) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        let f64_val = self.to_f64(fb, &val);
        Ok(fb.call(&IrType::Double, func_name, &[f64_val]))
    }

    fn builtin_to_i32(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        Ok(self.to_i32(fb, &val))
    }

    fn builtin_to_f64(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        Ok(self.to_f64(fb, &val))
    }

    fn builtin_to_f32(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        let f64_val = self.to_f64(fb, &val);
        Ok(fb.fptrunc(&f64_val, &IrType::Float))
    }

    fn builtin_rnd(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        if args.is_empty() {
            // RND() → random double 0.0 to <1.0
            let raw = fb.call(&IrType::I32, "rand", &[]);
            let raw_f64 = fb.sitofp(&raw, &IrType::Double);
            let max_val = fb.const_f64(32768.0);
            Ok(fb.fdiv(&raw_f64, &max_val))
        } else if args.len() == 1 {
            // RND(n) → random int 0 to n-1
            let n = self.compile_expr(fb, &args[0])?;
            let n_i32 = self.to_i32(fb, &n);
            let raw = fb.call(&IrType::I32, "rand", &[]);
            Ok(fb.srem(&raw, &n_i32))
        } else {
            // RND(lo, hi) → random int lo to hi inclusive
            let lo = self.compile_expr(fb, &args[0])?;
            let hi = self.compile_expr(fb, &args[1])?;
            let lo_i32 = self.to_i32(fb, &lo);
            let hi_i32 = self.to_i32(fb, &hi);
            let raw = fb.call(&IrType::I32, "rand", &[]);
            let range = fb.sub(&hi_i32, &lo_i32);
            let one = fb.const_i32(1);
            let range_plus = fb.add(&range, &one);
            let modded = fb.srem(&raw, &range_plus);
            Ok(fb.add(&lo_i32, &modded))
        }
    }

    fn builtin_round(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        let f64_val = self.to_f64(fb, &val);
        if args.len() >= 2 {
            // ROUND(x, n) → round to n decimal places
            let n = self.compile_expr(fb, &args[1])?;
            let n_f64 = self.to_f64(fb, &n);
            let ten = fb.const_f64(10.0);
            let factor = fb.call(&IrType::Double, "llvm.pow.f64", &[ten, n_f64]);
            let scaled = fb.fmul(&f64_val, &factor);
            let rounded = fb.call(&IrType::Double, "llvm.round.f64", &[scaled]);
            Ok(fb.fdiv(&rounded, &factor))
        } else {
            // ROUND(x) → round to nearest integer
            Ok(fb.call(&IrType::Double, "llvm.round.f64", &[f64_val]))
        }
    }

    // ========== String builtins ==========

    fn builtin_len(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let s = self.compile_expr(fb, &args[0])?;
        Ok(fb.call(&IrType::I32, "strlen", &[s]))
    }

    fn builtin_chr(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let code = self.compile_expr(fb, &args[0])?;
        let code_i32 = self.to_i32(fb, &code);
        // Build 1-byte string, then BSTR alloc
        let tmp = fb.alloca(&IrType::I8);
        let byte = fb.trunc(&code_i32, &IrType::I8);
        fb.store(&byte, &tmp);
        let one = fb.const_i32(1);
        Ok(fb.call(&IrType::Ptr, "pb_bstr_alloc", &[tmp, one]))
    }

    fn builtin_asc(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let s = self.compile_expr(fb, &args[0])?;
        let byte = fb.load(&IrType::I8, &s);
        Ok(fb.zext(&byte, &IrType::I32))
    }

    fn builtin_str(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let val = self.compile_expr(fb, &args[0])?;
        let buf_size = fb.const_i32(32);
        let buf = fb.call(&IrType::Ptr, "malloc", std::slice::from_ref(&buf_size));
        let f64_val = self.to_f64(fb, &val);
        let (fmt_name, _) = self.module.add_string_constant("%g");
        let fmt_ptr = Val::new(fmt_name, IrType::Ptr);
        fb.call_variadic_with_sig(
            &IrType::I32,
            "snprintf",
            &[buf.clone(), buf_size, fmt_ptr, f64_val],
            &[IrType::Ptr, IrType::I32, IrType::Ptr],
        );
        let len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&buf));
        let bstr = fb.call(&IrType::Ptr, "pb_bstr_alloc", &[buf.clone(), len]);
        fb.call_void("free", &[buf]);
        Ok(bstr)
    }

    /// Convert a numeric value to a string (like STR$), for use in & concatenation
    fn num_to_string(&mut self, fb: &mut FunctionBuilder, val: &Val) -> Val {
        let buf_size = fb.const_i32(32);
        let buf = fb.call(&IrType::Ptr, "malloc", std::slice::from_ref(&buf_size));
        let f64_val = self.to_f64(fb, val);
        let (fmt_name, _) = self.module.add_string_constant("%g");
        let fmt_ptr = Val::new(fmt_name, IrType::Ptr);
        fb.call_variadic_with_sig(
            &IrType::I32,
            "snprintf",
            &[buf.clone(), buf_size, fmt_ptr, f64_val],
            &[IrType::Ptr, IrType::I32, IrType::Ptr],
        );
        let len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&buf));
        let bstr = fb.call(&IrType::Ptr, "pb_bstr_alloc", &[buf.clone(), len]);
        fb.call_void("free", &[buf]);
        bstr
    }

    fn builtin_val(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let s = self.compile_expr(fb, &args[0])?;
        Ok(fb.call(&IrType::Double, "atof", &[s]))
    }

    fn builtin_left(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let s = self.compile_expr(fb, &args[0])?;
        let n = self.compile_expr(fb, &args[1])?;
        let n_i32 = self.to_i32(fb, &n);
        // SysAllocStringByteLen copies n bytes from src and null-terminates
        Ok(fb.call(&IrType::Ptr, "pb_bstr_alloc", &[s, n_i32]))
    }

    fn builtin_right(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let s = self.compile_expr(fb, &args[0])?;
        let n = self.compile_expr(fb, &args[1])?;
        let n_i32 = self.to_i32(fb, &n);
        let len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&s));
        let offset = fb.sub(&len, &n_i32);
        let src = fb.gep_byte(&s, &offset);
        Ok(fb.call(&IrType::Ptr, "pb_bstr_alloc", &[src, n_i32]))
    }

    fn builtin_mid(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        // MID$(s, start) or MID$(s, start, length)
        // PB MID$ is 1-based
        let s = self.compile_expr(fb, &args[0])?;
        let start = self.compile_expr(fb, &args[1])?;
        let start_i32 = self.to_i32(fb, &start);
        let one = fb.const_i32(1);
        let offset = fb.sub(&start_i32, &one); // convert to 0-based

        let src = fb.gep_byte(&s, &offset);

        if args.len() >= 3 {
            // MID$(s, start, length)
            let length = self.compile_expr(fb, &args[2])?;
            let len_i32 = self.to_i32(fb, &length);
            Ok(fb.call(&IrType::Ptr, "pb_bstr_alloc", &[src, len_i32]))
        } else {
            // MID$(s, start) — rest of string
            let src_len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&src));
            Ok(fb.call(&IrType::Ptr, "pb_bstr_alloc", &[src, src_len]))
        }
    }

    fn builtin_instr(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        // INSTR(haystack, needle) or INSTR(start, haystack, needle)
        // Returns 1-based position, or 0 if not found
        let (base, search_start, needle) = if args.len() >= 3 {
            let start = self.compile_expr(fb, &args[0])?;
            let start_i32 = self.to_i32(fb, &start);
            let one = fb.const_i32(1);
            let offset = fb.sub(&start_i32, &one); // 1-based to 0-based
            let h = self.compile_expr(fb, &args[1])?;
            let n = self.compile_expr(fb, &args[2])?;
            let h_offset = fb.gep_byte(&h, &offset);
            (h.clone(), h_offset, n)
        } else {
            let h = self.compile_expr(fb, &args[0])?;
            let n = self.compile_expr(fb, &args[1])?;
            (h.clone(), h, n)
        };

        let result_ptr = fb.alloca(&IrType::I32);
        let zero_i32 = fb.const_i32(0);
        fb.store(&zero_i32, &result_ptr);

        let found = fb.call(&IrType::Ptr, "strstr", &[search_start, needle]);
        let null_ptr = fb.const_null_ptr();
        let is_null = fb.icmp("eq", &found, &null_ptr);

        let then_label = fb.next_label("instr.found");
        let merge_label = fb.next_label("instr.merge");

        fb.condbr(&is_null, &merge_label, &then_label);

        fb.label(&then_label);
        let found_int = fb.ptrtoint(&found);
        let base_int = fb.ptrtoint(&base);
        let diff = fb.sub(&found_int, &base_int);
        let one_i32 = fb.const_i32(1);
        let pos = fb.add(&diff, &one_i32); // 1-based relative to original string
        fb.store(&pos, &result_ptr);
        fb.br(&merge_label);

        fb.label(&merge_label);
        Ok(fb.load(&IrType::I32, &result_ptr))
    }

    fn builtin_ucase(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let s = self.compile_expr(fb, &args[0])?;
        let len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&s));
        let one = fb.const_i32(1);
        let buf_size = fb.add(&len, &one);
        let buf = fb.call(&IrType::Ptr, "malloc", &[buf_size]);

        // Loop: for i = 0 to len-1, buf[i] = toupper(s[i])
        let cond_label = fb.next_label("ucase.cond");
        let body_label = fb.next_label("ucase.body");
        let exit_label = fb.next_label("ucase.exit");

        let i_ptr = fb.alloca(&IrType::I32);
        let zero_i32_val = fb.const_i32(0);
        fb.store(&zero_i32_val, &i_ptr);
        fb.br(&cond_label);

        fb.label(&cond_label);
        let i = fb.load(&IrType::I32, &i_ptr);
        let cmp = fb.icmp("slt", &i, &len);
        fb.condbr(&cmp, &body_label, &exit_label);

        fb.label(&body_label);
        let src_ptr = fb.gep_byte(&s, &i);
        let byte = fb.load(&IrType::I8, &src_ptr);
        let byte_i32 = fb.zext(&byte, &IrType::I32);
        let upper = fb.call(&IrType::I32, "toupper", &[byte_i32]);
        let upper_i8 = fb.trunc(&upper, &IrType::I8);
        let dst_ptr = fb.gep_byte(&buf, &i);
        fb.store(&upper_i8, &dst_ptr);
        let next_i = fb.add(&i, &one);
        fb.store(&next_i, &i_ptr);
        fb.br(&cond_label);

        fb.label(&exit_label);
        // Null-terminate
        let i_final = fb.load(&IrType::I32, &i_ptr);
        let null_ptr = fb.gep_byte(&buf, &i_final);
        let zero_byte = fb.const_i8(0);
        fb.store(&zero_byte, &null_ptr);
        Ok(self.bstr_from_buf(fb, &buf))
    }

    fn builtin_lcase(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let s = self.compile_expr(fb, &args[0])?;
        let len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&s));
        let one = fb.const_i32(1);
        let buf_size = fb.add(&len, &one);
        let buf = fb.call(&IrType::Ptr, "malloc", &[buf_size]);

        let cond_label = fb.next_label("lcase.cond");
        let body_label = fb.next_label("lcase.body");
        let exit_label = fb.next_label("lcase.exit");

        let i_ptr = fb.alloca(&IrType::I32);
        let zero_i32_val = fb.const_i32(0);
        fb.store(&zero_i32_val, &i_ptr);
        fb.br(&cond_label);

        fb.label(&cond_label);
        let i = fb.load(&IrType::I32, &i_ptr);
        let cmp = fb.icmp("slt", &i, &len);
        fb.condbr(&cmp, &body_label, &exit_label);

        fb.label(&body_label);
        let src_ptr = fb.gep_byte(&s, &i);
        let byte = fb.load(&IrType::I8, &src_ptr);
        let byte_i32 = fb.zext(&byte, &IrType::I32);
        let lower = fb.call(&IrType::I32, "tolower", &[byte_i32]);
        let lower_i8 = fb.trunc(&lower, &IrType::I8);
        let dst_ptr = fb.gep_byte(&buf, &i);
        fb.store(&lower_i8, &dst_ptr);
        let next_i = fb.add(&i, &one);
        fb.store(&next_i, &i_ptr);
        fb.br(&cond_label);

        fb.label(&exit_label);
        let i_final = fb.load(&IrType::I32, &i_ptr);
        let null_ptr = fb.gep_byte(&buf, &i_final);
        let zero_byte = fb.const_i8(0);
        fb.store(&zero_byte, &null_ptr);
        Ok(self.bstr_from_buf(fb, &buf))
    }

    fn builtin_ltrim(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        // Skip leading spaces, return new string
        let s = self.compile_expr(fb, &args[0])?;

        let cond_label = fb.next_label("ltrim.cond");
        let body_label = fb.next_label("ltrim.body");
        let exit_label = fb.next_label("ltrim.exit");

        let i_ptr = fb.alloca(&IrType::I32);
        let zero_i32_val = fb.const_i32(0);
        fb.store(&zero_i32_val, &i_ptr);
        fb.br(&cond_label);

        fb.label(&cond_label);
        let i = fb.load(&IrType::I32, &i_ptr);
        let cur_ptr = fb.gep_byte(&s, &i);
        let byte = fb.load(&IrType::I8, &cur_ptr);
        let space = fb.const_i8(32); // space character
        let is_space = fb.icmp("eq", &byte, &space);
        let not_null = fb.icmp("ne", &byte, &fb.const_i8(0));
        let cont = fb.and(&is_space, &not_null);
        fb.condbr(&cont, &body_label, &exit_label);

        fb.label(&body_label);
        let one = fb.const_i32(1);
        let next = fb.add(&i, &one);
        fb.store(&next, &i_ptr);
        fb.br(&cond_label);

        fb.label(&exit_label);
        let i_final = fb.load(&IrType::I32, &i_ptr);
        let start = fb.gep_byte(&s, &i_final);
        let rest_len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&start));
        let one = fb.const_i32(1);
        let buf_size = fb.add(&rest_len, &one);
        let buf = fb.call(&IrType::Ptr, "malloc", &[buf_size]);
        fb.call(&IrType::Ptr, "strcpy", &[buf.clone(), start]);
        Ok(self.bstr_from_buf(fb, &buf))
    }

    fn builtin_rtrim(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        // Copy string, then walk backwards removing trailing spaces
        let s = self.compile_expr(fb, &args[0])?;
        let len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&s));
        let one = fb.const_i32(1);
        let buf_size = fb.add(&len, &one);
        let buf = fb.call(&IrType::Ptr, "malloc", &[buf_size]);
        fb.call(&IrType::Ptr, "strcpy", &[buf.clone(), s]);

        // Walk backwards: i = len - 1, while i >= 0 and buf[i] == ' ', i--
        let cond_label = fb.next_label("rtrim.cond");
        let body_label = fb.next_label("rtrim.body");
        let exit_label = fb.next_label("rtrim.exit");

        let i_ptr = fb.alloca(&IrType::I32);
        let len_minus1 = fb.sub(&len, &one);
        fb.store(&len_minus1, &i_ptr);
        fb.br(&cond_label);

        fb.label(&cond_label);
        let i = fb.load(&IrType::I32, &i_ptr);
        let zero_i32_val = fb.const_i32(0);
        let ge_zero = fb.icmp("sge", &i, &zero_i32_val);
        let cur_ptr = fb.gep_byte(&buf, &i);
        let byte = fb.load(&IrType::I8, &cur_ptr);
        let space = fb.const_i8(32);
        let is_space = fb.icmp("eq", &byte, &space);
        let cont = fb.and(&ge_zero, &is_space);
        fb.condbr(&cont, &body_label, &exit_label);

        fb.label(&body_label);
        let next = fb.sub(&i, &one);
        fb.store(&next, &i_ptr);
        fb.br(&cond_label);

        fb.label(&exit_label);
        // Null-terminate at i+1
        let i_final = fb.load(&IrType::I32, &i_ptr);
        let term_pos = fb.add(&i_final, &one);
        let term_ptr = fb.gep_byte(&buf, &term_pos);
        let zero_byte = fb.const_i8(0);
        fb.store(&zero_byte, &term_ptr);
        Ok(self.bstr_from_buf(fb, &buf))
    }

    fn builtin_trim(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        // TRIM$ = LTRIM$(RTRIM$(s))
        // First RTRIM, then LTRIM
        let s = self.compile_expr(fb, &args[0])?;

        // RTRIM part: copy and truncate trailing spaces
        let len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&s));
        let one = fb.const_i32(1);
        let buf_size = fb.add(&len, &one);
        let buf = fb.call(&IrType::Ptr, "malloc", &[buf_size]);
        fb.call(&IrType::Ptr, "strcpy", &[buf.clone(), s]);

        let cond_label = fb.next_label("trim.rcond");
        let body_label = fb.next_label("trim.rbody");
        let exit_label = fb.next_label("trim.rexit");

        let i_ptr = fb.alloca(&IrType::I32);
        let len_minus1 = fb.sub(&len, &one);
        fb.store(&len_minus1, &i_ptr);
        fb.br(&cond_label);

        fb.label(&cond_label);
        let i = fb.load(&IrType::I32, &i_ptr);
        let zero_i32_val = fb.const_i32(0);
        let ge_zero = fb.icmp("sge", &i, &zero_i32_val);
        let cur_ptr = fb.gep_byte(&buf, &i);
        let byte = fb.load(&IrType::I8, &cur_ptr);
        let space = fb.const_i8(32);
        let is_space = fb.icmp("eq", &byte, &space);
        let cont = fb.and(&ge_zero, &is_space);
        fb.condbr(&cont, &body_label, &exit_label);

        fb.label(&body_label);
        let next = fb.sub(&i, &one);
        fb.store(&next, &i_ptr);
        fb.br(&cond_label);

        fb.label(&exit_label);
        let i_final = fb.load(&IrType::I32, &i_ptr);
        let term_pos = fb.add(&i_final, &one);
        let term_ptr = fb.gep_byte(&buf, &term_pos);
        let zero_byte = fb.const_i8(0);
        fb.store(&zero_byte, &term_ptr);

        // LTRIM part: skip leading spaces of buf
        let cond2 = fb.next_label("trim.lcond");
        let body2 = fb.next_label("trim.lbody");
        let exit2 = fb.next_label("trim.lexit");

        let j_ptr = fb.alloca(&IrType::I32);
        fb.store(&zero_i32_val, &j_ptr);
        fb.br(&cond2);

        fb.label(&cond2);
        let j = fb.load(&IrType::I32, &j_ptr);
        let cur_ptr2 = fb.gep_byte(&buf, &j);
        let byte2 = fb.load(&IrType::I8, &cur_ptr2);
        let is_space2 = fb.icmp("eq", &byte2, &space);
        let not_null = fb.icmp("ne", &byte2, &fb.const_i8(0));
        let cont2 = fb.and(&is_space2, &not_null);
        fb.condbr(&cont2, &body2, &exit2);

        fb.label(&body2);
        let next_j = fb.add(&j, &one);
        fb.store(&next_j, &j_ptr);
        fb.br(&cond2);

        fb.label(&exit2);
        let j_final = fb.load(&IrType::I32, &j_ptr);
        let start = fb.gep_byte(&buf, &j_final);
        let rest_len = fb.call(&IrType::I32, "strlen", std::slice::from_ref(&start));
        let result_size = fb.add(&rest_len, &one);
        let result = fb.call(&IrType::Ptr, "malloc", &[result_size]);
        fb.call(&IrType::Ptr, "strcpy", &[result.clone(), start]);
        fb.call_void("free", &[buf]);
        Ok(self.bstr_from_buf(fb, &result))
    }

    fn builtin_space(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let n = self.compile_expr(fb, &args[0])?;
        let n_i32 = self.to_i32(fb, &n);
        let one = fb.const_i32(1);
        let buf_size = fb.add(&n_i32, &one);
        let buf = fb.call(&IrType::Ptr, "malloc", &[buf_size]);

        // Loop: fill with spaces
        let cond_label = fb.next_label("space.cond");
        let body_label = fb.next_label("space.body");
        let exit_label = fb.next_label("space.exit");

        let i_ptr = fb.alloca(&IrType::I32);
        let zero_i32_val = fb.const_i32(0);
        fb.store(&zero_i32_val, &i_ptr);
        fb.br(&cond_label);

        fb.label(&cond_label);
        let i = fb.load(&IrType::I32, &i_ptr);
        let cmp = fb.icmp("slt", &i, &n_i32);
        fb.condbr(&cmp, &body_label, &exit_label);

        fb.label(&body_label);
        let dst = fb.gep_byte(&buf, &i);
        let space = fb.const_i8(32);
        fb.store(&space, &dst);
        let next = fb.add(&i, &one);
        fb.store(&next, &i_ptr);
        fb.br(&cond_label);

        fb.label(&exit_label);
        // Null-terminate
        let null_ptr = fb.gep_byte(&buf, &n_i32);
        let zero_byte = fb.const_i8(0);
        fb.store(&zero_byte, &null_ptr);
        Ok(self.bstr_from_buf(fb, &buf))
    }

    fn builtin_string_fn(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        // STRING$(count, char_code) or STRING$(count, string$)
        let n = self.compile_expr(fb, &args[0])?;
        let n_i32 = self.to_i32(fb, &n);
        let char_val = self.compile_expr(fb, &args[1])?;

        // Get the character byte
        let char_byte = if char_val.ty == IrType::Ptr {
            // STRING$(n, "X") — use first char
            fb.load(&IrType::I8, &char_val)
        } else {
            // STRING$(n, code) — use as char code
            let code_i32 = self.to_i32(fb, &char_val);
            fb.trunc(&code_i32, &IrType::I8)
        };

        let one = fb.const_i32(1);
        let buf_size = fb.add(&n_i32, &one);
        let buf = fb.call(&IrType::Ptr, "malloc", &[buf_size]);

        // Loop: fill with character
        let cond_label = fb.next_label("stringfn.cond");
        let body_label = fb.next_label("stringfn.body");
        let exit_label = fb.next_label("stringfn.exit");

        let i_ptr = fb.alloca(&IrType::I32);
        let zero_i32_val = fb.const_i32(0);
        fb.store(&zero_i32_val, &i_ptr);
        fb.br(&cond_label);

        fb.label(&cond_label);
        let i = fb.load(&IrType::I32, &i_ptr);
        let cmp = fb.icmp("slt", &i, &n_i32);
        fb.condbr(&cmp, &body_label, &exit_label);

        fb.label(&body_label);
        let dst = fb.gep_byte(&buf, &i);
        fb.store(&char_byte, &dst);
        let next = fb.add(&i, &one);
        fb.store(&next, &i_ptr);
        fb.br(&cond_label);

        fb.label(&exit_label);
        let null_ptr = fb.gep_byte(&buf, &n_i32);
        let zero_byte = fb.const_i8(0);
        fb.store(&zero_byte, &null_ptr);
        Ok(self.bstr_from_buf(fb, &buf))
    }

    // ========== PB Runtime builtins ==========

    fn builtin_format(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        // FORMAT$(number, format_string)
        let val = self.compile_expr(fb, &args[0])?;
        let f64_val = self.to_f64(fb, &val);
        let fmt = if args.len() >= 2 {
            self.compile_expr(fb, &args[1])?
        } else {
            Val::new(self.empty_string_name.clone(), IrType::Ptr)
        };
        Ok(fb.call(&IrType::Ptr, "pb_format", &[f64_val, fmt]))
    }

    fn builtin_parse(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        // PARSE$(string, delimiter, index) or PARSE$(string, delimiter) for count
        let s = self.compile_expr(fb, &args[0])?;
        let delim = self.compile_expr(fb, &args[1])?;
        let index = if args.len() >= 3 {
            let idx = self.compile_expr(fb, &args[2])?;
            self.to_i32(fb, &idx)
        } else {
            fb.const_i32(0) // 0 means "return count"
        };
        Ok(fb.call(&IrType::Ptr, "pb_parse", &[s, delim, index]))
    }

    fn builtin_parsecount(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let s = self.compile_expr(fb, &args[0])?;
        let delim = self.compile_expr(fb, &args[1])?;
        Ok(fb.call(&IrType::I32, "pb_parsecount", &[s, delim]))
    }

    fn builtin_remove(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let s = self.compile_expr(fb, &args[0])?;
        let chars = self.compile_expr(fb, &args[1])?;
        Ok(fb.call(&IrType::Ptr, "pb_remove", &[s, chars]))
    }

    fn builtin_using(&mut self, fb: &mut FunctionBuilder, args: &[Expr]) -> PbResult<Val> {
        let fmt = self.compile_expr(fb, &args[0])?;
        let val = self.compile_expr(fb, &args[1])?;
        let f64_val = self.to_f64(fb, &val);
        Ok(fb.call(&IrType::Ptr, "pb_using", &[fmt, f64_val]))
    }

    // ========== Array helpers ==========

    /// Evaluate a constant expression at compile time (for array bounds).
    fn eval_const_expr(expr: &Expr) -> Result<i64, &'static str> {
        match expr {
            Expr::IntegerLit(n) => Ok(*n),
            Expr::FloatLit(f) => Ok(*f as i64),
            Expr::Negate(inner) | Expr::UnaryOp(UnaryOp::Neg, inner) => {
                Ok(-Self::eval_const_expr(inner)?)
            }
            Expr::BinaryOp(op, left, right) => {
                let l = Self::eval_const_expr(left)?;
                let r = Self::eval_const_expr(right)?;
                match op {
                    BinaryOp::Add => Ok(l + r),
                    BinaryOp::Sub => Ok(l - r),
                    BinaryOp::Mul => Ok(l * r),
                    _ => Err("unsupported op in const expr"),
                }
            }
            _ => Err("non-constant expression in array bounds"),
        }
    }

    /// Convert any value to i64 (for GEP indices).
    #[allow(dead_code)]
    fn to_i64(&self, fb: &mut FunctionBuilder, val: &Val) -> Val {
        match &val.ty {
            IrType::I64 => val.clone(),
            IrType::I1 => fb.zext(val, &IrType::I64),
            IrType::I16 | IrType::I32 => fb.sext(val, &IrType::I64),
            IrType::Double | IrType::Float => fb.fptosi(val, &IrType::I64),
            _ => val.clone(),
        }
    }

    /// Compute flat index from multi-dimensional indices.
    /// Strides are computed at compile time from dimension counts.
    fn compute_flat_index(
        &self,
        fb: &mut FunctionBuilder,
        indices: &[Val],
        arr_info: &ArrayInfo,
    ) -> Val {
        let ndims = arr_info.dims.len();

        // Compute strides: stride[last] = 1, stride[i] = stride[i+1] * count[i+1]
        let mut strides = vec![1i64; ndims];
        for i in (0..ndims.saturating_sub(1)).rev() {
            strides[i] = strides[i + 1] * arr_info.dims[i + 1].1;
        }

        let mut result = fb.const_i32(0);
        for (i, idx) in indices.iter().enumerate() {
            if i >= ndims {
                break;
            }
            let idx_i32 = self.to_i32(fb, idx);
            let lower = arr_info.dims[i].0;
            let adjusted = if lower != 0 {
                let lower_val = fb.const_i32(lower as i32);
                fb.sub(&idx_i32, &lower_val)
            } else {
                idx_i32
            };
            if strides[i] != 1 {
                let stride_val = fb.const_i32(strides[i] as i32);
                let contribution = fb.mul(&adjusted, &stride_val);
                result = fb.add(&result, &contribution);
            } else {
                result = fb.add(&result, &adjusted);
            }
        }
        result
    }

    /// Declare a global array and register it in the symbol table.
    fn declare_global_array(&mut self, name: &str, pb_type: &PbType, bounds: &[DimBound]) {
        let norm_name = normalize_name(name);
        // Already declared — skip
        if self.symbols.lookup_array(&norm_name).is_some() {
            return;
        }
        let elem_ir = Self::ir_type_for(pb_type);
        let mut dims = Vec::new();
        let mut total = 1usize;
        for bound in bounds {
            let lower = Self::eval_const_expr(&bound.lower).unwrap_or(0);
            let upper = Self::eval_const_expr(&bound.upper).unwrap_or(0);
            let count = (upper - lower + 1) as usize;
            dims.push((lower, count as i64));
            total *= count;
        }
        let array_ir = IrType::Array(total, Box::new(elem_ir.clone()));

        if self.session_mode {
            // Defer: collect into session_fields for struct emission later
            if !self.session_fields.iter().any(|f| f.name == norm_name) {
                self.session_fields.push(SessionFieldInfo {
                    name: norm_name.clone(),
                    ir_type: array_ir,
                    pb_type: pb_type.clone(),
                    is_array: true,
                    elem_ir_type: Some(elem_ir),
                    array_dims: Some(dims),
                    total_elements: Some(total),
                });
            }
            self.pending_global_arrays.remove(&norm_name);
        } else {
            self.module
                .add_global(&norm_name, &array_ir, "zeroinitializer");
            self.symbols.insert_global_array(
                norm_name.clone(),
                ArrayInfo {
                    ptr_name: format!("@{}", norm_name),
                    array_ir_type: array_ir,
                    elem_ir_type: elem_ir,
                    pb_type: pb_type.clone(),
                    dims,
                    total_elements: total,
                },
            );
            self.pending_global_arrays.remove(&norm_name);
        }
    }

    // ========== Session Struct ==========

    /// Scan a function/sub body for DIM statements that resolve pending global arrays,
    /// and for GLOBAL scalar declarations (parsed as DimScope::Global).
    /// This handles the common PB pattern where GLOBAL arr() is in globals.inc but DIM arr(N)
    /// is inside a SetupDims() function body, as well as GLOBAL scalars declared inside SUBs.
    fn scan_body_for_global_dims(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            match stmt {
                Statement::Dim(dim) => {
                    let name = normalize_name(&dim.name);
                    if !dim.bounds.is_empty() && self.pending_global_arrays.contains_key(&name) {
                        let pb_type = self.pending_global_arrays.get(&name).unwrap().clone();
                        // Compute array layout and push to session_fields
                        self.declare_global_array(&name, &pb_type, &dim.bounds);
                    } else if dim.scope == DimScope::Global
                        && dim.bounds.is_empty()
                        && self.symbols.lookup(&name).is_none()
                    {
                        // GLOBAL scalar declared inside a SUB body (e.g., GLOBAL ForceSync AS LONG in SetupDims)
                        let vd = VarDecl {
                            name: dim.name.clone(),
                            pb_type: dim.pb_type.clone(),
                            is_array: false,
                            line: dim.line,
                        };
                        self.declare_global(&vd);
                    }
                }
                Statement::If(if_stmt) => {
                    self.scan_body_for_global_dims(&if_stmt.then_body);
                    for clause in &if_stmt.elseif_clauses {
                        self.scan_body_for_global_dims(&clause.body);
                    }
                    if !if_stmt.else_body.is_empty() {
                        self.scan_body_for_global_dims(&if_stmt.else_body);
                    }
                }
                Statement::For(for_stmt) => self.scan_body_for_global_dims(&for_stmt.body),
                Statement::DoLoop(do_stmt) => self.scan_body_for_global_dims(&do_stmt.body),
                Statement::WhileWend(w) => self.scan_body_for_global_dims(&w.body),
                Statement::SelectCase(sel) => {
                    for case in &sel.cases {
                        self.scan_body_for_global_dims(&case.body);
                    }
                    if !sel.else_body.is_empty() {
                        self.scan_body_for_global_dims(&sel.else_body);
                    }
                }
                _ => {}
            }
        }
    }

    /// Emit the PBSessionData struct type and global instance from collected session_fields.
    /// Registers each field in the symbol table with a constant GEP expression as its pointer.
    fn emit_session_struct(&mut self) {
        let struct_name = "PBSESSIONDATA";

        // Collect field types
        let field_types: Vec<IrType> = self
            .session_fields
            .iter()
            .map(|f| f.ir_type.clone())
            .collect();

        // Define the struct type: %PBSESSIONDATA = type { ... }
        self.module.define_struct_type(struct_name, &field_types);

        // Emit the global instance with dllexport
        self.module.add_global_exported(
            "pb.session",
            &IrType::Struct(struct_name.to_string()),
            "zeroinitializer",
        );

        // Register each field in the symbol table
        for (idx, field) in self.session_fields.iter().enumerate() {
            let gep_expr = format!(
                "getelementptr inbounds (%{}, ptr @pb.session, i32 0, i32 {})",
                struct_name, idx
            );

            if field.is_array {
                let dims = field.array_dims.clone().unwrap();
                let total = field.total_elements.unwrap();
                let elem_ir = field.elem_ir_type.clone().unwrap();
                self.symbols.insert_global_array(
                    field.name.clone(),
                    ArrayInfo {
                        ptr_name: gep_expr,
                        array_ir_type: field.ir_type.clone(),
                        elem_ir_type: elem_ir,
                        pb_type: field.pb_type.clone(),
                        dims,
                        total_elements: total,
                    },
                );
            } else {
                self.symbols.insert_global_with_ptr(
                    field.name.clone(),
                    gep_expr,
                    field.ir_type.clone(),
                    field.pb_type.clone(),
                );
            }
        }

        eprintln!(
            "[pbcompiler] Session struct: {} fields in %{}",
            self.session_fields.len(),
            struct_name
        );
    }

    /// Emit the GetSession() export function returning a pointer to the session struct.
    fn emit_get_session(&mut self) {
        let mut fb = self
            .module
            .create_function_builder_ex("GetSession", &IrType::Ptr, &[], true);
        fb.ret(&Val::new("@pb.session", IrType::Ptr));
        self.module.add_function_body(fb.finish());
    }

    // ========== Utility ==========

    /// Scan a function/sub body for DIM statements with bounds (array declarations).
    fn scan_body_for_array_names(body: &[Statement], out: &mut HashSet<String>) {
        for stmt in body {
            match stmt {
                Statement::Dim(dim) | Statement::Redim(dim) => {
                    if !dim.bounds.is_empty() {
                        out.insert(normalize_name(&dim.name));
                    }
                }
                Statement::Block(stmts) => Self::scan_body_for_array_names(stmts, out),
                _ => {}
            }
        }
    }

    fn ensure_variable_ptr(
        &mut self,
        fb: &mut FunctionBuilder,
        name: &str,
        original_name: &str,
    ) -> String {
        if let Some(info) = self.symbols.lookup(name) {
            return info.ptr_name.clone();
        }
        // Auto-allocate — use original_name (with type suffix) for type inference
        let pb_type = infer_type_from_name(original_name);
        let ir_type = Self::ir_type_for(&pb_type);
        let ptr = fb.alloca(&ir_type);
        if Self::is_string_pb(&pb_type) {
            fb.store(&Val::new(self.empty_string_name.clone(), IrType::Ptr), &ptr);
        } else {
            fb.store(&Val::new(ir_type.zero_literal(), ir_type.clone()), &ptr);
        }
        let ptr_name = ptr.name.clone();
        self.symbols
            .insert_local(name.to_string(), ptr_name.clone(), ir_type, pb_type);
        ptr_name
    }
}

/// Normalize a PB name: uppercase, strip type suffix.
fn normalize_name(name: &str) -> String {
    let upper = name.to_uppercase();
    let bytes = upper.as_bytes();
    let len = bytes.len();
    if len == 0 {
        return upper;
    }
    if len >= 2 {
        let last2 = &upper[len - 2..];
        if matches!(last2, "##" | "&&" | "%%" | "@@") {
            return upper[..len - 2].to_string();
        }
    }
    let last = bytes[len - 1];
    if matches!(last, b'#' | b'&' | b'%' | b'!' | b'@' | b'$') {
        return upper[..len - 1].to_string();
    }
    upper
}

/// Infer PB type from variable name suffix.
fn infer_type_from_name(name: &str) -> PbType {
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
    } else if name.ends_with('@') {
        PbType::Cur
    } else if name.ends_with('$') {
        PbType::String
    } else {
        PbType::Long
    }
}
