use std::path::Path;
use std::process;
use std::sync::{Arc, atomic::{AtomicBool, Ordering}};
use std::time::Instant;

use pb::error::PbResult;
use pb::lexer::Lexer;
use pb::parser::Parser;
use pb::preprocessor::Preprocessor;
use pbinterp::interpreter::Interpreter;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 3 || (args[1] != "run" && args[1] != "dump") {
        eprintln!("Usage: pbinterp run <file.bas> [--timeout <seconds>] [--json <output.json>] [--filter <pattern>]");
        eprintln!("       pbinterp dump <file.bas>");
        process::exit(2);
    }

    let file_path = &args[2];

    if args[1] == "dump" {
        // Dump preprocessed output with line numbers
        let mut preprocessor = Preprocessor::new();
        let path = Path::new(file_path);
        match preprocessor.process_file(path) {
            Ok(source_lines) => {
                for (i, sl) in source_lines.iter().enumerate() {
                    println!("{:6}: {}", i + 1, sl.text);
                }
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                process::exit(2);
            }
        }
        process::exit(0);
    }

    // Parse optional flags
    let timeout_secs = get_flag_value(&args, "--timeout")
        .and_then(|s| s.parse::<u64>().ok());
    let json_output = get_flag_value(&args, "--json");
    let filter = get_flag_value(&args, "--filter");

    // Set filter as env var so PB test framework can read it via ENVIRON$
    if let Some(ref f) = filter {
        std::env::set_var("PB_TEST_FILTER", f);
        eprintln!("[pbinterp] Filter: {}", f);
    }

    let total_start = Instant::now();

    // Run with optional timeout
    let result = if let Some(secs) = timeout_secs {
        run_with_timeout(file_path, secs)
    } else {
        run_file(file_path)
    };

    let total_elapsed = total_start.elapsed();

    match result {
        Ok((exit_code, stdout_capture)) => {
            eprintln!(
                "[pbinterp] Total execution time: {:.3}s",
                total_elapsed.as_secs_f64()
            );

            // Parse test results from stdout and produce JSON if requested
            if let Some(json_path) = json_output {
                let report = parse_test_output(&stdout_capture, total_elapsed.as_secs_f64());
                match std::fs::write(&json_path, &report) {
                    Ok(_) => eprintln!("[pbinterp] Test results written to {}", json_path),
                    Err(e) => eprintln!("[pbinterp] Warning: failed to write {}: {}", json_path, e),
                }
            }

            process::exit(exit_code);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            eprintln!(
                "[pbinterp] Failed after {:.3}s",
                total_elapsed.as_secs_f64()
            );

            // On error, still write JSON if requested
            if let Some(json_path) = json_output {
                let error_report = format!(
                    r#"{{"status":"error","error":"{}","duration_s":{:.3}}}"#,
                    e.to_string().replace('"', "\\\""),
                    total_elapsed.as_secs_f64()
                );
                let _ = std::fs::write(&json_path, &error_report);
            }

            process::exit(2);
        }
    }
}

/// Get the value following a flag like "--timeout 30"
fn get_flag_value(args: &[String], flag: &str) -> Option<String> {
    args.iter()
        .position(|a| a == flag)
        .and_then(|pos| args.get(pos + 1).cloned())
}

fn run_file(path: &str) -> PbResult<(i32, String)> {
    let path = Path::new(path);

    // Phase 1: Preprocess (resolve #INCLUDE, %CONSTANTS, strip directives)
    let t0 = Instant::now();
    let mut preprocessor = Preprocessor::new();
    let source_lines = preprocessor.process_file(path)?;

    eprintln!(
        "[pbinterp] Preprocessed {} lines from {} files ({:.3}s)",
        source_lines.len(),
        {
            let mut files: std::collections::HashSet<_> = std::collections::HashSet::new();
            for sl in &source_lines {
                files.insert(sl.file.display().to_string());
            }
            files.len()
        },
        t0.elapsed().as_secs_f64()
    );

    // Reassemble into single source string for lexing
    let mut combined = String::new();
    for sl in &source_lines {
        combined.push_str(&sl.text);
        combined.push('\n');
    }

    // Phase 2: Lex
    let t1 = Instant::now();
    let mut lexer = Lexer::new(&combined, path.to_str());
    let tokens = lexer.tokenize()?;

    eprintln!(
        "[pbinterp] Lexed {} tokens ({:.3}s)",
        tokens.len(),
        t1.elapsed().as_secs_f64()
    );

    // Phase 3: Parse
    let t2 = Instant::now();
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;

    eprintln!(
        "[pbinterp] Parsed {} top-level items ({:.3}s)",
        program.items.len(),
        t2.elapsed().as_secs_f64()
    );

    // Load preprocessor constants into interpreter
    let mut interpreter = Interpreter::new();
    for (name, value) in preprocessor.constants() {
        interpreter.env.set_constant(name, *value);
    }

    // Phase 4: Execute
    let t3 = Instant::now();
    let exit_code = interpreter.run(&program)?;
    eprintln!(
        "[pbinterp] Execution time: {:.3}s",
        t3.elapsed().as_secs_f64()
    );

    // Capture stdout buffer for JSON parsing
    let stdout_text = String::from_utf8_lossy(&interpreter.stdout).to_string();

    Ok((exit_code, stdout_text))
}

/// Run with a timeout — spawns execution in a thread, kills if exceeded
fn run_with_timeout(path: &str, timeout_secs: u64) -> PbResult<(i32, String)> {
    let path_owned = path.to_string();
    let timed_out = Arc::new(AtomicBool::new(false));
    let timed_out_clone = timed_out.clone();

    let handle = std::thread::spawn(move || {
        run_file(&path_owned)
    });

    // Wait with timeout
    let timeout = std::time::Duration::from_secs(timeout_secs);
    let start = Instant::now();

    loop {
        if handle.is_finished() {
            return match handle.join() {
                Ok(result) => result,
                Err(_) => {
                    use pb::error::PbError;
                    Err(PbError::runtime("Interpreter thread panicked"))
                }
            };
        }

        if start.elapsed() >= timeout {
            timed_out_clone.store(true, Ordering::Relaxed);
            eprintln!("\n!!! TIMEOUT after {}s !!!", timeout_secs);
            eprintln!("[pbinterp] Test execution exceeded timeout of {}s", timeout_secs);
            eprintln!("[pbinterp] This usually means a test is stuck in an infinite loop");
            eprintln!("[pbinterp] Check pb_modal_debug.log for the last modal interaction");
            eprintln!("[pbinterp] Check pb_debug.log for the last function call trace");

            // Try to read last few lines of modal debug log
            if let Ok(content) = std::fs::read_to_string("pb_modal_debug.log") {
                let lines: Vec<&str> = content.lines().collect();
                let start_idx = if lines.len() > 5 { lines.len() - 5 } else { 0 };
                eprintln!("\nLast modal log entries:");
                for line in &lines[start_idx..] {
                    eprintln!("  {}", line);
                }
            }

            process::exit(124); // Same exit code as GNU timeout
        }

        std::thread::sleep(std::time::Duration::from_millis(100));
    }
}

/// Parse test output from PRINT statements to generate JSON report
fn parse_test_output(stdout: &str, total_duration: f64) -> String {
    let mut suites: Vec<String> = Vec::new();
    let mut current_suite = String::new();
    let mut tests_run = 0i64;
    let mut assertions = 0i64;
    let mut passed = 0i64;
    let mut failed = 0i64;
    let mut skipped = 0i64;
    let mut failures: Vec<String> = Vec::new();

    for line in stdout.lines() {
        let trimmed = line.trim();

        // Detect test suite headers: "--- SuiteName Tests (functions) ---"
        if trimmed.starts_with("---") && trimmed.ends_with("---") {
            let suite_name = trimmed.trim_matches('-').trim().trim_matches('(').trim();
            if !suite_name.is_empty() {
                current_suite = suite_name.to_string();
                suites.push(current_suite.clone());
            }
        }

        // Parse summary lines
        if trimmed.starts_with("Tests run:") {
            if let Some(n) = trimmed.split(':').nth(1) {
                tests_run = n.trim().parse().unwrap_or(0);
            }
        }
        if trimmed.starts_with("Assertions:") {
            if let Some(n) = trimmed.split(':').nth(1) {
                assertions = n.trim().parse().unwrap_or(0);
            }
        }
        if trimmed.starts_with("Passed:") {
            if let Some(n) = trimmed.split(':').nth(1) {
                passed = n.trim().parse().unwrap_or(0);
            }
        }
        if trimmed.starts_with("Failed:") && !trimmed.contains("FAIL") {
            if let Some(n) = trimmed.split(':').nth(1) {
                failed = n.trim().parse().unwrap_or(0);
            }
        }
        if trimmed.starts_with("Skipped:") {
            if let Some(n) = trimmed.split(':').nth(1) {
                skipped = n.trim().parse().unwrap_or(0);
            }
        }

        // Capture FAIL lines
        if trimmed.contains("FAIL:") || trimmed.contains("FAILED:") {
            failures.push(format!(
                r#"{{"suite":"{}","detail":"{}"}}"#,
                current_suite.replace('"', "\\\""),
                trimmed.replace('"', "\\\"").replace('\\', "\\\\")
            ));
        }
    }

    let status = if failed > 0 { "failed" } else { "passed" };
    let failures_json = failures.join(",");
    let suites_json: Vec<String> = suites.iter().map(|s| format!(r#""{}""#, s.replace('"', "\\\""))).collect();

    format!(
        r#"{{
  "status": "{}",
  "tests_run": {},
  "assertions": {},
  "passed": {},
  "failed": {},
  "skipped": {},
  "duration_s": {:.3},
  "suites": [{}],
  "failures": [{}]
}}"#,
        status,
        tests_run,
        assertions,
        passed,
        failed,
        skipped,
        total_duration,
        suites_json.join(", "),
        failures_json
    )
}
