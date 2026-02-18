use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::error::{PbError, PbResult};

/// A resolved source line with file origin tracking.
#[derive(Debug, Clone)]
pub struct SourceLine {
    pub text: String,
    pub file: PathBuf,
    pub line_num: usize,
}

/// Strip trailing comment from a line, respecting string literals.
/// Returns the code portion (everything before the comment marker).
/// e.g. `IF X=1 _ ' old code` → `IF X=1 _`
///      `A$ = "it's fine"` → `A$ = "it's fine"` (no strip)
fn strip_trailing_comment(line: &str) -> &str {
    let bytes = line.as_bytes();
    let mut in_string = false;
    for i in 0..bytes.len() {
        if bytes[i] == b'"' {
            in_string = !in_string;
        } else if bytes[i] == b'\'' && !in_string {
            return &line[..i];
        }
    }
    line
}

/// Preprocessor: resolves #INCLUDE, %CONSTANTS, and strips directives.
pub struct Preprocessor {
    constants: HashMap<String, i64>,
    included: HashSet<PathBuf>,
}

impl Preprocessor {
    pub fn new() -> Self {
        Preprocessor {
            constants: HashMap::new(),
            included: HashSet::new(),
        }
    }

    /// Process a source file, returning all resolved lines (with includes inlined).
    pub fn process_file(&mut self, path: &Path) -> PbResult<Vec<SourceLine>> {
        let canonical = std::fs::canonicalize(path).map_err(|e| {
            PbError::io(format!("Cannot resolve path {}: {}", path.display(), e))
        })?;

        if self.included.contains(&canonical) {
            return Ok(Vec::new()); // cycle detection
        }
        self.included.insert(canonical.clone());

        let content = std::fs::read_to_string(&canonical).map_err(|e| {
            PbError::io(format!("Cannot read {}: {}", canonical.display(), e))
        })?;

        self.process_source(&content, &canonical)
    }

    fn process_source(&mut self, source: &str, file: &Path) -> PbResult<Vec<SourceLine>> {
        let mut lines = Vec::new();
        let raw_lines: Vec<&str> = source.lines().collect();
        let mut i = 0;
        let mut if_stack: Vec<bool> = Vec::new(); // stack of "are we in an active block?"

        while i < raw_lines.len() {
            let line_num = i + 1;
            let raw = raw_lines[i];
            let trimmed = raw.trim();

            // Check if we're inside an inactive #IF block
            let active = if_stack.iter().all(|&b| b);

            // Handle #IF / #ELSEIF / #ELSE / #ENDIF
            let upper = trimmed.to_uppercase();
            if upper.starts_with("#IF ") || upper.starts_with("#IF\t") {
                let condition = self.evaluate_if_condition(&trimmed[3..].trim());
                if active {
                    if_stack.push(condition);
                } else {
                    if_stack.push(false); // nested in inactive block
                }
                i += 1;
                continue;
            }
            if upper == "#ELSE" {
                let len = if_stack.len();
                let parent_active = if len > 1 {
                    if_stack[..len - 1].iter().all(|&b| b)
                } else {
                    true
                };
                if let Some(last) = if_stack.last_mut() {
                    if parent_active {
                        *last = !*last;
                    }
                }
                i += 1;
                continue;
            }
            if upper.starts_with("#ELSEIF ") || upper.starts_with("#ELSEIF\t") {
                let len = if_stack.len();
                let parent_active = if len > 1 {
                    if_stack[..len - 1].iter().all(|&b| b)
                } else {
                    true
                };
                let cond = self.evaluate_if_condition(trimmed[8..].trim());
                if let Some(last) = if_stack.last_mut() {
                    if parent_active {
                        if *last {
                            *last = false;
                        } else {
                            *last = cond;
                        }
                    }
                }
                i += 1;
                continue;
            }
            if upper == "#ENDIF" {
                if_stack.pop();
                i += 1;
                continue;
            }

            if !active {
                i += 1;
                continue;
            }

            // Handle line continuations: join lines ending with _
            // Must strip comments BEFORE checking for _, because a comment may
            // contain a trailing _ that isn't a real continuation marker.
            // e.g.: IF X=1 _ ' old code was: IF X=1 AND Y=2 _
            //           OR Z=3 THEN
            let mut full_line = raw.to_string();
            loop {
                let code_part = strip_trailing_comment(&full_line);
                let trimmed_code = code_part.trim_end();
                if !trimmed_code.ends_with('_') || trimmed_code.len() <= 1 {
                    break;
                }
                // Remove the trailing underscore from the code (not the comment)
                let before_underscore = &trimmed_code[..trimmed_code.len() - 1];
                full_line = before_underscore.to_string();
                i += 1;
                if i < raw_lines.len() {
                    full_line.push(' ');
                    full_line.push_str(raw_lines[i].trim());
                }
            }

            let trimmed_full = full_line.trim();

            // Skip empty lines and comments
            if trimmed_full.is_empty() || trimmed_full.starts_with('\'') {
                i += 1;
                continue;
            }
            // REM comment
            if trimmed_full.len() >= 3 && trimmed_full[..3].to_uppercase() == "REM"
                && (trimmed_full.len() == 3 || !trimmed_full.as_bytes()[3].is_ascii_alphanumeric())
            {
                i += 1;
                continue;
            }

            let upper_full = trimmed_full.to_uppercase();

            // #COMPILE — skip entirely
            if upper_full.starts_with("#COMPILE") {
                i += 1;
                continue;
            }

            // #DIM ALL — skip (we don't enforce)
            if upper_full.starts_with("#DIM") {
                i += 1;
                continue;
            }

            // #REGISTER — skip
            if upper_full.starts_with("#REGISTER") {
                i += 1;
                continue;
            }

            // #DEBUG — skip
            if upper_full.starts_with("#DEBUG") {
                i += 1;
                continue;
            }

            // #INCLUDE "file"
            if upper_full.starts_with("#INCLUDE") {
                let rest = trimmed_full[8..].trim();
                if let Some(inc_path) = extract_string(rest) {
                    let resolved = resolve_include_path(file, &inc_path);
                    match self.process_file(&resolved) {
                        Ok(inc_lines) => {
                            lines.extend(inc_lines);
                        }
                        Err(e) => {
                            eprintln!(
                                "Warning: failed to include {}: {}",
                                resolved.display(),
                                e
                            );
                        }
                    }
                }
                i += 1;
                continue;
            }

            // %CONSTANT = value  (define a constant)
            if trimmed_full.starts_with('%') {
                if let Some(eq_pos) = trimmed_full.find('=') {
                    let name = trimmed_full[..eq_pos].trim().to_uppercase();
                    let val_str = trimmed_full[eq_pos + 1..].trim();
                    // Strip trailing comments
                    let val_str = strip_comment(val_str);
                    let value = self.parse_constant_value(&val_str);
                    self.constants.insert(name.to_string(), value);
                }
                // Emit the line anyway so the parser can also see it
                lines.push(SourceLine {
                    text: full_line.clone(),
                    file: file.to_path_buf(),
                    line_num,
                });
                i += 1;
                continue;
            }

            // Regular line — emit it
            lines.push(SourceLine {
                text: full_line,
                file: file.to_path_buf(),
                line_num,
            });

            i += 1;
        }

        Ok(lines)
    }

    fn evaluate_if_condition(&self, condition: &str) -> bool {
        let cond = condition.trim().to_uppercase();

        // Handle NOT %DEF(%WINAPI) etc.
        if cond.starts_with("NOT ") {
            return !self.evaluate_if_condition(&cond[4..]);
        }

        // Handle %DEF(%CONSTANT)
        if cond.starts_with("%DEF(") {
            if let Some(end) = cond.find(')') {
                let const_name = cond[5..end].trim().to_string();
                return self.constants.contains_key(&const_name);
            }
            return false;
        }

        // Handle bare %CONSTANT (true if non-zero)
        if cond.starts_with('%') {
            if let Some(&val) = self.constants.get(&cond) {
                return val != 0;
            }
            return false;
        }

        // Handle expr - expr (non-zero is true)
        // Simple: just check if it's a defined constant
        false
    }

    fn parse_constant_value(&self, s: &str) -> i64 {
        let s = s.trim();

        // Hex literal
        if s.starts_with("&H") || s.starts_with("&h") {
            let hex = s[2..]
                .trim_end_matches(|c: char| c == '&' || c == '%' || c == '?');
            return i64::from_str_radix(hex, 16).unwrap_or(0);
        }

        // Reference to another constant
        if s.starts_with('%') {
            if let Some(&val) = self.constants.get(&s.to_uppercase()) {
                return val;
            }
            return 0;
        }

        // Simple integer
        s.parse::<i64>().unwrap_or(0)
    }

    pub fn get_constant(&self, name: &str) -> Option<i64> {
        self.constants.get(&name.to_uppercase()).copied()
    }

    pub fn constants(&self) -> &HashMap<String, i64> {
        &self.constants
    }
}

fn extract_string(s: &str) -> Option<String> {
    let s = s.trim();
    if s.starts_with('"') {
        if let Some(end) = s[1..].find('"') {
            return Some(s[1..end + 1].to_string());
        }
    }
    None
}

fn resolve_include_path(current_file: &Path, include_path: &str) -> PathBuf {
    // Convert backslashes to forward slashes for cross-platform
    let normalized = include_path.replace('\\', "/");
    let parent = current_file.parent().unwrap_or_else(|| Path::new("."));
    parent.join(normalized)
}

fn strip_comment(s: &str) -> String {
    // Find a comment marker that's not inside a string
    let mut in_string = false;
    for (i, ch) in s.char_indices() {
        if ch == '"' {
            in_string = !in_string;
        } else if ch == '\'' && !in_string {
            return s[..i].trim().to_string();
        }
    }
    s.trim().to_string()
}
