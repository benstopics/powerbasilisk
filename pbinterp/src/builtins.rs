use crate::value::Value;

/// Evaluate a built-in function. Returns None if not a builtin.
/// NOTE: The `name` parameter is already normalized (uppercased, suffix stripped).
/// So `STR$` comes in as `STR`, `FORMAT$` as `FORMAT`, etc.
pub fn call_builtin(name: &str, args: &[Value]) -> Option<Value> {
    match name {
        "ABS" => {
            let v = args.first().map(|a| a.to_f64()).unwrap_or(0.0);
            Some(Value::Double(v.abs()))
        }
        "INT" => {
            let v = args.first().map(|a| a.to_f64()).unwrap_or(0.0);
            Some(Value::Long(v.floor() as i32))
        }
        "FIX" => {
            let v = args.first().map(|a| a.to_f64()).unwrap_or(0.0);
            Some(Value::Long(v.trunc() as i32))
        }
        "CEIL" => {
            let v = args.first().map(|a| a.to_f64()).unwrap_or(0.0);
            Some(Value::Long(v.ceil() as i32))
        }
        "SQR" => {
            let v = args.first().map(|a| a.to_f64()).unwrap_or(0.0);
            Some(Value::Double(v.sqrt()))
        }
        "SGN" => {
            let v = args.first().map(|a| a.to_f64()).unwrap_or(0.0);
            Some(Value::Long(if v > 0.0 { 1 } else if v < 0.0 { -1 } else { 0 }))
        }
        "RND" => {
            // Pseudo-random using system time
            let seed = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .subsec_nanos();
            let r = ((seed as f64) / 4_294_967_295.0) % 1.0;

            if args.len() >= 2 {
                // RND(lo, hi) — return random integer in range [lo, hi]
                let lo = args[0].to_i64();
                let hi = args[1].to_i64();
                let range = (hi - lo + 1).max(1);
                Some(Value::Long((lo + (r * range as f64) as i64) as i32))
            } else if args.len() == 1 {
                // RND(n) — return random integer 0 to n-1
                let n = args[0].to_i64().max(1);
                Some(Value::Long((r * n as f64) as i32))
            } else {
                // RND — return 0.0 to 1.0
                Some(Value::Double(r))
            }
        }
        "LEN" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            Some(Value::Long(s.len() as i32))
        }
        "VAL" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            let v: f64 = s.trim().parse().unwrap_or(0.0);
            Some(Value::Double(v))
        }
        "STR" => {
            let v = args.first().map(|a| a.to_f64()).unwrap_or(0.0);
            if v == v.trunc() && v.abs() < 1e15 {
                Some(Value::Str(format!("{}", v as i64)))
            } else {
                Some(Value::Str(format!("{}", v)))
            }
        }
        "FORMAT" => Some(builtin_format(args)),
        "MID" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            let start = args.get(1).map(|a| a.to_i32()).unwrap_or(1) as usize;
            let len = args.get(2).map(|a| a.to_i32() as usize);

            if start == 0 || start > s.len() {
                return Some(Value::Str(String::new()));
            }

            let from = start - 1; // PB is 1-based
            match len {
                Some(l) => {
                    let end = (from + l).min(s.len());
                    Some(Value::Str(s[from..end].to_string()))
                }
                None => Some(Value::Str(s[from..].to_string())),
            }
        }
        "LEFT" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            let n = args.get(1).map(|a| a.to_i32()).unwrap_or(0) as usize;
            let end = n.min(s.len());
            Some(Value::Str(s[..end].to_string()))
        }
        "RIGHT" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            let n = args.get(1).map(|a| a.to_i32()).unwrap_or(0) as usize;
            if n >= s.len() {
                Some(Value::Str(s))
            } else {
                Some(Value::Str(s[s.len() - n..].to_string()))
            }
        }
        "INSTR" => {
            // INSTR([start,] string, search)
            // PB INSTR has 2 or 3 args: INSTR(haystack, needle) or INSTR(start, haystack, needle)
            let (start, haystack, needle) = if args.len() >= 3 {
                (
                    args[0].to_i32() as usize,
                    args[1].to_string_val(),
                    args[2].to_string_val(),
                )
            } else {
                (
                    1,
                    args.first().map(|a| a.to_string_val()).unwrap_or_default(),
                    args.get(1).map(|a| a.to_string_val()).unwrap_or_default(),
                )
            };

            if start == 0 || needle.is_empty() || start > haystack.len() {
                return Some(Value::Long(0));
            }

            let from = start - 1;
            match haystack[from..].find(&needle) {
                Some(pos) => Some(Value::Long((pos + from + 1) as i32)),
                None => Some(Value::Long(0)),
            }
        }
        "UCASE" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            Some(Value::Str(s.to_uppercase()))
        }
        "LCASE" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            Some(Value::Str(s.to_lowercase()))
        }
        "TRIM" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            Some(Value::Str(s.trim().to_string()))
        }
        "LTRIM" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            Some(Value::Str(s.trim_start().to_string()))
        }
        "RTRIM" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            Some(Value::Str(s.trim_end().to_string()))
        }
        "CHR" => {
            let n = args.first().map(|a| a.to_i32()).unwrap_or(0);
            Some(Value::Str(
                char::from_u32(n as u32).unwrap_or('\0').to_string(),
            ))
        }
        "ASC" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            let val = s.bytes().next().unwrap_or(0) as i32;
            Some(Value::Long(val))
        }
        "STRING" => {
            let count = args.first().map(|a| a.to_i32()).unwrap_or(0) as usize;
            let ch = args.get(1).map(|a| {
                if a.is_string() {
                    a.to_string_val().chars().next().unwrap_or(' ')
                } else {
                    char::from_u32(a.to_i32() as u32).unwrap_or(' ')
                }
            }).unwrap_or(' ');
            Some(Value::Str(ch.to_string().repeat(count)))
        }
        "SPACE" => {
            let n = args.first().map(|a| a.to_i32()).unwrap_or(0) as usize;
            Some(Value::Str(" ".repeat(n)))
        }
        "PARSE" => {
            // PARSE$(string, delimiter, index)
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            let delim = args.get(1).map(|a| a.to_string_val()).unwrap_or_default();
            let idx = args.get(2).map(|a| a.to_i32()).unwrap_or(1) as usize;

            let parts: Vec<&str> = if delim.is_empty() {
                vec![&s]
            } else {
                s.split(&delim).collect()
            };

            if idx == 0 || idx > parts.len() {
                Some(Value::Str(String::new()))
            } else {
                Some(Value::Str(parts[idx - 1].to_string()))
            }
        }
        "PARSECOUNT" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            let delim = args.get(1).map(|a| a.to_string_val()).unwrap_or_default();
            if s.is_empty() {
                Some(Value::Long(0))
            } else if delim.is_empty() {
                Some(Value::Long(1))
            } else {
                Some(Value::Long(s.split(&delim).count() as i32))
            }
        }
        "REMOVE" => {
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            let remove = args.get(1).map(|a| a.to_string_val()).unwrap_or_default();
            Some(Value::Str(s.replace(&remove, "")))
        }
        "REPLACE" => {
            // REPLACE old IN string WITH new
            // As a function: REPLACE$(string, old, new)
            let s = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            let old = args.get(1).map(|a| a.to_string_val()).unwrap_or_default();
            let new = args.get(2).map(|a| a.to_string_val()).unwrap_or_default();
            Some(Value::Str(s.replace(&old, &new)))
        }
        "FREEFILE" => {
            // This is handled specially in the interpreter
            Some(Value::Long(1))
        }
        "MAX" => {
            let a = args.first().map(|v| v.to_f64()).unwrap_or(0.0);
            let b = args.get(1).map(|v| v.to_f64()).unwrap_or(0.0);
            Some(Value::Double(a.max(b)))
        }
        "MIN" => {
            let a = args.first().map(|v| v.to_f64()).unwrap_or(0.0);
            let b = args.get(1).map(|v| v.to_f64()).unwrap_or(0.0);
            Some(Value::Double(a.min(b)))
        }
        "VARPTR" | "STRPTR" => {
            // Stub: return 0 for pointer operations
            Some(Value::Long(0))
        }
        "ISFALSE" => {
            let v = args.first().map(|a| a.is_truthy()).unwrap_or(true);
            Some(Value::Long(if v { 0 } else { -1 }))
        }
        "ISTRUE" => {
            let v = args.first().map(|a| a.is_truthy()).unwrap_or(false);
            Some(Value::Long(if v { -1 } else { 0 }))
        }
        "FUNCNAME" => {
            // Returns current function name — handled via interpreter, not here
            // When called as a builtin, return empty string
            Some(Value::Str(String::new()))
        }
        "ERRCLEAR" => {
            // Clear the ERR variable
            Some(Value::Long(0))
        }
        "ROUND" => {
            let v = args.first().map(|a| a.to_f64()).unwrap_or(0.0);
            let decimals = args.get(1).map(|a| a.to_i64()).unwrap_or(0);
            let factor = 10f64.powi(decimals as i32);
            Some(Value::Double((v * factor).round() / factor))
        }
        "USING" => {
            // USING$(format, value) — similar to FORMAT$
            Some(builtin_format(args))
        }
        "SIZEOF" => {
            // Return typical sizes for PB types
            Some(Value::Long(4))
        }
        "ENVIRON" => {
            // ENVIRON$("VAR_NAME") — read environment variable
            let var_name = args.first().map(|a| a.to_string_val()).unwrap_or_default();
            let val = std::env::var(&var_name).unwrap_or_default();
            Some(Value::Str(val))
        }
        _ => None,
    }
}

fn builtin_format(args: &[Value]) -> Value {
    let val = args.first().map(|a| a.to_f64()).unwrap_or(0.0);
    let fmt_str = args.get(1).map(|a| a.to_string_val());

    match fmt_str {
        Some(fmt) if !fmt.is_empty() => Value::Str(format_number(val, &fmt)),
        _ => {
            // Default FORMAT$ — just convert to string
            if val == val.trunc() && val.abs() < 1e15 {
                Value::Str(format!("{}", val as i64))
            } else {
                Value::Str(format!("{}", val))
            }
        }
    }
}

fn format_number(val: f64, fmt: &str) -> String {
    // Simple FORMAT$ implementation supporting common PB format strings
    // "#,##0.####" — commas with up to 4 decimal places
    // "#,##0" — commas, no decimals

    let use_commas = fmt.contains(',');
    let decimal_pos = fmt.find('.');
    let decimal_places = match decimal_pos {
        Some(pos) => {
            let after = &fmt[pos + 1..];
            after.chars().take_while(|c| *c == '#' || *c == '0').count()
        }
        None => 0,
    };

    let is_negative = val < 0.0;
    let abs_val = val.abs();

    // Round to the appropriate number of decimal places
    let multiplier = 10f64.powi(decimal_places as i32);
    let rounded = (abs_val * multiplier).round() / multiplier;

    // Format the number
    let formatted = if decimal_places > 0 {
        format!("{:.prec$}", rounded, prec = decimal_places)
    } else {
        format!("{}", rounded as i64)
    };

    // Split into integer and decimal parts
    let parts: Vec<&str> = formatted.splitn(2, '.').collect();
    let mut int_part = parts[0].to_string();
    let dec_part = if parts.len() > 1 { parts[1] } else { "" };

    // Add commas to integer part
    if use_commas && int_part.len() > 3 {
        let mut with_commas = String::new();
        for (i, ch) in int_part.chars().rev().enumerate() {
            if i > 0 && i % 3 == 0 {
                with_commas.push(',');
            }
            with_commas.push(ch);
        }
        int_part = with_commas.chars().rev().collect();
    }

    // Trim trailing zeros from decimal part if format uses #
    let trimmed_dec = if decimal_places > 0 {
        let has_required_zeros = fmt.contains('0');
        if !has_required_zeros || fmt.ends_with('#') {
            dec_part.trim_end_matches('0')
        } else {
            dec_part
        }
    } else {
        ""
    };

    let mut result = if is_negative {
        format!("-{}", int_part)
    } else {
        int_part
    };

    if !trimmed_dec.is_empty() {
        result.push('.');
        result.push_str(trimmed_dec);
    }

    result
}
