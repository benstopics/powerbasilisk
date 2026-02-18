use std::fmt;

use pb::ast::PbType;

#[derive(Debug, Clone)]
pub enum Value {
    Long(i32),
    Integer(i16),
    Dword(u32),
    Quad(i64),
    Single(f32),
    Double(f64),
    Ext(f64),      // approximated as f64
    Currency(i64), // fixed-point, stored as value * 10000
    Str(String),
    Null,
}

impl Value {
    pub fn default_for_type(pb_type: &PbType) -> Value {
        match pb_type {
            PbType::Long => Value::Long(0),
            PbType::Integer => Value::Integer(0),
            PbType::Double => Value::Double(0.0),
            PbType::Single => Value::Single(0.0),
            PbType::String | PbType::FixedString(_) => Value::Str(String::new()),
            PbType::Quad => Value::Quad(0),
            PbType::Dword => Value::Dword(0),
            PbType::Ext => Value::Ext(0.0),
            PbType::Cur => Value::Currency(0),
            PbType::Byte => Value::Long(0),
            PbType::Word => Value::Long(0),
            PbType::Variant => Value::Long(0),
            PbType::UserDefined(_) => Value::Long(0), // TODO: struct support
        }
    }

    pub fn to_f64(&self) -> f64 {
        match self {
            Value::Long(n) => *n as f64,
            Value::Integer(n) => *n as f64,
            Value::Dword(n) => *n as f64,
            Value::Quad(n) => *n as f64,
            Value::Single(n) => *n as f64,
            Value::Double(n) => *n,
            Value::Ext(n) => *n,
            Value::Currency(n) => *n as f64 / 10000.0,
            Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
            Value::Null => 0.0,
        }
    }

    pub fn to_i64(&self) -> i64 {
        match self {
            Value::Long(n) => *n as i64,
            Value::Integer(n) => *n as i64,
            Value::Dword(n) => *n as i64,
            Value::Quad(n) => *n,
            Value::Single(n) => *n as i64,
            Value::Double(n) => *n as i64,
            Value::Ext(n) => *n as i64,
            Value::Currency(n) => *n / 10000,
            Value::Str(s) => s.parse::<i64>().unwrap_or(0),
            Value::Null => 0,
        }
    }

    pub fn to_i32(&self) -> i32 {
        self.to_i64() as i32
    }

    pub fn to_string_val(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Long(n) => n.to_string(),
            Value::Integer(n) => n.to_string(),
            Value::Dword(n) => n.to_string(),
            Value::Quad(n) => n.to_string(),
            Value::Single(n) => format_float(*n as f64),
            Value::Double(n) => format_float(*n),
            Value::Ext(n) => format_float(*n),
            Value::Currency(n) => {
                let whole = *n / 10000;
                let frac = (*n % 10000).abs();
                if frac == 0 {
                    whole.to_string()
                } else {
                    format!("{}.{:04}", whole, frac)
                }
            }
            Value::Null => String::new(),
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::Str(_))
    }

    pub fn is_numeric(&self) -> bool {
        !self.is_string() && !matches!(self, Value::Null)
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Long(n) => *n != 0,
            Value::Integer(n) => *n != 0,
            Value::Dword(n) => *n != 0,
            Value::Quad(n) => *n != 0,
            Value::Single(n) => *n != 0.0,
            Value::Double(n) => *n != 0.0,
            Value::Ext(n) => *n != 0.0,
            Value::Currency(n) => *n != 0,
            Value::Str(s) => !s.is_empty(),
            Value::Null => false,
        }
    }

    /// Coerce to the target PB type
    pub fn coerce_to(&self, target: &PbType) -> Value {
        match target {
            PbType::Long => Value::Long(self.to_i32()),
            PbType::Integer => Value::Integer(self.to_i64() as i16),
            PbType::Double => Value::Double(self.to_f64()),
            PbType::Single => Value::Single(self.to_f64() as f32),
            PbType::Quad => Value::Quad(self.to_i64()),
            PbType::Dword => Value::Dword(self.to_i64() as u32),
            PbType::Ext => Value::Ext(self.to_f64()),
            PbType::Cur => Value::Currency((self.to_f64() * 10000.0) as i64),
            PbType::String | PbType::FixedString(_) => Value::Str(self.to_string_val()),
            _ => self.clone(),
        }
    }

    /// Binary arithmetic
    pub fn add(&self, other: &Value) -> Value {
        // String concatenation if either is string
        if self.is_string() || other.is_string() {
            return Value::Str(format!("{}{}", self.to_string_val(), other.to_string_val()));
        }
        promote_numeric(self, other, |a, b| a + b, |a, b| a + b)
    }

    pub fn sub(&self, other: &Value) -> Value {
        promote_numeric(self, other, |a, b| a - b, |a, b| a - b)
    }

    pub fn mul(&self, other: &Value) -> Value {
        promote_numeric(self, other, |a, b| a * b, |a, b| a * b)
    }

    pub fn div(&self, other: &Value) -> Value {
        let b = other.to_f64();
        if b == 0.0 {
            return Value::Double(0.0); // PB doesn't crash on div by zero in many contexts
        }
        Value::Double(self.to_f64() / b)
    }

    pub fn int_div(&self, other: &Value) -> Value {
        let b = other.to_i64();
        if b == 0 {
            return Value::Long(0);
        }
        Value::Long((self.to_i64() / b) as i32)
    }

    pub fn modulo(&self, other: &Value) -> Value {
        let b = other.to_i64();
        if b == 0 {
            return Value::Long(0);
        }
        Value::Long((self.to_i64() % b) as i32)
    }

    pub fn power(&self, other: &Value) -> Value {
        Value::Double(self.to_f64().powf(other.to_f64()))
    }

    pub fn negate(&self) -> Value {
        match self {
            Value::Long(n) => Value::Long(-*n),
            Value::Integer(n) => Value::Integer(-*n),
            Value::Quad(n) => Value::Quad(-*n),
            Value::Single(n) => Value::Single(-*n),
            Value::Double(n) => Value::Double(-*n),
            Value::Ext(n) => Value::Ext(-*n),
            Value::Currency(n) => Value::Currency(-*n),
            _ => Value::Double(-self.to_f64()),
        }
    }

    pub fn not(&self) -> Value {
        if self.is_truthy() {
            Value::Long(0)
        } else {
            Value::Long(-1) // PB TRUE = -1
        }
    }

    /// Comparison — returns PB boolean (0 or -1)
    pub fn compare_eq(&self, other: &Value) -> Value {
        let result = if self.is_string() || other.is_string() {
            self.to_string_val() == other.to_string_val()
        } else {
            (self.to_f64() - other.to_f64()).abs() < f64::EPSILON
        };
        Value::Long(if result { -1 } else { 0 })
    }

    pub fn compare_neq(&self, other: &Value) -> Value {
        let result = if self.is_string() || other.is_string() {
            self.to_string_val() != other.to_string_val()
        } else {
            (self.to_f64() - other.to_f64()).abs() >= f64::EPSILON
        };
        Value::Long(if result { -1 } else { 0 })
    }

    pub fn compare_lt(&self, other: &Value) -> Value {
        let result = if self.is_string() && other.is_string() {
            self.to_string_val() < other.to_string_val()
        } else {
            self.to_f64() < other.to_f64()
        };
        Value::Long(if result { -1 } else { 0 })
    }

    pub fn compare_gt(&self, other: &Value) -> Value {
        let result = if self.is_string() && other.is_string() {
            self.to_string_val() > other.to_string_val()
        } else {
            self.to_f64() > other.to_f64()
        };
        Value::Long(if result { -1 } else { 0 })
    }

    pub fn compare_lte(&self, other: &Value) -> Value {
        let result = if self.is_string() && other.is_string() {
            self.to_string_val() <= other.to_string_val()
        } else {
            self.to_f64() <= other.to_f64()
        };
        Value::Long(if result { -1 } else { 0 })
    }

    pub fn compare_gte(&self, other: &Value) -> Value {
        let result = if self.is_string() && other.is_string() {
            self.to_string_val() >= other.to_string_val()
        } else {
            self.to_f64() >= other.to_f64()
        };
        Value::Long(if result { -1 } else { 0 })
    }

    pub fn and(&self, other: &Value) -> Value {
        Value::Long(self.to_i32() & other.to_i32())
    }

    pub fn or(&self, other: &Value) -> Value {
        Value::Long(self.to_i32() | other.to_i32())
    }

    pub fn xor(&self, other: &Value) -> Value {
        Value::Long(self.to_i32() ^ other.to_i32())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string_val())
    }
}

fn format_float(n: f64) -> String {
    if n == n.trunc() && n.abs() < 1e15 {
        format!("{}", n as i64)
    } else {
        // Remove trailing zeros
        let s = format!("{}", n);
        s
    }
}

/// Promote two numeric values and apply operation
fn promote_numeric(
    a: &Value,
    b: &Value,
    int_op: impl Fn(i64, i64) -> i64,
    float_op: impl Fn(f64, f64) -> f64,
) -> Value {
    // If either is floating point, use float arithmetic
    let a_float = matches!(
        a,
        Value::Double(_) | Value::Single(_) | Value::Ext(_) | Value::Currency(_)
    );
    let b_float = matches!(
        b,
        Value::Double(_) | Value::Single(_) | Value::Ext(_) | Value::Currency(_)
    );

    if a_float || b_float {
        Value::Double(float_op(a.to_f64(), b.to_f64()))
    } else {
        // Both are integer types
        let result = int_op(a.to_i64(), b.to_i64());
        // Choose the widest integer type
        match (a, b) {
            (Value::Quad(_), _) | (_, Value::Quad(_)) => Value::Quad(result),
            _ => Value::Long(result as i32),
        }
    }
}
