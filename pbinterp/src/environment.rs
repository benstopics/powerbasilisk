use std::collections::HashMap;

use pb::ast::PbType;
use crate::value::Value;

/// Strip type suffix from a variable name to find the base name.
/// In PB, `Cash#` and `Cash` refer to the same variable; `StockOwned%` and `StockOwned` too.
/// We normalize ALL names to their base form (suffix stripped) for consistent lookup.
fn strip_type_suffix(name: &str) -> &str {
    let bytes = name.as_bytes();
    if bytes.is_empty() {
        return name;
    }
    let last = bytes[bytes.len() - 1];
    match last {
        b'#' | b'&' | b'%' | b'!' | b'@' | b'$' => {
            // Check for double suffixes: ##, &&, %%, @@
            if bytes.len() >= 2 && bytes[bytes.len() - 2] == last {
                &name[..name.len() - 2]
            } else {
                &name[..name.len() - 1]
            }
        }
        _ => name,
    }
}

/// Normalize a variable/array name: uppercase + strip type suffix.
/// This ensures `StockOwned%`, `StockOwned`, `STOCKOWNED%`, `STOCKOWNED` all map to `STOCKOWNED`.
pub fn normalize(name: &str) -> String {
    let upper = name.to_uppercase();
    strip_type_suffix(&upper).to_string()
}

/// Array descriptor with bounds and flat storage
#[derive(Debug, Clone)]
pub struct ArrayDesc {
    pub dims: Vec<(i64, i64)>, // (lower, upper) per dimension
    pub data: Vec<Value>,
    pub elem_type: PbType,
}

impl ArrayDesc {
    pub fn new(dims: Vec<(i64, i64)>, elem_type: &PbType) -> Self {
        let total = dims.iter().fold(1usize, |acc, (lo, hi)| {
            acc * ((hi - lo + 1) as usize).max(1)
        });
        let data = vec![Value::default_for_type(elem_type); total];
        ArrayDesc {
            dims,
            data,
            elem_type: elem_type.clone(),
        }
    }

    pub fn index(&self, indices: &[i64]) -> Option<usize> {
        if indices.len() != self.dims.len() {
            return None;
        }
        let mut flat = 0usize;
        let mut stride = 1usize;
        // Rightmost index varies fastest (row-major for PB)
        for i in (0..self.dims.len()).rev() {
            let (lo, hi) = self.dims[i];
            let idx = indices[i];
            if idx < lo || idx > hi {
                return None; // out of bounds
            }
            flat += ((idx - lo) as usize) * stride;
            stride *= (hi - lo + 1) as usize;
        }
        Some(flat)
    }

    pub fn get(&self, indices: &[i64]) -> Option<&Value> {
        self.index(indices).and_then(|i| self.data.get(i))
    }

    pub fn set(&mut self, indices: &[i64], value: Value) -> bool {
        if let Some(i) = self.index(indices) {
            if i < self.data.len() {
                self.data[i] = value;
                return true;
            }
        }
        false
    }
}

/// A single scope frame (for a SUB/FUNCTION call)
#[derive(Debug)]
pub struct CallFrame {
    pub name: String,
    pub locals: HashMap<String, Value>,
    pub local_arrays: HashMap<String, ArrayDesc>,
    pub return_value: Value,
    pub is_function: bool,
    /// Track which local variable names are STATIC (persist across calls)
    pub static_names: Vec<String>,
}

impl CallFrame {
    pub fn new(name: &str, is_function: bool, return_type: &PbType) -> Self {
        CallFrame {
            name: name.to_string(),
            locals: HashMap::new(),
            local_arrays: HashMap::new(),
            return_value: Value::default_for_type(return_type),
            is_function,
            static_names: Vec::new(),
        }
    }
}

/// The global environment
pub struct Environment {
    pub globals: HashMap<String, Value>,
    pub global_arrays: HashMap<String, ArrayDesc>,
    pub constants: HashMap<String, i64>,
    pub call_stack: Vec<CallFrame>,
    pub open_files: HashMap<i32, OpenFile>,
    next_file_num: i32,
}

pub struct OpenFile {
    pub path: String,
    pub content: String, // buffer for output
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            globals: HashMap::new(),
            global_arrays: HashMap::new(),
            constants: HashMap::new(),
            call_stack: Vec::new(),
            open_files: HashMap::new(),
            next_file_num: 1,
        }
    }

    pub fn freefile(&mut self) -> i32 {
        let n = self.next_file_num;
        self.next_file_num += 1;
        n
    }

    /// Get a variable value, searching locals first then globals.
    /// Names are normalized (uppercased + suffix stripped) for consistent lookup.
    pub fn get_var(&self, name: &str) -> Value {
        let key = normalize(name);

        // Check locals first (top of call stack)
        if let Some(frame) = self.call_stack.last() {
            if let Some(val) = frame.locals.get(&key) {
                return val.clone();
            }
        }

        // Check globals
        if let Some(val) = self.globals.get(&key) {
            return val.clone();
        }

        // Default
        Value::default_for_type(&type_from_name(name))
    }

    /// Set a variable value, searching locals first then globals.
    /// Names are normalized for consistent lookup.
    pub fn set_var(&mut self, name: &str, value: Value) {
        let key = normalize(name);

        // Check if it exists in locals
        if let Some(frame) = self.call_stack.last_mut() {
            if frame.locals.contains_key(&key) {
                frame.locals.insert(key, value);
                return;
            }
        }

        // Check globals
        if self.globals.contains_key(&key) {
            self.globals.insert(key, value);
            return;
        }

        // If we have a call stack, create as local; otherwise global
        if let Some(frame) = self.call_stack.last_mut() {
            frame.locals.insert(key, value);
        } else {
            self.globals.insert(key, value);
        }
    }

    /// Declare a local variable (name is normalized)
    pub fn declare_local(&mut self, name: &str, value: Value) {
        let key = normalize(name);
        if let Some(frame) = self.call_stack.last_mut() {
            frame.locals.insert(key, value);
        }
    }

    /// Mark a local variable as STATIC (will be persisted across calls)
    pub fn mark_static(&mut self, name: &str) {
        let key = normalize(name);
        if let Some(frame) = self.call_stack.last_mut() {
            if !frame.static_names.contains(&key) {
                frame.static_names.push(key);
            }
        }
    }

    /// Declare a global variable (name is normalized)
    pub fn declare_global(&mut self, name: &str, pb_type: &PbType) {
        let key = normalize(name);
        if !self.globals.contains_key(&key) {
            self.globals.insert(key, Value::default_for_type(pb_type));
        }
    }

    /// Declare a global array (empty, to be DIMed later; name is normalized)
    pub fn declare_global_array(&mut self, name: &str, _pb_type: &PbType) {
        let key = normalize(name);
        if !self.global_arrays.contains_key(&key) {
            self.global_arrays.insert(
                key,
                ArrayDesc {
                    dims: Vec::new(),
                    data: Vec::new(),
                    elem_type: _pb_type.clone(),
                },
            );
        }
    }

    /// DIM an array with bounds (name is normalized)
    pub fn dim_array(&mut self, name: &str, dims: Vec<(i64, i64)>, pb_type: &PbType, is_local: bool) {
        let key = normalize(name);
        let arr = ArrayDesc::new(dims, pb_type);

        if is_local {
            if let Some(frame) = self.call_stack.last_mut() {
                frame.local_arrays.insert(key, arr);
                return;
            }
        }
        self.global_arrays.insert(key, arr);
    }

    /// REDIM an array (resize, clearing data; name is normalized)
    pub fn redim_array(&mut self, name: &str, dims: Vec<(i64, i64)>, pb_type: &PbType) {
        let key = normalize(name);
        let arr = ArrayDesc::new(dims, pb_type);

        // Check locals first
        if let Some(frame) = self.call_stack.last_mut() {
            if frame.local_arrays.contains_key(&key) {
                frame.local_arrays.insert(key, arr);
                return;
            }
        }

        self.global_arrays.insert(key, arr);
    }

    /// Get array element (name is normalized)
    pub fn get_array(&self, name: &str, indices: &[i64]) -> Value {
        let key = normalize(name);

        // Check local arrays first
        if let Some(frame) = self.call_stack.last() {
            if let Some(arr) = frame.local_arrays.get(&key) {
                return arr.get(indices).cloned().unwrap_or(Value::Long(0));
            }
        }

        // Check global arrays
        if let Some(arr) = self.global_arrays.get(&key) {
            return arr.get(indices).cloned().unwrap_or(Value::Long(0));
        }

        Value::Long(0)
    }

    /// Set array element (name is normalized)
    pub fn set_array(&mut self, name: &str, indices: &[i64], value: Value) {
        let key = normalize(name);

        // Check local arrays first
        if let Some(frame) = self.call_stack.last_mut() {
            if let Some(arr) = frame.local_arrays.get_mut(&key) {
                arr.set(indices, value);
                return;
            }
        }

        // Check global arrays
        if let Some(arr) = self.global_arrays.get_mut(&key) {
            arr.set(indices, value);
            return;
        }
    }

    /// Check if a name refers to an array (name is normalized)
    pub fn is_array(&self, name: &str) -> bool {
        let key = normalize(name);

        if let Some(frame) = self.call_stack.last() {
            if frame.local_arrays.contains_key(&key) {
                return true;
            }
        }

        self.global_arrays.contains_key(&key)
    }

    pub fn push_frame(&mut self, frame: CallFrame) {
        self.call_stack.push(frame);
    }

    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        self.call_stack.pop()
    }

    /// Get the name of the current function/sub (for STATIC variable keying)
    pub fn current_function_name(&self) -> String {
        self.call_stack
            .last()
            .map(|f| f.name.to_uppercase())
            .unwrap_or_default()
    }

    pub fn set_constant(&mut self, name: &str, value: i64) {
        self.constants.insert(name.to_uppercase(), value);
    }

    pub fn get_constant(&self, name: &str) -> Option<i64> {
        self.constants.get(&name.to_uppercase()).copied()
    }
}

/// Infer PB type from variable name suffix (for default values)
fn type_from_name(name: &str) -> PbType {
    let upper = name.to_uppercase();
    if upper.ends_with("##") {
        PbType::Ext
    } else if upper.ends_with('#') {
        PbType::Double
    } else if upper.ends_with("&&") {
        PbType::Quad
    } else if upper.ends_with('&') {
        PbType::Long
    } else if upper.ends_with('%') {
        PbType::Integer
    } else if upper.ends_with('!') {
        PbType::Single
    } else if upper.ends_with("@@") {
        PbType::Cur
    } else if upper.ends_with('@') {
        PbType::Cur
    } else if upper.ends_with('$') {
        PbType::String
    } else {
        PbType::Long
    }
}
