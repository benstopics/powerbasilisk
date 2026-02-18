use std::collections::HashMap;

use crate::llvm_ir::IrType;
use pb::ast::PbType;

/// Information about a declared variable.
#[derive(Clone)]
pub struct VarInfo {
    /// LLVM IR name for the pointer (e.g., `%r3` for local alloca, `@CASH` for global)
    pub ptr_name: String,
    /// LLVM IR type of the value
    pub ir_type: IrType,
    /// PB type for semantic operations
    pub pb_type: PbType,
}

/// Information about a declared array.
#[derive(Clone)]
pub struct ArrayInfo {
    /// LLVM name (`@CASH` for global, `%r5` for local alloca)
    pub ptr_name: String,
    /// Full array type (`[1601 x double]`)
    pub array_ir_type: IrType,
    /// Element type (`double`)
    pub elem_ir_type: IrType,
    /// PB type of elements
    pub pb_type: PbType,
    /// (lower_bound, count) per dimension
    pub dims: Vec<(i64, i64)>,
    /// Total number of elements (product of all counts)
    pub total_elements: usize,
}

/// Tracks variables during codegen.
pub struct SymbolTable {
    locals: HashMap<String, VarInfo>,
    globals: HashMap<String, VarInfo>,
    local_arrays: HashMap<String, ArrayInfo>,
    global_arrays: HashMap<String, ArrayInfo>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            locals: HashMap::new(),
            globals: HashMap::new(),
            local_arrays: HashMap::new(),
            global_arrays: HashMap::new(),
        }
    }

    pub fn insert_local(
        &mut self,
        name: String,
        ptr_name: String,
        ir_type: IrType,
        pb_type: PbType,
    ) {
        self.locals.insert(
            name,
            VarInfo {
                ptr_name,
                ir_type,
                pb_type,
            },
        );
    }

    pub fn insert_global(&mut self, name: String, ir_type: IrType, pb_type: PbType) {
        let ptr_name = format!("@{}", name);
        self.globals.insert(
            name,
            VarInfo {
                ptr_name,
                ir_type,
                pb_type,
            },
        );
    }

    /// Insert a global variable with an explicit pointer name (e.g., a constant GEP expression).
    pub fn insert_global_with_ptr(
        &mut self,
        name: String,
        ptr_name: String,
        ir_type: IrType,
        pb_type: PbType,
    ) {
        self.globals.insert(
            name,
            VarInfo {
                ptr_name,
                ir_type,
                pb_type,
            },
        );
    }

    pub fn insert_local_array(&mut self, name: String, info: ArrayInfo) {
        self.local_arrays.insert(name, info);
    }

    pub fn insert_global_array(&mut self, name: String, info: ArrayInfo) {
        self.global_arrays.insert(name, info);
    }

    /// Look up a variable — locals first, then globals.
    pub fn lookup(&self, name: &str) -> Option<&VarInfo> {
        self.locals.get(name).or_else(|| self.globals.get(name))
    }

    /// Look up an array — local arrays first, then global arrays.
    pub fn lookup_array(&self, name: &str) -> Option<&ArrayInfo> {
        self.local_arrays
            .get(name)
            .or_else(|| self.global_arrays.get(name))
    }

    pub fn clear_locals(&mut self) {
        self.locals.clear();
        self.local_arrays.clear();
    }
}
