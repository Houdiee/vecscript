use crate::token::Type;
use std::{collections::HashMap, ops::Range};

pub struct SymbolTable {
    scopes: Vec<HashMap<String, SymbolInfo>>,
}

pub struct SymbolInfo {
    pub symbol_type: Type,
    pub declaration_span: Range<usize>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }
}

impl SymbolTable {
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, info: SymbolInfo) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, info);
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&SymbolInfo> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }
}
