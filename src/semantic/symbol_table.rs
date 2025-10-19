use crate::token::Type;
use std::collections::HashMap;

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Type>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, item_type: Type) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, item_type);
        }
    }
    pub fn lookup(&self, name: &str) -> Option<Type> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name).cloned())
    }
}
