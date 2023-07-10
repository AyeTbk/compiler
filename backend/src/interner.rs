use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(u32);

#[derive(Debug, Default)]
pub struct Interner {
    symbols: HashMap<String, Symbol>,
}

impl Interner {
    pub fn get(&self, s: &str) -> Option<Symbol> {
        self.symbols.get(s).copied()
    }

    pub fn get_or_intern(&mut self, s: &str) -> Symbol {
        if let Some(&symbol) = self.symbols.get(s) {
            symbol
        } else {
            let symbol = self.make_new_symbol();
            self.symbols.insert(s.to_string(), symbol);
            symbol
        }
    }

    pub fn symbol_as_str(&self, symbol: Symbol) -> &str {
        self.symbols
            .iter()
            .find(|(_, v)| **v == symbol)
            .map(|(k, _)| k)
            .expect("if the symbol exists, the entry should exist")
    }

    fn make_new_symbol(&mut self) -> Symbol {
        Symbol(self.symbols.len().try_into().unwrap())
    }
}
