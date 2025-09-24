use proc_macros::Trace;
use rustc_hash::FxBuildHasher;

use crate::{
    core::{
        error::{RuntimeError, RuntimeResult},
        ident::Ident,
        object::{Object, ObjectRef},
        symbol::{Symbol, SymbolCell, SymbolMap},
    },
    gc::Gc,
};

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
pub struct Bind {
    #[no_trace]
    pub symbol: Symbol,
    pub old: Object,
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
pub enum SpecStackItem {
    Bind(Bind),
    /// mark for start of a lexical envrionment
    LexicalMark,
    Unwind,
}

#[derive(Debug, Default)]
pub struct Environment {
    /// the obarray
    // TODO should this be GC'd? or make SymbolCell GC
    pub symbol_map: SymbolMap,

    pub spec_stack: Gc<Vec<SpecStackItem>>,

    pub stack_map: StackMap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FuncCellType {
    Function,
    Macro,
}

impl FuncCellType {
    fn ident(self) -> Ident {
        match self {
            FuncCellType::Function => Ident::special().function,
            FuncCellType::Macro => Ident::special().mcro,
        }
    }

    pub fn from_num(n: u64) -> Option<Self> {
        match n {
            0 => None,
            1 => Some(Self::Function),
            2 => Some(Self::Macro),
            _ => unreachable!(),
        }
    }
}

impl Environment {
    /// load symbol's value from the global symbol table. Neglect the stacked lexical values.
    /// this can be used to load function cells, as they are always global;
    /// or as the fallback for dynamic binding symbols, i.e. special symbol that created
    /// with `defvar` or `defconst`.
    pub fn load_symbol_with<F, T>(
        &self,
        symbol: Symbol,
        load_function_cell: Option<FuncCellType>,
        job: F,
    ) -> RuntimeResult<T>
    where
        F: FnOnce(&Object) -> RuntimeResult<T>,
    {
        self.symbol_map.get_symbol_cell_with(symbol, |cell| {
            let data = cell.data();
            match load_function_cell {
                Some(ty) => {
                    let ObjectRef::Cons(cons) = data.func.as_ref() else {
                        return Err(RuntimeError::wrong_type("cons", data.func.get_tag()));
                    };
                    let ObjectRef::Symbol(marker) = cons.car().as_ref() else {
                        return Err(RuntimeError::wrong_type("symbol", cons.car().get_tag()));
                    };

                    if marker.ident() == ty.ident() {
                        let val = cons.cdr();
                        return job(val);
                    } else {
                        return Err(RuntimeError::internal_error("not a function"));
                    }
                }
                None => {
                    tracing::debug!("loaded value: {:?}", data.value);
                    // let value = Object(data.value.0);
                    return job(&data.value);
                }
            }
        })
    }

    pub fn get_symbol_cell(&self, symbol: Symbol) -> Option<&SymbolCell> {
        self.symbol_map.get_symbol_cell(symbol)
    }

    pub fn get_or_init_symbol(&self, symbol: Symbol) -> &SymbolCell {
        self.symbol_map.get_or_init_symbol(symbol)
    }

    pub fn push_stackmap(&self, obj: &Object) {
        self.stack_map.push(obj);
    }

    pub fn pop_stackmap(&self, obj: &Object) {
        self.stack_map.pop(obj);
    }

    pub fn push_spec_stack(&self, item: SpecStackItem) {
        self.spec_stack.get_mut().push(item)
    }

    pub fn push_special_symbol(&self, symbol: Symbol, mut value: Object) -> RuntimeResult<()> {
        let old = self.symbol_map.get_symbol_cell_with(symbol, |cell| {
            let data = cell.data();
            let old = &mut value;
            std::mem::swap(&mut data.value, old);
            Ok(value)
        })?;
        self.push_spec_stack(SpecStackItem::Bind(Bind { symbol, old }));
        Ok(())
    }

    pub fn pop_spec_stack(&self) -> RuntimeResult<SpecStackItem> {
        let item = self.spec_stack.get_mut().pop();
        match item {
            Some(SpecStackItem::Bind(Bind { symbol, mut old })) => {
                old = self.symbol_map.get_symbol_cell_with(symbol, |cell| {
                    let data = cell.data();
                    let val = &mut old;
                    std::mem::swap(&mut data.value, val);
                    Ok(old)
                })?;
                Ok(SpecStackItem::Bind(Bind { symbol, old }))
            }
            Some(v) => Ok(v),
            None => Err(RuntimeError::InternalError {
                message: "cannot pop spec stack".to_string(),
            }),
        }
    }

    pub fn pop_lexical(&self) -> RuntimeResult<()> {
        while let item = self.pop_spec_stack()? {
            if matches!(item, SpecStackItem::LexicalMark) {
                return Ok(());
            }
        }
        return Err(RuntimeError::InternalError {
            message: "wrong structure for spec stack".to_string(),
        });
    }

    pub fn is_special(&self, symbol: Symbol) -> bool {
        self.symbol_map
            .get_symbol_cell(symbol)
            .map(|cell| cell.data().special)
            .unwrap_or(false)
    }

    /// declare a `defvar` or `defconst`
    fn declare_var(&self, ident: Ident) -> Symbol {
        self.symbol_map.intern(ident, true).0
    }
}

use scc::HashMap;
/// a pesudo stack map that tracks objects.
#[derive(Debug)]
pub struct StackMap {
    roots: HashMap<Object, u64, rustc_hash::FxBuildHasher>,
}

impl StackMap {
    pub fn new() -> Self {
        Self {
            roots: HashMap::with_capacity_and_hasher(128, FxBuildHasher::default()),
        }
    }

    pub fn push(&self, obj: &Object) {
        if obj.is_primitive() {
            return;
        }
        if let Some(mut entry) = self.roots.get_sync(obj) {
            let count = entry.get_mut();
            *count = *count + 1;
        } else {
            self.roots.insert_sync(obj.clone(), 1).unwrap();
        }
    }

    pub fn pop(&self, obj: &Object) {
        if obj.is_primitive() {
            return;
        }
        let mut need_remove = false;
        if let Some(mut entry) = self.roots.get_sync(obj) {
            let count = entry.get_mut();
            if *count > 1 {
                *count = *count - 1;
            } else {
                need_remove = true;
            }
        }
        if need_remove {
            self.roots.remove_sync(obj);
        }
    }
}

impl Default for StackMap {
    fn default() -> Self {
        Self::new()
    }
}
