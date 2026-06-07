use crate::{
    core::{
        indirect::Indirect,
        object::{Object, nil, write_object_slot},
    },
    gc::{self, Trace, Visitor},
};

#[repr(C)]
#[derive(Debug)]
pub struct LexicalBindings {
    pub ptr: *mut Object,
    pub len: usize,
    variables: Vec<Object>,
}

impl LexicalBindings {
    pub fn empty() -> Self {
        Self::from_vec(Vec::new())
    }

    pub fn with_len(len: usize) -> Self {
        Self::from_vec(vec![nil(); len])
    }

    pub fn from_vec(variables: Vec<Object>) -> Self {
        let mut bindings = Self {
            ptr: std::ptr::null_mut(),
            len: 0,
            variables,
        };
        bindings.refresh_header();
        bindings
    }

    fn refresh_header(&mut self) {
        self.ptr = self.variables.as_mut_ptr();
        self.len = self.variables.len();
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn as_ptr(&self) -> *const Object {
        self.ptr as *const Object
    }

    pub fn as_mut_ptr(&mut self) -> *mut Object {
        self.ptr
    }

    pub fn get_raw(&self, index: usize) -> Option<Object> {
        self.variables.get(index).cloned()
    }

    pub fn load(&self, index: usize) -> Option<Object> {
        let value = self.get_raw(index)?;
        if Indirect::is_object(value.clone()) {
            Some(unsafe { Indirect::from_object(value).load() })
        } else {
            Some(value)
        }
    }

    pub fn set_initial(&mut self, index: usize, value: Object) {
        self.variables[index] = value;
        self.refresh_header();
    }

    pub fn store_with_owner(
        &mut self,
        owner: gc::mmtk::util::ObjectReference,
        index: usize,
        value: Object,
    ) {
        let slot = &mut self.variables[index];
        if Indirect::is_object(slot.clone()) {
            unsafe { Indirect::from_object_mut(slot.clone()) }.store(value);
        } else {
            write_object_slot(owner, slot, value);
        }
        self.refresh_header();
    }
}

impl Default for LexicalBindings {
    fn default() -> Self {
        Self::empty()
    }
}

unsafe impl Trace for LexicalBindings {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        for object in &self.variables {
            unsafe { object.trace(visitor) };
        }
    }
}
