use lasso::Spur;
use std::marker::PhantomData;

#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Ident {
    pub(crate) spur: Spur,
    phantom: PhantomData<()>,
}

impl Ident {
    pub fn new(spur: Spur) -> Self {
        Self {
            spur,
            phantom: PhantomData,
        }
    }
    
    pub fn name<'a>(&self, interner: &'a lasso::ThreadedRodeo) -> &'a str {
        interner.resolve(&self.spur)
    }
    
    pub fn from_string(name: &str, interner: &mut lasso::ThreadedRodeo) -> Self {
        let spur = interner.get_or_intern(name);
        Self::new(spur)
    }
}
