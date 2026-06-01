use mmtk::util::{conversions::raw_align_up, Address};

#[repr(C)]
#[derive(Clone, Copy)]
pub struct LocalAllocationBuffer {
    pub cursor: Address,
    pub limit: Address,
}

impl LocalAllocationBuffer {
    pub const fn new() -> Self {
        Self {
            cursor: Address::ZERO,
            limit: Address::ZERO,
        }
    }

    pub fn allocate(&mut self, size: usize, alignment: usize, offset: usize) -> Option<Address> {
        let size = raw_align_up(size, alignment);
        let start = align_allocation_no_fill(self.cursor, alignment, offset);
        if start + size > self.limit {
            return None;
        }

        self.cursor = start + size;
        Some(start)
    }

    pub fn rebind(&mut self, cursor: Address, limit: Address) {
        self.cursor = cursor;
        self.limit = limit;
    }

    pub fn take(&mut self) -> (Address, Address) {
        let cursor = self.cursor;
        let limit = self.limit;
        self.reset();
        (cursor, limit)
    }

    fn reset(&mut self) {
        self.cursor = Address::ZERO;
        self.limit = Address::ZERO;
    }
}

fn align_allocation_no_fill(region: Address, alignment: usize, offset: usize) -> Address {
    let mask = (alignment - 1) as isize;
    let neg_off = -(offset as isize);
    let delta = neg_off.wrapping_sub(region.as_usize() as isize) & mask;

    region + delta
}
