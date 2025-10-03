use std::{
    cell::Cell,
    sync::{Arc, Mutex, MutexGuard},
};

use proc_macros::internal_fn;

use crate::core::{compiler::arch, object::Object};

fn parent_fp(fp: usize) -> Option<usize> {
    let parent_fp = unsafe { arch::parent_frame_pointer(fp as *const u8) };
    if parent_fp.is_null() {
        None
    } else {
        Some(parent_fp as usize)
    }
}

#[derive(Clone, Copy)]
struct Frame {
    fp: usize,
    ip: usize,
}

impl std::fmt::Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fp = self.fp as *const u8;
        let ip = self.ip as *const u8;
        f.debug_struct("Frame")
            .field("fp", &fp)
            .field("ip", &ip)
            .finish()
    }
}

impl Frame {
    fn new(fp: usize) -> Self {
        let ip = unsafe { arch::return_addr_of_frame(fp as *const u8) as usize };
        Self { fp, ip }
    }

    fn next(self) -> Option<Self> {
        parent_fp(self.fp).map(Frame::new)
    }
}

pub struct FunctionMetadata {
    pub start: usize,
    pub end: usize,

    /// Each entry is an `(offset, span, stack_map)` triple. Entries are sorted
    /// by code offset, and each stack map covers `span` bytes on the stack.
    pub stack_maps: Vec<(u32, u32, Arc<Vec<u32>>)>,
}

impl std::fmt::Debug for FunctionMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "function segment: {:p} {:p}",
            self.start as *const u8, self.end as *const u8
        )?;
        for (offset, span, sm) in self.stack_maps.iter() {
            let offset = *offset;
            // writeln!(f, "{:p} at {:p}, with span: {span}, stack map: {sm:?}", offset as *const u8, (offset as usize + self.start) as *const u8)?;
        }
        Ok(())
    }
}

impl FunctionMetadata {
    pub fn new(
        func_ptr: *const u8,
        func_size: usize,
        stack_maps: Vec<(u32, u32, Vec<u32>)>,
    ) -> Self {
        let start = func_ptr as usize;
        let end = start + func_size;
        let stack_maps = stack_maps
            .into_iter()
            .map(|(o, s, sm)| (o, s, Arc::new(sm)))
            .collect::<Vec<_>>();
        Self {
            start,
            end,
            stack_maps,
        }
    }

    #[inline]
    fn get(&self, ip: usize) -> Option<(u32, Arc<Vec<u32>>)> {
        if self.start <= ip && ip < self.end {
            let off = (ip - self.start) as u32;
            let idx = self
                .stack_maps
                .binary_search_by_key(&off, |(o, _, _)| *o)
                .ok()?;

            let (_, span, ref map) = self.stack_maps[idx];
            return Some((span, map.clone()));
        }
        None
    }
}

static FUNC_METAS: Mutex<Vec<FunctionMetadata>> = Mutex::new(Vec::new());

pub fn append_func_metadata(metadata: FunctionMetadata) {
    FUNC_METAS.lock().unwrap().push(metadata);
}

#[derive(Debug)]
pub struct StackMapIter<'s> {
    current_frame: Option<Frame>,
    stop_fp: usize,
    func_metas: MutexGuard<'s, Vec<FunctionMetadata>>,
}

impl<'s> Iterator for StackMapIter<'s> {
    type Item = (usize, Arc<Vec<u32>>);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(frame) = self.current_frame {
            if frame.fp == self.stop_fp {
                self.current_frame = None;
                return None;
            }
            tracing::info!("iterating: {:?}", self.current_frame);
            let next_frame = frame.next();
            let fp = parent_fp(frame.fp)?;
            for func_meta in self.func_metas.iter() {
                tracing::info!("metadata: {func_meta:?}");
                if let Some((span, stack_map)) = func_meta.get(frame.ip) {
                    let sp = fp - span as usize;
                    // Advance the frame before yielding, keeping the borrow scoped
                    // to this loop iteration so the returned reference remains valid.
                    self.current_frame = next_frame;
                    return Some((sp, stack_map));
                }
            }

            // No stack map for this frame; advance and continue.
            self.current_frame = next_frame;
        }

        None
    }
}

pub fn stack_map_lookup<'a>(start_fp: usize, stop_fp: usize) -> StackMapIter<'a> {
    let func_metas = FUNC_METAS.lock().unwrap();

    StackMapIter {
        current_frame: Some(Frame::new(start_fp)),
        stop_fp,
        func_metas,
    }
}

thread_local! {
    static TRAMPOLINE_START_FP: Cell<usize> = Cell::new(0);
}

#[internal_fn]
fn set_trampoline_start_fp(fp: i64) {
    tracing::info!("start fp: {:x}", fp);
    TRAMPOLINE_START_FP.set(fp as usize);
}

#[internal_fn]
fn gcroot_scan(start_fp: i64) {
    tracing::info!("calling gcroot");
    let stop_fp = TRAMPOLINE_START_FP.get();
    let iter = stack_map_lookup(start_fp as usize, stop_fp);
    for (sp, sm) in iter {
        tracing::info!("sp: {sp} {sm:?}");
        for offset in sm.iter() {
            let loc = sp + *offset as usize;
            let val = unsafe { *(loc as *const u64) };
            let obj = Object(val);
            tracing::info!("{obj:?}");
            std::mem::forget(obj);
        }
    }
}

#[internal_fn]
fn sm_dbg(ip: i64) {
    tracing::info!("ip from cranelift: {:?}", ip as *const u8);
}
