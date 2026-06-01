use std::{
    cell::Cell,
    collections::HashMap,
    sync::{Arc, Mutex, MutexGuard},
};

use cranelift::codegen::ir::{types, Type};
use crate::gc::{
    mmtk::util::{Address, VMMutatorThread},
    StackMapProvider, StackRootVisitor,
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
    pub stack_maps: Vec<(u32, u32, Arc<Vec<StackMapSlot>>)>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StackMapSlot {
    /// Cranelift IR type for the value stored in this stack slot.
    pub ty: Type,
    /// Offset from the active stack pointer reported by Cranelift.
    pub offset: u32,
}

impl StackMapSlot {
    pub fn new(ty: Type, offset: u32) -> Self {
        Self { ty, offset }
    }

    /// Torrent represents every Lisp `Object` as one tagged 64-bit machine word.
    pub fn is_tagged_object_word(&self) -> bool {
        self.ty == types::I64
    }
}

impl std::fmt::Debug for FunctionMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "function segment: {:p} {:p}",
            self.start as *const u8, self.end as *const u8
        )?;
        for (offset, span, sm) in self.stack_maps.iter() {
            writeln!(
                f,
                "  stack map @ {:p}, span: {span}, slots: {sm:?}",
                (*offset as usize + self.start) as *const u8
            )?;
        }
        Ok(())
    }
}

impl FunctionMetadata {
    pub fn new(
        func_ptr: *const u8,
        func_size: usize,
        stack_maps: Vec<(u32, u32, Vec<StackMapSlot>)>,
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
    fn get(&self, ip: usize) -> Option<(u32, Arc<Vec<StackMapSlot>>)> {
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
    type Item = (usize, Arc<Vec<StackMapSlot>>);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(frame) = self.current_frame {
            if frame.fp == self.stop_fp {
                self.current_frame = None;
                return None;
            }
            tracing::info!("iterating: {:?}", self.current_frame);
            let next_frame = frame.next();
            for func_meta in self.func_metas.iter() {
                tracing::info!("metadata: {func_meta:?}");
                if let Some((span, stack_map)) = func_meta.get(frame.ip) {
                    // Cranelift reports stack-map entries as offsets from the
                    // active SP, and `span` is the active FP-to-SP distance.
                    let sp = frame.fp - span as usize;
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

#[derive(Clone, Copy)]
struct StackScanBounds {
    start_fp: usize,
    stop_fp: usize,
}

static STACK_SCAN_BOUNDS: std::sync::LazyLock<Mutex<HashMap<usize, StackScanBounds>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

fn mutator_key(tls: VMMutatorThread) -> usize {
    let vmthread = tls.0;
    vmthread.0.to_address().as_usize()
}

fn record_current_stack_scan_bounds(start_fp: usize, stop_fp: usize) {
    let tls = crate::gc::current_mutator_tls();
    STACK_SCAN_BOUNDS
        .lock()
        .unwrap()
        .insert(mutator_key(tls), StackScanBounds { start_fp, stop_fp });
}

pub fn visit_stack_roots_from_frame(
    start_fp: usize,
    stop_fp: usize,
    visitor: &mut dyn StackRootVisitor,
) {
    for (sp, slots) in stack_map_lookup(start_fp, stop_fp) {
        for slot in slots.iter() {
            let address = unsafe { Address::from_usize(sp + slot.offset as usize) };
            visitor.visit_tagged_slot(address);
        }
    }
}

pub struct CraneliftStackMapProvider;

impl StackMapProvider for CraneliftStackMapProvider {
    fn visit_stack_roots(&self, tls: VMMutatorThread, visitor: &mut dyn StackRootVisitor) {
        let Some(bounds) = STACK_SCAN_BOUNDS
            .lock()
            .unwrap()
            .get(&mutator_key(tls))
            .copied()
        else {
            return;
        };

        visit_stack_roots_from_frame(bounds.start_fp, bounds.stop_fp, visitor);
    }
}

static CRANELIFT_STACK_MAP_PROVIDER: CraneliftStackMapProvider = CraneliftStackMapProvider;

pub fn install_stack_map_provider() {
    crate::gc::set_stackmap_provider(&CRANELIFT_STACK_MAP_PROVIDER);
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
        for slot in sm.iter() {
            let loc = sp + slot.offset as usize;
            let val = unsafe { *(loc as *const u64) };
            let obj = Object(val);
            tracing::info!("{obj:?}");
            std::mem::forget(obj);
        }
    }
    record_current_stack_scan_bounds(start_fp as usize, stop_fp);
}

#[internal_fn]
fn sm_dbg(ip: i64) {
    tracing::info!("ip from cranelift: {:?}", ip as *const u8);
}
