use std::{
    collections::HashMap,
    sync::{Arc, Mutex, MutexGuard},
};

use crate::gc::{
    JitRootFrame, MutatorThread, RootLink, StackMapProvider, StackRootVisitor,
    mmtk::util::{Address, VMMutatorThread},
    push_jit_root_frame, scan_root_chain,
};
use cranelift::codegen::ir::{AbiParam, Type, types};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::core::compiler::arch;

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

fn stack_map_for_ip(
    func_metas: &[FunctionMetadata],
    ip: usize,
) -> Option<(u32, Arc<Vec<StackMapSlot>>)> {
    func_metas.iter().find_map(|func_meta| func_meta.get(ip))
}

fn visit_stack_map_slots(
    sp: usize,
    stack_map: &[StackMapSlot],
    visitor: &mut dyn StackRootVisitor,
) {
    for slot in stack_map.iter() {
        if slot.is_tagged_object_word() {
            let address = unsafe { Address::from_usize(sp + slot.offset as usize) };
            visitor.visit_tagged_slot(address);
        }
    }
}

unsafe fn scan_cranelift_jit_frame(frame: &JitRootFrame, visitor: &mut dyn StackRootVisitor) {
    if frame.frame_pointer.is_null() || frame.return_pc.is_null() {
        return;
    }

    let stop_fp = frame.stop_frame_pointer as usize;
    let mut fp = frame.frame_pointer as usize;
    let mut ip = frame.return_pc as usize;
    let mut explicit_sp = (!frame.stack_pointer.is_null()).then_some(frame.stack_pointer as usize);
    let func_metas = FUNC_METAS.lock().unwrap();

    while fp != 0 && fp != stop_fp {
        let Some((span, stack_map)) = stack_map_for_ip(&func_metas, ip) else {
            break;
        };

        let sp = explicit_sp.take().unwrap_or(fp - span as usize);
        visit_stack_map_slots(sp, &stack_map, visitor);

        let Some(parent_fp) = parent_fp(fp) else {
            break;
        };
        if parent_fp <= fp {
            break;
        }

        fp = parent_fp;
        ip = unsafe { arch::return_addr_of_frame(fp as *const u8) as usize };
    }
}

struct CraneliftStackMapProvider;

impl StackMapProvider for CraneliftStackMapProvider {
    fn visit_stack_roots(&self, tls: VMMutatorThread, visitor: &mut dyn StackRootVisitor) {
        let thread = unsafe { MutatorThread::from_mutator_thread(tls) };
        let head = RootLink::from_bits(thread.root_head.get());

        unsafe {
            scan_root_chain(head, visitor, scan_cranelift_jit_frame);
        }
    }
}

static CRANELIFT_STACK_MAP_PROVIDER: CraneliftStackMapProvider = CraneliftStackMapProvider;

/// Install the root-chain scanner as MMTk's stack-map provider.
pub fn install_stack_map_provider() {
    crate::gc::set_stackmap_provider(&CRANELIFT_STACK_MAP_PROVIDER);
}

const JIT_NATIVE_CALL_STUB_PREFIX: &str = "jit_call_native_";
const MAX_JIT_NATIVE_CALL_ARGS: usize = 11;

macro_rules! define_jit_native_call_stub {
    ($name:ident, []) => {
        unsafe extern "C" fn $name(func: i64, thread: i64, dynamic: i64) -> i64 {
            unsafe {
                with_current_jit_root(thread, dynamic, || {
                    let func: unsafe extern "C" fn() -> i64 =
                        std::mem::transmute(func as *const u8);
                    func()
                })
            }
        }
    };
    ($name:ident, [$($arg:ident),+]) => {
        unsafe extern "C" fn $name(func: i64, thread: i64, dynamic: i64, $($arg: i64),+) -> i64 {
            unsafe {
                with_current_jit_root(thread, dynamic, || {
                    let func: unsafe extern "C" fn($($arg: i64),+) -> i64 =
                        std::mem::transmute(func as *const u8);
                    func($($arg),+)
                })
            }
        }
    };
}

define_jit_native_call_stub!(jit_call_native_0, []);
define_jit_native_call_stub!(jit_call_native_1, [a0]);
define_jit_native_call_stub!(jit_call_native_2, [a0, a1]);
define_jit_native_call_stub!(jit_call_native_3, [a0, a1, a2]);
define_jit_native_call_stub!(jit_call_native_4, [a0, a1, a2, a3]);
define_jit_native_call_stub!(jit_call_native_5, [a0, a1, a2, a3, a4]);
define_jit_native_call_stub!(jit_call_native_6, [a0, a1, a2, a3, a4, a5]);
define_jit_native_call_stub!(jit_call_native_7, [a0, a1, a2, a3, a4, a5, a6]);
define_jit_native_call_stub!(jit_call_native_8, [a0, a1, a2, a3, a4, a5, a6, a7]);
define_jit_native_call_stub!(jit_call_native_9, [a0, a1, a2, a3, a4, a5, a6, a7, a8]);
define_jit_native_call_stub!(jit_call_native_10, [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9]);
define_jit_native_call_stub!(
    jit_call_native_11,
    [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]
);

unsafe extern "C" fn jit_safepoint_poll_slow(thread: i64, dynamic: i64) -> i64 {
    unsafe {
        with_current_jit_root(thread, dynamic, || {
            let thread = &*(thread as *const crate::gc::MutatorThread);
            thread.safepoint_poll_slow();
            crate::core::object::NIL
        })
    }
}

unsafe fn with_current_jit_root(thread: i64, dynamic: i64, body: impl FnOnce() -> i64) -> i64 {
    let thread = unsafe { &*(thread as *const crate::gc::MutatorThread) };

    // Transition stubs are called directly from generated code. Their native
    // frame's saved parent frame pointer and return address identify the JIT
    // callsite that must be published while Rust may block for GC.
    let native_fp = unsafe { arch::current_frame_pointer() };
    let frame_pointer = unsafe { arch::parent_frame_pointer(native_fp) } as *const usize;
    let return_pc = unsafe { arch::return_addr_of_frame(native_fp) };
    let dynamic = dynamic as *const crate::core::env::DynamicEnv;
    let mut frame = JitRootFrame::new(frame_pointer, std::ptr::null(), return_pc, dynamic);
    let _guard = unsafe { push_jit_root_frame(thread, &mut frame) };

    body()
}

fn native_call_stub_name(arity: usize) -> String {
    format!("{JIT_NATIVE_CALL_STUB_PREFIX}{arity}")
}

fn native_call_stub_ptr(arity: usize) -> *const u8 {
    match arity {
        0 => jit_call_native_0 as *const u8,
        1 => jit_call_native_1 as *const u8,
        2 => jit_call_native_2 as *const u8,
        3 => jit_call_native_3 as *const u8,
        4 => jit_call_native_4 as *const u8,
        5 => jit_call_native_5 as *const u8,
        6 => jit_call_native_6 as *const u8,
        7 => jit_call_native_7 as *const u8,
        8 => jit_call_native_8 as *const u8,
        9 => jit_call_native_9 as *const u8,
        10 => jit_call_native_10 as *const u8,
        11 => jit_call_native_11 as *const u8,
        _ => panic!("unsupported JIT native call arity: {arity}"),
    }
}

fn declare_i64_import(module: &mut JITModule, name: &str, arity: usize) -> anyhow::Result<FuncId> {
    let mut sig = module.make_signature();
    for _ in 0..arity {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.returns.push(AbiParam::new(types::I64));
    Ok(module.declare_function(name, Linkage::Import, &sig)?)
}

pub fn register_jit_runtime_symbols(builder: &mut JITBuilder) {
    builder.symbol(
        "jit_safepoint_poll_slow",
        jit_safepoint_poll_slow as *const u8,
    );
    for arity in 0..=MAX_JIT_NATIVE_CALL_ARGS {
        let name = native_call_stub_name(arity);
        builder.symbol(&name, native_call_stub_ptr(arity));
    }
}

pub fn declare_jit_runtime_imports(
    module: &mut JITModule,
    builtin_funcs: &mut HashMap<String, FuncId>,
) -> anyhow::Result<()> {
    let safepoint = declare_i64_import(module, "jit_safepoint_poll_slow", 2)?;
    builtin_funcs.insert("jit_safepoint_poll_slow".to_string(), safepoint);

    for arity in 0..=MAX_JIT_NATIVE_CALL_ARGS {
        let name = native_call_stub_name(arity);
        let func_id = declare_i64_import(module, &name, arity + 3)?;
        builtin_funcs.insert(name, func_id);
    }

    Ok(())
}
