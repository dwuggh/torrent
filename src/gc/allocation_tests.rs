use crate::core::{
    cons::Cons,
    function::{Function, FunctionInit, FunctionKind, FunctionSignature},
    indirect::Indirect,
    number::{Float, LispFloat, LispInteger},
    object::{Object, nil},
    string::Str,
    tag::Tag,
    vector::Vector,
};
use crate::gc::{ConstructObject, GcContext, Headered, HeapObject, Trace, Visitor, VisitorImpl};
use cranelift::prelude::EntityRef;

fn function_init(count: usize) -> FunctionInit {
    FunctionInit {
        signature: FunctionSignature::new(1, 2, false),
        id: cranelift_module::FuncId::new(0),
        code: std::ptr::dangling(),
        kind: FunctionKind::Jit,
        binding_len: count,
    }
}

#[test]
fn sizes_placement_and_function_tails() {
    let _mutator = super::test_support::bind_nogc_mutator();
    let mut ctx = GcContext::new();
    let car = ctx.root(LispInteger(42).tag());
    let cdr = ctx.root(nil());
    let cons = ctx.alloc::<Cons>((&car, &cdr));
    assert_eq!(cons.as_ref().size(), 16);
    assert_eq!(cons.as_ref().car(), car.get(&ctx));
    assert_eq!(
        unsafe { Cons::start_from_object_ref(cons.object_ref()) }.as_usize(),
        cons.as_ref() as *const Cons as usize
    );
    assert_eq!(Cons::PRIMARY_TAG, super::GcTag::CONS);
    let float = ctx.alloc::<Float>(3.5);
    assert_eq!(
        float.as_ref().size(),
        std::mem::size_of::<Headered<Float>>()
    );
    let string = ctx.alloc::<Str>("owned".to_owned());
    assert_eq!(string.as_ref().as_ref(), "owned");
    let indirect = ctx.alloc::<Indirect>(&car);
    assert_eq!(indirect.as_ref().load(), *car.get(&ctx));
    let roots = ctx.root_slice(&[*car.get(&ctx), nil()]);
    let vector = ctx.alloc::<Vector>(&roots);
    assert_eq!(vector.as_ref().0, vec![*car.get(&ctx), nil()]);

    struct Slots(Vec<super::mmtk::util::Address>);
    impl VisitorImpl for Slots {
        fn visit_slot_address(&mut self, address: super::mmtk::util::Address) {
            self.0.push(address);
        }
    }
    for count in [0, 1, 2, 7, 32, 257] {
        let init = function_init(count);
        let mut function = ctx.alloc::<Function>(init);
        assert_eq!(function.as_ref().size(), Function::size_from_init(&init));
        assert_eq!(
            function.as_ref().size(),
            std::mem::size_of::<Headered<Function>>() + count * 8
        );
        assert_eq!(function.as_ref().bindings(), vec![nil(); count]);
        assert_eq!(function.as_ref().check_args(1), (true, 3, false));
        let mut slots = Slots(Vec::new());
        unsafe {
            function.as_ref().trace(&mut Visitor::new(&mut slots));
        }
        assert_eq!(slots.0.len(), count);
        for (index, slot) in slots.0.iter().enumerate() {
            assert_eq!(
                slot.as_usize(),
                function.as_ref().bindings().as_ptr() as usize + index * 8
            );
        }
        if count > 0 {
            function
                .as_mut()
                .set_binding(count - 1, *car.get(&ctx))
                .unwrap();
            assert_eq!(
                function.as_ref().binding(count - 1).unwrap(),
                *car.get(&ctx)
            );
        }
        assert!(function.as_mut().set_binding(count, nil()).is_err());
        let metadata = super::metadata_for::<Function>();
        assert_eq!(
            unsafe { (metadata.size)(function.object_ref()) },
            function.as_ref().size()
        );
    }
}

/// Each child owns an independent MMTk instance; other unit tests use NoGC.
#[test]
fn moving_gc_constructors_and_calls() {
    if std::env::var_os("TORRENT_MOVING_GC_CHILD").is_none() {
        let output = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "gc::allocation_tests::moving_gc_constructors_and_calls",
                "--nocapture",
            ])
            .env("TORRENT_MOVING_GC_CHILD", "1")
            .env("RUST_LOG", "error")
            .env("MMTK_PLAN", "SemiSpace")
            .env("MMTK_THREADS", "1")
            .env("MMTK_GC_TRIGGER", "FixedHeapSize:33554432")
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "child failed: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        return;
    }
    let _ = tracing_subscriber::fmt()
        .with_max_level(tracing::Level::ERROR)
        .try_init();
    super::init();
    let mut ctx = GcContext::new();
    let value = LispFloat::new(19.5, &mut ctx).tag();
    let value = ctx.root(value);
    let old = value.get(&ctx).raw();
    let nil_root = ctx.root(nil());
    super::test_support::collect_on_alloc(true);
    let cons = ctx.alloc::<Cons>((&value, &nil_root));
    assert_ne!(
        value.get(&ctx).raw(),
        old,
        "SemiSpace must actually move the input"
    );
    assert_eq!(cons.as_ref().car(), value.get(&ctx));
    let cons = ctx.root_gc(cons);
    let indirect = ctx.alloc::<Indirect>(&value);
    assert_eq!(indirect.as_ref().load(), *value.get(&ctx));
    let indirect = ctx.root_gc(indirect);
    let slice = ctx.root_slice(&[*value.get(&ctx), Object::from_heap_ref(cons.get_gc(&ctx))]);
    let vector = ctx.alloc::<Vector>(&slice);
    assert_eq!(vector.as_ref().0[0], *value.get(&ctx));
    let vector = ctx.root_gc(vector);
    let mut function = ctx.alloc::<Function>(function_init(3));
    function
        .as_mut()
        .set_binding(0, Object::from_heap_ref(cons.get_gc(&ctx)))
        .unwrap();
    function
        .as_mut()
        .set_binding(1, Object::from_heap_ref(indirect.get_gc(&ctx)))
        .unwrap();
    function
        .as_mut()
        .set_binding(2, Object::from_heap_ref(vector.get_gc(&ctx)))
        .unwrap();
    let function = ctx.root_gc(function);
    super::test_support::collect();
    assert_eq!(
        function.get_gc(&ctx).binding(0).unwrap(),
        Object::from_heap_ref(cons.get_gc(&ctx))
    );
    assert_eq!(
        function.get_gc(&ctx).load_lexical(1).unwrap(),
        *value.get(&ctx)
    );
    assert_eq!(
        vector.get_gc(&ctx).0[1],
        Object::from_heap_ref(cons.get_gc(&ctx))
    );

    let env = crate::core::env::Environment::new(&mut ctx);
    let mut jit = crate::core::compiler::jit::JIT::new(&env, &mut ctx);
    // Every allocation below forces a full moving collection, including allocation
    // inside native callbacks reached through both fixed and argv JIT calls.
    for (source, expected) in [
        ("(+ 1 2 3)", 6),
        ("(car (cons 42 nil))", 42),
        (
            "(let ((f (lambda (x &optional y) (if y (+ x y) x)))) (funcall f 7))",
            7,
        ),
        (
            "(let ((f (lambda (x &optional y) (+ x y)))) (funcall f 7 8))",
            15,
        ),
        (
            "(let ((f (lambda (x &rest xs) (+ x (car xs))))) (funcall f 7 8 9))",
            15,
        ),
        (
            "(let ((f (lambda (a b c d e f g h i) (+ a i)))) (funcall f 1 2 3 4 5 6 7 8 9))",
            10,
        ),
        (
            "(let ((make (lambda (x) (lambda () x)))) (let ((a (funcall make 10)) (b (funcall make 20))) (+ (funcall a) (funcall b))))",
            30,
        ),
        (
            "(let ((x 0)) (let ((inc (lambda () (setq x (+ x 1)))) (get (lambda () x))) (funcall inc) (funcall inc) (funcall get)))",
            2,
        ),
        (
            "(let ((x 17)) (let ((make (lambda () (lambda () x)))) (let ((f (funcall make))) (funcall f))))",
            17,
        ),
        (
            "(let ((make (lambda (x) (lambda () (car x))))) (let ((a (funcall make (cons 42 nil)))) (cons 0 nil) (funcall a)))",
            42,
        ),
        (
            "(progn (defvar fib (lambda (x) (if (< x 2) x (+ (funcall fib (- x 1)) (funcall fib (- x 2)))))) (funcall fib 8))",
            21,
        ),
    ] {
        eprintln!("evaluating {source}");
        let result = jit.eval_test(source, &env);
        assert_eq!(result, LispInteger(expected).tag(), "{source}: {result:?}");
    }

    // Install JIT functions into function cells to exercise direct JIT dispatch,
    // including an older generated frame holding a heap value during a nested GC.
    for (name, source) in [
        ("jit-list", "(lambda (x) (cons x nil))"),
        (
            "jit-optional",
            "(lambda (x &optional y z) (if z z (if y y x)))",
        ),
        (
            "jit-rest",
            "(lambda (x &rest xs) (+ (car x) (car (car xs))))",
        ),
    ] {
        let value = jit.eval_test(source, &env);
        let value = ctx.root(value);
        crate::core::runtime::store_symbol_function(name.into(), &value, &env, &mut ctx);
    }
    for (source, expected) in [
        (
            "(let ((x (cons 20 nil))) (+ (car (jit-list 22)) (car x)))",
            42,
        ),
        ("(car (jit-list 42))", 42),
        ("(jit-optional 1)", 1),
        ("(jit-optional 1 2)", 2),
        ("(jit-optional 1 2 3)", 3),
        ("(jit-rest (cons 20 nil) (cons 22 nil))", 42),
        (
            "(let ((f (lambda (x) (let ((get (lambda () x))) (setq x (+ x 1)) (+ x (funcall get)))))) (funcall f 20))",
            42,
        ),
    ] {
        eprintln!("evaluating direct {source}");
        assert_eq!(
            jit.eval_test(source, &env),
            LispInteger(expected).tag(),
            "{source}"
        );
    }
    let expression = crate::core::parser::parse_src("[1.5 \"owned\" [2.5]]")
        .unwrap()
        .remove(0);
    let syntax = expression.to_object(&mut ctx);
    let syntax = ctx.root(syntax);
    super::test_support::collect();
    let crate::core::object::ObjectRef::Vector(vector) = syntax.get(&ctx).as_ref() else {
        panic!("expected vector")
    };
    assert_eq!(vector.len(), 3);
    let crate::core::object::ObjectRef::Float(value) = vector[0].as_ref() else {
        panic!("expected float")
    };
    assert_eq!(value.0, 1.5);
    let crate::core::object::ObjectRef::Str(value) = vector[1].as_ref() else {
        panic!("expected string")
    };
    assert_eq!(value.as_ref(), "owned");
    let expected = format!("{:?}", syntax.get(&ctx));
    let generated = jit.eval_test("[1.5 \"owned\" [2.5]]", &env);
    assert_eq!(format!("{generated:?}"), expected);

    super::test_support::collect_on_alloc(false);
}

impl crate::core::compiler::jit::JIT {
    fn eval_test(&mut self, source: &str, env: &crate::core::env::Environment) -> Object {
        let expressions = crate::core::parser::parse_src(source).unwrap();
        assert_eq!(expressions.len(), 1);
        let expression = &expressions[0];
        crate::core::parser::macro_expansion::expand_and_resolve_everything(expression, env)
            .unwrap();
        let code = self.compile_expr(expression).unwrap();
        let dynamic = crate::core::env::DynamicEnv {
            len: 0,
            parent: std::ptr::null_mut(),
            bindings: [],
        };
        // compile_expr emits precisely this top-level ABI, and self owns its code.
        let entry: unsafe extern "C" fn(
            *const crate::core::env::Environment,
            *const crate::core::env::DynamicEnv,
            Object,
        ) -> Object = unsafe { std::mem::transmute(code) };
        unsafe { entry(env, &dynamic, nil()) }
    }
}
