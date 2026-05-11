// Test Option[T] where T is a function type

// --- Basic Option with function types ---

fn test_option_fn_unit(f : (() -> Unit)?) -> Unit {
  match f {
    Some(f) => f()
    None => println("None")
  }
}

fn test_option_fn_int(f : ((Int) -> Int)?) -> Unit {
  match f {
    Some(f) => println(f(10))
    None => println("None")
  }
}

fn test_option_fn_multi_arg(f : ((Int, Int) -> Int)?) -> Unit {
  match f {
    Some(f) => println(f(3, 4))
    None => println("None")
  }
}

// --- Option with higher-order function types ---

fn test_option_fn_returns_fn(f : ((Int) -> (Int) -> Int)?) -> Unit {
  match f {
    Some(f) => println(f(10)(20))
    None => println("None")
  }
}

fn test_option_fn_takes_fn(f : (((Int) -> Int) -> Int)?) -> Unit {
  match f {
    Some(f) => println(f(fn(x) { x * 2 }))
    None => println("None")
  }
}

// --- Option with closures (captured variables) ---

fn make_adder(n : Int) -> ((Int) -> Int)? {
  Some(fn(x) { x + n })
}

fn make_none_fn() -> ((Int) -> Int)? {
  None
}

fn test_option_closure() -> Unit {
  let add5 = make_adder(5)
  match add5 {
    Some(f) => println(f(10))
    None => println("None")
  }
  let none_fn = make_none_fn()
  match none_fn {
    Some(f) => println(f(10))
    None => println("None")
  }
}

// --- Nested Option with function types ---

fn test_nested_option_fn(f : ((Int) -> Int)??) -> Unit {
  match f {
    Some(Some(f)) => println(f(42))
    Some(None) => println("Some(None)")
    None => println("None")
  }
}

// --- Array of Option functions ---

fn test_array_option_fn(xs : Array[((Int) -> Int)?]) -> Unit {
  for x in xs {
    match x {
      Some(f) => println(f(1))
      None => println("None")
    }
  }
}

// --- Option with FuncRef (raw function type) ---

fn test_option_funcref(f : FuncRef[(Int) -> Int]?) -> Unit {
  match f {
    Some(f) => println(f(100))
    None => println("None")
  }
}

fn test_array_option_funcref(xs : Array[FuncRef[(Int) -> Int]?]) -> Unit {
  for x in xs {
    match x {
      Some(f) => println(f(5))
      None => println("None")
    }
  }
}

// --- Struct containing Option function fields ---

struct Handler {
  on_value : ((Int) -> Unit)?
  on_done : (() -> Unit)?
}

fn run_handler(h : Handler, value : Int) -> Unit {
  match h.on_value {
    Some(f) => f(value)
    None => println("no on_value")
  }
  match h.on_done {
    Some(f) => f()
    None => println("no on_done")
  }
}

// --- Option function passed through generic ---

fn[T, R] apply_option(f : ((T) -> R)?, x : T, default : R) -> R {
  match f {
    Some(f) => f(x)
    None => default
  }
}

// --- Option function stored in local variable and called later ---

fn test_option_fn_local() -> Unit {
  let mut f : ((Int) -> Int)? = None
  println(
    match f {
      Some(f) => f(1)
      None => -1
    },
  )
  f = Some(fn(x) { x * 10 })
  println(
    match f {
      Some(f) => f(1)
      None => -1
    },
  )
}

// --- Main ---

fn main {
  // Basic function types
  println("== option fn unit ==")
  test_option_fn_unit(Some(fn() { println("hello") }))
  test_option_fn_unit(None)

  println("== option fn int ==")
  test_option_fn_int(Some(fn(x) { x + 1 }))
  test_option_fn_int(None)

  println("== option fn multi arg ==")
  test_option_fn_multi_arg(Some(fn(a, b) { a * b }))
  test_option_fn_multi_arg(None)

  // Higher-order function types
  println("== option fn returns fn ==")
  test_option_fn_returns_fn(Some(fn(a) { fn(b) { a + b } }))
  test_option_fn_returns_fn(None)

  println("== option fn takes fn ==")
  test_option_fn_takes_fn(Some(fn(g) { g(21) }))
  test_option_fn_takes_fn(None)

  // Closures
  println("== option closure ==")
  test_option_closure()

  // Nested Option
  println("== nested option fn ==")
  test_nested_option_fn(Some(Some(fn(x) { x * 2 })))
  test_nested_option_fn(Some(None))
  test_nested_option_fn(None)

  // Array of Option functions
  println("== array option fn ==")
  test_array_option_fn([
    Some(fn(x) { x }),
    None,
    Some(fn(x) { x + 100 }),
    None,
  ])

  // FuncRef (raw function type)
  println("== option funcref ==")
  test_option_funcref(Some(fn(x) { x + 1 }))
  test_option_funcref(None)

  println("== array option funcref ==")
  test_array_option_funcref([
    Some(fn(x) { x * 2 }),
    None,
    Some(fn(x) { x * 3 }),
  ])

  // Struct with Option function fields
  println("== handler both ==")
  run_handler(
    { on_value: Some(fn(v) { println("value: \{v}") }), on_done: Some(fn() { println("done") }) },
    42,
  )

  println("== handler none ==")
  run_handler({ on_value: None, on_done: None }, 42)

  println("== handler partial ==")
  run_handler(
    { on_value: Some(fn(v) { println("got \{v}") }), on_done: None },
    99,
  )

  // Generic apply
  println("== apply option ==")
  println(apply_option(Some(fn(x : Int) { x * 3 }), 7, 0))
  println(apply_option(None, 7, 0))

  // Local mutable Option function
  println("== option fn local ==")
  test_option_fn_local()
}
