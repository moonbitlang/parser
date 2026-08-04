// `with` patterns supply default bindings for or-pattern alternatives that do
// not structurally bind a variable.

fn area(x : Int?) -> Int {
  match x {
    Some(a) | (None with a = 0) => a
  }
}

fn sum2(x : (Int, Int)?) -> Int {
  match x {
    Some((a, b)) | (None with a = 10, b = 20) => a + b
  }
}

// The parentheses make only the `Nothing` alternative get the default
// `r = -1`.
enum Shape {
  Circle(Int)
  Square(Int)
  Nothing
}

fn describe(s : Shape) -> Int {
  match s {
    Circle(r) | Square(r) | (Nothing with r = -1) => r
  }
}

// A constant-constructor default, and a per-alternative default combined with a
// structurally-bound variable.
fn pick(x : Int?) -> Int {
  match x {
    (Some(a) with b = Some(1)) | (None with a = 100, b = Some(2)) =>
      match b {
        Some(v) => a + v
        None => a
      }
  }
}

// A `with` pattern nested inside a constructor argument, with its own
// or-pattern.
enum E {
  A(Int?)
  B
}

fn nested(e : E) -> Int {
  match e {
    A(Some(n) | (None with n = 7)) => n
    B => -1
  }
}

// `with` at two levels, with each occurrence immediately under its respective
// or-pattern.
enum MultiLevelInput {
  Value(Int?)
  Empty
}

fn multi_level(x : MultiLevelInput) -> Int {
  match x {
    (Value(Some(a) | (None with a = 1)) with b = 2)
    | (Empty with a = 3, b = 4) => a + b
  }
}

// `with` at each element of a tuple pattern.
fn deep(x : (Int?, Int?)) -> Int {
  match x {
    (Some(a) | (None with a = 1), Some(b) | (None with b = 2)) => a + b
  }
}

// A guard can reference a variable that is structurally bound in one
// alternative and `with`-defaulted in another; the guard runs after the
// defaults are materialized.
fn guarded(x : Int?) -> Int {
  match x {
    Some(a) | (None with a = 0) if a > 5 => a * 2
    _ => -1
  }
}

// Guard over a multi-binding `with`: exercises guard duplication across the
// decision tree together with `with`-bound values.
fn guarded_multi(x : (Int, Int)?) -> Int {
  match x {
    Some((a, b)) | (None with a = 1, b = 2) if a + b > 5 => a + b
    _ => 0
  }
}

// An or-pattern containing per-alternative `with` defaults in an `is`
// expression.
fn in_is(x : Int?) -> Int {
  if x is ((Some(a) with b = 100) | (None with a = 0, b = 100)) {
    a + b
  } else {
    -1
  }
}

// `with` in a `guard ... is`, making the or-pattern bind uniformly.
fn in_guard(x : Int?) -> Int {
  guard x is (Some(a) | (None with a = 0))
  a
}

fn init {
  println(area(Some(5)))
  println(area(None))
  println(sum2(Some((3, 4))))
  println(sum2(None))
  println(describe(Circle(2)))
  println(describe(Square(9)))
  println(describe(Nothing))
  println(pick(Some(41)))
  println(pick(None))
  println(nested(A(Some(3))))
  println(nested(A(None)))
  println(nested(B))
  println(multi_level(Value(Some(5))))
  println(multi_level(Value(None)))
  println(deep((Some(3), Some(4))))
  println(deep((None, Some(4))))
  println(deep((Some(3), None)))
  println(deep((None, None)))
  println(guarded(Some(10)))
  println(guarded(Some(3)))
  println(guarded(None))
  println(guarded_multi(Some((3, 4))))
  println(guarded_multi(Some((1, 1))))
  println(guarded_multi(None))
  println(in_is(Some(5)))
  println(in_is(None))
  println(in_guard(Some(5)))
  println(in_guard(None))
}
