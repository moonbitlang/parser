//!build: -w -deprecated_syntax
///|
suberror LE {
  LE(Int)
} derive(Debug)

///| basic test
fn main {
  struct LT(Int) derive(Debug)
  fn f1(x : Int) -> LT raise LE {
    if x > 0 {
      LT(x)
    } else {
      raise LE(x)
    }
  }
  println(to_repr(to_result(() => f1(42))))
  println(to_repr(to_result(() => f1(-1))))
}

// test recursive newtype
fn init {
  struct T((S) -> Int)
  struct S(T)
  letrec f = fn(t: T) -> Int {
    (t.0)(S(t))
  }
  and use_t = fn(t: T) -> Int {
    f(t)
  }
  and use_s = fn(s: S) -> Int {
    f(s.0)
  }
  let t : T = T(fn (_x) { 42 })
  println(use_t(t))
  let t = T::T(fn (_x) { 43 })
  println(use_t(t))
  let s = S(t)
  println(use_s(s))
}

fn[X: Show, Y: Show] f(a: X, b: Y) -> Unit {
  struct A(B, Int) derive(Debug)
  struct B {
    f1: Array[C]
    f2: String
  } derive(Debug)
  enum C {
    C1
    C2(A)
  } derive(Debug)
  let c1 = C1
  let b1 = { f1: [c1], f2: "b1"}
  let a1 = (b1, 43)
  let c2 = C2(A(b1, 43))
  let b2 = { f1: [c1, c2], f2: "b2"}
  let a2 = (b2, 44)
  
  println(a)
  println(b)
  println(to_repr(a1))
  println(to_repr(a2))
  println(to_repr(b1))
  println(to_repr(b2))
  println(to_repr(c1))
  println(to_repr(c2))
}

///|
fn init {
  f("xxx", 42)
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
