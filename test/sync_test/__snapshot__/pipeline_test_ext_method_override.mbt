
//!build: -w -27

trait I {
  f(Self) -> Unit
}

fn[X: I] use_I(x: X) -> Unit {
  x.f()
}

type T Int

fn f(self: T) -> Unit { println(self.inner()) }
impl I for T with f(x) { println(x.inner() + 1) }

fn init {
  let t: T = T(42)
  t.f() // 42
  T::f(t) // 42
  use_I(t) // 43
}
