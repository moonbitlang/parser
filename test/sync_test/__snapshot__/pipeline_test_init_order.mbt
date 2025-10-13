//!build: -w -27
trait I {
  f(Self) -> Int
}

type T Int

let a: Int = g(T(1))

pub impl I for T with f(self: T) -> Int {
  self.inner() + b
}

pub fn[X:I] g(x: X) -> Int {
  x.f()
}

let b: Int = h(42)

pub fn h(x: Int) -> Int {
  x
}

fn main {
  println(a)
}
