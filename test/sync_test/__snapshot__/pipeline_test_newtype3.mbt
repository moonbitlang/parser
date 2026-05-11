//!build: -alert -deprecated

struct T(Int?) derive(Debug)

fn[X, Y] apply(f: (X) -> Y, x: X) -> Y {
  f(x)
}

fn init {
  let a = apply(a => T::T(a), Option::Some(1))
  debug(a)
}
