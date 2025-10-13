//!build: -w -27

type T Int? derive (Show)

fn[X, Y] apply(f: (X) -> Y, x: X) -> Y {
  f(x)
}

fn init {
  let a = apply(T::T, Option::Some(1))
  println(a)
}
