///|
#valtype
struct E {
  a : Int
  b : Int64
  c : Double
} derive(Debug)

///|
enum F {
  A(E, String)
  B(Bytes, E)
} derive(Debug)

///|
suberror X {
  X(Int)
} derive(Debug)

///|
fn f(a : Int, b : Int64, c : Double) -> F raise X {
  if a > 0 {
    F::A(E::{ a, b, c }, "string")
  } else if a < 0 {
    F::B(b"bytes", E::{ a, b, c })
  } else {
    raise X(a)
  }
}

///|
fn main {
  debug(to_result(() => f(1, 2, 3.14)))
  debug(to_result(() => f(-1, 2, 3.14)))
  debug(to_result(() => f(0, 2, 3.14)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
