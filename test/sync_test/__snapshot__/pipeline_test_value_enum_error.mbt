///|
#valtype
enum E {
  A
  B(Int)
  C(Double, Double)
} derive(Debug)

///|
suberror X {
  X(Int)
} derive(Debug)

///|
fn f(i : Int) -> E raise X {
  if i < 10 {
    A
  } else if i < 20 {
    B(i)
  } else if i < 30 {
    C(i.to_double(), i.to_double())
  } else {
    raise X(i)
  }
}

///|
fn main {
  debug(to_result(() => f(5)))
  debug(to_result(() => f(15)))
  debug(to_result(() => f(25)))
  debug(to_result(() => f(35)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
