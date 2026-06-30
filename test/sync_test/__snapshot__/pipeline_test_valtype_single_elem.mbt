///|
#valtype
struct R {
  p : Int
} derive(Debug)

///|
fn m(r : R) -> Int {
  r.p
}

///|
fn n(r : R) -> R {
  { p: r.p + 1 }
}

///|
suberror E {
  E(R)
} derive(Debug)

///|
fn o(i : Int) -> R raise E {
  if i < 0 {
    raise E({ p: i })
  } else {
    { p: i }
  }
}

///|
fn main {
  let r = R::{ p: 1 }
  println(r |> m)
  debug(n(r))
  debug(to_result(() => o(42)))
  debug(to_result(() => o(-1)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
