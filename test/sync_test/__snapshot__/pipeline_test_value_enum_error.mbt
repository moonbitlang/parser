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
  debug(try? f(5))
  debug(try? f(15))
  debug(try? f(25))
  debug(try? f(35))
}
