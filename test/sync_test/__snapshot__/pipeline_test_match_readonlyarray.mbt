///|
fn[T : Debug] f(x : ReadOnlyArray[T]) -> Unit {
  match x {
    [a, .. rest, b] => {
      debug(a)
      debug(b)
      debug(rest)
    }
    [a] => debug(a)
    [] => println("empty")
  }
}

///|
fn f1(x : ReadOnlyArray[(Int, Int)]) -> Unit {
  if x is [(a, b)] {
    println(a)
    println(b)
  } else {
    debug(x)
  }
}

///|
let a : ReadOnlyArray[Int] = [1, 2, 3]

///|
fn main {
  f([(1, 2), (3, 4), (5, 6)])
  f([Some(78)])
  f(([] : ReadOnlyArray[Unit]))
  f1([(9, 10)])
  f1([])
  let b = [..a, 4, 5]
  debug(b)
}
