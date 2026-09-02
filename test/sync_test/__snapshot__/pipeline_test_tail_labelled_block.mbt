// Regression test for tail-call info of labelled blocks: a recursive call in
// `break label~ f(...)` position is a tail call when the block itself is in
// tail position. `countdown` only completes at this depth when the call is
// compiled as a tail call; with the tail flag lost it overflows the stack.

///|
fn factorial(x : Int, acc : Int) -> Int {
  l~: {
    if x == 0 {
      break l~ acc
    }
    break l~ factorial(x - 1, acc * x)
  }
}

///|
fn countdown(n : Int) -> Int {
  l~: {
    if n == 0 {
      break l~ 0
    }
    break l~ countdown(n - 1)
  }
}

///|
fn main {
  println(factorial(5, 1))
  println(countdown(1000000))
}
