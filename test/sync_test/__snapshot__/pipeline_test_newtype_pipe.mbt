//!build: -w -27
type Arrow[T, B] (T) -> B

fn call(arr : Arrow[Int, Int]) -> Unit {
  (arr.inner())(1) |> println
}

fn increment(x : Int) -> Int {
  x + 1
}

fn main {
  call(Arrow(increment)) // Ok
  call(increment |> Arrow)
  let _ = increment |> Arrow
}
