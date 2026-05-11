///|
fn sum_view(xs : ArrayView[Int]) -> Int {
  let mut acc = 0
  for i = 0; i < xs.length(); i = i + 1 {
    acc += xs[i]
  }
  acc
}

///|
fn main {
  let bool_ro : ReadOnlyArray[Bool] = [true]
  println(bool_ro[0])

  let ints : ArrayView[Int] = [1, 2, 3, 4]
  println(sum_view(ints))
}
