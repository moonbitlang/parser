///|
fn f(arr : ArrayView[Int]) -> Unit {
  debug(arr)
}

///|
fn g(arr : ArrayView[(Int, Int)]) -> Unit {
  debug(arr)
}

///|
fn main {
  let a : FixedArray[Int] = [1, 2, 3]
  f(a)
  let aa = a[:]
  f(aa)
  let b : FixedArray[(Int, Int)] = [(1, 2), (3, 4), (5, 6)]
  g(b)
  let bb = b[:]
  g(bb)
}
