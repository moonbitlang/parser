///|
fn f1(x : @array.View[@array.View[Int]]) -> Int {
  x.hash()
}

///|
fn f2(x : @array.View[@array.View[Int]]) -> Unit {
  for i in x {
    for j in i {
      println(j)
    }
  }
}

///|
fn main {
  let arr : @array.View[@array.View[Int]] = [[1, 2, 3], [4, 5, 6]]
  println(f1(arr))
  f2(arr)
}
