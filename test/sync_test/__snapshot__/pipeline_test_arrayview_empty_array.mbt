///|
fn main {
  println(f([]))
  println(g([]))
}

///|
fn f(xs : Array[BytesView]) -> Array[BytesView] {
  xs.map(i => i)
}

///|
fn g(xs : Array[@array.View[Int]]) -> Array[@array.View[Int]] {
  xs.map(i => i)
}
