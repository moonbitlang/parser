///|
/// A package-private method shadowing the upstream `Array::length`. The
/// `xs.get(i) is Some(_)` peephole must not pick it up: it either uses the
/// methods defined by moonbitlang/core or bails out to the generic path.
fn Array::length(_self : Array[Int]) -> Int {
  0
}

///|
fn probe(xs : Array[Int], i : Int) -> Int {
  if xs.get(i) is Some(x) {
    x
  } else {
    -1
  }
}

///|
fn main {
  let xs = [7, 8]
  println(xs.length())
  println(probe(xs, 1))
  println(probe(xs, 5))
}
