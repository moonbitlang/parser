///|
fn f(xs : @list.List[Int]) -> Unit {
  l~: for i in 0..=1 {
    for j in xs {
      if j == 0 {
        break l~
      }
    }
  } nobreak {
    println("else")
  }
}

///|
fn init {
  f(@list.List([1, 2, 3]))
  f(@list.List([0]))
}
