///|
fn array(it : Iter[Int], cond : Bool) -> Array[Int] {
  [ 0, ..if cond { it }, 42 ]
}

///|
fn iter(it : Iter[Int], cond : Bool) -> Iter[Int] {
  [| 0, ..if cond { it }, 42 |]
}

///|
fn main {
  debug(array([| 1, 2, 3 |], true))
  debug(array([| 1, 2, 3 |], false))
  debug(iter([| 1, 2, 3 |], true).collect())
  debug(iter([| 1, 2, 3 |], false).collect())
}
