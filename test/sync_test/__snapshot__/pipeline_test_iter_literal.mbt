///|
fn dump_iter(it : Iter[Int]) -> Unit {
  println("start traversing iter...")
  while it.next() is Some(elem) {
    println("got elem: \{elem}")
  }
}

///|
fn[X : Show] tee(x : X) -> X {
  println("tee(\{x})")
  x
}

///|
fn main {
  println("=====")
  dump_iter(Iter::tap([| tee(1), tee(2), tee(3), tee(4) |], println))
  println("=====")
  {
    let it = Iter::tap([| 42, 43, 44 |], println)
    dump_iter([|
      tee(1),
      tee(2),
      ..({ println("eval iter"); it }),
      tee(3),
      tee(4),
    |])
  }
  println("=====")
  {
    let it1 = Iter::tap([| 1, 2, 3 |], println)
    let it2 = Iter::tap([| 4, 5, 6 |], println)
    dump_iter([|
      ..({ println("eval iter 1"); it1 }),
      ..({ println("eval iter 2"); it2 }),
    |])
  }
}
