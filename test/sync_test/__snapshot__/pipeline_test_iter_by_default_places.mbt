///|
fn tapped_range(lo : Int, hi : Int) -> Iter[Int] {
  lo.until(hi).tap(x => println("iter: generating(\{x})"))
}

///|
fn main {
  println("========")
  for x in [..tapped_range(0, 2), ..tapped_range(2, 4)] {
    println("loop: got element \{x}")
  }
  println("========")
  let it : Iter[Int] = [| ..tapped_range(0, 2), ..[..tapped_range(2, 4)] |]
  println("finished evaluating the iter")
  for x in it {
    println("loop: got element \{x}")
  }
}
