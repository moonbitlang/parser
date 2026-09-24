fn main {
  fn fact(n : Int) -> Int {
    match (if n <= 1 { Some(1) } else { None }) {
      Some(r) | (None with r = fact(n - 1)) => n * r
    }
  }
  println(fact(5))
}
