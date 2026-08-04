pub fn f(s: Bytes, i: Int) -> Unit {
  match (s, i) {
    ([u8be(b), ..], 0) => println(b)
    _ => println("i")
  }
}

fn main {
  f(b"abcba", 0)
}
