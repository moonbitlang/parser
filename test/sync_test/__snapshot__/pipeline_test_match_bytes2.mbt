///|
fn f(x : Bytes) -> Unit {
  match x {
    [.. b"\xff\xff", .. rest] => println(rest)
    _ => println("other")
  }
}

///|
fn main {
  f(b"\xff\xff\xff")
  f(b"\x00")
}
