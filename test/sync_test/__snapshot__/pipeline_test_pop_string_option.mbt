///|
pub fn probe() -> Int {
  let a : Array[String?] = [Some("a"), None]
  let _ = a.pop()
  a.length()
}

///|
fn main {
  println(probe())
}
