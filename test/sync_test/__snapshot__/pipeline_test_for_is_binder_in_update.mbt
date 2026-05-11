fn main {
  for i = Some(0); i is Some(a) && a < 10; i = Some(a + 1) {
    println(a)
  }
}
