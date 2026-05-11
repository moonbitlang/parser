fn main {
  fn f(a, b) -> Int {
    a + b 
  }
  println(f({println(1); 1}, {println(2); 2}))
}
