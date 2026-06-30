#locals(f)
fn rec(f: (Int) -> Int, n: Int) -> Unit {
  if n <= 10 {
    for i in 0..<n {
      println(f(i))
    }
  } else {
    rec(i => i + 1, n-1)
  }
}

fn main {
  rec(i => i + 1, 10)
}
