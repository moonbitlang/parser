#valtype
priv enum E {
  A
  N(Double, String)
}

fn g(a : E, b : E) -> Bool {
  match a {
    N(x, _) =>
      match b {
        N(y, _) => x == y
        _ => false
      }
    A => false
  }
}

fn main {
  println(g(N(1.0, "s"), N(1.0, "t")))
  println(g(N(1.0, "s"), A))
  println(g(A, A))
}
