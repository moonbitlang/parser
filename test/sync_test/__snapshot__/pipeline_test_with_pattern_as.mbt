enum E {
  A(x~: Int)
  B
}

fn f(e: E) -> Unit {
  match e {
    (A(..) as a with x = 0) | (B with a = B, x = 0) =>
      match a {
        A(x=field) => println(field + x)
        B => println(x)
      }
  }
}

fn main {
  f(A(x=42))
}
