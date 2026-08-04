enum E {
  A
  B
  C
}

enum F {
  X(E)
  Y
}

fn f(x: F) -> Unit {
  match x {
    X((A with x = "a") | (B with x = "b") | (C with x = "xc")) => println(x)
    Y => println("y")
  }
}

fn main {
  f(X(A))
  f(X(B))
  f(X(C))
  f(Y)
}
