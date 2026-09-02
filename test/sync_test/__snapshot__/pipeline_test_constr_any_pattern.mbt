suberror E1 {
  Err1(Int)
  Err2(String)
}

suberror S {
  SErr(Int)
}

fn thrower(i : Int) -> Unit raise {
  if i == 0 {
    raise Err1(42)
  } else if i == 1 {
    raise Err2("hello")
  } else if i == 2 {
    raise SErr(7)
  }
}

fn classify(i : Int) -> String {
  try {
    thrower(i)
    "no error"
  } catch {
    // `e` has type `E1`, so the inner match needs no catch-all
    E1::_ as e =>
      match e {
        Err1(n) => "E1.Err1(\{n})"
        Err2(s) => "E1.Err2(\{s})"
      }
    S::_ => "S"
    _ => "other"
  }
}

fn init {
  println(classify(0))
  println(classify(1))
  println(classify(2))
  println(classify(3))
}
