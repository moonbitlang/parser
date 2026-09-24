///|
#non_exhaustive
enum NonEx {
  A
  B
  C
}

///|
// simulate the case where `C` is a newly introduced constructor
#warnings("-partial_match")
fn non_ex(e : NonEx) -> Unit {
  match e {
    A | B => println("normal branch hit")
    NonEx::.. => println("unknown constructor branch hit")
  }
}

///|
enum Ex {
  A
  B
  C
}

///|
// simulate the case where upstream removes `#non_exhaustive`
#warnings("-unknown_constr_pat_on_exhaustive_type")
fn ex(e : Ex) -> Unit {
  match e {
    A | B => println("normal branch hit")
    Ex::.. => println("unknown constructor branch hit")
  }
}

///|
fn main {
  println("====")
  non_ex(A)
  non_ex(C)
  println("====")
  ex(A)
  ex(C)
}
