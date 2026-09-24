#non_exhaustive
enum E {
  A
  B
}

fn f(e : E?) -> Int {
  match e {
    None | Some(A) => 1
    Some(B) => 2
    Some(E::..) => 0
  }
}
