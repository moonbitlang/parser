///|
fn single(x : Int?) -> Int {
  match x {
    Some(a) | (None with a = 0) => a
  }
}

///|
fn multi(x : (Int, Int)?) -> Int {
  match x {
    Some((a, b)) | (None with a = 1, b = 2) => a + b
  }
}

///|
/// A `with` default may be any constant: a literal, a negative literal, a
/// nullary constructor, or a constructor applied to constant arguments.
fn rhs_variety(x : Int?) -> Unit {
  match x {
    (Some(_) with a = Some(1)) => ignore(a)
    (_ with b = None, c = -1, d = 'x', e = "s", f = Some(Some(1))) => ignore(b)
  }
}

///|
/// A parenthesized `with` pattern can be used as a non-final constructor
/// argument without confusing its bindings with the argument-list comma.
fn in_constr_args(e : E) -> Unit {
  match e {
    A((Some(x) with a = 0), x = 1) => println(100)
    _ => ()
  }
}
