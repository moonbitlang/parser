enum A {
  X
}

fn f(a : A) -> Unit {
  match a {
    &A::X => ()
    _ => ()
  }
}

fn g(a : A) -> Unit {
  match a {
    &A::X(_) => ()
    &A::_ => ()
    &@pkg.T::Y(_) => ()
    &@pkg.T::_ => ()
    _ => ()
  }
}

