suberror E1 {
  Err1(Int)
  Err2(String)
}

fn f(x : Error) -> Unit {
  match x {
    E1::_ => ()
    @pkg.E2::_ => ()
    E1::_ as e => ignore(e)
    E1::_ | @pkg.E2::_ => ()
    _ => ()
  }
}

fn g(x : Error) -> Bool {
  x is E1::_
}

fn h(f : () -> Unit raise) -> Unit {
  try f() catch {
    E1::_ => ()
    _ => ()
  }
}
