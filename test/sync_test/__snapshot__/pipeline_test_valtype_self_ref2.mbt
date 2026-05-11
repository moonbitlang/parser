///|
#valtype
pub(all) struct T {
  p : (T, T)?
  i : Int
} derive(Debug)

///|
pub fn q(t : T) -> T {
  if t.p is Some(p) {
    p.0
  } else {
    t
  }
}

///|
fn main {
  let t1 = T::{ p: None, i: 1 }
  let t2 = T::{ p: Some((t1, t1)), i: 2 }
  let t3 = T::{ p: Some((t2, t2)), i: 3 }
  debug(t1 |> q)
  debug(t2 |> q)
  debug(t3 |> q)
}
