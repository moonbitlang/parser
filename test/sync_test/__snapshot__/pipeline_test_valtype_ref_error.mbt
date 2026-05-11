///|
#valtype
struct S {
  a : Int
  b : Bytes
  c : Int64?
  d : Bool
} derive(Debug)

///|
suberror E {
  E(Int)
} derive(Debug)

///|
fn make_s(i : Int) -> S raise E {
  if i < 0 {
    raise E(i)
  } else {
    S::{
      a: i,
      b: Bytes::makei(i, i => i.to_byte()),
      c: Some(i.to_int64()),
      d: true,
    }
  }
}

///|
fn main {
  debug(try? make_s(-1))
  debug(try? make_s(5))
}
