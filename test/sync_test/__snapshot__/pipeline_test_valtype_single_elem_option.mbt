///|
/// Exercise the interaction between transparent value structs and `Option` for
/// both scalar and reference representations.
#valtype
struct IntBox {
  a : Int
} derive(Debug, Eq)

///|
#valtype
struct StrBox {
  a : String
} derive(Debug, Eq)

///|
fn unwrap_or(o : IntBox?, d : Int) -> Int {
  match o {
    Some(x) => x.a
    None => d
  }
}

///|
fn main {
  let some_i : IntBox? = Some(IntBox::{ a: 42 })
  let none_i : IntBox? = None
  println(unwrap_or(some_i, -1))
  println(unwrap_or(none_i, -1))
  println(some_i == Some(IntBox::{ a: 42 }))
  println(none_i is None)
  debug(some_i)
  debug(none_i)
  let some_s : StrBox? = Some(StrBox::{ a: "hi" })
  let none_s : StrBox? = None
  debug(some_s)
  debug(none_s)
  let nested : Option[Option[IntBox]] = Some(None)
  println(nested is Some(None))
  debug(Some(Some(IntBox::{ a: 7 })))
  let arr : Array[IntBox] = [IntBox::{ a: 1 }, IntBox::{ a: 2 }]
  debug(arr)
}
