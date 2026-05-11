///|
fn f(x : &ToJson) -> Unit {
  debug(x.to_json())
}

///|
suberror E {
  E(Int)
}

///|
fn main {
  f((E(42) : Error))
}
