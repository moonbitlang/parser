///|
const False : String = "😭False"

///|
const True : String = "true😭"

///|
fn f(s : String) -> Unit {
  match s {
    [.. False, .. rest, .. True ] => println(rest)
    _ => println(s)
  }
}

///| suffix reads must not use UTF-16 offsets before wide suffix chars match
fn f2(s : String) -> Unit {
  match s {
    [.. _, 'b', '😀', 'a'] => println("wide suffix")
    _ => println("other")
  }
}

///|
fn main {
  f("false")
  f([..False, '😭', ..True])
  f2("b😀a")
  f2("bxa")
}
