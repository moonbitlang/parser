///|
fn g(v : StringView) -> Int {
  ignore(v)
  2
}

///|
fn main {
  println(g("abc".view()))
  f()
}

let x : StringView = "abc"

fn f() -> Unit {
  ignore(x)
}

