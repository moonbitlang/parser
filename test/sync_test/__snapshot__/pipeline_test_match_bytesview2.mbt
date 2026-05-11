///|
fn f(bs : BytesView) -> Unit {
  match bs {
    [b'a',  .. rest,  b'z'] => println(rest)
    [b'b', b'c',  .. rest,  b'x', b'y'] => println(rest)
    _ => println("other")
  }
}

///|
fn main {
  f(b"abcxyz")
  f(b"abcxyz"[1:5])
  f(b"bcxyza")
  f(b"bc123xy")
}
