///|
fn bytes_contains(s : BytesView, c : Byte) -> Bool {
  for a in s {
    if a == c {
      return true
    }
  } nobreak {
    false
  }
}

///|
const A : Bytes = b"HELLO"

///|
fn main {
  println(bytes_contains(b"中文", b'\xE4'))
  println(bytes_contains(A, b'L'))
  println(bytes_contains(Bytes::make(5, b'a'), b'e'))
  println(bytes_contains([b'A', ..A], b'e'))
}
