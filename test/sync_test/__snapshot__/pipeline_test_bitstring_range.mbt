//!build: -w -27

fn f1(x: Bytes) -> Unit {
  match x {
    [ u3(a), .. ] => println(a)
    _ => println("f1 other")
  }
}

fn f2(x: Bytes) -> Unit {
  match x {
    [ u3(a) ] => println(a)
    _ => println("f2 other")
  }
}

fn main {
  f1(b"")
  f2(b"")
  f1(b"\xAA")
  f2(b"\xAA")
  f1(b"\xAA\xAA")
  f2(b"\xAA\xAA")

}
