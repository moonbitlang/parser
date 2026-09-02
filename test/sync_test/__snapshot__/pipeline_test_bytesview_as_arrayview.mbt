fn cast(x : BytesView) -> ArrayView[Byte] = "%bytesview.as_arrayview"

fn sum(av : ArrayView[Byte]) -> Int {
  let mut acc = 0
  for i = 0; i < av.length(); i = i + 1 {
    acc = acc + av[i].to_int()
  }
  acc
}

fn main {
  let b : Bytes = b"hello world"
  // whole view
  let av = cast(b[:])
  println(av.length())
  println(av[0].to_int())
  // view with non-zero start
  let av2 = cast(b[6:9])
  println(av2.length())
  for i = 0; i < av2.length(); i = i + 1 {
    println(av2[i].to_int())
  }
  // subview of the casted view
  let av3 = av2[1:]
  println(av3[0].to_int())
  println(sum(av2))
}
