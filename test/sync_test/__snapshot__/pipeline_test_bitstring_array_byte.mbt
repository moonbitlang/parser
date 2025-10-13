///|
fn f(x : Array[Byte]) -> Unit {
  match x {
    [u8(a), u16(b), u32(c), u64(d), bytes(16, bs), .. rest] => {
      println(a.to_be_bytes())
      println(b.to_be_bytes())
      println(c.to_be_bytes())
      println(d.to_be_bytes())
      println(bs)
      println(rest)
    }
    _ => ()
  }
}

///|
fn g(x : ArrayView[Byte]) -> Unit {
  match x {
    [u8le(a), u16le(b), u32le(c), u64le(d), bytes(16, bs), .. rest] => {
      println(a.to_be_bytes())
      println(b.to_be_bytes())
      println(c.to_be_bytes())
      println(d.to_be_bytes())
      println(bs)
      println(rest)
    }
    _ => ()
  }
}

///|
fn main {
  let arr : Array[Byte] = Array::makei(36, i => i.to_byte())
  f(arr)
  let arr2 : ArrayView[Byte] = arr[:]
  g(arr2)
}
