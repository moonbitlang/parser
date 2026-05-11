fn init {
  let v = ["a", "b"]
  v.push("c")
  v.push("d")
  debug(v)
  println(v[0])
}

fn init {
  let v = [Bytes::from_array([b'\x41', b'\x00']), Bytes::from_array([b'\x42', b'\x00'])]
  v.push(Bytes::from_array([b'\x43', b'\x00']))
  v.push(Bytes::from_array([b'\x44', b'\x00']))
  debug(v)
  println(v[0][0])
  println(v[3][0])
}
