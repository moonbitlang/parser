fn main {
  let word = "bc"
  let number = 123
  let writer = Buffer(size_hint=1)
  writer <+ b"a\{word}d\{number}"
  println(writer.to_bytes())
  debug(writer.to_bytes().to_array())


  let plain = Buffer(size_hint=1)
  plain <+ b"abcd123"
  println(plain.to_bytes())
  debug(plain.to_bytes().to_array())

  let conditional = Buffer(size_hint=1)
  let some = Some(conditional)
  some <? b"abcd\{number}"
  println(conditional.to_bytes())
  debug(conditional.to_bytes().to_array())

  let escaped = Buffer(size_hint=1)
  escaped <+ b"\x7B\{number}\x7D\o000\xFF"
  println(escaped.to_bytes())
  debug(escaped.to_bytes().to_array())
}
