fn main {
  let _ = writer <+ b"a\{x}c"
  let _ = maybe <? b"a\{x}c"
  let _ = writer <+ b"abc"
  // nested bytes interpolation
  writer <+ b"123\{w <+ b"456"}321"
  writer <+ b"123\{w <+ b"456\{w <+ b"789"}654"}321"
}
