fn escape_in_char() -> Unit {
  println(Repr('\u{5154}'))
  println(Repr('\u5154'))
  println(Repr('\u{4D}'))
  println(Repr('\n'))
  println(Repr('\r'))
  println(Repr('\b'))
  println(Repr('\t'))
  println(Repr('\\'))
  println(Repr('\f'))
  println(Repr('\/'))
}

fn escape_in_string() -> Unit {
  println(Repr("\u{5154}"))
  println(Repr("\u5154"))
  println(Repr("\u{4D}"))
  println(Repr("\n"))
  println(Repr("\r"))
  println(Repr("\b"))
  println(Repr("\t"))
  println(Repr("\\"))
  println(Repr("\f"))
  println(Repr("\/"))
}

fn escape_in_byte() -> Unit {
  println(b'\o115')
  println(b'\n')
  println(b'\r')
  println(b'\b')
  println(b'\t')
  println(b'\\')
  println(b'\f')
  println(b'\/')
}

fn escape_in_bytes() -> Unit {
  println(b"\o115")
  println(b"\n")
  println(b"\r")
  println(b"\b")
  println(b"\t")
  println(b"\\")
  println(b"\f")
  println(b"\/")
}

fn main {
  println("escape in char:")
  escape_in_char()
  println("escape in byte:")
  escape_in_byte()
  println("escape in string:")
  escape_in_string()
  println("escape in bytes:")
  escape_in_bytes()
}
