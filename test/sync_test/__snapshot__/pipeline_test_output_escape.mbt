fn escape_in_char() -> Unit {
  println(to_repr('\u{5154}'))
  println(to_repr('\u5154'))
  println(to_repr('\u{4D}'))
  println(to_repr('\n'))
  println(to_repr('\r'))
  println(to_repr('\b'))
  println(to_repr('\t'))
  println(to_repr('\\'))
  println(to_repr('\f'))
  println(to_repr('\/'))
}

fn escape_in_string() -> Unit {
  println(to_repr("\u{5154}"))
  println(to_repr("\u5154"))
  println(to_repr("\u{4D}"))
  println(to_repr("\n"))
  println(to_repr("\r"))
  println(to_repr("\b"))
  println(to_repr("\t"))
  println(to_repr("\\"))
  println(to_repr("\f"))
  println(to_repr("\/"))
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
