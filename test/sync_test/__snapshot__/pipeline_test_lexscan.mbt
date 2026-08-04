fn scan(lexbuf : @lexbuf.Lexbuf) -> String {
  lexscan lexbuf {
    (re"^[a-z]+") as word => "word:\{word}"
    re"^[0-9]+" => "int"
    re"^ +" => scan(lexbuf)
    _ => "other"
  }
}

fn main {
  let lexbuf = @lexbuf.Lexbuf::from_string("abc 123 ?!")
  println(scan(lexbuf))
  println(scan(lexbuf))
  println(scan(lexbuf))
  println(scan(lexbuf))
  println(lexbuf.__get_cursor())
}
