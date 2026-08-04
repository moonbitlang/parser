fn chunked(chunks : ReadOnlyArray[String]) -> @lexbuf.Lexbuf {
  let mut chunk_index = 0
  @lexbuf.Lexbuf::from_fn(fn() {
    if chunk_index < chunks.length() {
      let chunk = chunks[chunk_index]
      chunk_index += 1
      Some(chunk)
    } else {
      None
    }
  })
}

fn scan_capture(lexbuf : @lexbuf.Lexbuf) -> String {
  lexscan lexbuf {
    (re"^(?:.|$)" as captured) => captured.to_owned()
    _ => "capture:other"
  }
}

fn scan_longest(lexbuf : @lexbuf.Lexbuf) -> String {
  lexscan lexbuf with longest {
    re"^(?:.|$)" => "longest:any"
    re"^abc" => "longest:abc"
    _ => "longest:other"
  }
}

fn scan_dead_catchall(lexbuf : @lexbuf.Lexbuf) -> String {
  lexscan lexbuf {
    re"^.*?$" => "covered"
    _ => "unreachable"
  }
}

fn main {
  let capture = chunked(["x", "y"])
  println(scan_capture(capture))
  println(capture.__get_cursor())

  let longest = chunked(["a", "b", "x"])
  println(scan_longest(longest))
  println(longest.__get_cursor())

  let dead_catchall = chunked(["x", "y"])
  println(scan_dead_catchall(dead_catchall))
  println(dead_catchall.__get_cursor())
}
