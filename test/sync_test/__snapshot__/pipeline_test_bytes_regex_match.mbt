// `=~` and `lexmatch` on `Bytes` and `BytesView` run through `BytesRegex`.

const METHOD = reb"(GET|POST)"

const SPACE : BytesRegex = reb" "

fn is_request(data : BytesView) -> Bool {
  data =~ (METHOD + SPACE)
}

fn method_and_rest(data : Bytes) -> String {
  if data =~ (reb"^[A-Z]+ " as head, after=rest) {
    "\{head.length()} \{rest.length()}"
  } else {
    "no match"
  }
}

fn classify(data : BytesView) -> String {
  lexmatch data {
    (reb"^\xef\xbb\xbf", after=rest) => "bom, \{rest.length()} left"
    (reb"^[\x80-\xff]" as high, after=rest) =>
      "high \{high}, \{rest.length()} left"
    (reb"\r\n", before=head, after=tail) =>
      "line of \{head.length()}, \{tail.length()} left"
    reb"^[a-z][0-9]+$" => "letter and digits"
    _ => "other"
  }
}

// Every input is matched, so a catch-all is not needed.
fn first_byte(data : BytesView) -> Int {
  lexmatch data {
    reb"^$" => -1
    (reb"^[\x00-\xff]" as b, after=_) => b.to_int()
  }
}

fn main {
  println(is_request(b"GET /"))
  println(is_request(b"PUT /"))
  println(is_request(b"xx POST /"[2:]))
  println(method_and_rest(b"GET /index"))
  println(method_and_rest(b"get /index"))
  println(classify(b"\xef\xbb\xbfabc"))
  println(classify(b"\xff\x00"))
  println(classify(b"one\r\ntwo"))
  println(classify(b"x123"))
  println(classify(b"!"))
  println(first_byte(b""))
  println(first_byte(b"\x00"))
  println(first_byte(b"\xfe\x41"))
  // The name of a binder is not a regex group name.
  if b"a" =~ (reb"a" as 字节) {
    println(字节)
  }
  let (l : Bytes) = b"key=\xe2\x82\xac"
  lexmatch l {
    ((reb"^[a-z]+" as k) + reb"=", after=v) => println("\{k.length()} \{v.length()}")
    _ => println("none")
  }
}
