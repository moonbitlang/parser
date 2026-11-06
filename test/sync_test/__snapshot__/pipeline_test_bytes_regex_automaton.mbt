// Bytes run through an automaton: several cases, captures, `with longest`.

const SPACE = reb" "

fn dispatch(d : BytesView) -> String {
  lexmatch d {
    (reb"^\xef\xbb\xbf", after=r) => "bom, \{r.length()} left"
    (reb"^[\x80-\xff]" as high, after=r) => "high \{high}, \{r.length()} left"
    (reb"\r\n", before=head, after=tail) =>
      "line of \{head.length()}, \{tail.length()} left"
    reb"^[a-z][0-9]+$" => "letter and digits"
    (reb"^[A-Z]+" as verb, after=r) =>
      "verb of \{verb.length()}, \{r.length()} left"
    _ => "other"
  }
}

// One byte, then the bytes around it.
fn captures(d : BytesView) -> String {
  lexmatch d {
    ((reb"[a-z]" as first) + SPACE + (reb"[^ ]+" as word), before=b, after=a) =>
      "\{first} \{word.length()} \{b.length()} \{a.length()}"
    _ => "none"
  }
}

// Every input is matched, so a catch-all is not needed.
fn classify(d : BytesView) -> Int {
  lexmatch d {
    reb"^$" => 0
    (reb"^[\x00-\x7f]", after=_) => 1
    (reb"^[\x80-\xff]", after=_) => 2
  }
}

fn longest(d : Bytes) -> String {
  lexmatch d with longest {
    (reb"^(a|ab|abc)", after=r) => "first \{r.length()}"
    (reb"^abcd|^[\x00-\xff]{2}" as two, after=r) =>
      "second \{two.length()} \{r.length()}"
    _ => "none"
  }
}

fn main {
  println(dispatch(b"\xef\xbb\xbfabc"))
  println(dispatch(b"\xff\x00"))
  println(dispatch(b"one\r\ntwo"))
  println(dispatch(b"x123"))
  println(dispatch(b"GET /"))
  println(dispatch(b"!"))
  let data = b"--a word--"
  println(captures(data[:]))
  println(captures(data[3:]))
  println(captures(b"\xff\xfe x"))
  println(classify(b""))
  println(classify(b"a"))
  println(classify(b"\x80"))
  println(classify(b"\xff"))
  println(longest(b"abcde"))
  println(longest(b"abc"))
  println(longest(b"ab"))
  println(longest(b"\xff\x00\x01"))
  println(longest(b"z"))
}
