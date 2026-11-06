// `reb"..."` as a value: a BytesRegex, built from a literal checked at
// compile time, and composed with `+` and `|` in constants and at run time.
const METHOD : BytesRegex = reb"^(?:GET|POST) "

const PATH = reb"/[a-z]+"

const REQUEST : BytesRegex = METHOD + PATH

const NEWLINE : BytesRegex = reb"\r\n" | reb"\n"

fn bytes_to_string(v : BytesView) -> String {
  v.to_owned().to_array().map(b => b.to_int().to_string()).join(" ")
}

fn show(re : BytesRegex, input : Bytes) -> Unit {
  match re.execute(input) {
    Some(m) => println("[\{bytes_to_string(m.content())}]")
    None => println("none")
  }
}

fn main {
  // Bytes outside ASCII, written \xHH.
  let bom = reb"^\xef\xbb\xbf(?<rest>[a-z]+)"
  match bom.execute(b"\xef\xbb\xbfhi") {
    Some(m) =>
      match m.named_group("rest") {
        Some(rest) => println("rest [\{bytes_to_string(rest)}]")
        None => println("no rest")
      }
    None => println("none")
  }
  show(REQUEST, b"GET /index")
  show(REQUEST, b"PUT /index")
  show(NEWLINE, b"a\r\nb")
  // `.` and negated classes are any byte.
  show(reb"a.c", b"a\xffc")
  show(reb"[^a]+", b"a\x80\x00a")
  // Only ASCII letters fold.
  show(reb"(?i:\xc9|e)", b"\xe9E")
  // Folded: both operands are literals.
  let either = reb"a" | reb"\xff"
  show(either, b"\x00\xff")
  let both = reb"a" + reb"\xff"
  show(both, b"\xffa\xff")
  // Not folded: the operands are not known here.
  show(join(reb"a", reb"\xff"), b"\xffa\xff")
  show(pick(reb"a", reb"\xff"), b"\x00\xff")
  // Not folded: the result could not be read back from its printed form,
  // where the name would appear twice, but the regex is fine at run time.
  let twice = reb"(?<x>a)" + reb"(?<x>b)"
  show(twice, b"ab")
  f()
}

#inline(never)
fn join(a : BytesRegex, b : BytesRegex) -> BytesRegex {
  a + b
}

#inline(never)
fn pick(a : BytesRegex, b : BytesRegex) -> BytesRegex {
  a | b
}

fn f(re? : BytesRegex = reb"abc") -> Unit {
  println(re.execute(b"xabc") is Some(_))
}
