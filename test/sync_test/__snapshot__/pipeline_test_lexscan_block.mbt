//!build: -w -20-27-lexmatch_first_match -regex-backend block

///|
fn scan_string(input : String) -> String {
  lexscan input {
    (re"[0-9]+" as digits, before=prefix, after=suffix) =>
      "int prefix=\{prefix} digits=\{digits} suffix=\{suffix}"
    (re"abc" as word, before=prefix, after=suffix) =>
      "word prefix=\{prefix} word=\{word} suffix=\{suffix}"
    rest => "catchall rest=\{rest}"
  }
}

///|
fn scan_longest(input : StringView) -> String {
  lexscan input with longest {
    (re"^[a-z]+" as word, after=tail) => "longest word=\{word} tail=\{tail}"
    (re"^." as head, after=tail) => "longest head=\{head} tail=\{tail}"
    _ => "longest empty"
  }
}

///|
fn scan_unicode(input : StringView) -> String {
  lexscan input {
    (re"😋" as face, before=prefix, after=suffix) =>
      "unicode prefix=\{prefix} face=\{face} suffix=\{suffix}"
    rest => "unicode rest=\{rest}"
  }
}

///|
fn main {
  println(scan_string("xx123yy"))
  println(scan_string("xxabc!"))
  println(scan_string("!!"))
  println(scan_longest("hello123".view()))
  println(scan_longest("😋tail".view()))
  println(scan_unicode("xx😋yy".view()))
}
