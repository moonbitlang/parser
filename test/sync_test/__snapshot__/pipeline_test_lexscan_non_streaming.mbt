//!build: -w -20-27

///|
fn scan_string(input : String) -> String {
  lexmatch input {
    (re"[0-9]+" as digits, before=prefix, after=suffix) =>
      "string:int prefix=\{prefix} digits=\{digits} suffix=\{suffix}"
    (re"abc" as word, before=prefix, after=suffix) =>
      "string:word prefix=\{prefix} word=\{word} suffix=\{suffix}"
    rest => "string:catchall rest=\{rest}"
  }
}

///|
fn scan_string_char(input : String) -> String {
  lexmatch input {
    (re"." as head, before=_, after=tail) =>
      "string:char head=\{head} tail=\{tail}"
    rest => "string:char rest=\{rest}"
  }
}

///|
fn scan_view(input : StringView) -> String {
  lexmatch input {
    (re"[A-Z]+" as caps, before=prefix, after=suffix) =>
      "view:caps prefix=\{prefix} caps=\{caps} suffix=\{suffix}"
    (re"😋" as face, before=prefix, after=suffix) =>
      "view:emoji prefix=\{prefix} face=\{face} suffix=\{suffix}"
    rest => "view:catchall rest=\{rest}"
  }
}

///|
fn scan_view_longest(input : StringView) -> String {
  lexmatch input with longest {
    (re"^[a-z]+" as word, after=tail) =>
      "view-longest:word=\{word} tail=\{tail}"
    (re"^." as head, after=tail) =>
      "view-longest:char=\{head} tail=\{tail}"
    _ => "view-longest:empty"
  }
}

///|
fn main {
  println(scan_string("xx123yy"))
  println(scan_string("xxabc!"))
  println(scan_string("!!"))
  println(scan_string_char("😋ok"))
  println(scan_view("--abCDEfg--".view(start_offset=2, end_offset=9)))
  println(scan_view("xx😋yy".view(start_offset=2, end_offset=6)))
  println(scan_view("??".view()))
  println(scan_view_longest("hello123".view()))
  println(scan_view_longest("😋tail".view()))
  println(scan_view_longest("".view()))
}
