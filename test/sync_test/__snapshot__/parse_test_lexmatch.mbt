fn init {
  lexmatch s with longest {
    re"^a$" => ()
    (re"^a", after=_) => ()
    _ => ()
  }
  lexmatch s {
    re"^a$" => ()
    (re"a$", before=_) => ()
    (re"a", before=_, after=_) => ()
    _ => ()
  }
  lexmatch s {
    (re"^a$" as a) => ()
    (re"^a" + re"b$") => ()
    (re"^a" + (re"b$" as b)) => ()
    (re"^a" + re"b", after=c) => ()
    ((re"^a" as a) + re"b", after=c) => ()
    s2 => ()
  }
  lexmatch s with xxx {
    re"" => ()
    a => ()
  }
}
