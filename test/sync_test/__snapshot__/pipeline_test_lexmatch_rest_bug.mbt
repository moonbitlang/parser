fn init {
  let s = "abc"
  lexmatch (try! s[0:1]) with longest {
    (re"^a", after=rest) => println(rest)
    _ => ()
  }
  let no_match = lexmatch "ba" with longest {
    (re"^a", after=_) => true
    _ => false
  }
  if no_match {
    panic()
  }
}
