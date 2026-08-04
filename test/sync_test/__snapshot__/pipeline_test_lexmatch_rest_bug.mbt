fn init {
  let s = "abc"
  lexscan (try! s[0:1]) with longest {
    (re"^a", after=rest) => println(rest)
    _ => ()
  }
  let no_match = lexscan "ba" with longest {
    (re"^a", after=_) => true
    _ => false
  }
  if no_match {
    panic()
  }
}
