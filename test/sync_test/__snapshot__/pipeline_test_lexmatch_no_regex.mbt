//!build: -w -20-27

fn main {
  let h = "hello"
  lexmatch h {
    (re"^he", after=x) => println(x )
    _ => panic()
  }
  let no_match = lexmatch "xhello" {
    (re"^he", after=_) => true
    _ => false
  }
  if no_match {
    panic()
  }
}
