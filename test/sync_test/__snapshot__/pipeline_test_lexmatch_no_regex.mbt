//!build: -w -20-27

fn main {
  let h = "hello"
  lexscan h {
    (re"^he", after=x) => println(x )
    _ => panic()
  }
  let no_match = lexscan "xhello" {
    (re"^he", after=_) => true
    _ => false
  }
  if no_match {
    panic()
  }
}
