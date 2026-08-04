//!build: -w -20-27

fn main {
  let f = "helo" 
  let rest = lexscan f {
    (re"^el", after=rest) => Some(rest)
    _ => None
  }
  if rest is Some(rest) {
    println(rest)
  }
}
