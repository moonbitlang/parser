//!build: -w -20-27-lexmatch_first_match

fn main {
  let h = "hello"
  lexmatch h {
    ("he", x) => println(x )
    _ => panic()
  }
}