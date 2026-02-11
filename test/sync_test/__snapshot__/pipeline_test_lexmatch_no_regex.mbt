fn main {
  let h = "hello"
  lexmatch h {
    ("he", x) => println(x )
    _ => panic()
  }
}