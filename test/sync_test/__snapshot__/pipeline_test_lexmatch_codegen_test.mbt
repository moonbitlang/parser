//!build: -w -20-27

fn main {
  let f = "helo" 
  if f lexmatch? ("el",r) {
    println(r)
  }
}