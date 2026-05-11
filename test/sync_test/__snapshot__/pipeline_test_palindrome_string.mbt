///|
pub fn palin(p : String) -> Bool {
  for view = p.view() {
    match view {
      [head, .. middle, tail] =>
        if head == tail {
          continue middle
        } else {
          return false
        }
      [_] | [] => break true
    }
  }
}

///|
fn main {
  println(palin("abba"))
  println(palin(""))
  println(palin("🤣"))
  println(palin("🤣🤣"))
  println(palin("🤣😂🤣"))
  println(palin("🤣😂😂🤣"))
  println(palin("🤣😂🎉😂🤣"))
  println("----")
  println(palin("🤣😂🎉🤣"))
  println(palin("hello"))
  println(palin("abca"))
}
