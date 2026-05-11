fn f(x : Int) -> String raise {
  let m = { "x": 1, "y": 2 }
  for _, _ in m {
    while true {
      match x {
        0 => return "return"
        1 => fail("error")
        _ => break
      }
    }
    break
  }
  "normal"
}

fn init {
  debug(try? f(0))
  debug(try? f(1))
  debug(try? f(2))
}
