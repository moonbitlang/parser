fn main {
  let value = answer~: {
    if true {
      break answer~ 42
    }
    0
  }
  ignore(value)
}
