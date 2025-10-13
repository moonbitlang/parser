///|
fn main {
  let val = Some("a")
  fn b() {
    { val, }
  }

  ignore(b)
}
