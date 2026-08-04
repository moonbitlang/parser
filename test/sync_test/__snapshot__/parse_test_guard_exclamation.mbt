fn unwrap(value : Int?) -> Int {
  guard! value is Some(inner)
  inner
}

///|
fn negated(value : Bool) -> Bool {
  guard !value
  true
}
