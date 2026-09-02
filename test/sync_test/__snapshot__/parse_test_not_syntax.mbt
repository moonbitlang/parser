fn semi_insertion() -> Bool {
  let a : Bool = false
  !a
}


///|
fn expr_first_set() -> Unit {
  let a = ; //force to use hand-written parser
  var b = (!a)
  let _ = b
}

