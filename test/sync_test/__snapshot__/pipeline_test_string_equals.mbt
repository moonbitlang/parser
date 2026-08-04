fn f(x: String) -> Int {
  match x {
    "ok11" => 1
    "ok2" => 2
    "ok3" => 3
    _ => 4
  }
}
fn no_inline(x : String, y : Int) -> Int {
  println("noinline")
  f("ok \{x} \{y + 3}")
}

fn test_string_length_boundary(length : Int) -> Unit {
  let lhs = String::make(length, 'a')
  let rhs = String::make(length, 'a')
  println(lhs == rhs)
  println(lhs == String::make(length - 1, 'a') + "b")
}

fn init {
  test_string_length_boundary(7)
  test_string_length_boundary(8)
  test_string_length_boundary(9)
  println(f("ok11"))
  println(f("ok2"))
  println(f("ok3"))
  println(f("ok33"))
  println(f("ok" + (3).to_string()))
  println(no_inline("ok", 3))
}
