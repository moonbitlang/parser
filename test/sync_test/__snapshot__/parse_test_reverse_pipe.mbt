fn init {
  f(y) <| x
  sum3(1, 2) <| x
  C(1) <| x
  Foo::C(1) <| x
  f <| () => { x }
  f <| (x) => { x }
  f <| _ => { x }
  f <| x => { x }
}

fn init {
  f() <|
  "abcd"
}

test "function lhs" {
  fn (x : Int) { x + 1 } <| y // allowed in syntax, the lhs will be rejected in typer 
}

test "error recovery for arrow fn body" {
  f() <| x => x + 1
  f() <| x => g(x,y)
}
