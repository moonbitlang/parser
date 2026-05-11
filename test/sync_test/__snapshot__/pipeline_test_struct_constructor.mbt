///|
struct S {
  x : Int
  y : Int
} derive(Debug)

///|
#callsite(autofill(loc))
fn S::S(x~ : Int, y? : Int = x, loc~ : SourceLoc) -> S raise {
  if y == 42 {
    raise Failure::Failure("fourty-two")
  }
  println("S::new(x=\{x}, y=\{y}) at \{loc}")
  { x, y }
}

///|
fn init {
  debug(try? S(x=1))
  debug(try? S::S(x=2, y=3))
  debug(try? (S(x=4, y=42) : S))
}
