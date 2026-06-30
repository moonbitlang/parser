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
  debug(to_result(() => S(x=1)))
  debug(to_result(() => S::S(x=2, y=3)))
  debug(to_result(() => (S(x=4, y=42) : S)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
