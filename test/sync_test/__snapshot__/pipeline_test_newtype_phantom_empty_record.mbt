///|
/// An empty record is represented as `Unit`. Its phantom type argument must
/// not make an enclosing newtype look recursive.
struct Empty[T] {}

///|
struct EmptyWrapper(Empty[EmptyWrapper])

///|
pub fn EmptyWrapper::make() -> EmptyWrapper {
  EmptyWrapper(Empty::{})
}

///|
/// Keep the call boundary visible to backend lowering.
#inline(never)
pub fn EmptyWrapper::consume(self : EmptyWrapper) -> Unit {
  ignore(self)
  println("empty")
}

///|
fn main {
  EmptyWrapper::make().consume()
}
