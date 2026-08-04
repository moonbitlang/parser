///|
/// A boxed enum is a nominal representation anchor. Looking through its
/// definition would incorrectly classify `Wrapper` as a recursive newtype.
enum Boxed {
  End
  Next(Wrapper)
}

///|
struct Wrapper(Boxed)

///|
pub fn Wrapper::end() -> Wrapper {
  Wrapper(Boxed::End)
}

///|
pub fn Wrapper::next(self : Wrapper) -> Wrapper {
  Wrapper(Boxed::Next(self))
}

///|
pub fn Wrapper::depth(self : Wrapper) -> Int {
  match self.0 {
    Boxed::End => 0
    Boxed::Next(next) => 1 + next.depth()
  }
}

///|
fn main {
  let value = Wrapper::end().next().next().next()
  println(value.depth())
}
