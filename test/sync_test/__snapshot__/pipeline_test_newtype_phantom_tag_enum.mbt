///|
/// A tag-only enum is represented as a scalar. Its phantom type argument must
/// not make an enclosing newtype look recursive.
enum Tag[T] {
  A
  B
}

///|
struct TagWrapper(Tag[TagWrapper])

///|
pub fn TagWrapper::make(a : Bool) -> TagWrapper {
  if a {
    TagWrapper(Tag::A)
  } else {
    TagWrapper(Tag::B)
  }
}

///|
pub fn TagWrapper::is_a(self : TagWrapper) -> Bool {
  match self.0 {
    Tag::A => true
    Tag::B => false
  }
}

///|
fn main {
  println(TagWrapper::make(true).is_a())
  println(TagWrapper::make(false).is_a())
}
