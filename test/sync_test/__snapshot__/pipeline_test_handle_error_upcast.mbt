struct T {}

fn T::new() -> T raise {
  if false {
    fail("Error")
  }
  T::{  }
}

pub(open) trait Any {}

impl Any for T

struct Test {
  f : () -> &Any raise
}

fn main {
  let _ : Test = { f: () => T::new() }

}
