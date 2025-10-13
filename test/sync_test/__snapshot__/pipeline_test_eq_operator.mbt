//!build: -w -27
type MyInt Int

fn MyInt::equal(x : MyInt, y : MyInt) -> Bool {
  x.inner() == y.inner()
}

impl Eq for MyInt with equal(x, y) {
  x.inner() == 42 || y.inner() == 42 || x.inner() == y.inner()
}

fn init {
  println(MyInt(42) == MyInt(24))
}
