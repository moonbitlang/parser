//!build: -w -27
trait T {
  f(Self) -> Int
}

type F (Int) -> Int

impl T for F with f(self: F) -> Int {
  (self.inner())(42)
}

fn use_t(t: &T) -> Int {
  t.f()
}

fn init {
  let f = F::F(fn (x) { x })
  let a = use_t(f)
  println(a)
}
