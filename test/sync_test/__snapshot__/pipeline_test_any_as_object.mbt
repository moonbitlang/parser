//!build: -w -27
trait T {
  f(Self) -> Int
}

type F (F) -> Int

impl T for F with f(self: F) -> Int {
  (self.inner())(self)
}

fn use_t(t: &T) -> Int {
  t.f()
}

fn init {
  let f = F::F(fn (_x) { 42 })
  let a = use_t(f)
  println(a)
}
