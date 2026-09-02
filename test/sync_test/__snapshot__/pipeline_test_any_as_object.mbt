
trait T {
  fn f(Self) -> Int
}

struct F((F) -> Int)

struct S(FuncRef[(S) -> Int])

impl T for F with f(self: F) -> Int {
  (self.0)(self)
}

impl T for S with f(self: S) -> Int {
  (self.0)(self)
}

fn use_t(t: &T) -> Int {
  t.f()
}

fn use_t_twice(t: &T) -> Int {
  t.f() + t.f()
}

fn init {
  let f = F::F(fn (_x) { 42 })
  let a = use_t(f)
  println(a)
  let s = S::S(fn (_x) { 7 })
  let b = use_t_twice(s)
  println(b)
}
