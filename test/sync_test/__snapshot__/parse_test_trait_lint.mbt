
struct Self {}
enum Self {}

fn[Self] f(x: Self) -> Self { x }

priv struct T {}

pub trait I {
  fn f(Self) -> T
  fn[X] f(Self) -> Int
}

trait I_private { fn m(Self) }

pub fn[X: I_private] f(x: X) {}

trait I_dup_method {
  fn m1(Self)
  fn m1(Self)
  fn m2(Self) -> Int
  fn m2(Self) -> Double
  fn m2(Self) -> Char
}

pub fn I_private::m(self: Int) {}
fn I_private::m(self: String) {}
