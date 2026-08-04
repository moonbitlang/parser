///|
trait I {
  #as_free_fn
  fn[X] no_bound(Self, X) -> Unit
  #as_free_fn
  fn[X : Show] has_bound(Self, X) -> Unit
}

///|
struct T1(Int)

///|
impl I for T1 with fn no_bound(_, _) {
  println("no bound")
}

///|
impl I for T1 with fn has_bound(_, x) {
  println(x) // ok
}

///|
fn init {
  let t1 = T1(42)
  I::no_bound(t1, t1)
  no_bound(t1, t1)
  I::has_bound(t1, 42)
  has_bound(t1, 42)
}

///|
fn[X : I] poly(x : X) -> Unit {
  println("==== inside polymorphic function ====")
  x.no_bound(42)
  x.no_bound(x)
  I::no_bound(x, x)
  x.has_bound(42)
  I::has_bound(x, "42")
}

///|
fn init {
  poly(T1(42))
}

///|
struct P[X](X)

///|
impl[X : I] I for P[X] with fn no_bound(self, x) {
  self.0.no_bound(x)
}

///|
impl[X : I] I for P[X] with fn has_bound(self, x) {
  self.0.has_bound(x)
}

///|
fn init {
  let t1 = T1(42)
  let pt1 = P(t1)
  I::no_bound(pt1, pt1)
  no_bound(pt1, t1)
  I::has_bound(pt1, 42)
  has_bound(pt1, "42")
  poly(pt1)
}

///|
trait PolyWithDefault {
  fn[X : Show] with_default(Self, X) -> Unit = _
}

///|
impl PolyWithDefault with fn with_default(_, x) {
  println("default impl: \{x}")
}

///|
impl PolyWithDefault for T1

///|
impl[X] PolyWithDefault for P[X]

///|
fn init {
  let t = T1(42)
  let pt = P(t)
  t.with_default(42)
  PolyWithDefault::with_default(t, "fourty-two")
  pt.with_default(42)
  PolyWithDefault::with_default(pt, "fourty-two")
}

///|
extend T1 with PolyWithDefault::{with_default}

///|
extend P with PolyWithDefault::{with_default}
