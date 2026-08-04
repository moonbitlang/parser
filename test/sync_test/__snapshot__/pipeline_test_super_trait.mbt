
trait Super {
  fn f(Self) -> Unit
}

fn[X: Super] useSuper(x: X) -> Unit {
  x.f()
}


trait Derived: Super {
  fn g(Self) -> Unit
}

fn[X: Derived] useDerived(x: X) -> Unit {
  useSuper(x)
  Super::f(x)
  x.g()
}


struct T1(Int)
let t1: T1 = T1(42)

impl Super for T1 with f(_x: T1) -> Unit { println("T1::f") }
impl Derived for T1 with g(_x: T1) -> Unit { println("T1::g") }

fn init {
  println("===")
  useSuper(t1)
  useDerived(t1)
}


struct T2(Int)
let t2: T2 = T2(42)

impl Super for T2 with f(_x) { println("Super::f[T2]") }
impl Derived for T2 with g(_x: T2) -> Unit { println("T2::g") }

fn init {
  println("===")
  let t2: T2 = T2(42)
  useSuper(t2)
  useDerived(t2)
}


trait Cycle1: Cycle2 { fn f(Self) -> Unit }
trait Cycle2: Cycle1 { fn g(Self) -> Unit }

impl Cycle1 for T1 with f(_self) { println("T1::f") }
impl Cycle2 for T1 with g(_self) { println("T1::g") }

fn init {
  println("===")
  Cycle1::f(t1)
  Cycle2::g(t1)
}


trait Diamond_top { fn top(Self) -> Unit }
trait Diamond_left: Diamond_top { fn left(Self) -> Unit }
trait Diamond_right: Diamond_top { fn right(Self) -> Unit }
trait Diamond_bottom: Diamond_left + Diamond_right { fn bottom(Self) -> Unit }

// [Diamond_top] should appear only once in the super-trait closure of [Diamond_bottom]
pub fn[X: Diamond_bottom] useBottom(x: X) -> Unit {
  Diamond_top::top(x)
  Diamond_left::left(x)
  Diamond_right::right(x)
  x.bottom()
}


// testing trait object
fn useDerivedObject(obj: &Derived) -> Unit {
  obj.f()
  useSuper(obj)
  Super::f(obj)
  obj.g()
  useDerived(obj)
  Derived::g(obj)
}

fn init {
  println("===")
  useDerivedObject(t1)
}

extend &Derived with Super::{f}
