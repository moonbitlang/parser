trait I1 {
  fn f(Self) -> Unit
}

impl I1 for Int with f(_self) { println("I1::f") }

// mixing user-defined & object method
trait I3 {
  fn f(Self) -> Unit
  fn g(Self) -> Unit
}

impl I3 for &I1 with f(self : &I1) -> Unit {
  self.f()
}

impl I3 for &I1 with g(_self : &I1) -> Unit {
  println("I1::g")
}

fn init {
  let obj : &I3 = 42 as &I1
  obj.f()
  obj.g()
}
