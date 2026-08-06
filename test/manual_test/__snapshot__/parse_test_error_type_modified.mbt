fn f1() -> Int raise String {
  0
}

fn f2() -> Int raise String {
  raise "xxx"
}

fn[A, B] f3(a: (Int) -> A raise B) -> A {
  a!!
}

fn[A, B] f4(a: (Int) -> A raise B) -> A raise B {
  a!
}

fn[A, B] f5() -> Unit {
  let x1: (Int) -> A? raise B = ()
  let x2: (Int) -> (Int) -> Int raise String = ()
  let x3: (Int) -> ((Int) -> Int) raise String = ()
  let x4: (Int) -> Int raise = ()
  let x5: (Int) -> Int raise? = ()
}

trait I {
  fn f(Self) -> Self raise Int
}

impl Trait for Int with method() -> Int raise Bool {}

suberror E1 Int
suberror E2
suberror E3 {
  A
  B(Int, x~ : String)
  C(x~ : Strint, Char, y~ : Bool)
}

fn f7() {
  let _ : () -> () -> Unit!! = () // should be invalid
  let _ : () -> () -> Unit! = () // ok, the bang is for inner function
  let _ : () -> (() -> Unit!)! = () // ok
  let _ : () -> () -> Unit!Error!Error = () // should be invalid
  let _ : () -> () -> Unit!Error = () // ok, the bang is for inner function
  let _ : () -> (() -> Unit!Error)!Error = () // ok
}
