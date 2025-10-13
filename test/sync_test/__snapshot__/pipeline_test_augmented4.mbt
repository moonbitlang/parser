//!build: -w -27
fn init {
  let r1 = { val: 1 }
  let r2 = { val: 2 }
  let mut r = r1
  r.val += { r = r2; 1 }
  println((r1, r2))
}

fn init {
  let r1 = { val: 1 }
  let r2 = { val: 2 }
  let mut r = r1
  r.val = r.val + { r = r2; 1 }
  println((r1, r2))
}

fn init {
  let r1 = { val: 1 }
  let r2 = { val: 2 }
  let mut r = r1
  let obj = r
  obj.val = obj.val + { r = r2; 1 }
  println((r1, r2))
}


type MyInt Int derive(Show)
let bad_global : Ref[() -> Unit] = { val: fn () {} }
impl Add for MyInt with add(self : MyInt, other : MyInt) -> MyInt {
  (bad_global.val)()
  MyInt(self.inner() + other.inner())
}

fn init {
  let r1 = { val: MyInt::MyInt(1) }
  let r2 = { val: MyInt::MyInt(2) }
  let mut r = r1
  bad_global.val = fn () { r = r2 }
  r.val += MyInt::MyInt(1)
  println((r1, r2))
}

fn init {
  let r = { val: 1 }
  r.val += { r.val = 42; r.val }
  println(r)
}
