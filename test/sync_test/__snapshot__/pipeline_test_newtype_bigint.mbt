//!build: -w -27
type MyBigInt BigInt
fn f(x : MyBigInt) -> Unit {
  match x.inner() {
    0 => println("zero")
    _ => ()
  }
}

fn init {
  f(0)
}
