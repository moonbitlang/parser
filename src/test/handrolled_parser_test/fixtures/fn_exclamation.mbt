fn f() {
  fn f1!() { 42 }
  fn f2! {
    _ => 42
  }
  apply(fn! (x) { x + 1 }, 42)
  apply(fn! { x => x + 1}, 42)
}

fn g!() {

}
