///|
pub struct T {
  x : Int
  y : String
}

///|
fn f(x : ArrayView[T]) -> Unit {
  for rest = x {
    match rest {
      [{ y: "x", .. }, .. rest] => continue rest
      [{ y: "y", .. }, ..] => {
        println("y")
        break
      }
      _ => break
    }
  }
}

///|
fn main {
  f([{ x: 1, y: "x" }, { x: 2, y: "x" }, { x: 3, y: "y" }, { x: 4, y: "z" }])
}
