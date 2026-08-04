#valtype
enum Color {
  RGB(Int, Int, Int)
  Named(String)
}

fn main {
  let c = Color::RGB(255, 128, 0)   // construct a known constructor
  let s = match c {                 // ...and match it in the same scope
    RGB(r, g, b) => "rgb(\{r},\{g},\{b})"
    Named(n) => n
  }
  println(s)
}
