trait I {
  fn f(Self) -> Int
}

impl I for Int with fn f(self) { self }

fn main {
  ignore(Some("x").unwrap())
  let cell : &I? = Some(42)
  let app = cell.unwrap()
  println(app.f())
}
