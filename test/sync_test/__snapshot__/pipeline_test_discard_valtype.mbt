#valtype
struct RBG {
  r: Int
  g: Int
  b: Int
}

#inline(never)
fn make() -> RBG {
  RBG::{ r: 0, g: 0, b: 0 }
}

fn main {
  let _a = make()
}
