#valtype
struct Bad {
  r : String?
  n : UInt64
}

///|
fn main {
  let xs = [1, 2, 3]
  let ys = xs.map(i => Bad::{ r: None, n: i.to_uint64() })
  let y = ys[0]
  let extra = match y.r {
    None => 0UL
    Some(s) => s.length().to_uint64()
  }
  println((y.n + extra).to_string())
}