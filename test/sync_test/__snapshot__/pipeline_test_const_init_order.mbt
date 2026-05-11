///|
struct T {
  x : Int
  y : Int?
} derive(Debug)

///|
pub let a : T = { x: 0, y: Some(1) }

///|
fn main {
  debug(a)
}
