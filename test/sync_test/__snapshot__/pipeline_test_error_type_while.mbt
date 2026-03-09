///|
suberror E {
  E(String)
}

///|
pub fn woo() -> Int raise E {
  while false {
    raise E("oh no")
  } nobreak {
    1
  }
}
