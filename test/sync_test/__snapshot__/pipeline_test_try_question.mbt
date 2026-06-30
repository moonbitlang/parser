//!build: -w -20
suberror MyErr derive(Debug)

fn f(coin : Bool) -> Int raise MyErr {
  if coin {
    42
  } else {
    raise MyErr
  }
}

fn main {
  debug(try? f(true))
  debug(try? f(false))
  debug(try? f(f(true) < 0))
  fn outside(coin) -> _ raise MyErr {
    let x = f(coin)
    try? f(x < 0)
  }
  debug(try? outside(true))
  debug(try? outside(false))
}
