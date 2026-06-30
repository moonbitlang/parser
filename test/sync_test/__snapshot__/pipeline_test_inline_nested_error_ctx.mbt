suberror E

fn outer() -> Unit {
  try {
    fn nested() -> Unit raise {
      fn helper() -> Unit raise {
        raise E 
      }
      helper()
    }
    nested()
  } catch {
    E => println(0)
    _ => println(1)
  }
}

fn main {
  outer()
}
