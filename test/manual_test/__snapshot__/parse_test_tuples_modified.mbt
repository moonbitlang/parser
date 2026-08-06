let a = 42
let b = 24

fn init {
  let _ = ()
}

fn init {
  let _ = (a)
}

fn init {
  let _ = (a,)
}

fn init {
  let _ = (a,b)
}

fn init {
  let _ = (a,b,)
}

fn init {
  let _ = (a: Int)
}

fn init {
  let _ = (a + b: Int)
}

fn init {
  let _ = (a + b,)
}

fn init {
  let _ = (a, a+b)
}

fn init {
  let _ = (a, a+b,)
}
