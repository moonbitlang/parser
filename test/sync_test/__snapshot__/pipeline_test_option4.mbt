enum T {
  Next(T?)
}

pub fn f1(x: T?) -> Int {
  for curr = x, acc = 0 {
    match (curr, acc) {
      (Some(Next(t)), acc) => continue t, acc + 1
      (None, acc) => break acc
    }
  }
}

fn init {
  println(f1(Some(Next(None))))
}
