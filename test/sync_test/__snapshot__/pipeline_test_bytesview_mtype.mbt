pub fn[T] map(a : ArrayView[T]) -> Unit {
  println(a.length())
}


trait T {
  f(Self) -> Unit
}

impl[A] T for FixedArray[A] with f(a) {
  let v : ArrayView[A] = a
  map(v)
}

fn g(t: &T) -> Unit {
  T::f(t)
}

fn main {

  let c : FixedArray[Byte] = [b'b']
  g(c)
  println(c[:].length())
}
