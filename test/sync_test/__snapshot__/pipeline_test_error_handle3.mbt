suberror StringError String derive(Show)
suberror FloatError Float derive(Show)

trait TF {
  f1(Self) -> Float raise = _
  f2(Self) -> Float raise FloatError = _
}

impl TF for Int with f1(x) -> Float raise {
  if x >= 0 {
    x.to_float()
  } else {
    raise StringError("x is negative")
  }
}

impl TF for Int with f2(x) -> Float raise FloatError {
  if x >= 0 {
    x.to_float()
  } else {
    raise FloatError(x.to_float())
  }
}

fn use_tf(x: &TF) -> Unit raise {
  let a = x.f1()
  println(a)
  let b = (try? x.f2()).unwrap()
  println(b)
}

fn init {
  (try? use_tf(42)).unwrap()
}

impl TF with f1(x) -> Float raise {
  if (a) { raise StringError("err") }
  42
}

impl TF with f2(x) -> Float raise FloatError {
  if (a) { raise FloatError(-1.0) }
  42
}

impl TF for Int64 with f1(x) -> Float raise {
  if (a) { raise StringError("err") }
  x.to_float()
}

impl TF for Int64 with f2(x) -> Float raise FloatError {
  if (a) { raise FloatError(-1.0) }
  x.to_float()
}

impl TF for Double with f1(x) {
  if (a) { raise StringError("err") }
  x.to_float()
}

impl TF for Double with f2(x) {
  if (a) { raise FloatError(-1.0) }
  x.to_float()
}

impl TF for String

fn init {
  let a = (try? TF::f1("xxx")).unwrap()
  println(a)
  (try? use_tf(42L)).unwrap()
  (try? use_tf(42.0)).unwrap()
}

let a = false
