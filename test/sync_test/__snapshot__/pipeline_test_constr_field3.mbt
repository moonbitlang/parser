
enum T {
  B(Int?)
}

fn f(x: Int?) -> Unit {
  debug(x)
}

fn f1(x: T) -> Unit {
  match x {
    B((Some(_)) as y)  => f(y)
  }
}

fn init {
  f1(T::B(Some(2)))
}
