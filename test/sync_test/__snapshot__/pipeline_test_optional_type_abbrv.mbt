fn f(x : Int?) -> Array[Int]? {
  match x {
    None => None
    Some(x) => Some([x])
  }
}

fn init {
  f(None) |> debug
  f(Some(1)) |> debug
}

fn init {
  let mut x : (Double,Int)? = None
  x = Some((1.2, 5))
  debug(x)
}
