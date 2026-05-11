fn use_debug(x: &Debug) -> String {
  to_repr(x).to_string()
}

fn init {
  let s: String? = Option::None
  println(use_debug(s))
}
