trait I {
  fn f(Self) -> Unit = _
  fn g(Self) -> Unit
}

impl I with fn f(_) {}
