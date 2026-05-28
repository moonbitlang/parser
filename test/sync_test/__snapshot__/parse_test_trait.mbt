
/// doc for I
trait I {
  f1(Self) -> Int
  f2(Self, Int, Double) -> String
  async f3() -> Self
}

trait Functor {
  fmap(Self) -> Self
}

trait Poly {
  fn[X : Debug] meth(Self, x : X) -> Unit
}
