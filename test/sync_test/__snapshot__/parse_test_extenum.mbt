pub extenum Expr[T] {
  Int(T)
  Add(Expr[T], Expr[T])
}

extenum Expr[T] += {
  Mul(Expr[T], Expr[T])
}

pub extenum @pkg.Expr[T] += {
  Let(String, @pkg.Expr[T], @pkg.Expr[T])
}
