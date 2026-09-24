extenum Bare

priv extenum Private

pub extenum Public

pub(all) extenum Generic[T]

pub(all) extenum Generic[T] += {
  Value(T)
}

extenum Derived[T] derive(Debug)

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

pub extenum Last
