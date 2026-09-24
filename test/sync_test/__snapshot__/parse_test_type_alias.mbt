type T1 = Int
pub type T2[X, Y] = (X) -> Y

/// semantically invalid, but syntatically allowed
type T3 = Array[T2] derive(Show)
type Bad[F[X]] = Int
