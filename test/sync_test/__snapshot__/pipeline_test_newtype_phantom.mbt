///|
/// Regression test: a phantom type parameter in a transparent newtype wrapper
/// must not make the recursion analysis treat the outer newtype as recursive.
///
/// `Phantom[T]` ignores `T` — its runtime representation is just `Int` — so
/// `Phantom[S]` is represented as `Int` regardless of `S`. The newtype `S` is
/// therefore NOT self-referential and must unbox to a scalar `Int`.
///
/// The newtype representation walk in mtype.ml must follow the *instantiated*
/// underlying (`Phantom[S]` -> `Int`) rather than counting the phantom type
/// argument `S`. Otherwise `S` is wrongly represented as `AnyRef` while its
/// produced value is a scalar `Int`, so construction emits a `Make_newtype` that
/// erases to the integer while calls/returns expect `AnyRef` — a ref-any cast on
/// an integer (an ICE / miscompile). This is the phantom counterpart of
/// `newtype_rec_generic_wrapper.mbt`'s used-param case.
struct Phantom[T](Int)

///|
struct S(Phantom[S])

///|
fn wrap(n : Int) -> S {
  S(Phantom(n))
}

///|
fn unwrap(s : S) -> Int {
  s.0.0
}

///|
fn main {
  println(unwrap(wrap(42)))
  println(unwrap(wrap(-7)))
}
