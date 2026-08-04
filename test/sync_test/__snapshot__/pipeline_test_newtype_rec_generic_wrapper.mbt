///|
/// Regression test: recursion that flows through a *generic* wrapper whose type
/// parameter is actually *used* must still be detected as recursive.
///
/// `Wrap[T]` uses `T` in its representation (`FixedArray[T]`), so `Rec`, which is
/// `Wrap[Rec]`, is genuinely self-referential (`Rec = FixedArray[Rec]`) and must
/// be represented as `AnyRef`.
///
/// The newtype representation walk in mtype.ml must follow the *instantiated*
/// underlying of the wrapper. `Wrap`'s declaration underlying is
/// `FixedArray[T]`; only after substituting the argument `[Rec]` does it become
/// `FixedArray[Rec]`, where `Rec` (self) appears. Following the *uninstantiated*
/// underlying instead sees only `Wrap`'s `Tparam`, misses the cycle, classifies
/// `Rec` as non-recursive, and then `from_stype` unfolds
/// `Rec -> Wrap[Rec] -> FixedArray[Rec] -> Rec -> ...` forever ("type recursion
/// limit exceeded in monomorphization"). This is the used-param counterpart of
/// the phantom case: the fix must handle both by instantiating the underlying.
struct Wrap[T](FixedArray[T])

///|
struct Rec(Wrap[Rec])

///|
fn count(r : Rec) -> Int {
  let children = r.0.0
  let mut n = 1
  for i = 0; i < children.length(); i = i + 1 {
    n = n + count(children[i])
  }
  n
}

///|
fn main {
  let leaf : Rec = Rec(Wrap([]))
  let node : Rec = Rec(Wrap([leaf, leaf, leaf]))
  let root : Rec = Rec(Wrap([node, leaf]))
  println(count(leaf))
  println(count(node))
  println(count(root))
}
