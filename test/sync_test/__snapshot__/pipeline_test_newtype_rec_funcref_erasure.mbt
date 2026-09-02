///|
/// Regression test: a recursive newtype is erased to `T_any` to break the
/// representation cycle, but erasure hides only the *static* cycle -- the value
/// keeps whatever representation the underlying type had. A chain of transparent
/// newtypes bottoming out at `FuncRef` is still a code pointer, and must be
/// erased to the func-hierarchy handle (`Ref_any_func`/`funcref`), never to the
/// boxed one (`Ref_any`/`anyref`).
///
/// Getting that wrong is not a warning: on wasm-gc `funcref` and `anyref` are
/// disjoint hierarchies, so the module fails validation with `expected type
/// anyref, found (ref N)`; on C/LLVM the value is instead treated as a heap
/// object and refcounted, so `moonbit_incref` reads and writes the *code*
/// segment at `fp - 8`.
///
/// The classification must therefore come from the newtype's *resolved*
/// representation, not from the syntactic head of its declared underlying type:
/// the head is only the whole story when it is a real type constructor.

// --- one newtype hop before reaching the FuncRef ---------------------------
// `N`'s declared underlying is `M`, not a `FuncRef`, but `N` is transparent
// over `M` which is transparent over `FuncRef`, so `N` is a code pointer too.

///|
struct N(M)

///|
struct M(FuncRef[(N) -> Int])

// --- a longer chain, so the alias resolves through several hops ------------

///|
struct P(Q)

///|
struct Q(R)

///|
struct R(FuncRef[(P) -> Int])

// --- the hop is a *generic* newtype ----------------------------------------
// `Wrap`'s underlying is a type parameter, so only the instantiated form
// reveals the `FuncRef`.

///|
struct Wrap[T](T)

///|
struct S(Wrap[FuncRef[(S) -> Int]])

// --- same cycle, different representations per member ----------------------
// `X` and `Y` are both marked recursive, but `X` is a raw function pointer and
// `Y` is a boxed array. The classification is per-newtype, not per-cycle, so
// these two must stay split.

///|
struct X(FuncRef[(Y) -> Int])

///|
struct Y(Array[X])

// --- the underlying is an *optimized* Option over the FuncRef --------------
// `Option[FuncRef[_]]` is not boxed. Every backend keeps it as a nullable
// function reference -- wasm-gc as `Ref_func ~nullable:true`, wasm1 as table
// index 0 for None, C/LLVM as a null function pointer -- so the wrapper is
// representation-preserving and `Opt` is still a code pointer. Classifying it
// as boxed puts it back in the anyref/refcounted paths.

///|
struct Opt(FuncRef[(Opt) -> Int]?)

// --- control: a recursive newtype that really is boxed ---------------------

///|
struct Tree(Array[Tree])

///|
/// Projects the erased newtype back to the funcref and calls it, so `N` is read
/// through as well as constructed.
fn call_through(n : N) -> Int {
  (n.0.0)(n)
}

///|
fn size(t : Tree) -> Int {
  let mut n = 1
  for child in t.0 {
    n = n + size(child)
  }
  n
}

///|
fn main {
  // one hop
  let m = M(_n => 42)
  println(call_through(N(m)))

  // three hops: P -> Q -> R -> FuncRef
  let p = P(Q(R(_p => 7)))
  println((p.0.0.0)(p))

  // generic hop
  let s = S(Wrap(_x => 1))
  println(((s.0).0)(s))

  // mixed cycle: X is a raw func, Y is boxed
  let x = X(_y => 3)
  let y = Y([x, x])
  println((x.0)(y))
  println(y.0.length())

  // optimized Option underlying, both payloads
  let o = Opt(Some(_x => 5))
  match o.0 {
    Some(f) => println(f(o))
    None => println("none")
  }
  println(Opt(None).0 is None)

  // boxed recursive newtype still behaves
  let leaf = Tree([])
  println(size(Tree([leaf, Tree([leaf])])))
}
