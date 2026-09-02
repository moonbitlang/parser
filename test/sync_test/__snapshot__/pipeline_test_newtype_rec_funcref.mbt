// Recursive newtypes over `FuncRef`: the newtype occurs inside its own raw
// function type. Unlike the closure case (`struct F((F) -> Int)`), the runtime
// representation of such a newtype is a *function* reference, so erasing the
// recursive occurrence to `anyref` is not sound on wasm-gc.

///|
/// Recursion in argument position -- the minimal shape.
struct S(FuncRef[(S) -> Int])

///|
/// Mutual recursion between two `FuncRef` newtypes.
struct A(FuncRef[(B) -> Int])

///|
struct B(FuncRef[(A) -> Int])

///|
/// Recursion in return position.
struct R(FuncRef[(Int) -> R])

// --- Already-working neighbours, kept here as regression guards ---

///|
/// Enum payload rather than a newtype field.
enum E {
  Mk(FuncRef[(E) -> Int])
}

///|
/// Named field: the struct itself is boxed, so the recursion goes through a
/// struct reference and never hits the func/any split.
struct N {
  f : FuncRef[(N) -> Int]
}

///|
/// Non-recursive newtype over `FuncRef`.
struct P(FuncRef[(Int) -> Int])

// --- Values ---

///|
let leaf : S = S(_s => 0)

///|
/// `node` reaches back through its own argument, so the `S` parameter is really
/// consumed rather than ignored.
let node : S = S(s => (s.0)(leaf) + 1)

///|
let a0 : A = A(_b => 1)

///|
let b0 : B = B(_a => 2)

///|
let a1 : A = A(b => (b.0)(a0) * 10)

///|
fn main {
  // 1. construct and call, argument unused
  let s0 = S(_s => 42)
  println((s0.0)(s0))

  // 2. argument consumed: node(node) -> node(leaf) + 1 -> 0 + 1 + 1
  println((node.0)(node))

  // 3. mutual recursion: a1(b0) -> b0(a0) * 10 -> 2 * 10
  println((a1.0)(b0))
  println((b0.0)(a1))

  // 4. recursion in return position -- `R` flows out of its own funcref
  let r = R(_i => R(_j => abort("deep")))
  let r2 = (r.0)(1)
  println(r2.0 is _)

  // 5. the newtype in an array slot
  let xs : Array[S] = [leaf, node, s0]
  println((xs[2].0)(xs[0]))

  // 6. the newtype in an `Option` payload
  let os : S? = Some(node)
  match os {
    Some(s) => println((s.0)(leaf))
    None => println("none")
  }

  // 7. regression guards
  let e = Mk(_x => 7)
  match e {
    Mk(f) => println(f(e))
  }
  let n = N::{ f: _x => 8 }
  println((n.f)(n))
  let p = P(x => x + 1)
  println((p.0)(8))
  let v = V::{ f: _x => 11 }
  println((v.f)(v))
}

///|
/// A single-field value struct is transparent like a newtype, despite using
/// record syntax. Its erased recursive handle must retain the raw-func ABI.
#valtype
struct V {
  f : FuncRef[(V) -> Int]
}
