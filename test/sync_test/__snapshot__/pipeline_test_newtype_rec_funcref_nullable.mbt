///|
/// Regression test: erasing a recursive newtype to the func-hierarchy handle
/// (`Ref_any_func`) keeps the representation of its underlying type, and when
/// that underlying is an *optimized* `Option[FuncRef[_]]`, that representation
/// is a nullable function reference whose `None` is null. The erased handle
/// must therefore stay nullable: classifying `Ref_any_func` as non-nullable
/// makes the wasm-gc backend re-assert non-null when reading late-init toplevel
/// globals (`generic_get`) and closure captures, so a legitimate `None` traps
/// on a null dereference before any user code can inspect the value.

// --- None read back from a toplevel global ---------------------------------
// Toplevel ref globals are declared nullable and late-initialized in the start
// function; the read-back is where a wrong non-null re-assertion traps.

///|
struct Opt(FuncRef[(Opt) -> Int]?)

///|
let none : Opt = Opt(None)

// --- control: a Some payload through the same toplevel-global path ----------

///|
let some : Opt = Opt(Some(_x => 5))

// --- None unpacked from a closure environment ------------------------------
// Captured variables classified non-nullable get the same re-assertion when
// the closure body reads them out of the environment struct.

///|
fn apply(f : () -> Bool) -> Bool {
  f()
}

// --- control: recursion through a boxed constructor is not erased ----------
// `Array` keeps a nominal type id, so `OptA` never goes through the erased
// handle: its ltype stays `Ref_nullable` and the read back needs no assertion.

///|
struct OptA(Array[OptA]?)

///|
let none_boxed : OptA = OptA(None)

///|
fn main {
  // toplevel global holding None
  println(none.0 is None)

  // toplevel global holding Some: the value really is non-null here, so
  // projecting and calling through it must keep working without the assertion
  match some.0 {
    Some(f) => println(f(some))
    None => println("none")
  }

  // closure capture holding None
  let o : Opt = Opt(None)
  println(apply(() => o.0 is None))

  // boxed control
  println(none_boxed.0 is None)
}
