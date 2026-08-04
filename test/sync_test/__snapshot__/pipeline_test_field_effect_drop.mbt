// Regression for #2555: projecting a field from a tuple/record *literal* must
// still evaluate the other fields. Before the fix, `(f(), g()).0` dropped the
// call to `g()` (and `(f(), g()).1` dropped `f()`) at core lowering, so the
// side effects below were silently lost.

fn log_and_get(tag : String, v : Int) -> Int {
  println(tag)
  v
}

fn init {
  // .0 must still run the second element's effect ("b")
  let a = (log_and_get("a", 1), log_and_get("b", 2)).0
  println(a)
  // .1 must still run the first element's effect ("c")
  let b = (log_and_get("c", 10), log_and_get("d", 20)).1
  println(b)
}

struct S {
  x : Int
  y : Int
}

fn init {
  let x = { x: log_and_get("x", 1), y: log_and_get("y", 2) }.x
  println(x)
  let y = { x: log_and_get("x", 1), y: log_and_get("y", 2) }.y
  println(y)
}

// Companion checks that no per-field binding survives when it isn't needed.
// `Core.field` may introduce an `elem_N`/field temp while lowering a projection,
// but it must not leak into the optimized IR for these large tuples. Inspect the
// promoted `.corex.lifted` snapshot: it should contain no `elem_N` bindings.
fn init {
  // All siblings are pure (`base + k`). `Core.field` takes the slow path (the
  // adds are not "obviously pure"), so it mints a temp for the selected field;
  // DCE then drops every pure sibling and collapses the temp, leaving just the
  // selected expression with no residual name -- even across ten fields.
  let base = log_and_get("base", 100) // opaque, blocks constant-folding
  let picked = (
      base + 0,
      base + 1,
      base + 2,
      base + 3,
      base + 4,
      base + 5,
      base + 6,
      base + 7,
      base + 8,
      base + 9,
    ).6
  println(picked) // expect 106

  // Selecting the *last* field of a large effectful tuple: the siblings must
  // still run in order, but the selected element is returned directly, so no
  // temp is introduced regardless of tuple size.
  let last = (
      log_and_get("p", 1),
      log_and_get("q", 2),
      log_and_get("r", 3),
      log_and_get("s", 4),
      log_and_get("t", 5),
    ).4
  println(last) // p,q,r,s,t run in order; expect 5
}
