fn extend(
  extend : Int,
  extend~ : Int,
  extend? : Int,
) -> Unit {
  extend(extend, extend~, extend=extend, extend?, extend?=extend)
  extend = extend.extend
  extend += extend
  let extend = { extend, extend: extend }
  match extend {
    Constr(extend, extend~, extend=extend) => ...
  }
  let _ = extend => ...
  let _ = (extend, extend : Int) => ...
}

struct S {
  extend : Int
}

enum E {
  C(extend~ : Int)
}

trait I {
  extend(extend : Int, extend~ : Int) -> Unit
}

pub using @pkg {extend, f as extend}
