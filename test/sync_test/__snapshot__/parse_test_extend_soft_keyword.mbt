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
