//!build: -w -deprecated_syntax
struct T(Int) derive(Debug)

// local type inside an `impl` for foreign trait
impl @json.FromJson for T with from_json(json, _path) {
  struct S { x : Int }
  // the `if` is used to surpress unbox optimization
  let s : S = if json == 42 {
    { x: 42 }
  } else {
    { x: 24 }
  }
  s.x
}

fn init {
  debug((to_result(() => @json.from_json(42)) : Result[T, _]))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
