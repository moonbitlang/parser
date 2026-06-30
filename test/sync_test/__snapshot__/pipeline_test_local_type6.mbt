//!build: -alert -deprecated -w -27
///|
traitalias @json.FromJson

///|
struct X0(Int) derive(Debug)

///|
impl Default for X0 with default() {
  0
}

///|
test {
  let a = X0::default() // Type::default and Default::default both works
  println(to_repr(a))
}

///|
fn[T : Debug + ToJson + Default + @json.FromJson] id(_x : T) -> Unit {
  struct U(T) derive(Debug, ToJson, Default, FromJson)
  let v = U::default() // U[_'a]
  println(to_repr(v))
  debug(ToJson::to_json(v))
  println(to_repr(v.0))
  let u = fn(x) raise { @json.from_json(x) }
  debug(to_result(() => u([1])))
}

///|
fn[T : Debug + ToJson + Default + @json.FromJson] id2(x : T) -> Unit {
  struct U(T) derive(Debug, ToJson, Default, FromJson)
  let _ = U::default() // currently it is U[_]
  let _ : U = Default::default() // currently it is U[_]
  let _ : (U) -> Repr = Debug::to_repr

}

///|
fn main {
  id(1)
  id2(1)
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
