struct T(Int?) derive(Debug)

fn init {
  let a = T::T(Option::Some(1))
  debug(a)
}

fn init {
  let a = Some(1)
  debug(a)
}

struct Point(Int,Int,Int) derive(ToJson)

fn init {
  println(@json.to_json(Point(1,2,3)).stringify())
}
