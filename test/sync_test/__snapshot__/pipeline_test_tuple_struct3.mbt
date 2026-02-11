struct T(Int?) derive (Show)

fn init {
  let a = T::T(Option::Some(1))
  println(a)
}

fn init {
  let a = Some(1)
  println(a)
}

struct Point(Int,Int,Int) derive(ToJson)

fn init {
  println(Point(1,2,3).to_json().stringify())
}
