trait Any {}

let arr : Array[&Any] = []

fn[T : Any] push_item(item : T) -> Unit {
  arr.push(item)
}

fn main {
  push_item(Func(x => x + 1))
  push_item(Func(x => println(x + "!")))
  println(arr.length())
}

struct Func[X, Y] ((X) -> Y)

impl[X, Y] Any for Func[X, Y]
