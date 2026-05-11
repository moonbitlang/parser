fn test_array_option_funcref(xs : Array[FuncRef[(Int) -> Int]]) -> Unit {
  for x in xs {
    println(use_funcref(x))
  }
}

fn test_option_option_funcref(foo : FuncRef[(Int) -> Int]??) -> Unit {
  if foo is Some(fo) && fo is Some(f) {
    println(f(5))
  } 
}

fn use_funcref(x: FuncRef[(Int) -> Int]) -> Int {
  x(5)
}

fn main {
  let arr: Array[FuncRef[(Int) -> Int]] = []
  for i in 1..<10 {
    arr.push((fn(x) { x * 2 }))
  }
  test_array_option_funcref(arr)

  test_option_option_funcref(Some(Some(fn(x) { x * 2 })))
}
