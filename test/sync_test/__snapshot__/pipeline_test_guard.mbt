suberror Exception {
  Exception(String)
} derive(Debug)

fn div(x : Int?, y : Int?) -> Int raise Exception {
  guard x is Some(x) else { raise Exception("x is none") }
  guard y is Some(y) else { raise Exception("y is none") }
  guard y != 0 else { raise Exception("y is zero") }
  x / y
}

fn[A] default_value(x : A?, default : A) -> A {
  guard x is Some(x) else { return default }
  x
}

fn local_control_flow(lbound : Int, ubound : Int, xs : Array[Int]) -> Int {
  let mut sum = 0
  let mut count = 0
  for x in xs {
    let r = {
      guard x < ubound else { ubound }
      guard x > lbound else { lbound }
      x
    }
    sum += r
    count += 1
  }
  
  guard count > 0 else { 0 }
  sum / count
}

suberror InvalidJson {
  InvalidJson(String)
} derive(Debug)

fn get_data(x : Json) -> String raise InvalidJson {
  match x {
    Object({ "data": String(data), .. }) => data
    Array(_) => raise InvalidJson("x is array")
    Null => raise InvalidJson("x is null")
    True | False => raise InvalidJson ("x is bool")
    Number(_) => raise InvalidJson("x is number")
    String(_) => raise InvalidJson("x is string")
  }
  
}

fn init {
  debug(to_result(() => div(Some(10), Some(2))))
  debug(to_result(() => div(Some(10), Some(0))))
  debug(to_result(() => div(Some(10), None)))
  debug(to_result(() => div(None, Some(2))))
  debug(to_result(() => div(None, None)))

  println(default_value(Some(10), 5))
  println(default_value(None, 5))

  println(local_control_flow(1, 10, [0,0,0,0]))
  println(local_control_flow(1, 10, [0,0,20,20]))
  println(local_control_flow(1, 10, [7,7,7,7]))

  debug(to_result(() => get_data({"data": "hello"})))
  debug(to_result(() => get_data([1,2,3])))
  debug(to_result(() => get_data(Json::null())))
  debug(to_result(() => get_data(true)))
  debug(to_result(() => get_data(false)))
  debug(to_result(() => get_data(100)))
  debug(to_result(() => get_data("string")))
  
  
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
