fn init {
  let _f = fn (a: array[int]) {
    let [a, b, c] = a
    let [.., a] = a
    // let [..rest, a] = a
    // let [a, ..rest] = a
    let [a, .._] = a
    // Currently not parse
    // let [a, ..rest, b] = a
    // let [a, .., b] = a
  }
}


fn[T] ignore(x : T) {
  ignore(x)
}

fn init{
  match [1,2,3] {
    [.., a, b] => ignore((a,b))
    [.., a, b,] => ignore((a,b))
    [.. a,b] => ignore((a,b)) //parse ok
  }
}

fn init{
   match [1,2,3] {
    [a, b, .._] => ignore((a,b)) 
    [a, b, .., ] => ignore((a,b)) 
    [a,b .._] =>  ignore((a,b)) //parse fail
    [a, .., b,] => ignore((a,b)) //fail
    [.., a, .., b] => ignore((a,b)) //fail
    [.., a, b, .._] => ignore((a,b)) //fail
    [a, .., b, .._] => ignore((a,b)) //fail
  } 
}

fn unfinished_array_pattern() -> Unit {
  match a {
    [x, 
  }
}

fn unfinished_array_pattern_dotdot() -> Unit {
  match a {
    [x, .]
  }
}
