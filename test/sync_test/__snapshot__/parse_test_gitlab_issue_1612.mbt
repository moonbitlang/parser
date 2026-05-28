/// for more information, see http://192.168.86.3:53000/compiler/ideas/pulls/6545
fn div(x : Int,y : Int) -> Int raise {
  if y == 0 {
    raise (Failure "x") // parse error
  }
  x / y 
}

fn hello(x : Int,y : Int) -> Int raise {
   div(x,y)
}
