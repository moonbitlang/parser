fn[X] id(x : X)->(X,X){
  (x,x)
}
fn init {
  let v = []
  // let id = id
  let v0 = {
    debug(v)
    id(v)
  }
  let u = [3].append(v0.0)
  debug(v)
}