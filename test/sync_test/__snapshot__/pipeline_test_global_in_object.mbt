let global : Ref[Int] = { val: 42 }

fn init {
  let obj : &Debug = global
  debug(obj)
  println(global.val)
}
