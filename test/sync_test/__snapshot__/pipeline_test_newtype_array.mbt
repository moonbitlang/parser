//!build: -w -27
type MyArray Array[Int]

fn init {
  let a : MyArray = [ 1, 2, 3 ]
  a.inner().each(println)
  a.inner().iter().each(println)
  match a.inner() {
    [ x ] => println(x)
    [ .. arr ] => println(arr)
  }
}

type MyFixed FixedArray[Int]

fn init {
  let a : MyFixed = [ 1, 2, 3 ]
  a.inner().each(println)
  a.inner().iter().each(println)
  match a.inner() {
    [ x, y, z ] => println([ x, y, z ])
    _ => ()
  }
}
