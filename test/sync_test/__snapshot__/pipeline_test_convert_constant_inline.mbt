fn main {
  println((42 : Int).to_int64())
  println((-1 : Int).to_int64())
  println((0x7fffffff : Int).to_int64())
  println((0x80000000 : Int).to_int64())
  println((42 : Int64).to_int())
  println((-1 : Int64).to_int())
  println((0x7fffffff : Int64).to_int())
  println((0xffffffff : Int64).to_int()) // should not inline
}
