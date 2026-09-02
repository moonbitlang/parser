///|
fn main {
  let bs1 = Bytes::makei(10, i => i.to_byte())
  let bs2 = Bytes::makei(10, i => i.to_byte())
  let bv1 = bs1[1:9]
  let bv2 = bs2[1:9]
  println(physical_equal(bv1, bv2))
}
