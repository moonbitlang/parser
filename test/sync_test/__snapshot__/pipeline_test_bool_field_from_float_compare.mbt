///|
struct JsBoolFieldRepro {
  flag : Bool
}

///|
fn main {
  let false_flag = 2.0F <= 1.0F
  let false_value = JsBoolFieldRepro::{ flag: false_flag }
  println(false_value.flag == false)

  let true_flag = 0.5F <= 1.0F
  let true_value = JsBoolFieldRepro::{ flag: true_flag }
  println(true_value.flag == true)
}
