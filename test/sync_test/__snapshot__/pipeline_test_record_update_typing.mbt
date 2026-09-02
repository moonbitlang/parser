struct Poly_r[T, K, L] {
  pr1:T
  pr2:K
  pr3:L
}

struct R {
  r1:Int
  r2:Bool
  r3:Double
}

struct Nested_r {
  nr1:Int
  nr2:R
}

fn[T, K, L] update_poly_r_snd_check (pr: Poly_r[T, K, L], pr2: K) ->
     Poly_r[T, K, L] {
  let ret : Poly_r[T, K, L] = {..pr, pr2, }
  ret
}

fn[T, K, L] update_poly_r_snd_infer (pr: Poly_r[T, K, L], pr2: K) ->
     Poly_r[T, K, L] {
  {..pr, pr2, }
}

fn init {
  let ra = { r1:893, r2:false, r3:46.49 }
  let rb_infer = {..ra, r1:894, }
  let rb_check : R = {..rb_infer, r3:89.4, }
  let nr = { nr1:16, nr2:{..rb_check, r2:true, } }
  let nr_check : Nested_r = { nr1:16, nr2:{..rb_check, r2:true, } }
  ()
}

///|
#valtype
struct WideRecord {
  first : Int
  second : Int
  third : Int
  fourth : Int
  fifth : Int
  sixth : Int
  seventh : Int
}

///|
#valtype
struct WideTuple(Int, Int, Int, Int, Int, Int, Int)

///|
fn update_wide_record(record : WideRecord) -> WideRecord {
  {
    ..record,
    seventh: record.seventh * 10,
    first: record.first * 10,
    second: record.second * 10,
  }
}

///|
fn check_wide_value_structs(condition : Bool) -> Unit {
  if !condition {
    abort("wide value struct update failure")
  }
}

///|
fn init {
  let original = WideRecord::{
    first: 1,
    second: 2,
    third: 3,
    fourth: 4,
    fifth: 5,
    sixth: 6,
    seventh: 7,
  }
  let updated = update_wide_record(original)
  check_wide_value_structs(updated.first == 10)
  check_wide_value_structs(updated.second == 20)
  check_wide_value_structs(updated.third == 3)
  check_wide_value_structs(updated.fourth == 4)
  check_wide_value_structs(updated.fifth == 5)
  check_wide_value_structs(updated.sixth == 6)
  check_wide_value_structs(updated.seventh == 70)
  check_wide_value_structs(original.first == 1)
  check_wide_value_structs(original.second == 2)
  check_wide_value_structs(original.seventh == 7)

  let tuple = WideTuple(1, 2, 3, 4, 5, 6, 7)
  match tuple {
    WideTuple(first, second, third, fourth, fifth, sixth, seventh) =>
      check_wide_value_structs(
        first == 1 &&
        second == 2 &&
        third == 3 &&
        fourth == 4 &&
        fifth == 5 &&
        sixth == 6 &&
        seventh == 7,
      )
  }
}
