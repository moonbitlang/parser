///|
fn f(xs : Array[Int], ys : Array[String], zs : Iter[Int]) -> Unit {
  println("-----")
  let mut sum1 = 0
  let n = l1~: for x in xs {
    let mut sum2 = ""
    let s = l2~: for y in ys {
      sum2 = sum2 + y
      l3~: for z in zs {
        match z {
          0 => { break l1~ 42 }
          1 => { continue l1~ }
          2 => { break l2~ "forty-two" }
          3 => { continue l2~ }
          4 => { 
            println("break l3")
            break l3~ 
          }
          5 => { 
            println("continue l3")
            continue l3~ 
          }
          _ => ()
        }
        println("unrecognized int")
      }
    } else {
      sum2
    }
    sum1 = sum1 + x + s.length()
  } else {
    sum1
  }
  println(n)
}

fn main {
  f([1, 2], ["x", "xx"], [-1, -1].iter())
  f([1, 2], ["x", "xx"], [0].iter())
  f([1, 2], ["x", "xx"], [1].iter())
  f([1, 2], ["x", "xx"], [2].iter())
  f([1, 2], ["x", "xx"], [3].iter())
  f([1, 2], ["x", "xx"], [4, 4].iter())
  f([1, 2], ["x", "xx"], [5, 5].iter())
}
