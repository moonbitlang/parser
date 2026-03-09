fn init {
  let a = while false {  } nobreak { 42 }
  println(a)
  let b = for ; false ; {  } nobreak { 42 }
  println(b)
  let c = for ; ; { break 42 } nobreak { 0 }
  println(c)
  let d = while true { break 42 } nobreak { 0 }
  println(d)
  let e = while false {  } nobreak { for ; ; { break 42 } nobreak { 0 } }
  println(e)
}
