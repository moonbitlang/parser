///|
/// A single-field `#valtype` struct for each representation class its field can
/// take (numeric widths, references, boxed struct, enum, function).
#valtype
struct WI64 {
  a : Int64
} derive(Debug)

///|
#valtype
struct WDouble {
  a : Double
} derive(Debug)

///|
#valtype
struct WBool {
  a : Bool
} derive(Debug)

///|
#valtype
struct WStr {
  a : String
} derive(Debug)

///|
#valtype
struct WArr {
  a : Array[Int]
} derive(Debug)

///|
#valtype
struct WBytes {
  a : Bytes
} derive(Debug)

///|
struct Point {
  x : Int
  y : Int
} derive(Debug)

///|
#valtype
struct WStruct {
  a : Point
} derive(Debug)

///|
enum Color {
  Red
  Green
  Blue
} derive(Debug)

///|
#valtype
struct WEnum {
  a : Color
} derive(Debug)

///|
#valtype
struct WFn {
  a : (Int) -> Int
}

///|
fn main {
  debug(WI64::{ a: 9_000_000_000L })
  debug(WDouble::{ a: 3.5 })
  debug(WBool::{ a: true })
  debug(WStr::{ a: "hello" })
  debug(WArr::{ a: [1, 2, 3] })
  debug(WBytes::{ a: b"ab" })
  debug(WStruct::{ a: Point::{ x: 1, y: 2 } })
  debug(WEnum::{ a: Green })
  let i64box = WI64::{ a: 100L }
  println(i64box.a)
  let strbox = WStr::{ a: "world" }
  println(strbox.a)
  let arrbox = WArr::{ a: [4, 5, 6] }
  println(arrbox.a.length())
  let structbox = WStruct::{ a: Point::{ x: 10, y: 20 } }
  println(structbox.a.x)
  let fnbox = WFn::{ a: n => n + 1 }
  println((fnbox.a)(41))
}
