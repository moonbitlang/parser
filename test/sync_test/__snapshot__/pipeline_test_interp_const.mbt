///|
const IntConst : Int = 42

///|
const ByteConst : Byte = 42

///|
const StringConst : String = "😆😆"

///|
const CharConst : Char = '→'

///|
const Int64Const : Int64 = 42

///|
const UIntConst : UInt = 42

///|
const UInt64Const : UInt64 = 42

///|
const Interp : String =
  $|\{true},\{false} : Bool
  $|\{IntConst} : Int
  $|\{ByteConst} : Byte
  $|\{Int64Const} : Int64
  $|\{UIntConst} : UInt
  $|\{UInt64Const} : UInt64
  $|\{CharConst} : Char
  $|\{StringConst} : String

///|
let interp : String =
  $|\{true},\{false} : Bool
  $|\{IntConst} : Int
  $|\{ByteConst} : Byte
  $|\{Int64Const} : Int64
  $|\{UIntConst} : UInt
  $|\{UInt64Const} : UInt64
  $|\{CharConst} : Char
  $|\{StringConst} : String

///|
fn main {
  println(Interp)
  println(interp is Interp)
}
