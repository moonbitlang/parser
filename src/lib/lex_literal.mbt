///|
pub typealias CharLiteral = String

///|
pub typealias StringLiteral = String

///|
pub typealias ByteLiteral = String

///|
pub typealias BytesLiteral = String

///|
pub struct InterpSource {
  source : String
  loc : Location
} derive(Show)

///|
pub enum InterpElem {
  InterpLit(repr~ : String, loc~ : Location)
  InterpSource(InterpSource)
} derive(Show)

///|
pub typealias InterpLiteral = Array[InterpElem]
