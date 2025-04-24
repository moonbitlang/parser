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
  loc : Loc
} derive(Show)

///|
pub enum InterpElem {
  InterpLit(repr~ : String, loc~ : Loc)
  InterpSource(InterpSource)
} derive(Show)

///|
pub typealias InterpLiteral = Array[InterpElem]
