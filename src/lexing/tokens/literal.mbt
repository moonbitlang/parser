///|
pub typealias String as CharLiteral

///|
pub typealias String as StringLiteral

///|
pub typealias String as ByteLiteral

///|
pub typealias String as BytesLiteral

///|
pub(all) struct InterpSource {
  source : String
  loc : Location
} derive(Show, ToJson, Eq)

///|
pub(all) enum InterpElem {
  InterpLit(repr~ : String, loc~ : Location)
  InterpSource(InterpSource)
} derive(Show, Eq)

///|
pub typealias Array[InterpElem] as InterpLiteral
