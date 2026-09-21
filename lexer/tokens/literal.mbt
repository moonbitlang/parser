///|
pub type CharLiteral = String

///|
pub type StringLiteral = String

///|
pub type ByteLiteral = String

///|
pub type BytesLiteral = String

///|
pub(all) struct InterpSource {
  source : String
  loc : Location
} derive(Debug, ToJson, Eq)

///|
pub(all) enum InterpElem {
  InterpLit(repr~ : String, loc~ : Location)
  InterpSource(InterpSource)
} derive(Debug, Eq)

///|
pub type InterpLiteral = Array[InterpElem]

///|
fn InterpLiteral::to_string_repr(self : InterpLiteral) -> String {
  let buf = StringBuilder()
  buf.write_char('"')
  for elem in self {
    match elem {
      InterpLit(repr~, ..) => buf.write_string(repr)
      InterpSource(source) => {
        buf.write_string("\\{")
        buf.write_string(source.source)
        buf.write_char('}')
      }
    }
  } nobreak {
    buf.write_char('"')
    buf.to_string()
  }
}

///|
#deprecated
#doc(hidden)
pub extend InterpElem with Debug::{to_repr}

///|
#deprecated
#doc(hidden)
pub extend InterpElem with Eq::{not_equal, equal}

///|
#deprecated
#doc(hidden)
pub extend InterpSource with Debug::{to_repr}

///|
#deprecated
#doc(hidden)
pub extend InterpSource with Eq::{not_equal, equal}

///|
#deprecated
#doc(hidden)
pub extend InterpSource with ToJson::{to_json}
