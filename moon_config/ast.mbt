///|
pub(all) enum Ast {
  Null(loc~ : Location)
  Bool(Bool, loc~ : Location)
  Str(String, loc~ : Location)
  Float(String, loc~ : Location)
  Arr(Vector[Ast], loc~ : Location)
  Obj(Vector[(String, Ast)], loc~ : Location)
} derive(Debug, Eq)

///|
pub fn Ast::loc(self : Self) -> Location {
  match self {
    Null(loc~) => loc
    Bool(_, loc~) => loc
    Str(_, loc~) => loc
    Float(_, loc~) => loc
    Arr(_, loc~) => loc
    Obj(_, loc~) => loc
  }
}

///|
pub fn Ast::as_bool(self : Self) -> Bool? {
  match self {
    Bool(value, ..) => Some(value)
    _ => None
  }
}

///|
pub fn Ast::as_string(self : Self) -> String? {
  match self {
    Str(str, ..) => Some(str)
    _ => None
  }
}

///|
pub fn Ast::as_number(self : Self) -> String? {
  match self {
    Float(float, ..) => Some(float)
    _ => None
  }
}

///|
pub fn Ast::as_array(self : Self) -> Vector[Ast]? {
  match self {
    Arr(content, ..) => Some(content)
    _ => None
  }
}

///|
pub fn Ast::as_object(self : Self) -> Vector[(String, Ast)]? {
  match self {
    Obj(fields, ..) => Some(fields)
    _ => None
  }
}

///|
pub impl ToJson for Ast with to_json(self) {
  match self {
    Null(loc~) => { "type": "Null", "loc": loc }
    Bool(value, loc~) => { "type": "Bool", "value": value, "loc": loc }
    Str(str, loc~) => { "type": "Str", "str": str, "loc": loc }
    Float(float, loc~) => { "type": "Float", "float": float, "loc": loc }
    Arr(content, loc~) => { "type": "Arr", "content": content, "loc": loc }
    Obj(fields, loc~) => { "type": "Obj", "fields": fields, "loc": loc }
  }
}
