///|
pub(all) enum Ast {
  Null(Location)
  False(Location)
  True(Location)
  Str(str~ : String, loc~ : Location)
  Float(float~ : String, loc~ : Location)
  Arr(content~ : Array[Ast], loc~ : Location)
  Obj(map~ : Map[String, Ast], loc~ : Location)
} derive(Debug, Eq)

///|
pub fn Ast::loc(self : Self) -> Location {
  match self {
    Null(loc) => loc
    False(loc) => loc
    True(loc) => loc
    Str(loc~, ..) => loc
    Float(loc~, ..) => loc
    Arr(loc~, ..) => loc
    Obj(loc~, ..) => loc
  }
}

///|
pub fn Ast::as_bool(self : Self) -> Bool? {
  match self {
    True(_) => Some(true)
    False(_) => Some(false)
    _ => None
  }
}

///|
pub fn Ast::as_string(self : Self) -> String? {
  match self {
    Str(str~, ..) => Some(str)
    _ => None
  }
}

///|
pub fn Ast::as_number(self : Self) -> String? {
  match self {
    Float(float~, ..) => Some(float)
    _ => None
  }
}

///|
pub fn Ast::as_array(self : Self) -> Array[Ast]? {
  match self {
    Arr(content~, ..) => Some(content)
    _ => None
  }
}

///|
pub fn Ast::as_object(self : Self) -> Map[String, Ast]? {
  match self {
    Obj(map~, ..) => Some(map)
    _ => None
  }
}

///|
pub impl ToJson for Ast with to_json(self) {
  match self {
    Null(loc) => { "type": "Null", "loc": loc }
    False(loc) => { "type": "False", "loc": loc }
    True(loc) => { "type": "True", "loc": loc }
    Str(str~, loc~) => { "type": "Str", "str": str, "loc": loc }
    Float(float~, loc~) => { "type": "Float", "float": float, "loc": loc }
    Arr(content~, loc~) => { "type": "Arr", "content": content, "loc": loc }
    Obj(map~, loc~) => { "type": "Obj", "map": map, "loc": loc }
  }
}
