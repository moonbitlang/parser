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
