///|
pub(all) enum Visibility {
  Default
  Pub(attr~ : String?, loc~ : Location)
  Priv(loc~ : Location)
} derive(ToJson)

///|
pub(all) enum Constant {
  Bool(Bool)
  Byte(ByteLiteral)
  Bytes(BytesLiteral)
  Char(CharLiteral)
  Int(String)
  Int64(String)
  UInt(String)
  UInt64(String)
  Float(String)
  Double(String)
  String(StringLiteral)
  BigInt(String)
} derive(ToJson)

///|
pub(all) struct Name {
  name : String
  loc : Location
} derive(ToJson)

///|
pub(all) struct QualifiedName {
  pkg : String?
  name : String
  loc : Location
} derive(ToJson)

///|
pub(all) enum Type {
  TAny(Location)
  TArrow(
    @immut/list.T[Type],
    Type,
    err~ : ErrorType,
    is_async~ : Bool,
    loc~ : Location
  )
  TTuple(@immut/list.T[Type], loc~ : Location)
  TName(QualifiedName, @immut/list.T[Type], loc~ : Location)
  TOption(Type, loc~ : Location, question_loc~ : Location)
  TObject(QualifiedName)
} derive(ToJson)

///|
pub(all) enum ErrorType {
  ErrorType(Type)
  DefaultErrorType(Location)
  NoErrorType
  MaybeError(Type)
} derive(ToJson)

///|
pub(all) struct ConstrParam {
  type_ : Type
  mut_ : Bool
  label : Name?
} derive(ToJson)

///|
pub(all) struct ConstrDecl {
  name : Name
  args : @immut/list.T[ConstrParam]?
  tag : (String, Location)?
  loc : Location
} derive(ToJson)

///|
pub(all) enum ExceptionDecl {
  NoPayload
  SinglePayload(Type)
  EnumPayload(@immut/list.T[ConstrDecl])
} derive(ToJson)

///|
pub(all) struct FieldDecl {
  name : Name
  type_ : Type
  mut_ : Bool
  vis : Visibility
  loc : Location
} derive(ToJson)

///|
pub(all) enum TypeDesc {
  Abstract
  Extern
  Newtype(Type)
  Error(ExceptionDecl)
  Variant(@immut/list.T[ConstrDecl])
  Record(@immut/list.T[FieldDecl])
  Alias(Type)
} derive(ToJson)
