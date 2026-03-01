///|
pub(all) struct Name {
  name : String
  loc : Location
} derive(ToJson)

///|
pub(all) struct QualifiedName {
  name : QualifiedIdent
  loc : Location
}

///|
pub(all) struct TypeParamWithConstraints {
  name : Name
  constraints : @list.List[QualifiedName]
}

///|
pub(all) enum TypeParamNoConstraints {
  Name(Name)
  Underscore(Location)
}

///|
pub(all) struct MethodSelfType {
  name : String
  is_object : Bool
  loc : Location
}

///|
pub(all) struct FuncSig {
  attrs : Attributes
  type_name : MethodSelfType?
  name : Name
  params : @list.List[Parameter]
  return_ : (Type, ErrorType)
  type_params : @list.List[TypeParamWithConstraints]
  is_async : Location?
}

///|
pub(all) enum TypeDefinition {
  Abstract
  Newtype(Type)
  TupleStruct(@list.List[Type])
  Error(@syntax.ExceptionDecl)
  Variant(@list.List[ConstrDecl])
  Record(fields~ : @list.List[FieldDecl], constr~ : FuncSig?)
  Alias(Type)
}

///|
pub(all) struct TypeSig {
  name : Name
  type_params : @list.List[TypeParamNoConstraints]
  components : TypeDefinition
  vis : Visibility
  attrs : Attributes
}

///|
pub(all) enum AliasSig {
  TypeAlias(
    name~ : Name,
    type_params~ : @list.List[TypeParamNoConstraints],
    type_~ : Type,
    vis~ : Visibility,
    attrs~ : Attributes
  )
  Using(
    pkg~ : Name,
    target~ : Name,
    name~ : Name?,
    kind~ : @syntax.UsingKind,
    attrs~ : Attributes
  )
  FuncAlias(name~ : Name, type_name~ : Name, attrs~ : Attributes)
}

///|
pub(all) struct TraitMethodSig {
  name : Name
  params : @list.List[Parameter]
  has_default : Bool
  return_ : (Type, ErrorType)
  attrs : Attributes
}

///|
pub(all) struct TraitSig {
  name : Name
  super_traits : @list.List[QualifiedName]
  methods : @list.List[TraitMethodSig]
  vis : Visibility
  attrs : Attributes
}

///|
pub(all) struct ImplSig {
  type_params : @list.List[TypeParamWithConstraints]
  type_ : Type
  trait_name : QualifiedName
  attrs : Attributes
}

///|
pub(all) struct ConstSig {
  name : Name
  type_ : Type
  value : Constant
  attrs : Attributes
}

///|
pub(all) struct ValueSig {
  name : Name
  type_ : Type
  attrs : Attributes
}

///|
pub(all) struct PackageImport {
  name : String
  alias_ : String?
}

///|
pub(all) enum Sig {
  Func(FuncSig)
  Type(TypeSig)
  Alias(AliasSig)
  Trait(TraitSig)
  Impl(ImplSig)
  Const(ConstSig)
  Value(ValueSig)
}

///|
pub fn Sig::attrs(self : Sig) -> Attributes {
  match self {
    Const({ attrs, .. })
    | Impl({ attrs, .. })
    | Trait({ attrs, .. })
    | Alias(Using(attrs~, ..))
    | Alias(TypeAlias(attrs~, ..))
    | Alias(FuncAlias(attrs~, ..))
    | Type({ attrs, .. })
    | Func({ attrs, .. })
    | Value({ attrs, .. }) => attrs
  }
}

///|
pub(all) struct Mbti {
  package_name : String
  imports : @list.List[PackageImport]
  sigs : @list.List[(Sig, Location)]
  loc : Location
}
