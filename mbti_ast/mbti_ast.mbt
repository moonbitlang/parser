///|
pub(all) struct Name {
  name : String
  loc : Location
} derive(Debug, ToJson)

///|
pub(all) struct QualifiedName {
  name : QualifiedIdent
  loc : Location
} derive(Debug)

///|
pub(all) struct TypeParamWithConstraints {
  name : Name
  constraints : @list.List[QualifiedName]
} derive(Debug)

///|
pub(all) enum TypeParamNoConstraints {
  Name(Name)
  Underscore(Location)
} derive(Debug)

///|
pub(all) struct MethodSelfType {
  name : String
  is_object : Bool
  loc : Location
} derive(Debug)

///|
pub(all) struct FuncSig {
  attrs : Attributes
  type_name : MethodSelfType?
  name : Name
  params : @list.List[Parameter]
  return_ : (Type, ErrorType)
  type_params : @list.List[TypeParamWithConstraints]
  is_async : Location?
} derive(Debug)

///|
pub(all) enum TypeDefinition {
  Abstract
  Newtype(Type)
  TupleStruct(@list.List[Type])
  Error(@syntax.ExceptionDecl)
  ExtensibleEnum(@list.List[ConstrDecl])
  ExtendEnum(target~ : QualifiedName, constructors~ : @list.List[ConstrDecl])
  Variant(@list.List[ConstrDecl])
  Record(fields~ : @list.List[FieldDecl])
  Alias(Type)
} derive(Debug)

///|
pub(all) struct TypeSig {
  name : Name
  type_params : @list.List[TypeParamNoConstraints]
  components : TypeDefinition
  vis : Visibility
  attrs : Attributes
  derives : @list.List[QualifiedName]
} derive(Debug)

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
} derive(Debug)

///|
pub(all) struct TraitMethodSig {
  name : Name
  type_params : @list.List[TypeParamWithConstraints]
  params : @list.List[Parameter]
  is_async : Location?
  has_default : Bool
  return_ : (Type, ErrorType)
  attrs : Attributes
} derive(Debug)

///|
pub(all) struct TraitSig {
  name : Name
  super_traits : @list.List[QualifiedName]
  methods : @list.List[TraitMethodSig]
  vis : Visibility
  attrs : Attributes
} derive(Debug)

///|
pub(all) struct ImplSig {
  type_params : @list.List[TypeParamWithConstraints]
  type_ : Type
  trait_name : QualifiedName
  attrs : Attributes
} derive(Debug)

///|
pub(all) struct ConstSig {
  name : Name
  type_ : Type
  value : Constant
  attrs : Attributes
} derive(Debug)

///|
pub(all) struct ValueSig {
  name : Name
  type_ : Type
  attrs : Attributes
} derive(Debug)

///|
pub(all) struct PackageImport {
  name : String
  alias_ : String?
} derive(Debug)

///|
pub(all) enum Sig {
  Func(FuncSig)
  Type(TypeSig)
  Alias(AliasSig)
  Trait(TraitSig)
  Impl(ImplSig)
  Const(ConstSig)
  Value(ValueSig)
} derive(Debug)

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
  base_pos : BasePos
} derive(Debug)
