///|
pub(all) struct Name {
  name : String
  loc : Location
} derive(ToJson)

///|
pub(all) struct QualifiedName {
  name : QualifiedIdent
  loc : Location
} derive(ToJson)

///|
pub(all) struct TypeParamWithConstraints {
  name : Name
  constraints : @list.List[QualifiedName]
} derive(ToJson)

///|
pub(all) enum TypeParamNoConstraints {
  Name(Name)
  Underscore(Location)
} derive(ToJson(style="legacy"))

///|
pub(all) enum Parameter {
  Positional(Type)
  Labelled(Label, Type)
  Autofill(Label, Type)
  OptionalDefault(Label, Type)
  OptionalOption(Label, Type)
} derive(ToJson(style="legacy"))

///|
pub(all) enum TraitMethodParameter {
  Positional(Type)
  Labelled(Label, Type)
} derive(ToJson(style="legacy"))

///|
pub(all) struct FuncSig {
  attr : @list.List[(String, String?, String)]
  type_name : Name?
  name : Name
  params : @list.List[Parameter]
  return_ : (Type, ErrorType)
  type_params : @list.List[TypeParamWithConstraints]
} derive(ToJson)

///|
pub(all) struct TypeSig {
  name : Name
  type_params : @list.List[TypeParamNoConstraints]
  components : TypeDesc
  vis : Visibility
} derive(ToJson)

///|
pub(all) enum AliasSig {
  TypeAlias(
    name~ : Name,
    type_params~ : @list.List[TypeParamNoConstraints],
    type_~ : Type,
    vis~ : Visibility
  )
  TraitAlias(name~ : Name, trait_name~ : QualifiedName, vis~ : Visibility)
  FnAlias(name~ : Name, type_name~ : QualifiedName, loc~ : Location)
} derive(ToJson(style="legacy"))

///|
pub(all) struct TraitMethodSig {
  name : Name
  params : @list.List[TraitMethodParameter]
  has_default : Bool
  return_ : (Type, ErrorType)
} derive(ToJson)

///|
pub(all) struct TraitSig {
  name : Name
  super_traits : @list.List[QualifiedName]
  methods : @list.List[TraitMethodSig]
  vis : Visibility
} derive(ToJson)

///|
pub(all) enum ImplSig {
  Trait(
    type_params~ : @list.List[TypeParamWithConstraints],
    type_~ : Type,
    trait_name~ : QualifiedName
  )
  DefaultImpl(trait_name~ : Name, method_name~ : Name)
} derive(ToJson(style="legacy"))

///|
pub(all) struct ConstSig {
  name : Name
  type_ : Type
  value : Constant
} derive(ToJson)

///|
pub(all) struct ValueSig {
  name : Name
  type_ : Type
} derive(ToJson)

///|
pub(all) struct PackageImport {
  name : String
  alias_ : String?
} derive(ToJson)

///|
pub(all) enum Sig {
  Func(FuncSig)
  Type(TypeSig)
  Alias(AliasSig)
  Trait(TraitSig)
  Impl(ImplSig)
  Const(ConstSig)
  Value(ValueSig)
} derive(ToJson(style="legacy"))

///|
pub(all) struct Mbti {
  package_name : String
  imports : @list.List[PackageImport]
  sigs : @list.List[(Sig, Location)]
} derive(ToJson)
