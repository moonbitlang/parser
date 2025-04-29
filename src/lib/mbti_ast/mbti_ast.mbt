///|
pub(all) struct TypeParamWithConstraints {
  name : String
  constraints : @immut/list.T[QualifiedName]
} derive(ToJson)

///|
pub(all) enum TypeParamNoConstraints {
  Name(Name)
  Underscore(Location)
} derive(ToJson)

///|
pub(all) enum Parameter {
  Positional(Type)
  Labelled(Name, Type)
  Autofill(Name, Type)
  OptionalDefault(Name, Type)
  OptionalOption(Name, Type)
} derive(ToJson)

///|
pub(all) enum TraitMethodParameter {
  Positional(Type)
  Labelled(Name, Type)
} derive(ToJson)

///|
pub(all) struct FuncSig {
  name : Name
  params : @immut/list.T[Parameter]
  return_ : (Type, ErrorType)
  type_params : @immut/list.T[TypeParamWithConstraints]
} derive(ToJson)

///|
pub(all) struct TypeSig {
  name : Name
  type_params : @immut/list.T[TypeParamNoConstraints]
  components : TypeDesc
  vis : Visibility
} derive(ToJson)

///|
pub(all) enum AliasSig {
  TypeAlias(
    name~ : Name,
    type_params~ : TypeParamNoConstraints,
    type_~ : Type,
    vis~ : Visibility
  )
  TraitAlias(name~ : Name, trait_name~ : QualifiedName, vis~ : Visibility)
} derive(ToJson)

///|
pub(all) struct TraitMethodSig {
  name : Name
  params : @immut/list.T[TraitMethodParameter]
  has_default : Bool
  return_ : (Type, ErrorType)
} derive(ToJson)

///|
pub(all) struct TraitSig {
  name : Name
  super_traits : @immut/list.T[QualifiedName]
  methods : @immut/list.T[TraitMethodSig]
  vis : Visibility
} derive(ToJson)

///|
pub(all) enum ImplSig {
  Trait(
    type_params~ : @immut/list.T[TypeParamWithConstraints],
    type_~ : Type,
    trait_name~ : QualifiedName
  )
  Method(type_name~ : Name, methods~ : @immut/list.T[FuncSig])
  DefaultImpl(trait_name~ : Name, method_name~ : Name)
} derive(ToJson)

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
} derive(ToJson)

///|
pub(all) struct Mbti {
  package_name : String
  imports : @immut/list.T[PackageImport]
  sigs : @immut/list.T[(Sig, Location)]
} derive(ToJson)
