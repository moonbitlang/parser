///|
pub(all) enum TypePath {
  Unit
  Bool
  Byte
  Char
  Int
  Int16
  Int64
  UInt
  UInt16
  UInt64
  Float
  Double
  String
  Option
  Result
  ErrorValueResult
  FixedArray
  Bytes
  Ref
  Error
  Toplevel(pkg~ : String, id~ : String)
  Local(toplevel_id~ : ToplevelId, name~ : String)
  Tuple(Int)
  Constr(ty~ : TypePath, tag~ : ConstrTag)
  MaybeError
} derive(Compare, Hash, Eq)

///|
pub fn TypePath::equal_with(self : TypePath, other : TypePath) -> Bool {
  match (self, other) {
    (Result, ErrorValueResult) | (ErrorValueResult, Result) => true
    _ => self == other
  }
}

///|
pub(all) enum ToplevelId {
  Regular(pkg~ : String, name~ : String)
  Method(self_type~ : TypePath, name~ : String)
  LocalMethod(pkg~ : String, self_type~ : TypePath, name~ : String)
  ExtMethod(
    pkg~ : String,
    trait_~ : TypePath,
    self_type~ : TypePath,
    name~ : String
  )
} derive(Compare, Hash, Eq)

///|
pub let type_path_unit : TypePath = TypePath::Unit

///|
pub let type_path_bool : TypePath = TypePath::Bool

///|
pub let type_path_byte : TypePath = TypePath::Byte

///|
pub let type_path_char : TypePath = TypePath::Char

///|
pub let type_path_int : TypePath = TypePath::Int

///|
pub let type_path_int16 : TypePath = TypePath::Int16

///|
pub let type_path_int64 : TypePath = TypePath::Int64

///|
pub let type_path_uint : TypePath = TypePath::UInt

///|
pub let type_path_uint16 : TypePath = TypePath::UInt16

///|
pub let type_path_uint64 : TypePath = TypePath::UInt64

///|
pub let type_path_float : TypePath = TypePath::Float

///|
pub let type_path_double : TypePath = TypePath::Double

///|
pub let type_path_string : TypePath = TypePath::String

///|
pub let type_path_option : TypePath = TypePath::Option

///|
pub let type_path_result : TypePath = TypePath::Result

///|
pub let type_path_multi_value_result : TypePath = TypePath::ErrorValueResult

///|
pub let type_path_fixed_array : TypePath = TypePath::FixedArray

///|
pub let type_path_bytes : TypePath = TypePath::Bytes

///|
pub let type_path_ref : TypePath = TypePath::Ref

///|
pub let type_path_error : TypePath = TypePath::Error

///|
pub let type_path_maybe_error : TypePath = TypePath::MaybeError

///|
pub let type_path_no_error : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="NoError",
)

///|
pub let type_path_sourceloc : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="SourceLoc",
)

///|
pub let type_path_argsloc : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="ArgsLoc",
)

///|
pub let type_path_array : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="Array",
)

///|
pub let type_path_arrayview : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="ArrayView",
)

///|
pub let type_path_map : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="Map",
)

///|
pub let type_path_json : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="Json",
)

///|
pub let type_path_iter : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="Iter",
)

///|
pub let type_path_iter2 : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="Iter2",
)

///|
pub let type_path_iter_result : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="IterResult",
)

///|
pub let type_path_bytesview : TypePath = TypePath::Toplevel(
  pkg="moonbitlang/core/bytes",
  id="View",
)

///|
pub let type_path_stringview : TypePath = TypePath::Toplevel(
  pkg="moonbitlang/core/string",
  id="StringView",
)

// This type is used for desugaring [foreach] only,
// and is not exposed to users.
// Note that builtin package is under our control,
// so no need to worry about name clash here.

///|
pub let type_path_foreach_result : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="ForeachResult",
)

///|
pub let type_path_bigint : TypePath = TypePath::Toplevel(
  pkg="moonbitlang/core/bigint",
  id="BigInt",
)

///|
pub let type_path_maybe_uninit : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="UnsafeMaybeUninit",
)

///|
pub let type_path_func_ref : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="FuncRef",
)

// Trait paths

///|
pub let trait_eq : TypePath = TypePath::Toplevel(pkg=builtin_package, id="Eq")

///|
pub let trait_compare : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="Compare",
)

///|
pub let trait_hash : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="Hash",
)

///|
pub let trait_show : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="Show",
)

///|
pub let trait_default : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="Default",
)

///|
pub let trait_to_json : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="ToJson",
)

///|
pub let default_impl_placeholder : TypePath = TypePath::Toplevel(
  pkg="",
  id="$default_impl",
)

// Traits for operator overloading

///|
pub let trait_add : TypePath = TypePath::Toplevel(pkg=builtin_package, id="Add")

///|
pub let trait_sub : TypePath = TypePath::Toplevel(pkg=builtin_package, id="Sub")

///|
pub let trait_mul : TypePath = TypePath::Toplevel(pkg=builtin_package, id="Mul")

///|
pub let trait_div : TypePath = TypePath::Toplevel(pkg=builtin_package, id="Div")

///|
pub let trait_mod : TypePath = TypePath::Toplevel(pkg=builtin_package, id="Mod")

///|
pub let trait_neg : TypePath = TypePath::Toplevel(pkg=builtin_package, id="Neg")

///|
pub let trait_shl : TypePath = TypePath::Toplevel(pkg=builtin_package, id="Shl")

///|
pub let trait_shr : TypePath = TypePath::Toplevel(pkg=builtin_package, id="Shr")

///|
pub let trait_bit_and : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="BitAnd",
)

///|
pub let trait_bit_or : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="BitOr",
)

///|
pub let trait_bit_xor : TypePath = TypePath::Toplevel(
  pkg=builtin_package,
  id="BitXOr",
)
