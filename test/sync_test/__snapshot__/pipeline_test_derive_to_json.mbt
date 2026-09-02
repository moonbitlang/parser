///|
struct T1[X] {
  x : Int
  y : String
  z : Bool
  w : X
  m : (Int, Int)
} derive(ToJson, Debug)

///|
struct NT[X](Array[X]) derive(ToJson, Debug)

///|
enum E1[X, Y] {
  C1
  C2(x~ : T1[X], (Double, NT[Y]))
} derive(ToJson, Debug)

///|
suberror ErrNoPayload derive(ToJson, Debug)

///|
suberror ErrNewType {
  ErrNewType(Int)
} derive(ToJson, Debug)

///|
fn init {
  let t1 = { x: 42, y: "str", z: true, w: [0], m: (1, 2) }
  println(@json.to_json(t1).stringify())
  println(@json.to_json(NT([t1])).stringify())
  println(@json.to_json((C1 : E1[Int, Int])).stringify())
  println(@json.to_json(E1::C2(x=t1, (4.2, NT([{ "k": "v" }])))).stringify())
  println(@json.to_json(ErrNoPayload).stringify())
  println(@json.to_json(ErrNewType(42)).stringify())
}

///|
struct StructWithOptionalFields {
  number : Int
  string : String
  optional_number : Int?
  nested_optional_number : Int??
  optional_string : String?
  nested_optional_string : String??
  number2 : Int
} derive(ToJson)

///|
fn init {
  println("struct with optional fields:")
  let s = {
    number: 42,
    string: "str",
    optional_number: Some(42),
    nested_optional_number: Some(Some(42)),
    optional_string: Some("str"),
    nested_optional_string: Some(Some("str")),
    number2: 42,
  }
  let expected : Json = {
    "number": 42,
    "string": "str",
    "optional_number": 42,
    "nested_optional_number": [42],
    "optional_string": "str",
    "nested_optional_string": ["str"],
    "number2": 42,
  }
  let json = @json.to_json(s)
  println("\{json == expected}, json:\{json.stringify()}")
  let s = {
    number: 42,
    string: "str",
    optional_number: None,
    nested_optional_number: None,
    optional_string: None,
    nested_optional_string: None,
    number2: 42,
  }
  let expected : Json = { "number": 42, "string": "str", "number2": 42 }
  let json = @json.to_json(s)
  println("\{json == expected}, json:\{json.stringify()}")
  let s = {
    number: 42,
    string: "str",
    optional_number: Some(42),
    nested_optional_number: Some(None),
    optional_string: Some("str"),
    nested_optional_string: Some(None),
    number2: 42,
  }
  let expected : Json = {
    "number": 42,
    "string": "str",
    "optional_number": 42,
    "nested_optional_number": Json::null(),
    "optional_string": "str",
    "nested_optional_string": Json::null(),
    "number2": 42,
  }
  let json = @json.to_json(s)
  println("\{json == expected}, json:\{json.stringify()}")
}

// Derive arguments
// Field and variant renames

///|
struct FieldRename {
  a : Int
} derive(ToJson(fields(a(rename="b"))))

///|
enum VariantRename {
  RenameMeToD1(Int)
  C2(String)
} derive(ToJson(cases(RenameMeToD1(rename="D1"))))

///|
struct FieldRenameAllCamel {
  my_field : Int
} derive(ToJson(rename_fields="camelCase"))

///|
struct FieldRenameAllScreamingSnake {
  my_field : Int
} derive(ToJson(rename_fields="SCREAMING_SNAKE_CASE"))

///|
enum VariantRenameAllSnake {
  MyVariant(Int)
} derive(ToJson(rename_cases="snake_case"))

///|
fn init {
  println("")
  println("Field and variant renames")
  let s : FieldRename = { a: 42 }
  println(@json.to_json(s).stringify())
  let s = VariantRename::RenameMeToD1(42)
  println(@json.to_json(s).stringify())
  let s : FieldRenameAllCamel = { my_field: 42 }
  println(@json.to_json(s).stringify())
  let s : FieldRenameAllScreamingSnake = { my_field: 42 }
  println(@json.to_json(s).stringify())
  let s = VariantRenameAllSnake::MyVariant(42)
  println(@json.to_json(s).stringify())
}

// ==== New enum styling options ====

///|
enum NewStyleLegacy {
  C1
  C2(String)
  C3(Int, Bool)
} derive(ToJson(style="legacy"))

///|
enum NewStyleFlat {
  C1
  C2(String)
  C3(Int, Bool)
} derive(ToJson(style="flat"))

///|
fn init {
  println("")
  println("New enum style options")
  //
  println("Legacy style")
  let s = NewStyleLegacy::C1
  println(@json.to_json(s).stringify())
  let s = NewStyleLegacy::C2("str")
  println(@json.to_json(s).stringify())
  let s = NewStyleLegacy::C3(42, true)
  println(@json.to_json(s).stringify())
  //
  println("Flat style")
  let s = NewStyleFlat::C1
  println(@json.to_json(s).stringify())
  let s = NewStyleFlat::C2("str")
  println(@json.to_json(s).stringify())
  let s = NewStyleFlat::C3(42, true)
  println(@json.to_json(s).stringify())
}

// ==== Additional comprehensive tests ====

///|
struct TupleStruct(Int,String,Bool) derive(ToJson, Eq, Debug)

///|
struct EmptyStruct {} derive(ToJson, Eq, Debug)

///|
enum SingleVariantEnum {
  OnlyVariant(Int, String)
} derive(ToJson(style="flat"), Eq, Debug)

///|
enum MixedEnum {
  NoArgs
  OneArg(Int)
  TwoArgs(String, Bool)
  NamedArgs(x~ : Int, y~ : String)
  MixedArgs(Int, y~ : String, Bool)
} derive(ToJson(style="flat"), Eq, Debug)

///|
enum DeepNestedEnum {
  Level1(Level2)
} derive(ToJson(style="flat"), Eq, Debug)

///|
enum Level2 {
  Level2Variant(Level3)
} derive(ToJson(style="flat"), Eq, Debug)

///|
struct Level3 {
  value : Int
  nested : Level4?
} derive(ToJson, Eq, Debug)

///|
enum Level4 {
  FinalLevel(String)
} derive(ToJson(style="flat"), Eq, Debug)

///|
struct GenericStruct[T, U] {
  first : T
  second : U
  optional : T?
} derive(ToJson, Eq, Debug)

///|
enum GenericEnum[T] {
  Value(T)
  Pair(T, T)
} derive(ToJson(style="flat"), Eq, Debug)

///|
enum EdgeCaseEnum {
  ZeroFields
  OneField(Int)
  MultipleFields(String, Bool, Double)
  NamedField(name~ : String)
  MixedFields(Int, named~ : String, Bool)
} derive(ToJson(style="flat"), Eq, Debug)

///|
fn init {
  println("")
  println("=== Additional comprehensive tests ===")

  // Test tuple structs
  println("Tuple struct tests:")
  let ts = TupleStruct(42, "hello", true)
  let ts_json = @json.to_json(ts)
  println("Original: \{Debug::to_repr(ts)}")
  println("JSON: \{ts_json.stringify()}")
  println("Expected structure: [\"TupleStruct\", 42, \"hello\", true]")

  // Test empty struct
  println("Empty struct tests:")
  let es = EmptyStruct::{  }
  let es_json = @json.to_json(es)
  println("Original: \{Debug::to_repr(es)}")
  println("JSON: \{es_json.stringify()}")
  println("Expected structure: {}")

  // Test single variant enum
  println("Single variant enum tests (flat style):")
  let sve = SingleVariantEnum::OnlyVariant(123, "test")
  let sve_json = @json.to_json(sve)
  println("Original: \{Debug::to_repr(sve)}")
  println("JSON: \{sve_json.stringify()}")
  println("Expected structure: [\"OnlyVariant\", 123, \"test\"]")

  // Test mixed enum variants in flat style
  println("Mixed enum tests (flat style):")
  let variants = [
    MixedEnum::NoArgs,
    MixedEnum::OneArg(42),
    MixedEnum::TwoArgs("hello", true),
    MixedEnum::NamedArgs(x=10, y="world"),
    MixedEnum::MixedArgs(5, y="mixed", false),
  ]
  for i, variant in variants {
    let json = @json.to_json(variant)
    println("Variant \{i}: \{Debug::to_repr(variant)}")
    println("JSON: \{json.stringify()}")
    match variant {
      NoArgs => println("Expected: \"NoArgs\"")
      OneArg(_) => println("Expected: [\"OneArg\", 42]")
      TwoArgs(_, _) => println("Expected: [\"TwoArgs\", \"hello\", true]")
      NamedArgs(x=_, y=_) => println("Expected: [\"NamedArgs\", 10, \"world\"]")
      MixedArgs(_, y=_, _) =>
        println("Expected: [\"MixedArgs\", 5, \"mixed\", false]")
    }
    println("---")
  }

  // Test deep nesting
  println("Deep nesting tests:")
  let deep = DeepNestedEnum::Level1(
    Level2::Level2Variant(Level3::{
      value: 42,
      nested: Some(Level4::FinalLevel("deep")),
    }),
  )
  let deep_json = @json.to_json(deep)
  println("Original: \{Debug::to_repr(deep)}")
  println("JSON: \{deep_json.stringify()}")
  println("Expected nested structure with flat enum style")

  // Test generics
  println("Generic types tests:")
  let gs : GenericStruct[Int, String] = {
    first: 42,
    second: "hello",
    optional: Some(24),
  }
  let gs_json = @json.to_json(gs)
  println("Original: \{Debug::to_repr(gs)}")
  println("JSON: \{gs_json.stringify()}")
  let ge : GenericEnum[String] = GenericEnum::Pair("first", "second")
  let ge_json = @json.to_json(ge)
  println("Original: \{Debug::to_repr(ge)}")
  println("JSON: \{ge_json.stringify()}")
  println("Expected: [\"Pair\", \"first\", \"second\"]")
}

///|
fn init {
  println("")
  println("=== Edge case verification ===")

  // Test edge cases for flat style
  println("Edge case enum tests (flat style):")
  let edge_cases = [
    EdgeCaseEnum::ZeroFields,
    EdgeCaseEnum::OneField(100),
    EdgeCaseEnum::MultipleFields("test", false, 3.14),
    EdgeCaseEnum::NamedField(name="example"),
    EdgeCaseEnum::MixedFields(99, named="mixed", true),
  ]
  for i, case in edge_cases {
    let json = @json.to_json(case)
    println("Edge case \{i}: \{Debug::to_repr(case)}")
    println("JSON: \{json.stringify()}")
    match case {
      ZeroFields => println("Expected: \"ZeroFields\" (string for zero fields)")
      OneField(_) => println("Expected: [\"OneField\", 100]")
      MultipleFields(_, _, _) =>
        println("Expected: [\"MultipleFields\", \"test\", false, 3.14]")
      NamedField(name=_) => println("Expected: [\"NamedField\", \"example\"]")
      MixedFields(_, named=_, _) =>
        println("Expected: [\"MixedFields\", 99, \"mixed\", true]")
    }
    println("---")
  }
}
// Note: We can't have the same enum with different styles in one file,
// so we'll define equivalent enums for comparison

///|
enum LegacyStyle {
  NoArgs
  WithArgs(Int, String)
} derive(ToJson(style="legacy"))

///|
enum FlatStyle {
  NoArgs
  WithArgs(Int, String)
} derive(ToJson(style="flat"))

///|
fn init {
  println("")
  println("=== Style comparison tests ===")

  // Compare legacy vs flat styles side by side
  println("Style comparison for same enum:")
  let legacy_no_args = LegacyStyle::NoArgs
  let flat_no_args = FlatStyle::NoArgs
  println("Legacy style (no args): \{@json.to_json(legacy_no_args).stringify()}")
  println("Flat style (no args): \{@json.to_json(flat_no_args).stringify()}")
  let legacy_with_args = LegacyStyle::WithArgs(42, "test")
  let flat_with_args = FlatStyle::WithArgs(42, "test")
  println("Legacy style (with args): \{@json.to_json(legacy_with_args).stringify()}")
  println("Flat style (with args): \{@json.to_json(flat_with_args).stringify()}")
  println(
    "Legacy should use object format: {\"$tag\": \"variant\", \"0\": val1, \"1\": val2}",
  )
  println(
    "Flat should use array format: [\"variant\", val1, val2] or just \"variant\"",
  )
}
