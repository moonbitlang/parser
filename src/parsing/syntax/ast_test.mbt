///|
let dummy_loc : @ast_types.Location = {
  let pos = @ast_types.Position::{ fname: "", lnum: 0, bol: 0, cnum: 0 }
  { start: pos, end: pos }
}

///|
let null : Json = Json::null()

///|
let dummy_string_literal : @ast_types.StringLiteral = "test_string"

///|
let dummy_byte_literal : @ast_types.ByteLiteral = "b'A'"

///|
let dummy_bytes_literal : @ast_types.BytesLiteral = "b\"ABC\""

///|
let dummy_char_literal : @ast_types.CharLiteral = "'A'"

///|
let dummy_docstring : @ast_types.DocString = {
  content: @list.of(["Test documentation"]),
  loc: dummy_loc,
}

///|
let dummy_attribute : @ast_types.Attribute = {
  loc: dummy_loc,
  raw: "test_attr",
  parsed: None,
}

///|
test "visibility to_json" {
  @json.inspect(@syntax.Default, content={ "type": "Visibility::Default" })
  @json.inspect(@syntax.Priv(loc=dummy_loc), content={
    "type": "Visibility::Priv",
    "loc": null,
  })
  @json.inspect(@syntax.Pub(attr=Some("readonly"), loc=dummy_loc), content={
    "type": "Visibility::Pub",
    "attr": "readonly",
    "loc": null,
  })
  @json.inspect(@syntax.Pub(attr=None, loc=dummy_loc), content={
    "type": "Visibility::Pub",
    "attr": null,
    "loc": null,
  })
}

///|
test "constant to_json" {
  @json.inspect(@syntax.Bool(true), content={
    "type": "Constant::Bool",
    "0": true,
  })
  @json.inspect(@syntax.Byte(dummy_byte_literal), content={
    "type": "Constant::Byte",
    "0": "b'A'",
  })
  @json.inspect(@syntax.Bytes(dummy_bytes_literal), content={
    "type": "Constant::Bytes",
    "0": "b\"ABC\"",
  })
  @json.inspect(@syntax.Char(dummy_char_literal), content={
    "type": "Constant::Char",
    "0": "'A'",
  })
  @json.inspect(@syntax.Int("42"), content={
    "type": "Constant::Int",
    "0": "42",
  })
  @json.inspect(@syntax.Int64("42L"), content={
    "type": "Constant::Int64",
    "0": "42L",
  })
  @json.inspect(@syntax.UInt("42U"), content={
    "type": "Constant::UInt",
    "0": "42U",
  })
  @json.inspect(@syntax.UInt64("42UL"), content={
    "type": "Constant::UInt64",
    "0": "42UL",
  })
  @json.inspect(@syntax.Float("3.14"), content={
    "type": "Constant::Float",
    "0": "3.14",
  })
  @json.inspect(@syntax.Double("3.14159"), content={
    "type": "Constant::Double",
    "0": "3.14159",
  })
  @json.inspect(@syntax.Constant::String(dummy_string_literal), content={
    "type": "Constant::String",
    "0": "test_string",
  })
  @json.inspect(@syntax.BigInt("123456789012345678901234567890"), content={
    "type": "Constant::BigInt",
    "0": "123456789012345678901234567890",
  })
}

///|
test "label to_json" {
  let label = @syntax.Label::{ name: "test_label", loc: dummy_loc }
  @json.inspect(label, content={
    "type": "Label",
    "name": "test_label",
    "loc": null,
  })
}

///|
test "constr_name to_json" {
  let constr_name = @syntax.ConstrName::{
    name: "TestConstructor",
    loc: dummy_loc,
  }
  @json.inspect(constr_name, content={
    "type": "ConstrName",
    "name": "TestConstructor",
    "loc": null,
  })
}

///|
test "long_ident to_json" {
  @json.inspect(@syntax.LongIdent::Ident(name="test_ident"), content={
    "type": "LongIdent::Ident",
    "0": "test_ident",
  })
  @json.inspect(@syntax.LongIdent::Dot(pkg="test_pkg", id="test_name"), content={
    "type": "LongIdent::Dot",
    "pkg": "test_pkg",
    "id": "test_name",
  })
}

///|
test "type_name to_json" {
  let type_name = @syntax.TypeName::{
    name: @syntax.LongIdent::Ident(name="TestType"),
    is_object: false,
    loc: dummy_loc,
  }
  @json.inspect(type_name, content={
    "type": "TypeName",
    "name": { "type": "LongIdent::Ident", "0": "TestType" },
    "is_object": false,
    "loc": null,
  })
}

///|
test "constr_id to_json" {
  let constr_id = @syntax.ConstrId::{
    id: @syntax.Dot(pkg="test_pkg", id="TestConstructor"),
    loc: dummy_loc,
  }
  @json.inspect(constr_id, content={
    "type": "ConstrId",
    "id": {
      "type": "LongIdent::Dot",
      "pkg": "test_pkg",
      "id": "TestConstructor",
    },
    "loc": null,
  })
}

///|
test "error_type to_json" {
  @json.inspect(@syntax.NoErrorType, content={
    "type": "ErrorType::NoErrorType",
  })
  @json.inspect(@syntax.ErrorType::DefaultErrorType(loc=dummy_loc), content={
    "type": "ErrorType::DefaultErrorType",
    "loc": null,
  })
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@syntax.ErrorType(ty=int_type), content={
    "type": "ErrorType::ErrorType",
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
  @json.inspect(@syntax.MaybeError(ty=int_type), content={
    "type": "ErrorType::MaybeError",
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
}

///|
test "type to_json" {
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@syntax.Type::Any(loc=dummy_loc), content={
    "type": "Type::Any",
    "loc": null,
  })
  @json.inspect(
    @syntax.Type::Arrow(
      args=@list.of([int_type]),
      res=int_type,
      err=@syntax.NoErrorType,
      is_async=false,
      loc=dummy_loc,
    ),
    content={
      "type": "Type::Arrow",
      "args": [
        {
          "type": "Type::Name",
          "constr_id": {
            "type": "ConstrId",
            "id": { "type": "LongIdent::Ident", "0": "Int" },
            "loc": null,
          },
          "tys": [],
          "loc": null,
        },
      ],
      "res": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "LongIdent::Ident", "0": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "err": { "type": "ErrorType::NoErrorType" },
      "is_async": false,
      "loc": null,
    },
  )
  @json.inspect(@syntax.Type::Tuple(tys=@list.of([int_type]), loc=dummy_loc), content={
    "type": "Type::Tuple",
    "tys": [
      {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "LongIdent::Ident", "0": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
    ],
    "loc": null,
  })
  @json.inspect(
    @syntax.Type::Option(ty=int_type, loc=dummy_loc, question_loc=dummy_loc),
    content={
      "type": "Type::Option",
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "LongIdent::Ident", "0": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "loc": null,
      "question_loc": null,
    },
  )
  @json.inspect(
    @syntax.Type::Object(@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="TestObject"),
      loc: dummy_loc,
    }),
    content={
      "type": "Type::Object",
      "0": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "TestObject" },
        "loc": null,
      },
    },
  )
}

///|
test "hole to_json" {
  @json.inspect(@syntax.Synthesized, content={ "type": "Hole::Synthesized" })
  @json.inspect(@syntax.Incomplete, content={ "type": "Hole::Incomplete" })
  @json.inspect(@syntax.Todo, content={ "type": "Hole::Todo" })
}

///|
test "argument_kind to_json" {
  let label = @syntax.Label::{ name: "test_label", loc: dummy_loc }
  @json.inspect(@syntax.ArgumentKind::Positional, content={
    "type": "ArgumentKind::Positional",
  })
  @json.inspect(@syntax.ArgumentKind::Labelled(label), content={
    "type": "ArgumentKind::Labelled",
    "0": { "type": "Label", "name": "test_label", "loc": null },
  })
  @json.inspect(@syntax.ArgumentKind::LabelledPun(label), content={
    "type": "ArgumentKind::LabelledPun",
    "0": { "type": "Label", "name": "test_label", "loc": null },
  })
  @json.inspect(
    @syntax.ArgumentKind::LabelledOption(label~, question_loc=dummy_loc),
    content={
      "type": "ArgumentKind::LabelledOption",
      "label": { "type": "Label", "name": "test_label", "loc": null },
      "question_loc": null,
    },
  )
  @json.inspect(
    @syntax.ArgumentKind::LabelledOptionPun(label~, question_loc=dummy_loc),
    content={
      "type": "ArgumentKind::LabelledOptionPun",
      "label": { "type": "Label", "name": "test_label", "loc": null },
      "question_loc": null,
    },
  )
}

///|
test "fn_kind to_json" {
  @json.inspect(@syntax.FnKind::Lambda, content={ "type": "FnKind::Lambda" })
  @json.inspect(@syntax.FnKind::Matrix, content={ "type": "FnKind::Matrix" })
  @json.inspect(@syntax.FnKind::Arrow, content={ "type": "FnKind::Arrow" })
}

///|
test "group to_json" {
  @json.inspect(@syntax.Group::Brace, content={ "type": "Group::Brace" })
  @json.inspect(@syntax.Group::Paren, content={ "type": "Group::Paren" })
}

///|
test "trailing_mark to_json" {
  @json.inspect(@syntax.TrailingMark::Comma, content={
    "type": "Trailing::Comma",
  })
  @json.inspect(@syntax.TrailingMark::Semi, content={ "type": "Trailing::Semi" })
  @json.inspect(@syntax.TrailingMark::None, content={ "type": "Trailing::None" })
}

///|
test "apply_attr to_json" {
  @json.inspect(@syntax.NoAttr, content={ "type": "ApplyAttr::NoAttr" })
  @json.inspect(@syntax.ApplyAttr::Exclamation, content={
    "type": "ApplyAttr::Exclamation",
  })
  @json.inspect(@syntax.ApplyAttr::Question, content={
    "type": "ApplyAttr::Question",
  })
}

///|
test "TryOperatorKind to_json" {
  @json.inspect(@syntax.TryOperatorKind::Question, content={
    "type": "TryOperatorKind::Question",
  })
  @json.inspect(@syntax.TryOperatorKind::Exclamation, content={
    "type": "TryOperatorKind::Exclamation",
  })
}

///|
test "binder to_json" {
  let binder = @syntax.Binder::{ name: "test_binder", loc: dummy_loc }
  @json.inspect(binder, content={
    "type": "Binder",
    "name": "test_binder",
    "loc": null,
  })
}

///|
test "var to_json" {
  let variable = @syntax.Var::{
    name: @syntax.LongIdent::Ident(name="test_var"),
    loc: dummy_loc,
  }
  @json.inspect(variable, content={
    "type": "Var",
    "name": { "type": "LongIdent::Ident", "0": "test_var" },
    "loc": null,
  })
}

///|
test "constructor_extra_info to_json" {
  let type_name = @syntax.TypeName::{
    name: @syntax.LongIdent::Ident(name="TestType"),
    is_object: false,
    loc: dummy_loc,
  }
  @json.inspect(@syntax.NoExtraInfo, content={
    "type": "ConstructorExtraInfo::NoExtraInfo",
  })
  @json.inspect(@syntax.TypeName(type_name), content={
    "type": "ConstructorExtraInfo::TypeName",
    "0": {
      "type": "TypeName",
      "name": { "type": "LongIdent::Ident", "0": "TestType" },
      "is_object": false,
      "loc": null,
    },
  })
  @json.inspect(@syntax.Package("test_package"), content={
    "type": "ConstructorExtraInfo::Package",
    "0": "test_package",
  })
}

///|
test "constructor to_json" {
  let constr_name = @syntax.ConstrName::{
    name: "TestConstructor",
    loc: dummy_loc,
  }
  let constr = @syntax.Constructor::{
    name: constr_name,
    extra_info: @syntax.NoExtraInfo,
    loc: dummy_loc,
  }
  @json.inspect(constr, content={
    "type": "Constructor",
    "name": { "type": "ConstrName", "name": "TestConstructor", "loc": null },
    "extra_info": { "type": "ConstructorExtraInfo::NoExtraInfo" },
    "loc": null,
  })
}

///|
test "accessor to_json" {
  let label = @syntax.Label::{ name: "test_field", loc: dummy_loc }
  @json.inspect(@syntax.Accessor::Label(label), content={
    "type": "Accessor::Label",
    "0": { "type": "Label", "name": "test_field", "loc": null },
  })
  @json.inspect(@syntax.Accessor::Index(tuple_index=0, loc=dummy_loc), content={
    "type": "Accessor::Index",
    "tuple_index": 0,
    "loc": null,
  })
  @json.inspect(@syntax.Accessor::Newtype(loc=dummy_loc), content={
    "type": "Accessor::Newtype",
    "loc": null,
  })
}

///|
test "exception_decl to_json" {
  @json.inspect(@syntax.NoPayload, content={
    "type": "ExceptionDecl::NoPayload",
  })
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@syntax.SinglePayload(int_type), content={
    "type": "ExceptionDecl::SinglePayload",
    "0": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
  let constr_name = @syntax.ConstrName::{ name: "TestConstr", loc: dummy_loc }
  let constr_decl = @syntax.ConstrDecl::{
    name: constr_name,
    args: None,
    tag: None,
    loc: dummy_loc,
    doc: dummy_docstring,
  }
  @json.inspect(@syntax.EnumPayload(@list.of([constr_decl])), content={
    "type": "ExceptionDecl::EnumPayload",
    "0": [
      {
        "type": "ConstrDecl",
        "name": { "type": "ConstrName", "name": "TestConstr", "loc": null },
        "args": null,
        "tag": null,
        "doc": { "content": ["Test documentation"], "loc": null },
        "loc": null,
      },
    ],
  })
}

///|
test "type_desc to_json" {
  @json.inspect(@syntax.Abstract, content={ "type": "TypeDesc::Abstract" })
  @json.inspect(@syntax.Extern, content={ "type": "TypeDesc::Extern" })
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@syntax.SinglePayload(int_type), content={
    "type": "ExceptionDecl::SinglePayload",
    "0": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
  @json.inspect(@syntax.Error(@syntax.NoPayload), content={
    "type": "TypeDesc::Error",
    "0": { "type": "ExceptionDecl::NoPayload" },
  })
  @json.inspect(@syntax.TypeDesc::Alias(int_type), content={
    "type": "TypeDesc::Alias",
    "0": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
}

///|
test "dot_dot_binder to_json" {
  @json.inspect(@syntax.Underscore, content={
    "type": "DotDotBinder::Underscore",
  })
  @json.inspect(@syntax.NoBinder, content={ "type": "DotDotBinder::NoBinder" })
  let binder = @syntax.Binder::{ name: "test_binder", loc: dummy_loc }
  @json.inspect(@syntax.BinderAs(binder), content={
    "type": "DotDotBinder::BinderAs",
    "0": { "type": "Binder", "name": "test_binder", "loc": null },
  })
  @json.inspect(@syntax.DotDotBinder::Binder(binder), content={
    "type": "DotDotBinder::Binder",
    "0": { "type": "Binder", "name": "test_binder", "loc": null },
  })
}

///|
test "array_pattern to_json" {
  let any_pattern = @syntax.Pattern::Any(loc=dummy_loc)
  @json.inspect(@syntax.Pattern(any_pattern), content={
    "type": "ArrayPattern::Pattern",
    "0": { "type": "Pattern::Any", "loc": null },
  })
  @json.inspect(@syntax.StringSpread(dummy_string_literal), content={
    "type": "ArrayPattern::StringSpread",
    "0": "test_string",
  })
  @json.inspect(@syntax.BytesSpread(dummy_bytes_literal), content={
    "type": "ArrayPattern::BytesSpread",
    "0": "b\"ABC\"",
  })
  let binder = @syntax.Binder::{ name: "test_binder", loc: dummy_loc }
  @json.inspect(
    @syntax.ConstSpread(binder~, pkg=Some("test_pkg"), loc=dummy_loc),
    content={
      "type": "ArrayPattern::ConstSpread",
      "binder": { "type": "Binder", "name": "test_binder", "loc": null },
      "pkg": "test_pkg",
      "loc": null,
    },
  )
  @json.inspect(@syntax.ConstSpread(binder~, pkg=None, loc=dummy_loc), content={
    "type": "ArrayPattern::ConstSpread",
    "binder": { "type": "Binder", "name": "test_binder", "loc": null },
    "pkg": null,
    "loc": null,
  })
}

///|
test "array_patterns to_json" {
  let any_pattern = @syntax.Pattern(@syntax.Pattern::Any(loc=dummy_loc))
  @json.inspect(@syntax.Closed(@list.of([any_pattern])), content={
    "type": "ArrayPatterns::Closed",
    "0": [
      {
        "type": "ArrayPattern::Pattern",
        "0": { "type": "Pattern::Any", "loc": null },
      },
    ],
  })
  @json.inspect(
    @syntax.Open(
      @list.of([any_pattern]),
      @list.of([any_pattern]),
      @syntax.Underscore,
    ),
    content={
      "type": "ArrayPatterns::Open",
      "0": [
        {
          "type": "ArrayPattern::Pattern",
          "0": { "type": "Pattern::Any", "loc": null },
        },
      ],
      "1": [
        {
          "type": "ArrayPattern::Pattern",
          "0": { "type": "Pattern::Any", "loc": null },
        },
      ],
      "2": { "type": "DotDotBinder::Underscore" },
    },
  )
}

///|
test "embedded_code to_json" {
  @json.inspect(@syntax.CodeString(dummy_string_literal), content={
    "type": "EmbeddedCode::CodeString",
    "0": "test_string",
  })
  @json.inspect(
    @syntax.CodeMultilineString(@list.of(["line1", "line2", "line3"])),
    content={
      "type": "EmbeddedCode::CodeMultilineString",
      "0": ["line1", "line2", "line3"],
    },
  )
}

///|
test "func_stubs to_json" {
  @json.inspect(
    @syntax.Import(
      module_name=dummy_string_literal,
      func_name=dummy_string_literal,
    ),
    content={
      "type": "FuncStubs::Import",
      "module_name": "test_string",
      "func_name": "test_string",
    },
  )
  @json.inspect(
    @syntax.Embedded(
      language=Some(dummy_string_literal),
      code=@syntax.CodeString(dummy_string_literal),
    ),
    content={
      "type": "FuncStubs::Embedded",
      "language": "test_string",
      "code": { "type": "EmbeddedCode::CodeString", "0": "test_string" },
    },
  )
  @json.inspect(
    @syntax.Embedded(
      language=None,
      code=@syntax.CodeString(dummy_string_literal),
    ),
    content={
      "type": "FuncStubs::Embedded",
      "language": null,
      "code": { "type": "EmbeddedCode::CodeString", "0": "test_string" },
    },
  )
}

///|
test "parameter to_json" {
  let binder = @syntax.Binder::{ name: "param", loc: dummy_loc }
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  @json.inspect(
    @syntax.Parameter::DiscardPositional(ty=Some(int_type), loc=dummy_loc),
    content={
      "type": "Parameter::DiscardPositional",
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "LongIdent::Ident", "0": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "loc": null,
    },
  )
  @json.inspect(@syntax.Parameter::DiscardPositional(ty=None, loc=dummy_loc), content={
    "type": "Parameter::DiscardPositional",
    "ty": null,
    "loc": null,
  })
  @json.inspect(@syntax.Parameter::Positional(binder~, ty=Some(int_type)), content={
    "type": "Parameter::Positional",
    "binder": { "type": "Binder", "name": "param", "loc": null },
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
  @json.inspect(@syntax.Parameter::Positional(binder~, ty=None), content={
    "type": "Parameter::Positional",
    "binder": { "type": "Binder", "name": "param", "loc": null },
    "ty": null,
  })
  @json.inspect(@syntax.Parameter::Labelled(binder~, ty=Some(int_type)), content={
    "type": "Parameter::Labelled",
    "binder": { "type": "Binder", "name": "param", "loc": null },
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
  @json.inspect(
    @syntax.Parameter::Optional(binder~, default=unit_expr, ty=Some(int_type)),
    content={
      "type": "Parameter::Optional",
      "binder": { "type": "Binder", "name": "param", "loc": null },
      "default": { "type": "Expr::Unit", "loc": null, "faked": false },
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "LongIdent::Ident", "0": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
    },
  )
  @json.inspect(
    @syntax.Parameter::QuestionOptional(binder~, ty=Some(int_type)),
    content={
      "type": "Parameter::QuestionOptional",
      "binder": { "type": "Binder", "name": "param", "loc": null },
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "LongIdent::Ident", "0": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
    },
  )
}

///|
test "interp_elem to_json" {
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  @json.inspect(
    @syntax.InterpElem::Literal(repr=dummy_string_literal, loc=dummy_loc),
    content={
      "type": "InterpElem::Literal",
      "repr": "test_string",
      "loc": null,
    },
  )
  @json.inspect(@syntax.InterpElem::Expr(expr=unit_expr, loc=dummy_loc), content={
    "type": "InterpElem::Expr",
    "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
    "loc": null,
  })
}

///|
test "multiline_string_elem to_json" {
  let interp_elem = @syntax.InterpElem::Literal(
    repr=dummy_string_literal,
    loc=dummy_loc,
  )
  @json.inspect(@syntax.MultilineStringElem::String("test string"), content={
    "type": "MultilineString::String",
    "0": "test string",
  })
  @json.inspect(@syntax.MultilineStringElem::Interp(@list.of([interp_elem])), content={
    "type": "MultilineString::Interp",
    "0": [{ "type": "InterpElem::Literal", "repr": "test_string", "loc": null }],
  })
}

///|
test "spreadable_elem to_json" {
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  @json.inspect(@syntax.SpreadableElem::Regular(unit_expr), content={
    "type": "SpreadableElem::Regular",
    "0": { "type": "Expr::Unit", "loc": null, "faked": false },
  })
  @json.inspect(@syntax.SpreadableElem::Spread(expr=unit_expr, loc=dummy_loc), content={
    "type": "SpreadableElem::Spread",
    "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
    "loc": null,
  })
}

///|
test "map_expr_elem to_json" {
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let int_constant = @syntax.Int("42")
  let map_elem = @syntax.MapExprElem::{
    key: int_constant,
    expr: unit_expr,
    key_loc: dummy_loc,
    loc: dummy_loc,
  }
  @json.inspect(map_elem, content={
    "type": "MapExprElem",
    "key": { "type": "Constant::Int", "0": "42" },
    "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
    "key_loc": null,
    "loc": null,
  })
}

///|
test "field_def to_json" {
  let label = @syntax.Label::{ name: "field_name", loc: dummy_loc }
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let field_def = @syntax.FieldDef::{
    label,
    expr: unit_expr,
    is_pun: false,
    loc: dummy_loc,
  }
  @json.inspect(field_def, content={
    "type": "FieldDef",
    "label": { "type": "Label", "name": "field_name", "loc": null },
    "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
    "is_pun": false,
    "loc": null,
  })
}

///|
test "argument to_json" {
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let argument = @syntax.Argument::{
    value: unit_expr,
    kind: @syntax.ArgumentKind::Positional,
  }
  @json.inspect(argument, content={
    "type": "Argument",
    "value": { "type": "Expr::Unit", "loc": null, "faked": false },
    "kind": { "type": "ArgumentKind::Positional" },
  })
}

///|
test "case to_json" {
  let any_pattern = @syntax.Pattern::Any(loc=dummy_loc)
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let case_with_guard = @syntax.Case::{
    pattern: any_pattern,
    guard_: Some(unit_expr),
    body: unit_expr,
  }
  let case_without_guard = @syntax.Case::{
    pattern: any_pattern,
    guard_: None,
    body: unit_expr,
  }
  @json.inspect(case_with_guard, content={
    "type": "Case",
    "pattern": { "type": "Pattern::Any", "loc": null },
    "guard_": { "type": "Expr::Unit", "loc": null, "faked": false },
    "body": { "type": "Expr::Unit", "loc": null, "faked": false },
  })
  @json.inspect(case_without_guard, content={
    "type": "Case",
    "pattern": { "type": "Pattern::Any", "loc": null },
    "guard_": null,
    "body": { "type": "Expr::Unit", "loc": null, "faked": false },
  })
}

///|
test "multi_arg_case to_json" {
  let any_pattern = @syntax.Pattern::Any(loc=dummy_loc)
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let multi_case = @syntax.MultiArgCase::{
    patterns: @list.of([any_pattern]),
    guard_: Some(unit_expr),
    body: unit_expr,
  }
  @json.inspect(multi_case, content={
    "type": "MultiArgCase",
    "patterns": [{ "type": "Pattern::Any", "loc": null }],
    "guard_": { "type": "Expr::Unit", "loc": null, "faked": false },
    "body": { "type": "Expr::Unit", "loc": null, "faked": false },
  })
}

///|
test "static_assertion to_json" {
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let assertion = @syntax.StaticAssertion::{
    ty: int_type,
    trait_: @syntax.LongIdent::Ident(name="Show"),
    loc: dummy_loc,
    msg: "Test assertion message",
  }
  @json.inspect(assertion, content={
    "type": "StaticAssertion",
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
    "trait_": { "type": "LongIdent::Ident", "0": "Show" },
    "loc": null,
    "msg": "Test assertion message",
  })
}

///|
test "func to_json" {
  let binder = @syntax.Binder::{ name: "param", loc: dummy_loc }
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let param = @syntax.Parameter::Positional(binder~, ty=Some(int_type))
  let any_pattern = @syntax.Pattern::Any(loc=dummy_loc)
  let multi_case = @syntax.MultiArgCase::{
    patterns: @list.of([any_pattern]),
    guard_: None,
    body: unit_expr,
  }
  @json.inspect(
    @syntax.Func::Lambda(
      parameters=@list.of([param]),
      params_loc=dummy_loc,
      body=unit_expr,
      return_type=Some(int_type),
      error_type=@syntax.NoErrorType,
      kind=@syntax.FnKind::Lambda,
      has_error=Some(dummy_loc),
      is_async=false,
      loc=dummy_loc,
    ),
    content={
      "type": "Func::Lambda",
      "parameters": [
        {
          "type": "Parameter::Positional",
          "binder": { "type": "Binder", "name": "param", "loc": null },
          "ty": {
            "type": "Type::Name",
            "constr_id": {
              "type": "ConstrId",
              "id": { "type": "LongIdent::Ident", "0": "Int" },
              "loc": null,
            },
            "tys": [],
            "loc": null,
          },
        },
      ],
      "params_loc": null,
      "body": { "type": "Expr::Unit", "loc": null, "faked": false },
      "return_type": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "LongIdent::Ident", "0": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "error_type": { "type": "ErrorType::NoErrorType" },
      "kind": { "type": "FnKind::Lambda" },
      "has_error": null,
      "is_async": false,
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Func::Match(
      cases=@list.of([multi_case]),
      has_error=None,
      is_async=true,
      fn_loc=dummy_loc,
      loc=dummy_loc,
    ),
    content={
      "type": "Func::Match",
      "cases": [
        {
          "type": "MultiArgCase",
          "patterns": [{ "type": "Pattern::Any", "loc": null }],
          "guard_": null,
          "body": { "type": "Expr::Unit", "loc": null, "faked": false },
        },
      ],
      "has_error": null,
      "is_async": true,
      "fn_loc": null,
      "loc": null,
    },
  )
}

///|
test "expr basic types to_json" {
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let int_constant = @syntax.Int("42")
  let variable = @syntax.Var::{
    name: @syntax.LongIdent::Ident(name="test_var"),
    loc: dummy_loc,
  }
  let label = @syntax.Label::{ name: "test_label", loc: dummy_loc }
  let argument = @syntax.Argument::{
    value: unit_expr,
    kind: @syntax.Positional,
  }
  @json.inspect(@syntax.Expr::Unit(loc=dummy_loc, faked=false), content={
    "type": "Expr::Unit",
    "loc": null,
    "faked": false,
  })
  @json.inspect(@syntax.Expr::Constant(c=int_constant, loc=dummy_loc), content={
    "type": "Expr::Constant",
    "c": { "type": "Constant::Int", "0": "42" },
    "loc": null,
  })
  @json.inspect(@syntax.Expr::Ident(id=variable, loc=dummy_loc), content={
    "type": "Expr::Ident",
    "id": {
      "type": "Var",
      "name": { "type": "LongIdent::Ident", "0": "test_var" },
      "loc": null,
    },
    "loc": null,
  })
  @json.inspect(
    @syntax.Apply(
      func=unit_expr,
      args=@list.of([argument]),
      attr=@syntax.NoAttr,
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Apply",
      "func": { "type": "Expr::Unit", "loc": null, "faked": false },
      "args": [
        {
          "type": "Argument",
          "value": { "type": "Expr::Unit", "loc": null, "faked": false },
          "kind": { "type": "ArgumentKind::Positional" },
        },
      ],
      "attr": { "type": "ApplyAttr::NoAttr" },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Infix(op=variable, lhs=unit_expr, rhs=unit_expr, loc=dummy_loc),
    content={
      "type": "Expr::Infix",
      "op": {
        "type": "Var",
        "name": { "type": "LongIdent::Ident", "0": "test_var" },
        "loc": null,
      },
      "lhs": { "type": "Expr::Unit", "loc": null, "faked": false },
      "rhs": { "type": "Expr::Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(@syntax.Unary(op=variable, expr=unit_expr, loc=dummy_loc), content={
    "type": "Expr::Unary",
    "op": {
      "type": "Var",
      "name": { "type": "LongIdent::Ident", "0": "test_var" },
      "loc": null,
    },
    "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
    "loc": null,
  })
  @json.inspect(
    @syntax.Expr::Array(exprs=@list.of([unit_expr]), loc=dummy_loc),
    content={
      "type": "Expr::Array",
      "exprs": [{ "type": "Expr::Unit", "loc": null, "faked": false }],
      "loc": null,
    },
  )
  let spread_elem = @syntax.SpreadableElem::Regular(unit_expr)
  @json.inspect(
    @syntax.ArraySpread(elems=@list.of([spread_elem]), loc=dummy_loc),
    content={
      "type": "Expr::ArraySpread",
      "elems": [
        {
          "type": "SpreadableElem::Regular",
          "0": { "type": "Expr::Unit", "loc": null, "faked": false },
        },
      ],
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.ArrayGet(array=unit_expr, index=unit_expr, loc=dummy_loc),
    content={
      "type": "Expr::ArrayGet",
      "array": { "type": "Expr::Unit", "loc": null, "faked": false },
      "index": { "type": "Expr::Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Expr::Tuple(exprs=@list.of([unit_expr, unit_expr]), loc=dummy_loc),
    content={
      "type": "Expr::Tuple",
      "exprs": [
        { "type": "Expr::Unit", "loc": null, "faked": false },
        { "type": "Expr::Unit", "loc": null, "faked": false },
      ],
      "loc": null,
    },
  )
  @json.inspect(@syntax.Expr::Hole(loc=dummy_loc, kind=@syntax.Todo), content={
    "type": "Expr::Hole",
    "loc": null,
    "kind": { "type": "Hole::Todo" },
  })
  @json.inspect(@syntax.Return(return_value=Some(unit_expr), loc=dummy_loc), content={
    "type": "Expr::Return",
    "return_value": { "type": "Expr::Unit", "loc": null, "faked": false },
    "loc": null,
  })
  @json.inspect(@syntax.Return(return_value=None, loc=dummy_loc), content={
    "type": "Expr::Return",
    "return_value": null,
    "loc": null,
  })
  @json.inspect(@syntax.Raise(err_value=unit_expr, loc=dummy_loc), content={
    "type": "Expr::Raise",
    "err_value": { "type": "Expr::Unit", "loc": null, "faked": false },
    "loc": null,
  })
  @json.inspect(
    @syntax.Break(arg=Some(unit_expr), label=Some(label), loc=dummy_loc),
    content={
      "type": "Expr::Break",
      "arg": { "type": "Expr::Unit", "loc": null, "faked": false },
      "label": { "type": "Label", "name": "test_label", "loc": null },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Continue(args=@list.of([unit_expr]), label=None, loc=dummy_loc),
    content={
      "type": "Expr::Continue",
      "args": [{ "type": "Expr::Unit", "loc": null, "faked": false }],
      "label": null,
      "loc": null,
    },
  )
}

///|
test "expr complex types to_json" {
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let binder = @syntax.Binder::{ name: "test_binder", loc: dummy_loc }
  let label = @syntax.Label::{ name: "test_label", loc: dummy_loc }
  let any_pattern = @syntax.Pattern::Any(loc=dummy_loc)
  let case = @syntax.Case::{
    pattern: any_pattern,
    guard_: None,
    body: unit_expr,
  }
  let type_name = @syntax.TypeName::{
    name: @syntax.LongIdent::Ident(name="TestType"),
    is_object: false,
    loc: dummy_loc,
  }
  let field_def = @syntax.FieldDef::{
    label,
    expr: unit_expr,
    is_pun: false,
    loc: dummy_loc,
  }
  let variable = @syntax.Var::{
    name: @syntax.LongIdent::Ident(name="test_var"),
    loc: dummy_loc,
  }
  @json.inspect(
    @syntax.If(
      cond=unit_expr,
      ifso=unit_expr,
      ifnot=Some(unit_expr),
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::If",
      "cond": { "type": "Expr::Unit", "loc": null, "faked": false },
      "ifso": { "type": "Expr::Unit", "loc": null, "faked": false },
      "ifnot": { "type": "Expr::Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Guard(
      cond=unit_expr,
      otherwise=Some(unit_expr),
      body=unit_expr,
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Guard",
      "cond": { "type": "Expr::Unit", "loc": null, "faked": false },
      "otherwise": { "type": "Expr::Unit", "loc": null, "faked": false },
      "body": { "type": "Expr::Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(@syntax.Defer(expr=unit_expr, body=unit_expr, loc=dummy_loc), content={
    "type": "Expr::Defer",
    "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
    "body": { "type": "Expr::Unit", "loc": null, "faked": false },
    "loc": null,
  })
  @json.inspect(@syntax.Is(expr=unit_expr, pat=any_pattern, loc=dummy_loc), content={
    "type": "Expr::Is",
    "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
    "pat": { "type": "Pattern::Any", "loc": null },
    "loc": null,
  })
  @json.inspect(
    @syntax.Let(
      pattern=any_pattern,
      expr=unit_expr,
      body=unit_expr,
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Let",
      "pattern": { "type": "Pattern::Any", "loc": null },
      "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
      "body": { "type": "Expr::Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.LetMut(
      binder~,
      ty=Some(int_type),
      expr=unit_expr,
      body=unit_expr,
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::LetMut",
      "binder": { "type": "Binder", "name": "test_binder", "loc": null },
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "LongIdent::Ident", "0": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
      "body": { "type": "Expr::Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Sequence(
      exprs=@list.of([unit_expr]),
      last_expr=unit_expr,
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Sequence",
      "exprs": [{ "type": "Expr::Unit", "loc": null, "faked": false }],
      "last_expr": { "type": "Expr::Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Expr::Record(
      type_name=Some(type_name),
      fields=@list.of([field_def]),
      trailing=@syntax.TrailingMark::Comma,
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Record",
      "type_name": {
        "type": "TypeName",
        "name": { "type": "LongIdent::Ident", "0": "TestType" },
        "is_object": false,
        "loc": null,
      },
      "fields": [
        {
          "type": "FieldDef",
          "label": { "type": "Label", "name": "test_label", "loc": null },
          "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
          "is_pun": false,
          "loc": null,
        },
      ],
      "trailing": { "type": "Trailing::Comma" },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Field(
      record=unit_expr,
      accessor=@syntax.Accessor::Label(label),
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Field",
      "record": { "type": "Expr::Unit", "loc": null, "faked": false },
      "accessor": {
        "type": "Accessor::Label",
        "0": { "type": "Label", "name": "test_label", "loc": null },
      },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Expr::Match(
      expr=unit_expr,
      cases=@list.of([case]),
      match_loc=dummy_loc,
      using_=Some(label),
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Match",
      "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
      "cases": [
        {
          "type": "Case",
          "pattern": { "type": "Pattern::Any", "loc": null },
          "guard_": null,
          "body": { "type": "Expr::Unit", "loc": null, "faked": false },
        },
      ],
      "match_loc": null,
      "using_": { "type": "Label", "name": "test_label", "loc": null },
      "loc": null,
    },
  )
  @json.inspect(@syntax.Pipe(lhs=unit_expr, rhs=unit_expr, loc=dummy_loc), content={
    "type": "Expr::Pipe",
    "lhs": { "type": "Expr::Unit", "loc": null, "faked": false },
    "rhs": { "type": "Expr::Unit", "loc": null, "faked": false },
    "loc": null,
  })
  @json.inspect(
    @syntax.Assign(
      var_=variable,
      expr=unit_expr,
      augmented_by=Some(variable),
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Assign",
      "var_": {
        "type": "Var",
        "name": { "type": "LongIdent::Ident", "0": "test_var" },
        "loc": null,
      },
      "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
      "augmented_by": {
        "type": "Var",
        "name": { "type": "LongIdent::Ident", "0": "test_var" },
        "loc": null,
      },
      "loc": null,
    },
  )
}

///|
test "pattern to_json" {
  let binder = @syntax.Binder::{ name: "test_binder", loc: dummy_loc }
  let any_pattern = @syntax.Pattern::Any(loc=dummy_loc)
  let int_constant = @syntax.Int("42")
  let constant_pattern = @syntax.Pattern::Constant(
    c=int_constant,
    loc=dummy_loc,
  )
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let constr_name = @syntax.ConstrName::{
    name: "TestConstructor",
    loc: dummy_loc,
  }
  let constr = @syntax.Constructor::{
    name: constr_name,
    extra_info: @syntax.NoExtraInfo,
    loc: dummy_loc,
  }
  let label = @syntax.Label::{ name: "test_field", loc: dummy_loc }
  let field_pat = @syntax.FieldPat::{
    label,
    pattern: any_pattern,
    is_pun: false,
    loc: dummy_loc,
  }
  let map_elem = @syntax.MapPatElem::{
    key: int_constant,
    pat: any_pattern,
    match_absent: false,
    key_loc: dummy_loc,
    loc: dummy_loc,
  }
  let constr_pat_arg = @syntax.ConstrPatArg::{
    pat: any_pattern,
    kind: @syntax.ArgumentKind::Positional,
  }
  let array_patterns = @syntax.ArrayPatterns::Closed(@list.of([]))
  @json.inspect(
    @syntax.Pattern::Alias(pat=any_pattern, alias_=binder, loc=dummy_loc),
    content={
      "type": "Pattern::Alias",
      "pat": { "type": "Pattern::Any", "loc": null },
      "alias": { "type": "Binder", "name": "test_binder", "loc": null },
      "loc": null,
    },
  )
  @json.inspect(@syntax.Pattern::Array(pats=array_patterns, loc=dummy_loc), content={
    "type": "Pattern::Array",
    "pats": { "type": "ArrayPatterns::Closed", "0": [] },
    "loc": null,
  })
  @json.inspect(@syntax.Expr::Constant(c=int_constant, loc=dummy_loc), content={
    "type": "Expr::Constant",
    "c": { "type": "Constant::Int", "0": "42" },
    "loc": null,
  })
  @json.inspect(
    @syntax.Pattern::Constraint(pat=any_pattern, ty=int_type, loc=dummy_loc),
    content={
      "type": "Pattern::Constraint",
      "pat": { "type": "Pattern::Any", "loc": null },
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "LongIdent::Ident", "0": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Pattern::Constr(
      constr~,
      args=Some(@list.of([constr_pat_arg])),
      is_open=false,
      loc=dummy_loc,
    ),
    content={
      "type": "Pattern::Constr",
      "constr": {
        "type": "Constructor",
        "name": { "type": "ConstrName", "name": "TestConstructor", "loc": null },
        "extra_info": { "type": "ConstructorExtraInfo::NoExtraInfo" },
        "loc": null,
      },
      "args": [
        {
          "type": "ConstrPatArg",
          "pat": { "type": "Pattern::Any", "loc": null },
          "kind": { "type": "ArgumentKind::Positional" },
        },
      ],
      "is_open": false,
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Pattern::Constr(constr~, args=None, is_open=true, loc=dummy_loc),
    content={
      "type": "Pattern::Constr",
      "constr": {
        "type": "Constructor",
        "name": { "type": "ConstrName", "name": "TestConstructor", "loc": null },
        "extra_info": { "type": "ConstructorExtraInfo::NoExtraInfo" },
        "loc": null,
      },
      "args": null,
      "is_open": true,
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Or(pat1=any_pattern, pat2=constant_pattern, loc=dummy_loc),
    content={
      "type": "Pattern::Or",
      "pat1": { "type": "Pattern::Any", "loc": null },
      "pat2": {
        "type": "Pattern::Constant",
        "c": { "type": "Constant::Int", "0": "42" },
        "loc": null,
      },
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Pattern::Tuple(
      pats=@list.of([any_pattern, constant_pattern]),
      loc=dummy_loc,
    ),
    content={
      "type": "Pattern::Tuple",
      "pats": [
        { "type": "Pattern::Any", "loc": null },
        {
          "type": "Pattern::Constant",
          "c": { "type": "Constant::Int", "0": "42" },
          "loc": null,
        },
      ],
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Pattern::Record(
      fields=@list.of([field_pat]),
      is_closed=true,
      loc=dummy_loc,
    ),
    content={
      "type": "Pattern::Record",
      "fields": [
        {
          "type": "FieldPat",
          "label": { "type": "Label", "name": "test_field", "loc": null },
          "pattern": { "type": "Pattern::Any", "loc": null },
          "is_pun": false,
          "loc": null,
        },
      ],
      "is_closed": true,
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Pattern::Map(
      elems=@list.of([map_elem]),
      is_closed=false,
      loc=dummy_loc,
    ),
    content={
      "type": "Pattern::Map",
      "elems": [
        {
          "type": "MapPatElem",
          "key": { "type": "Constant::Int", "0": "42" },
          "pat": { "type": "Pattern::Any", "loc": null },
          "match_absent": false,
          "key_loc": null,
          "loc": null,
        },
      ],
      "is_closed": false,
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.Range(
      lhs=any_pattern,
      rhs=constant_pattern,
      inclusive=true,
      loc=dummy_loc,
    ),
    content={
      "type": "Pattern::Range",
      "lhs": { "type": "Pattern::Any", "loc": null },
      "rhs": {
        "type": "Pattern::Constant",
        "c": { "type": "Constant::Int", "0": "42" },
        "loc": null,
      },
      "inclusive": true,
      "loc": null,
    },
  )
}

///|
test "field_name to_json" {
  let field_name = @syntax.FieldName::{ label: "test_field", loc: dummy_loc }
  @json.inspect(field_name, content={
    "type": "FieldName",
    "label": "test_field",
    "loc": null,
  })
}

///|
test "field_decl to_json" {
  let field_name = @syntax.FieldName::{ label: "test_field", loc: dummy_loc }
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let field_decl = @syntax.FieldDecl::{
    name: field_name,
    ty: int_type,
    mut_: false,
    vis: @syntax.Default,
    loc: dummy_loc,
    doc: dummy_docstring,
  }
  @json.inspect(field_decl, content={
    "type": "FieldDecl",
    "name": { "type": "FieldName", "label": "test_field", "loc": null },
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
    "mut_": false,
    "vis": { "type": "Visibility::Default" },
    "loc": null,
    "doc": { "content": ["Test documentation"], "loc": null },
  })
}

///|
test "constr_param to_json" {
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let label = @syntax.Label::{ name: "test_label", loc: dummy_loc }
  let param_with_label = @syntax.ConstrParam::{
    ty: int_type,
    mut_: false,
    label: Some(label),
  }
  let param_without_label = @syntax.ConstrParam::{
    ty: int_type,
    mut_: true,
    label: None,
  }
  @json.inspect(param_with_label, content={
    "type": "ConstrParam",
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
    "mut_": false,
    "label": { "type": "Label", "name": "test_label", "loc": null },
  })
  @json.inspect(param_without_label, content={
    "type": "ConstrParam",
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
    "mut_": true,
    "label": null,
  })
}

///|
test "constr_decl to_json" {
  let constr_name = @syntax.ConstrName::{
    name: "TestConstructor",
    loc: dummy_loc,
  }
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let label = @syntax.Label::{ name: "test_label", loc: dummy_loc }
  let param = @syntax.ConstrParam::{
    ty: int_type,
    mut_: false,
    label: Some(label),
  }
  let constr_with_args = @syntax.ConstrDecl::{
    name: constr_name,
    args: Some(@list.of([param])),
    tag: Some(("tag_value", dummy_loc)),
    loc: dummy_loc,
    doc: dummy_docstring,
  }
  let constr_without_args = @syntax.ConstrDecl::{
    name: constr_name,
    args: None,
    tag: None,
    loc: dummy_loc,
    doc: dummy_docstring,
  }
  @json.inspect(constr_with_args, content={
    "type": "ConstrDecl",
    "name": { "type": "ConstrName", "name": "TestConstructor", "loc": null },
    "args": [
      {
        "type": "ConstrParam",
        "ty": {
          "type": "Type::Name",
          "constr_id": {
            "type": "ConstrId",
            "id": { "type": "LongIdent::Ident", "0": "Int" },
            "loc": null,
          },
          "tys": [],
          "loc": null,
        },
        "mut_": false,
        "label": { "type": "Label", "name": "test_label", "loc": null },
      },
    ],
    "tag": ["tag_value", null],
    "doc": { "content": ["Test documentation"], "loc": null },
    "loc": null,
  })
  @json.inspect(constr_without_args, content={
    "type": "ConstrDecl",
    "name": { "type": "ConstrName", "name": "TestConstructor", "loc": null },
    "args": null,
    "tag": null,
    "doc": { "content": ["Test documentation"], "loc": null },
    "loc": null,
  })
}

///|
test "type_var_constraint to_json" {
  let constraint = @syntax.TypeVarConstraint::{
    trait_: @syntax.LongIdent::Ident(name="Show"),
    loc: dummy_loc,
  }
  @json.inspect(constraint, content={
    "type": "TypeVarConstraint",
    "trait_": { "type": "LongIdent::Ident", "0": "Show" },
    "loc": null,
  })
}

///|
test "type_var_binder to_json" {
  let constraint = @syntax.TypeVarConstraint::{
    trait_: @syntax.LongIdent::Ident(name="Show"),
    loc: dummy_loc,
  }
  let type_var_binder = @syntax.TypeVarBinder::{
    name: "T",
    constraints: @list.of([constraint]),
    loc: dummy_loc,
  }
  @json.inspect(type_var_binder, content={
    "type": "TypeVarBinder",
    "name": "T",
    "constraints": [
      {
        "type": "TypeVarConstraint",
        "trait_": { "type": "LongIdent::Ident", "0": "Show" },
        "loc": null,
      },
    ],
    "loc": null,
  })
}

///|
test "type_decl_binder to_json" {
  let binder_with_name = @syntax.TypeDeclBinder::{
    name: Some("TypeVar"),
    loc: dummy_loc,
  }
  let binder_without_name = @syntax.TypeDeclBinder::{
    name: None,
    loc: dummy_loc,
  }
  @json.inspect(binder_with_name, content={
    "type": "TypeDeclBinder",
    "name": "TypeVar",
    "loc": null,
  })
  @json.inspect(binder_without_name, content={
    "type": "TypeDeclBinder",
    "name": null,
    "loc": null,
  })
}

///|
test "alias_target to_json" {
  let binder = @syntax.Binder::{ name: "test_binder", loc: dummy_loc }
  let label = @syntax.Label::{ name: "target_label", loc: dummy_loc }
  let alias_with_target = @syntax.AliasTarget::{ binder, target: Some(label) }
  let alias_without_target = @syntax.AliasTarget::{ binder, target: None }
  @json.inspect(alias_with_target, content={
    "type": "AliasTarget",
    "binder": { "type": "Binder", "name": "test_binder", "loc": null },
    "target": { "type": "Label", "name": "target_label", "loc": null },
  })
  @json.inspect(alias_without_target, content={
    "type": "AliasTarget",
    "binder": { "type": "Binder", "name": "test_binder", "loc": null },
    "target": null,
  })
}

///|
test "field_pat to_json" {
  let label = @syntax.Label::{ name: "field_label", loc: dummy_loc }
  let any_pattern = @syntax.Pattern::Any(loc=dummy_loc)
  let field_pat = @syntax.FieldPat::{
    label,
    pattern: any_pattern,
    is_pun: true,
    loc: dummy_loc,
  }
  @json.inspect(field_pat, content={
    "type": "FieldPat",
    "label": { "type": "Label", "name": "field_label", "loc": null },
    "pattern": { "type": "Pattern::Any", "loc": null },
    "is_pun": true,
    "loc": null,
  })
}

///|
test "constr_pat_arg to_json" {
  let any_pattern = @syntax.Pattern::Any(loc=dummy_loc)
  let label = @syntax.Label::{ name: "arg_label", loc: dummy_loc }
  let arg = @syntax.ConstrPatArg::{
    pat: any_pattern,
    kind: @syntax.ArgumentKind::Labelled(label),
  }
  @json.inspect(arg, content={
    "type": "ConstrPatArg",
    "pat": { "type": "Pattern::Any", "loc": null },
    "kind": {
      "type": "ArgumentKind::Labelled",
      "0": { "type": "Label", "name": "arg_label", "loc": null },
    },
  })
}

///|
test "map_pat_elem to_json" {
  let int_constant = @syntax.Int("42")
  let any_pattern = @syntax.Pattern::Any(loc=dummy_loc)
  let map_elem = @syntax.MapPatElem::{
    key: int_constant,
    pat: any_pattern,
    match_absent: true,
    key_loc: dummy_loc,
    loc: dummy_loc,
  }
  @json.inspect(map_elem, content={
    "type": "MapPatElem",
    "key": { "type": "Constant::Int", "0": "42" },
    "pat": { "type": "Pattern::Any", "loc": null },
    "match_absent": true,
    "key_loc": null,
    "loc": null,
  })
}

///|
test "local_type_decl to_json" {
  let deriving_directive = @syntax.DerivingDirective::{
    type_name: @syntax.TypeName::{
      name: @syntax.LongIdent::Ident(name="Show"),
      is_object: false,
      loc: dummy_loc,
    },
    args: @list.of([]),
    loc: dummy_loc,
  }
  let local_type_decl = @syntax.LocalTypeDecl::{
    tycon: "TestType",
    tycon_loc: dummy_loc,
    components: @syntax.Abstract,
    deriving: @list.of([deriving_directive]),
  }
  @json.inspect(local_type_decl, content={
    "type": "LocalTypeDecl",
    "tycon": "TestType",
    "tycon_loc": null,
    "components": { "type": "TypeDesc::Abstract" },
    "deriving": [
      {
        "type": "DerivingDirective",
        "type_name": {
          "type": "TypeName",
          "name": { "type": "LongIdent::Ident", "0": "Show" },
          "is_object": false,
          "loc": null,
        },
        "args": [],
        "loc": null,
      },
    ],
  })
}

///|
test "deriving_directive to_json" {
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let argument = @syntax.Argument::{
    value: unit_expr,
    kind: @syntax.Positional,
  }
  let deriving_directive = @syntax.DerivingDirective::{
    type_name: @syntax.TypeName::{
      name: @syntax.Dot(pkg="std", id="Show"),
      is_object: true,
      loc: dummy_loc,
    },
    args: @list.of([argument]),
    loc: dummy_loc,
  }
  @json.inspect(deriving_directive, content={
    "type": "DerivingDirective",
    "type_name": {
      "type": "TypeName",
      "name": { "type": "LongIdent::Dot", "pkg": "std", "id": "Show" },
      "is_object": true,
      "loc": null,
    },
    "args": [
      {
        "type": "Argument",
        "value": { "type": "Expr::Unit", "loc": null, "faked": false },
        "kind": { "type": "ArgumentKind::Positional" },
      },
    ],
    "loc": null,
  })
}

///|
test "type_decl to_json" {
  let type_decl_binder = @syntax.TypeDeclBinder::{
    name: Some("T"),
    loc: dummy_loc,
  }
  let type_decl = @syntax.TypeDecl::{
    tycon: "TestType",
    tycon_loc: dummy_loc,
    params: @list.of([type_decl_binder]),
    components: @syntax.Extern,
    attrs: @list.of([dummy_attribute]),
    doc: dummy_docstring,
    type_vis: @syntax.Default,
    deriving: @list.of([]),
    loc: dummy_loc,
  }
  @json.inspect(type_decl, content={
    "type": "TypeDecl",
    "tycon": "TestType",
    "tycon_loc": null,
    "params": [{ "type": "TypeDeclBinder", "name": "T", "loc": null }],
    "components": { "type": "TypeDesc::Extern" },
    "attrs": [{ "loc": null, "raw": "test_attr" }],
    "doc": { "content": ["Test documentation"], "loc": null },
    "type_vis": { "type": "Visibility::Default" },
    "deriving": [],
    "loc": null,
  })
}

///|
test "decl_body to_json" {
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let local_type_decl = @syntax.LocalTypeDecl::{
    tycon: "LocalType",
    tycon_loc: dummy_loc,
    components: @syntax.Abstract,
    deriving: @list.of([]),
  }
  @json.inspect(
    @syntax.DeclBody(local_types=@list.of([local_type_decl]), expr=unit_expr),
    content={
      "type": "DeclBody::DeclBody",
      "local_types": [
        {
          "type": "LocalTypeDecl",
          "tycon": "LocalType",
          "tycon_loc": null,
          "components": { "type": "TypeDesc::Abstract" },
          "deriving": [],
        },
      ],
      "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
    },
  )
  @json.inspect(
    @syntax.DeclStubs(
      @syntax.Import(
        module_name=dummy_string_literal,
        func_name=dummy_string_literal,
      ),
    ),
    content={
      "type": "DeclBody::DeclStubs",
      "0": {
        "type": "FuncStubs::Import",
        "module_name": "test_string",
        "func_name": "test_string",
      },
    },
  )
}

///|
test "fun_decl to_json" {
  let binder = @syntax.Binder::{ name: "test_func", loc: dummy_loc }
  let type_name = @syntax.TypeName::{
    name: @syntax.LongIdent::Ident(name="TestType"),
    is_object: false,
    loc: dummy_loc,
  }
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let param = @syntax.Parameter::Positional(binder~, ty=Some(int_type))
  let type_var_constraint = @syntax.TypeVarConstraint::{
    trait_: @syntax.LongIdent::Ident(name="Show"),
    loc: dummy_loc,
  }
  let type_var_binder = @syntax.TypeVarBinder::{
    name: "T",
    constraints: @list.of([type_var_constraint]),
    loc: dummy_loc,
  }
  let fun_decl = @syntax.FunDecl::{
    type_name: Some(type_name),
    name: binder,
    has_error: Some(dummy_loc),
    is_async: true,
    decl_params: Some(@list.of([param])),
    params_loc: dummy_loc,
    quantifiers: @list.of([type_var_binder]),
    return_type: Some(int_type),
    error_type: @syntax.NoErrorType,
    vis: @syntax.Priv(loc=dummy_loc),
    attrs: @list.of([dummy_attribute]),
    doc: dummy_docstring,
  }
  @json.inspect(fun_decl, content={
    "type": "FunDecl",
    "type_name": {
      "type": "TypeName",
      "name": { "type": "LongIdent::Ident", "0": "TestType" },
      "is_object": false,
      "loc": null,
    },
    "name": { "type": "Binder", "name": "test_func", "loc": null },
    "has_error": null,
    "is_async": true,
    "decl_params": [
      {
        "type": "Parameter::Positional",
        "binder": { "type": "Binder", "name": "test_func", "loc": null },
        "ty": {
          "type": "Type::Name",
          "constr_id": {
            "type": "ConstrId",
            "id": { "type": "LongIdent::Ident", "0": "Int" },
            "loc": null,
          },
          "tys": [],
          "loc": null,
        },
      },
    ],
    "params_loc": null,
    "quantifiers": [
      {
        "type": "TypeVarBinder",
        "name": "T",
        "constraints": [
          {
            "type": "TypeVarConstraint",
            "trait_": { "type": "LongIdent::Ident", "0": "Show" },
            "loc": null,
          },
        ],
        "loc": null,
      },
    ],
    "return_type": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
    "error_type": { "type": "ErrorType::NoErrorType" },
    "vis": { "type": "Visibility::Priv", "loc": null },
    "attrs": [{ "loc": null, "raw": "test_attr" }],
    "doc": { "content": ["Test documentation"], "loc": null },
  })
}

///|
test "trait_method_decl to_json" {
  let binder = @syntax.Binder::{ name: "trait_method", loc: dummy_loc }
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let param = @syntax.Parameter::Positional(binder~, ty=Some(int_type))
  let type_var_binder = @syntax.TypeVarBinder::{
    name: "T",
    constraints: @list.of([]),
    loc: dummy_loc,
  }
  let trait_method = @syntax.TraitMethodDecl::{
    name: binder,
    has_error: None,
    is_async: false,
    quantifiers: @list.of([type_var_binder]),
    params: @list.of([param]),
    return_type: Some(int_type),
    error_type: @syntax.ErrorType::DefaultErrorType(loc=dummy_loc),
    has_default: Some(dummy_loc),
    loc: dummy_loc,
  }
  @json.inspect(trait_method, content={
    "type": "TraitMethodDecl",
    "name": { "type": "Binder", "name": "trait_method", "loc": null },
    "has_error": null,
    "is_async": false,
    "quantifiers": [
      { "type": "TypeVarBinder", "name": "T", "constraints": [], "loc": null },
    ],
    "params": [
      {
        "type": "Parameter::Positional",
        "binder": { "type": "Binder", "name": "trait_method", "loc": null },
        "ty": {
          "type": "Type::Name",
          "constr_id": {
            "type": "ConstrId",
            "id": { "type": "LongIdent::Ident", "0": "Int" },
            "loc": null,
          },
          "tys": [],
          "loc": null,
        },
      },
    ],
    "return_type": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
    "error_type": { "type": "ErrorType::DefaultErrorType", "loc": null },
    "has_default": null,
    "loc": null,
  })
}

///|
test "trait_decl to_json" {
  let trait_binder = @syntax.Binder::{ name: "TestTrait", loc: dummy_loc }
  let super_constraint = @syntax.TypeVarConstraint::{
    trait_: @syntax.LongIdent::Ident(name="SuperTrait"),
    loc: dummy_loc,
  }
  let method_binder = @syntax.Binder::{ name: "method", loc: dummy_loc }
  let trait_method = @syntax.TraitMethodDecl::{
    name: method_binder,
    has_error: None,
    is_async: false,
    quantifiers: @list.of([]),
    params: @list.of([]),
    return_type: None,
    error_type: @syntax.NoErrorType,
    has_default: None,
    loc: dummy_loc,
  }
  let trait_decl = @syntax.TraitDecl::{
    name: trait_binder,
    supers: @list.of([super_constraint]),
    methods: @list.of([trait_method]),
    vis: @syntax.Pub(attr=None, loc=dummy_loc),
    loc: dummy_loc,
    attrs: @list.of([dummy_attribute]),
    doc: dummy_docstring,
  }
  @json.inspect(trait_decl, content={
    "type": "TraitDecl",
    "name": { "type": "Binder", "name": "TestTrait", "loc": null },
    "supers": [
      {
        "type": "TypeVarConstraint",
        "trait_": { "type": "LongIdent::Ident", "0": "SuperTrait" },
        "loc": null,
      },
    ],
    "methods": [
      {
        "type": "TraitMethodDecl",
        "name": { "type": "Binder", "name": "method", "loc": null },
        "has_error": null,
        "is_async": false,
        "quantifiers": [],
        "params": [],
        "return_type": null,
        "error_type": { "type": "ErrorType::NoErrorType" },
        "has_default": null,
        "loc": null,
      },
    ],
    "vis": { "type": "Visibility::Pub", "attr": null, "loc": null },
    "loc": null,
    "attrs": [{ "loc": null, "raw": "test_attr" }],
    "doc": { "content": ["Test documentation"], "loc": null },
  })
}

///|
test "impl variants to_json" {
  let unit_expr = @syntax.Expr::Unit(loc=dummy_loc, faked=false)
  let type_decl = @syntax.TypeDecl::{
    tycon: "TestType",
    tycon_loc: dummy_loc,
    params: @list.of([]),
    components: @syntax.Abstract,
    attrs: @list.of([]),
    doc: dummy_docstring,
    type_vis: @syntax.Default,
    deriving: @list.of([]),
    loc: dummy_loc,
  }
  let binder = @syntax.Binder::{ name: "test", loc: dummy_loc }
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(
    @syntax.TopExpr(
      expr=unit_expr,
      is_main=true,
      local_types=@list.of([]),
      loc=dummy_loc,
    ),
    content={
      "type": "Impl::TopExpr",
      "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
      "is_main": true,
      "local_types": [],
      "loc": null,
    },
  )
  @json.inspect(
    @syntax.TopTest(
      expr=unit_expr,
      name=Some((dummy_string_literal, dummy_loc)),
      params=None,
      local_types=@list.of([]),
      loc=dummy_loc,
      attrs=@list.of([]),
      doc=dummy_docstring,
    ),
    content={
      "type": "Impl::TopTest",
      "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
      "name": ["test_string", null],
      "params": null,
      "local_types": [],
      "loc": null,
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": null },
    },
  )
  @json.inspect(@syntax.TopTypeDef(type_decl), content={
    "type": "Impl::TopTypeDef",
    "0": {
      "type": "TypeDecl",
      "tycon": "TestType",
      "tycon_loc": null,
      "params": [],
      "components": { "type": "TypeDesc::Abstract" },
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": null },
      "type_vis": { "type": "Visibility::Default" },
      "deriving": [],
      "loc": null,
    },
  })
  let fun_decl = @syntax.FunDecl::{
    type_name: None,
    name: binder,
    has_error: None,
    is_async: false,
    decl_params: None,
    params_loc: dummy_loc,
    quantifiers: @list.of([]),
    return_type: None,
    error_type: @syntax.NoErrorType,
    vis: @syntax.Default,
    attrs: @list.of([]),
    doc: dummy_docstring,
  }
  let decl_body = @syntax.DeclBody(local_types=@list.of([]), expr=unit_expr)
  @json.inspect(@syntax.TopFuncDef(fun_decl~, decl_body~, loc=dummy_loc), content={
    "type": "Impl::TopFuncDef",
    "fun_decl": {
      "type": "FunDecl",
      "type_name": null,
      "name": { "type": "Binder", "name": "test", "loc": null },
      "has_error": null,
      "is_async": false,
      "decl_params": null,
      "params_loc": null,
      "quantifiers": [],
      "return_type": null,
      "error_type": { "type": "ErrorType::NoErrorType" },
      "vis": { "type": "Visibility::Default" },
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": null },
    },
    "decl_body": {
      "type": "DeclBody::DeclBody",
      "local_types": [],
      "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
    },
    "loc": null,
  })
  @json.inspect(
    @syntax.TopLetDef(
      binder~,
      ty=Some(int_type),
      expr=unit_expr,
      vis=@syntax.Default,
      is_constant=true,
      loc=dummy_loc,
      attrs=@list.of([]),
      doc=dummy_docstring,
    ),
    content={
      "type": "Impl::TopLetDef",
      "binder": { "type": "Binder", "name": "test", "loc": null },
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "LongIdent::Ident", "0": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "expr": { "type": "Expr::Unit", "loc": null, "faked": false },
      "vis": { "type": "Visibility::Default" },
      "is_constant": true,
      "loc": null,
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": null },
    },
  )
  let view = @syntax.TopView(
    quantifiers=@list.empty(),
    source_ty=int_type,
    view_type_name="TestViewType",
    view_func_name=binder,
    view_type_loc=dummy_loc,
    params_loc=dummy_loc,
    view_constrs=@list.empty(),
    parameters=@list.empty(),
    body=unit_expr,
    vis=@syntax.Default,
    attrs=@list.empty(),
    doc=dummy_docstring,
    loc=dummy_loc,
  )
  @json.inspect(view, content={
    "type": "Impl::TopView",
    "quantifiers": [],
    "source_ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "LongIdent::Ident", "0": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
    "view_constrs": [],
    "body": { "type": "Expr::Unit", "loc": null, "faked": false },
    "ty_name": "TestViewType",
    "parameters": [],
    "func_name": { "type": "Binder", "name": "test", "loc": null },
    "ty_loc": null,
    "params_loc": null,
    "vis": { "type": "Visibility::Default" },
    "loc": null,
    "attrs": [],
    "doc": { "content": ["Test documentation"], "loc": null },
  })
}
