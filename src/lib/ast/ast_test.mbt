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
  @json.inspect(@ast.Default, content={ "type": "Default" })
  @json.inspect(@ast.Priv(loc=dummy_loc), content={
    "type": "Priv",
    "loc": null,
  })
  @json.inspect(@ast.Pub(attr=Some("readonly"), loc=dummy_loc), content={
    "type": "Pub",
    "attr": "readonly",
    "loc": null,
  })
  @json.inspect(@ast.Pub(attr=None, loc=dummy_loc), content={
    "type": "Pub",
    "attr": null,
    "loc": null,
  })
}

///|
test "constant to_json" {
  @json.inspect(@ast.Bool(true), content={ "type": "Bool", "0": true })
  @json.inspect(@ast.Byte(dummy_byte_literal), content={
    "type": "Byte",
    "0": "b'A'",
  })
  @json.inspect(@ast.Bytes(dummy_bytes_literal), content={
    "type": "Bytes",
    "0": "b\"ABC\"",
  })
  @json.inspect(@ast.Char(dummy_char_literal), content={
    "type": "Char",
    "0": "'A'",
  })
  @json.inspect(@ast.Int("42"), content={ "type": "Int", "0": "42" })
  @json.inspect(@ast.Int64("42L"), content={ "type": "Int64", "0": "42L" })
  @json.inspect(@ast.UInt("42U"), content={ "type": "UInt", "0": "42U" })
  @json.inspect(@ast.UInt64("42UL"), content={ "type": "UInt64", "0": "42UL" })
  @json.inspect(@ast.Float("3.14"), content={ "type": "Float", "0": "3.14" })
  @json.inspect(@ast.Double("3.14159"), content={
    "type": "Double",
    "0": "3.14159",
  })
  @json.inspect(@ast.Constant::String(dummy_string_literal), content={
    "type": "String",
    "0": "test_string",
  })
  @json.inspect(@ast.BigInt("123456789012345678901234567890"), content={
    "type": "BigInt",
    "0": "123456789012345678901234567890",
  })
}

///|
test "label to_json" {
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  @json.inspect(label, content={
    "type": "Label",
    "name": "test_label",
    "loc": null,
  })
}

///|
test "constr_name to_json" {
  let constr_name = @ast.ConstrName::{ name: "TestConstructor", loc: dummy_loc }
  @json.inspect(constr_name, content={
    "type": "ConstrName",
    "name": "TestConstructor",
    "loc": null,
  })
}

///|
test "long_ident to_json" {
  @json.inspect(@ast.LongIdent::Ident(name="test_ident"), content={
    "type": "Ident",
    "name": "test_ident",
  })
  @json.inspect(@ast.LongIdent::Dot(pkg="test_pkg", id="test_name"), content={
    "type": "Dot",
    "pkg": "test_pkg",
    "id": "test_name",
  })
}

///|
test "type_name to_json" {
  let type_name = @ast.TypeName::{
    name: @ast.LongIdent::Ident(name="TestType"),
    is_object: false,
    loc: dummy_loc,
  }
  @json.inspect(type_name, content={
    "type": "TypeName",
    "name": { "type": "Ident", "name": "TestType" },
    "is_object": false,
    "loc": null,
  })
}

///|
test "constr_id to_json" {
  let constr_id = @ast.ConstrId::{
    id: @ast.Dot(pkg="test_pkg", id="TestConstructor"),
    loc: dummy_loc,
  }
  @json.inspect(constr_id, content={
    "type": "ConstrId",
    "id": { "type": "Dot", "pkg": "test_pkg", "id": "TestConstructor" },
    "loc": null,
  })
}

///|
test "error_type to_json" {
  @json.inspect(@ast.NoErrorType, content={ "type": "ErrorType::NoErrorType" })
  @json.inspect(@ast.ErrorType::DefaultErrorType(loc=dummy_loc), content={
    "type": "ErrorType::DefaultErrorType",
    "loc": null,
  })
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@ast.ErrorType(ty=int_type), content={
    "type": "ErrorType::ErrorType",
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
  @json.inspect(@ast.MaybeError(ty=int_type), content={
    "type": "ErrorType::MaybeError",
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
}

///|
test "type to_json" {
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@ast.Type::Any(loc=dummy_loc), content={
    "type": "Type::Any",
    "loc": null,
  })
  @json.inspect(
    @ast.Type::Arrow(
      args=@list.of([int_type]),
      res=int_type,
      err=@ast.NoErrorType,
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
            "id": { "type": "Ident", "name": "Int" },
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
          "id": { "type": "Ident", "name": "Int" },
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
  @json.inspect(@ast.Type::Tuple(tys=@list.of([int_type]), loc=dummy_loc), content={
    "type": "Type::Tuple",
    "tys": [
      {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
    ],
    "loc": null,
  })
  @json.inspect(
    @ast.Type::Option(ty=int_type, loc=dummy_loc, question_loc=dummy_loc),
    content={
      "type": "Type::Option",
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
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
    @ast.Type::Object(@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="TestObject"),
      loc: dummy_loc,
    }),
    content={
      "type": "Type::Object",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "TestObject" },
        "loc": null,
      },
    },
  )
}

///|
test "hole to_json" {
  @json.inspect(@ast.Synthesized, content={ "type": "Hole::Synthesized" })
  @json.inspect(@ast.Incomplete, content={ "type": "Hole::Incomplete" })
  @json.inspect(@ast.Todo, content={ "type": "Hole::Todo" })
}

///|
test "argument_kind to_json" {
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  @json.inspect(@ast.ArgumentKind::Positional, content={
    "type": "ArgumentKind::Positional",
  })
  @json.inspect(@ast.ArgumentKind::Labelled(label), content={
    "type": "ArgumentKind::Labelled",
    "0": { "type": "Label", "name": "test_label", "loc": null },
  })
  @json.inspect(@ast.ArgumentKind::LabelledPun(label), content={
    "type": "ArgumentKind::LabelledPun",
    "0": { "type": "Label", "name": "test_label", "loc": null },
  })
  @json.inspect(
    @ast.ArgumentKind::LabelledOption(label~, question_loc=dummy_loc),
    content={
      "type": "ArgumentKind::LabelledOption",
      "label": { "type": "Label", "name": "test_label", "loc": null },
      "question_loc": null,
    },
  )
  @json.inspect(
    @ast.ArgumentKind::LabelledOptionPun(label~, question_loc=dummy_loc),
    content={
      "type": "ArgumentKind::LabelledOptionPun",
      "label": { "type": "Label", "name": "test_label", "loc": null },
      "question_loc": null,
    },
  )
}

///|
test "fn_kind to_json" {
  @json.inspect(@ast.FnKind::Lambda, content={ "type": "FnKind::Lambda" })
  @json.inspect(@ast.FnKind::Matrix, content={ "type": "FnKind::Matrix" })
  @json.inspect(@ast.FnKind::Arrow, content={ "type": "FnKind::Arrow" })
}

///|
test "group to_json" {
  @json.inspect(@ast.Group::Brace, content={ "type": "Group::Brace" })
  @json.inspect(@ast.Group::Paren, content={ "type": "Group::Paren" })
}

///|
test "trailing_mark to_json" {
  @json.inspect(@ast.TrailingMark::Comma, content={ "type": "Trailing::Comma" })
  @json.inspect(@ast.TrailingMark::Semi, content={ "type": "Trailing::Semi" })
  @json.inspect(@ast.TrailingMark::None, content={ "type": "Trailing::None" })
}

///|
test "apply_attr to_json" {
  @json.inspect(@ast.NoAttr, content={ "type": "ApplyAttr::NoAttr" })
  @json.inspect(@ast.ApplyAttr::Exclamation, content={
    "type": "ApplyAttr::Exclamation",
  })
  @json.inspect(@ast.ApplyAttr::Question, content={
    "type": "ApplyAttr::Question",
  })
}

///|
test "TryOperatorKind to_json" {
  @json.inspect(@ast.TryOperatorKind::Question, content={
    "type": "TryOperatorKind::Question",
  })
  @json.inspect(@ast.TryOperatorKind::Exclamation, content={
    "type": "TryOperatorKind::Exclamation",
  })
}

///|
test "binder to_json" {
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  @json.inspect(binder, content={
    "type": "Binder",
    "name": "test_binder",
    "loc": null,
  })
}

///|
test "var to_json" {
  let variable = @ast.Var::{
    name: @ast.LongIdent::Ident(name="test_var"),
    loc: dummy_loc,
  }
  @json.inspect(variable, content={
    "type": "Var",
    "name": { "type": "Ident", "name": "test_var" },
    "loc": null,
  })
}

///|
test "constructor_extra_info to_json" {
  let type_name = @ast.TypeName::{
    name: @ast.LongIdent::Ident(name="TestType"),
    is_object: false,
    loc: dummy_loc,
  }
  @json.inspect(@ast.NoExtraInfo, content={
    "type": "ConstructorExtraInfo::NoExtraInfo",
  })
  @json.inspect(@ast.TypeName(type_name), content={
    "type": "ConstructorExtraInfo::TypeName",
    "0": {
      "type": "TypeName",
      "name": { "type": "Ident", "name": "TestType" },
      "is_object": false,
      "loc": null,
    },
  })
  @json.inspect(@ast.Package("test_package"), content={
    "type": "ConstructorExtraInfo::Package",
    "0": "test_package",
  })
}

///|
test "constructor to_json" {
  let constr_name = @ast.ConstrName::{ name: "TestConstructor", loc: dummy_loc }
  let constr = @ast.Constructor::{
    name: constr_name,
    extra_info: @ast.NoExtraInfo,
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
  let label = @ast.Label::{ name: "test_field", loc: dummy_loc }
  @json.inspect(@ast.Accessor::Label(label), content={
    "type": "Accessor::Label",
    "0": { "type": "Label", "name": "test_field", "loc": null },
  })
  @json.inspect(@ast.Accessor::Index(tuple_index=0, loc=dummy_loc), content={
    "type": "Accessor::Index",
    "tuple_index": 0,
    "loc": null,
  })
  @json.inspect(@ast.Accessor::Newtype(loc=dummy_loc), content={
    "type": "Accessor::Newtype",
    "loc": null,
  })
}

///|
test "exception_decl to_json" {
  @json.inspect(@ast.NoPayload, content={ "type": "ExceptionDecl::NoPayload" })
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@ast.SinglePayload(int_type), content={
    "type": "ExceptionDecl::SinglePayload",
    "0": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
  let constr_name = @ast.ConstrName::{ name: "TestConstr", loc: dummy_loc }
  let constr_decl = @ast.ConstrDecl::{
    name: constr_name,
    args: None,
    tag: None,
    loc: dummy_loc,
    doc: dummy_docstring,
  }
  @json.inspect(@ast.EnumPayload(@list.of([constr_decl])), content={
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
  @json.inspect(@ast.Abstract, content={ "type": "TypeDesc::Abstract" })
  @json.inspect(@ast.Extern, content={ "type": "TypeDesc::Extern" })
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@ast.SinglePayload(int_type), content={
    "type": "ExceptionDecl::SinglePayload",
    "0": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
  @json.inspect(@ast.Error(@ast.NoPayload), content={
    "type": "TypeDesc::Error",
    "0": { "type": "ExceptionDecl::NoPayload" },
  })
  @json.inspect(@ast.TypeDesc::Alias(int_type), content={
    "type": "TypeDesc::Alias",
    "0": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
}

///|
test "dot_dot_binder to_json" {
  @json.inspect(@ast.Underscore, content={ "type": "DotDotBinder::Underscore" })
  @json.inspect(@ast.NoBinder, content={ "type": "DotDotBinder::NoBinder" })
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  @json.inspect(@ast.BinderAs(binder), content={
    "type": "DotDotBinder::BinderAs",
    "0": { "type": "Binder", "name": "test_binder", "loc": null },
  })
  @json.inspect(@ast.DotDotBinder::Binder(binder), content={
    "type": "DotDotBinder::Binder",
    "0": { "type": "Binder", "name": "test_binder", "loc": null },
  })
}

///|
test "array_pattern to_json" {
  let any_pattern = @ast.Pattern::Any(loc=dummy_loc)
  @json.inspect(@ast.Pattern(any_pattern), content={
    "type": "ArrayPattern::Pattern",
    "0": { "type": "Pattern::Any", "loc": null },
  })
  @json.inspect(@ast.StringSpread(dummy_string_literal), content={
    "type": "ArrayPattern::StringSpread",
    "0": "test_string",
  })
  @json.inspect(@ast.BytesSpread(dummy_bytes_literal), content={
    "type": "ArrayPattern::BytesSpread",
    "0": "b\"ABC\"",
  })
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  @json.inspect(
    @ast.ConstSpread(binder~, pkg=Some("test_pkg"), loc=dummy_loc),
    content={
      "type": "ArrayPattern::ConstSpread",
      "binder": { "type": "Binder", "name": "test_binder", "loc": null },
      "pkg": "test_pkg",
      "loc": null,
    },
  )
  @json.inspect(@ast.ConstSpread(binder~, pkg=None, loc=dummy_loc), content={
    "type": "ArrayPattern::ConstSpread",
    "binder": { "type": "Binder", "name": "test_binder", "loc": null },
    "pkg": null,
    "loc": null,
  })
}

///|
test "array_patterns to_json" {
  let any_pattern = @ast.Pattern(@ast.Pattern::Any(loc=dummy_loc))
  @json.inspect(@ast.Closed(@list.of([any_pattern])), content={
    "type": "ArrayPatterns::Closed",
    "0": [
      {
        "type": "ArrayPattern::Pattern",
        "0": { "type": "Pattern::Any", "loc": null },
      },
    ],
  })
  @json.inspect(
    @ast.Open(@list.of([any_pattern]), @list.of([any_pattern]), @ast.Underscore),
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
  @json.inspect(@ast.CodeString(dummy_string_literal), content={
    "type": "EmbeddedCode::CodeString",
    "0": "test_string",
  })
  @json.inspect(
    @ast.CodeMultilineString(@list.of(["line1", "line2", "line3"])),
    content={
      "type": "EmbeddedCode::CodeMultilineString",
      "0": ["line1", "line2", "line3"],
    },
  )
}

///|
test "func_stubs to_json" {
  @json.inspect(
    @ast.Import(
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
    @ast.Embedded(
      language=Some(dummy_string_literal),
      code=@ast.CodeString(dummy_string_literal),
    ),
    content={
      "type": "FuncStubs::Embedded",
      "language": "test_string",
      "code": { "type": "EmbeddedCode::CodeString", "0": "test_string" },
    },
  )
  @json.inspect(
    @ast.Embedded(language=None, code=@ast.CodeString(dummy_string_literal)),
    content={
      "type": "FuncStubs::Embedded",
      "language": null,
      "code": { "type": "EmbeddedCode::CodeString", "0": "test_string" },
    },
  )
}

///|
test "parameter to_json" {
  let binder = @ast.Binder::{ name: "param", loc: dummy_loc }
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  @json.inspect(
    @ast.Parameter::DiscardPositional(ty=Some(int_type), loc=dummy_loc),
    content={
      "type": "Parameter::DiscardPositional",
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "loc": null,
    },
  )
  @json.inspect(@ast.Parameter::DiscardPositional(ty=None, loc=dummy_loc), content={
    "type": "Parameter::DiscardPositional",
    "ty": null,
    "loc": null,
  })
  @json.inspect(@ast.Parameter::Positional(binder~, ty=Some(int_type)), content={
    "type": "Parameter::Positional",
    "binder": { "type": "Binder", "name": "param", "loc": null },
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
  @json.inspect(@ast.Parameter::Positional(binder~, ty=None), content={
    "type": "Parameter::Positional",
    "binder": { "type": "Binder", "name": "param", "loc": null },
    "ty": null,
  })
  @json.inspect(@ast.Parameter::Labelled(binder~, ty=Some(int_type)), content={
    "type": "Parameter::Labelled",
    "binder": { "type": "Binder", "name": "param", "loc": null },
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
  @json.inspect(
    @ast.Parameter::Optional(binder~, default=unit_expr, ty=Some(int_type)),
    content={
      "type": "Parameter::Optional",
      "binder": { "type": "Binder", "name": "param", "loc": null },
      "default": { "type": "Unit", "loc": null, "faked": false },
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
    },
  )
  @json.inspect(@ast.Parameter::QuestionOptional(binder~, ty=Some(int_type)), content={
    "type": "Parameter::QuestionOptional",
    "binder": { "type": "Binder", "name": "param", "loc": null },
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
  })
}

///|
test "interp_elem to_json" {
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  @json.inspect(
    @ast.InterpElem::Literal(repr=dummy_string_literal, loc=dummy_loc),
    content={
      "type": "InterpElem::Literal",
      "repr": "test_string",
      "loc": null,
    },
  )
  @json.inspect(@ast.InterpElem::Expr(expr=unit_expr, loc=dummy_loc), content={
    "type": "InterpElem::Expr",
    "expr": { "type": "Unit", "loc": null, "faked": false },
    "loc": null,
  })
}

///|
test "multiline_string_elem to_json" {
  let interp_elem = @ast.InterpElem::Literal(
    repr=dummy_string_literal,
    loc=dummy_loc,
  )
  @json.inspect(@ast.MultilineStringElem::String("test string"), content={
    "type": "MultilineString::String",
    "0": "test string",
  })
  @json.inspect(@ast.MultilineStringElem::Interp(@list.of([interp_elem])), content={
    "type": "MultilineString::Interp",
    "0": [{ "type": "InterpElem::Literal", "repr": "test_string", "loc": null }],
  })
}

///|
test "spreadable_elem to_json" {
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  @json.inspect(@ast.SpreadableElem::Regular(unit_expr), content={
    "type": "SpreadableElem::Regular",
    "0": { "type": "Unit", "loc": null, "faked": false },
  })
  @json.inspect(@ast.SpreadableElem::Spread(expr=unit_expr, loc=dummy_loc), content={
    "type": "SpreadableElem::Spread",
    "expr": { "type": "Unit", "loc": null, "faked": false },
    "loc": null,
  })
}

///|
test "map_expr_elem to_json" {
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let int_constant = @ast.Int("42")
  let map_elem = @ast.MapExprElem::{
    key: int_constant,
    expr: unit_expr,
    key_loc: dummy_loc,
    loc: dummy_loc,
  }
  @json.inspect(map_elem, content={
    "type": "MapExprElem",
    "key": { "type": "Int", "0": "42" },
    "expr": { "type": "Unit", "loc": null, "faked": false },
    "key_loc": null,
    "loc": null,
  })
}

///|
test "field_def to_json" {
  let label = @ast.Label::{ name: "field_name", loc: dummy_loc }
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let field_def = @ast.FieldDef::{
    label,
    expr: unit_expr,
    is_pun: false,
    loc: dummy_loc,
  }
  @json.inspect(field_def, content={
    "type": "FieldDef",
    "label": { "type": "Label", "name": "field_name", "loc": null },
    "expr": { "type": "Unit", "loc": null, "faked": false },
    "is_pun": false,
    "loc": null,
  })
}

///|
test "argument to_json" {
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let argument = @ast.Argument::{
    value: unit_expr,
    kind: @ast.ArgumentKind::Positional,
  }
  @json.inspect(argument, content={
    "type": "Argument",
    "value": { "type": "Unit", "loc": null, "faked": false },
    "kind": { "type": "ArgumentKind::Positional" },
  })
}

///|
test "case to_json" {
  let any_pattern = @ast.Pattern::Any(loc=dummy_loc)
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let case_with_guard = @ast.Case::{
    pattern: any_pattern,
    guard_: Some(unit_expr),
    body: unit_expr,
  }
  let case_without_guard = @ast.Case::{
    pattern: any_pattern,
    guard_: None,
    body: unit_expr,
  }
  @json.inspect(case_with_guard, content={
    "type": "Case",
    "pattern": { "type": "Pattern::Any", "loc": null },
    "guard_": { "type": "Unit", "loc": null, "faked": false },
    "body": { "type": "Unit", "loc": null, "faked": false },
  })
  @json.inspect(case_without_guard, content={
    "type": "Case",
    "pattern": { "type": "Pattern::Any", "loc": null },
    "guard_": null,
    "body": { "type": "Unit", "loc": null, "faked": false },
  })
}

///|
test "multi_arg_case to_json" {
  let any_pattern = @ast.Pattern::Any(loc=dummy_loc)
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let multi_case = @ast.MultiArgCase::{
    patterns: @list.of([any_pattern]),
    guard_: Some(unit_expr),
    body: unit_expr,
  }
  @json.inspect(multi_case, content={
    "type": "MultiArgCase",
    "patterns": [{ "type": "Pattern::Any", "loc": null }],
    "guard_": { "type": "Unit", "loc": null, "faked": false },
    "body": { "type": "Unit", "loc": null, "faked": false },
  })
}

///|
test "static_assertion to_json" {
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let assertion = @ast.StaticAssertion::{
    ty: int_type,
    trait_: @ast.LongIdent::Ident(name="Show"),
    loc: dummy_loc,
    msg: "Test assertion message",
  }
  @json.inspect(assertion, content={
    "type": "StaticAssertion",
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
    "trait_": { "type": "Ident", "name": "Show" },
    "loc": null,
    "msg": "Test assertion message",
  })
}

///|
test "func to_json" {
  let binder = @ast.Binder::{ name: "param", loc: dummy_loc }
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let param = @ast.Parameter::Positional(binder~, ty=Some(int_type))
  let any_pattern = @ast.Pattern::Any(loc=dummy_loc)
  let multi_case = @ast.MultiArgCase::{
    patterns: @list.of([any_pattern]),
    guard_: None,
    body: unit_expr,
  }
  @json.inspect(
    @ast.Func::Lambda(
      parameters=@list.of([param]),
      params_loc=dummy_loc,
      body=unit_expr,
      return_type=Some(int_type),
      error_type=@ast.NoErrorType,
      kind=@ast.FnKind::Lambda,
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
              "id": { "type": "Ident", "name": "Int" },
              "loc": null,
            },
            "tys": [],
            "loc": null,
          },
        },
      ],
      "params_loc": null,
      "body": { "type": "Unit", "loc": null, "faked": false },
      "return_type": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
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
    @ast.Func::Match(
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
          "body": { "type": "Unit", "loc": null, "faked": false },
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
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let int_constant = @ast.Int("42")
  let variable = @ast.Var::{
    name: @ast.LongIdent::Ident(name="test_var"),
    loc: dummy_loc,
  }
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  let argument = @ast.Argument::{ value: unit_expr, kind: @ast.Positional }
  @json.inspect(@ast.Expr::Unit(loc=dummy_loc, faked=false), content={
    "type": "Unit",
    "loc": null,
    "faked": false,
  })
  @json.inspect(@ast.Expr::Constant(c=int_constant, loc=dummy_loc), content={
    "type": "Expr::Constant",
    "c": { "type": "Int", "0": "42" },
    "loc": null,
  })
  @json.inspect(@ast.Expr::Ident(id=variable, loc=dummy_loc), content={
    "type": "Expr::Ident",
    "id": {
      "type": "Var",
      "name": { "type": "Ident", "name": "test_var" },
      "loc": null,
    },
    "loc": null,
  })
  @json.inspect(
    @ast.Apply(
      func=unit_expr,
      args=@list.of([argument]),
      attr=@ast.NoAttr,
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Apply",
      "func": { "type": "Unit", "loc": null, "faked": false },
      "args": [
        {
          "type": "Argument",
          "value": { "type": "Unit", "loc": null, "faked": false },
          "kind": { "type": "ArgumentKind::Positional" },
        },
      ],
      "attr": { "type": "ApplyAttr::NoAttr" },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Infix(op=variable, lhs=unit_expr, rhs=unit_expr, loc=dummy_loc),
    content={
      "type": "Expr::Infix",
      "op": {
        "type": "Var",
        "name": { "type": "Ident", "name": "test_var" },
        "loc": null,
      },
      "lhs": { "type": "Unit", "loc": null, "faked": false },
      "rhs": { "type": "Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(@ast.Unary(op=variable, expr=unit_expr, loc=dummy_loc), content={
    "type": "Expr::Unary",
    "op": {
      "type": "Var",
      "name": { "type": "Ident", "name": "test_var" },
      "loc": null,
    },
    "expr": { "type": "Unit", "loc": null, "faked": false },
    "loc": null,
  })
  @json.inspect(@ast.Expr::Array(exprs=@list.of([unit_expr]), loc=dummy_loc), content={
    "type": "Expr::Array",
    "exprs": [{ "type": "Unit", "loc": null, "faked": false }],
    "loc": null,
  })
  let spread_elem = @ast.SpreadableElem::Regular(unit_expr)
  @json.inspect(
    @ast.ArraySpread(elems=@list.of([spread_elem]), loc=dummy_loc),
    content={
      "type": "Expr::ArraySpread",
      "elems": [
        {
          "type": "SpreadableElem::Regular",
          "0": { "type": "Unit", "loc": null, "faked": false },
        },
      ],
      "loc": null,
    },
  )
  @json.inspect(
    @ast.ArrayGet(array=unit_expr, index=unit_expr, loc=dummy_loc),
    content={
      "type": "Expr::ArrayGet",
      "array": { "type": "Unit", "loc": null, "faked": false },
      "index": { "type": "Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Expr::Tuple(exprs=@list.of([unit_expr, unit_expr]), loc=dummy_loc),
    content={
      "type": "Expr::Tuple",
      "exprs": [
        { "type": "Unit", "loc": null, "faked": false },
        { "type": "Unit", "loc": null, "faked": false },
      ],
      "loc": null,
    },
  )
  @json.inspect(@ast.Expr::Hole(loc=dummy_loc, kind=@ast.Todo), content={
    "type": "Expr::Hole",
    "loc": null,
    "kind": { "type": "Hole::Todo" },
  })
  @json.inspect(@ast.Return(return_value=Some(unit_expr), loc=dummy_loc), content={
    "type": "Expr::Return",
    "return_value": { "type": "Unit", "loc": null, "faked": false },
    "loc": null,
  })
  @json.inspect(@ast.Return(return_value=None, loc=dummy_loc), content={
    "type": "Expr::Return",
    "return_value": null,
    "loc": null,
  })
  @json.inspect(@ast.Raise(err_value=unit_expr, loc=dummy_loc), content={
    "type": "Expr::Raise",
    "err_value": { "type": "Unit", "loc": null, "faked": false },
    "loc": null,
  })
  @json.inspect(
    @ast.Break(arg=Some(unit_expr), label=Some(label), loc=dummy_loc),
    content={
      "type": "Expr::Break",
      "arg": { "type": "Unit", "loc": null, "faked": false },
      "label": { "type": "Label", "name": "test_label", "loc": null },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Continue(args=@list.of([unit_expr]), label=None, loc=dummy_loc),
    content={
      "type": "Expr::Continue",
      "args": [{ "type": "Unit", "loc": null, "faked": false }],
      "label": null,
      "loc": null,
    },
  )
}

///|
test "expr complex types to_json" {
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  let any_pattern = @ast.Pattern::Any(loc=dummy_loc)
  let case = @ast.Case::{ pattern: any_pattern, guard_: None, body: unit_expr }
  let type_name = @ast.TypeName::{
    name: @ast.LongIdent::Ident(name="TestType"),
    is_object: false,
    loc: dummy_loc,
  }
  let field_def = @ast.FieldDef::{
    label,
    expr: unit_expr,
    is_pun: false,
    loc: dummy_loc,
  }
  let variable = @ast.Var::{
    name: @ast.LongIdent::Ident(name="test_var"),
    loc: dummy_loc,
  }
  @json.inspect(
    @ast.If(
      cond=unit_expr,
      ifso=unit_expr,
      ifnot=Some(unit_expr),
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::If",
      "cond": { "type": "Unit", "loc": null, "faked": false },
      "ifso": { "type": "Unit", "loc": null, "faked": false },
      "ifnot": { "type": "Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Guard(
      cond=unit_expr,
      otherwise=Some(unit_expr),
      body=unit_expr,
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Guard",
      "cond": { "type": "Unit", "loc": null, "faked": false },
      "otherwise": { "type": "Unit", "loc": null, "faked": false },
      "body": { "type": "Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(@ast.Defer(expr=unit_expr, body=unit_expr, loc=dummy_loc), content={
    "type": "Expr::Defer",
    "expr": { "type": "Unit", "loc": null, "faked": false },
    "body": { "type": "Unit", "loc": null, "faked": false },
    "loc": null,
  })
  @json.inspect(@ast.Is(expr=unit_expr, pat=any_pattern, loc=dummy_loc), content={
    "type": "Expr::Is",
    "expr": { "type": "Unit", "loc": null, "faked": false },
    "pat": { "type": "Pattern::Any", "loc": null },
    "loc": null,
  })
  @json.inspect(
    @ast.Let(pattern=any_pattern, expr=unit_expr, body=unit_expr, loc=dummy_loc),
    content={
      "type": "Expr::Let",
      "pattern": { "type": "Pattern::Any", "loc": null },
      "expr": { "type": "Unit", "loc": null, "faked": false },
      "body": { "type": "Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.LetMut(
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
          "id": { "type": "Ident", "name": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "expr": { "type": "Unit", "loc": null, "faked": false },
      "body": { "type": "Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Sequence(
      exprs=@list.of([unit_expr]),
      last_expr=unit_expr,
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Sequence",
      "exprs": [{ "type": "Unit", "loc": null, "faked": false }],
      "last_expr": { "type": "Unit", "loc": null, "faked": false },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Expr::Record(
      type_name=Some(type_name),
      fields=@list.of([field_def]),
      trailing=@ast.TrailingMark::Comma,
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Record",
      "type_name": {
        "type": "TypeName",
        "name": { "type": "Ident", "name": "TestType" },
        "is_object": false,
        "loc": null,
      },
      "fields": [
        {
          "type": "FieldDef",
          "label": { "type": "Label", "name": "test_label", "loc": null },
          "expr": { "type": "Unit", "loc": null, "faked": false },
          "is_pun": false,
          "loc": null,
        },
      ],
      "trailing": { "type": "Trailing::Comma" },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Field(
      record=unit_expr,
      accessor=@ast.Accessor::Label(label),
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Field",
      "record": { "type": "Unit", "loc": null, "faked": false },
      "accessor": {
        "type": "Accessor::Label",
        "0": { "type": "Label", "name": "test_label", "loc": null },
      },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Expr::Match(
      expr=unit_expr,
      cases=@list.of([case]),
      match_loc=dummy_loc,
      using_=Some(label),
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Match",
      "expr": { "type": "Unit", "loc": null, "faked": false },
      "cases": [
        {
          "type": "Case",
          "pattern": { "type": "Pattern::Any", "loc": null },
          "guard_": null,
          "body": { "type": "Unit", "loc": null, "faked": false },
        },
      ],
      "match_loc": null,
      "using_": { "type": "Label", "name": "test_label", "loc": null },
      "loc": null,
    },
  )
  @json.inspect(@ast.Pipe(lhs=unit_expr, rhs=unit_expr, loc=dummy_loc), content={
    "type": "Expr::Pipe",
    "lhs": { "type": "Unit", "loc": null, "faked": false },
    "rhs": { "type": "Unit", "loc": null, "faked": false },
    "loc": null,
  })
  @json.inspect(
    @ast.Assign(
      var_=variable,
      expr=unit_expr,
      augmented_by=Some(variable),
      loc=dummy_loc,
    ),
    content={
      "type": "Expr::Assign",
      "var_": {
        "type": "Var",
        "name": { "type": "Ident", "name": "test_var" },
        "loc": null,
      },
      "expr": { "type": "Unit", "loc": null, "faked": false },
      "augmented_by": {
        "type": "Var",
        "name": { "type": "Ident", "name": "test_var" },
        "loc": null,
      },
      "loc": null,
    },
  )
}

///|
test "pattern to_json" {
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  let any_pattern = @ast.Pattern::Any(loc=dummy_loc)
  let int_constant = @ast.Int("42")
  let constant_pattern = @ast.Pattern::Constant(c=int_constant, loc=dummy_loc)
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let constr_name = @ast.ConstrName::{ name: "TestConstructor", loc: dummy_loc }
  let constr = @ast.Constructor::{
    name: constr_name,
    extra_info: @ast.NoExtraInfo,
    loc: dummy_loc,
  }
  let label = @ast.Label::{ name: "test_field", loc: dummy_loc }
  let field_pat = @ast.FieldPat::{
    label,
    pattern: any_pattern,
    is_pun: false,
    loc: dummy_loc,
  }
  let map_elem = @ast.MapPatElem::{
    key: int_constant,
    pat: any_pattern,
    match_absent: false,
    key_loc: dummy_loc,
    loc: dummy_loc,
  }
  let constr_pat_arg = @ast.ConstrPatArg::{
    pat: any_pattern,
    kind: @ast.ArgumentKind::Positional,
  }
  let array_patterns = @ast.ArrayPatterns::Closed(@list.of([]))
  @json.inspect(
    @ast.Pattern::Alias(pat=any_pattern, alias_=binder, loc=dummy_loc),
    content={
      "type": "Pattern::Alias",
      "pat": { "type": "Pattern::Any", "loc": null },
      "alias": { "type": "Binder", "name": "test_binder", "loc": null },
      "loc": null,
    },
  )
  @json.inspect(@ast.Pattern::Array(pats=array_patterns, loc=dummy_loc), content={
    "type": "Pattern::Array",
    "pats": { "type": "ArrayPatterns::Closed", "0": [] },
    "loc": null,
  })
  @json.inspect(@ast.Expr::Constant(c=int_constant, loc=dummy_loc), content={
    "type": "Expr::Constant",
    "c": { "type": "Int", "0": "42" },
    "loc": null,
  })
  @json.inspect(
    @ast.Pattern::Constraint(pat=any_pattern, ty=int_type, loc=dummy_loc),
    content={
      "type": "Pattern::Constraint",
      "pat": { "type": "Pattern::Any", "loc": null },
      "ty": {
        "type": "Type::Name",
        "constr_id": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Pattern::Constr(
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
    @ast.Pattern::Constr(constr~, args=None, is_open=true, loc=dummy_loc),
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
    @ast.Or(pat1=any_pattern, pat2=constant_pattern, loc=dummy_loc),
    content={
      "type": "Pattern::Or",
      "pat1": { "type": "Pattern::Any", "loc": null },
      "pat2": {
        "type": "Pattern::Constant",
        "c": { "type": "Int", "0": "42" },
        "loc": null,
      },
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Pattern::Tuple(
      pats=@list.of([any_pattern, constant_pattern]),
      loc=dummy_loc,
    ),
    content={
      "type": "Pattern::Tuple",
      "pats": [
        { "type": "Pattern::Any", "loc": null },
        {
          "type": "Pattern::Constant",
          "c": { "type": "Int", "0": "42" },
          "loc": null,
        },
      ],
      "loc": null,
    },
  )
  @json.inspect(
    @ast.Pattern::Record(
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
    @ast.Pattern::Map(
      elems=@list.of([map_elem]),
      is_closed=false,
      loc=dummy_loc,
    ),
    content={
      "type": "Pattern::Map",
      "elems": [
        {
          "type": "MapPatElem",
          "key": { "type": "Int", "0": "42" },
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
    @ast.Range(
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
        "c": { "type": "Int", "0": "42" },
        "loc": null,
      },
      "inclusive": true,
      "loc": null,
    },
  )
}

///|
test "field_name to_json" {
  let field_name = @ast.FieldName::{ label: "test_field", loc: dummy_loc }
  @json.inspect(field_name, content={
    "type": "FieldName",
    "label": "test_field",
    "loc": null,
  })
}

///|
test "field_decl to_json" {
  let field_name = @ast.FieldName::{ label: "test_field", loc: dummy_loc }
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let field_decl = @ast.FieldDecl::{
    name: field_name,
    ty: int_type,
    mut_: false,
    vis: @ast.Default,
    loc: dummy_loc,
  }
  @json.inspect(field_decl, content={
    "type": "FieldDecl",
    "name": { "type": "FieldName", "label": "test_field", "loc": null },
    "ty": {
      "type": "Type::Name",
      "constr_id": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
    "mut_": false,
    "vis": { "type": "Default" },
    "loc": null,
  })
}

///|
test "constr_param to_json" {
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  let param_with_label = @ast.ConstrParam::{
    ty: int_type,
    mut_: false,
    label: Some(label),
  }
  let param_without_label = @ast.ConstrParam::{
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
        "id": { "type": "Ident", "name": "Int" },
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
        "id": { "type": "Ident", "name": "Int" },
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
  let constr_name = @ast.ConstrName::{ name: "TestConstructor", loc: dummy_loc }
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  let param = @ast.ConstrParam::{
    ty: int_type,
    mut_: false,
    label: Some(label),
  }
  let constr_with_args = @ast.ConstrDecl::{
    name: constr_name,
    args: Some(@list.of([param])),
    tag: Some(("tag_value", dummy_loc)),
    loc: dummy_loc,
    doc: dummy_docstring,
  }
  let constr_without_args = @ast.ConstrDecl::{
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
            "id": { "type": "Ident", "name": "Int" },
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
  let constraint = @ast.TypeVarConstraint::{
    trait_: @ast.LongIdent::Ident(name="Show"),
    loc: dummy_loc,
  }
  @json.inspect(constraint, content={
    "type": "TypeVarConstraint",
    "trait_": { "type": "Ident", "name": "Show" },
    "loc": null,
  })
}

///|
test "type_var_binder to_json" {
  let constraint = @ast.TypeVarConstraint::{
    trait_: @ast.LongIdent::Ident(name="Show"),
    loc: dummy_loc,
  }
  let type_var_binder = @ast.TypeVarBinder::{
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
        "trait_": { "type": "Ident", "name": "Show" },
        "loc": null,
      },
    ],
    "loc": null,
  })
}

///|
test "type_decl_binder to_json" {
  let binder_with_name = @ast.TypeDeclBinder::{
    name: Some("TypeVar"),
    loc: dummy_loc,
  }
  let binder_without_name = @ast.TypeDeclBinder::{ name: None, loc: dummy_loc }
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
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  let label = @ast.Label::{ name: "target_label", loc: dummy_loc }
  let alias_with_target = @ast.AliasTarget::{ binder, target: Some(label) }
  let alias_without_target = @ast.AliasTarget::{ binder, target: None }
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
  let label = @ast.Label::{ name: "field_label", loc: dummy_loc }
  let any_pattern = @ast.Pattern::Any(loc=dummy_loc)
  let field_pat = @ast.FieldPat::{
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
  let any_pattern = @ast.Pattern::Any(loc=dummy_loc)
  let label = @ast.Label::{ name: "arg_label", loc: dummy_loc }
  let arg = @ast.ConstrPatArg::{
    pat: any_pattern,
    kind: @ast.ArgumentKind::Labelled(label),
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
  let int_constant = @ast.Int("42")
  let any_pattern = @ast.Pattern::Any(loc=dummy_loc)
  let map_elem = @ast.MapPatElem::{
    key: int_constant,
    pat: any_pattern,
    match_absent: true,
    key_loc: dummy_loc,
    loc: dummy_loc,
  }
  @json.inspect(map_elem, content={
    "type": "MapPatElem",
    "key": { "type": "Int", "0": "42" },
    "pat": { "type": "Pattern::Any", "loc": null },
    "match_absent": true,
    "key_loc": null,
    "loc": null,
  })
}

///|
test "local_type_decl to_json" {
  let deriving_directive = @ast.DerivingDirective::{
    type_name: @ast.TypeName::{
      name: @ast.LongIdent::Ident(name="Show"),
      is_object: false,
      loc: dummy_loc,
    },
    args: @list.of([]),
    loc: dummy_loc,
  }
  let local_type_decl = @ast.LocalTypeDecl::{
    tycon: "TestType",
    tycon_loc: dummy_loc,
    components: @ast.Abstract,
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
          "name": { "type": "Ident", "name": "Show" },
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
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let argument = @ast.Argument::{ value: unit_expr, kind: @ast.Positional }
  let deriving_directive = @ast.DerivingDirective::{
    type_name: @ast.TypeName::{
      name: @ast.Dot(pkg="std", id="Show"),
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
      "name": { "type": "Dot", "pkg": "std", "id": "Show" },
      "is_object": true,
      "loc": null,
    },
    "args": [
      {
        "type": "Argument",
        "value": { "type": "Unit", "loc": null, "faked": false },
        "kind": { "type": "ArgumentKind::Positional" },
      },
    ],
    "loc": null,
  })
}

///|
test "type_decl to_json" {
  let type_decl_binder = @ast.TypeDeclBinder::{
    name: Some("T"),
    loc: dummy_loc,
  }
  let type_decl = @ast.TypeDecl::{
    tycon: "TestType",
    tycon_loc: dummy_loc,
    params: @list.of([type_decl_binder]),
    components: @ast.Extern,
    attrs: @list.of([dummy_attribute]),
    doc: dummy_docstring,
    type_vis: @ast.Default,
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
    "type_vis": { "type": "Default" },
    "deriving": [],
    "loc": null,
  })
}

///|
test "decl_body to_json" {
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let local_type_decl = @ast.LocalTypeDecl::{
    tycon: "LocalType",
    tycon_loc: dummy_loc,
    components: @ast.Abstract,
    deriving: @list.of([]),
  }
  @json.inspect(
    @ast.DeclBody(local_types=@list.of([local_type_decl]), expr=unit_expr),
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
      "expr": { "type": "Unit", "loc": null, "faked": false },
    },
  )
  @json.inspect(
    @ast.DeclStubs(
      @ast.Import(
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
  let binder = @ast.Binder::{ name: "test_func", loc: dummy_loc }
  let type_name = @ast.TypeName::{
    name: @ast.LongIdent::Ident(name="TestType"),
    is_object: false,
    loc: dummy_loc,
  }
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let param = @ast.Parameter::Positional(binder~, ty=Some(int_type))
  let type_var_constraint = @ast.TypeVarConstraint::{
    trait_: @ast.LongIdent::Ident(name="Show"),
    loc: dummy_loc,
  }
  let type_var_binder = @ast.TypeVarBinder::{
    name: "T",
    constraints: @list.of([type_var_constraint]),
    loc: dummy_loc,
  }
  let fun_decl = @ast.FunDecl::{
    type_name: Some(type_name),
    name: binder,
    has_error: Some(dummy_loc),
    is_async: true,
    decl_params: Some(@list.of([param])),
    params_loc: dummy_loc,
    quantifiers: @list.of([type_var_binder]),
    return_type: Some(int_type),
    error_type: @ast.NoErrorType,
    vis: @ast.Priv(loc=dummy_loc),
    attrs: @list.of([dummy_attribute]),
    doc: dummy_docstring,
  }
  @json.inspect(fun_decl, content={
    "type": "FunDecl",
    "type_name": {
      "type": "TypeName",
      "name": { "type": "Ident", "name": "TestType" },
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
            "id": { "type": "Ident", "name": "Int" },
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
            "trait_": { "type": "Ident", "name": "Show" },
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
        "id": { "type": "Ident", "name": "Int" },
        "loc": null,
      },
      "tys": [],
      "loc": null,
    },
    "error_type": { "type": "ErrorType::NoErrorType" },
    "vis": { "type": "Priv", "loc": null },
    "attrs": [{ "loc": null, "raw": "test_attr" }],
    "doc": { "content": ["Test documentation"], "loc": null },
  })
}

///|
test "trait_method_decl to_json" {
  let binder = @ast.Binder::{ name: "trait_method", loc: dummy_loc }
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  let param = @ast.Parameter::Positional(binder~, ty=Some(int_type))
  let type_var_binder = @ast.TypeVarBinder::{
    name: "T",
    constraints: @list.of([]),
    loc: dummy_loc,
  }
  let trait_method = @ast.TraitMethodDecl::{
    name: binder,
    has_error: None,
    is_async: false,
    quantifiers: @list.of([type_var_binder]),
    params: @list.of([param]),
    return_type: Some(int_type),
    error_type: @ast.ErrorType::DefaultErrorType(loc=dummy_loc),
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
            "id": { "type": "Ident", "name": "Int" },
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
        "id": { "type": "Ident", "name": "Int" },
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
  let trait_binder = @ast.Binder::{ name: "TestTrait", loc: dummy_loc }
  let super_constraint = @ast.TypeVarConstraint::{
    trait_: @ast.LongIdent::Ident(name="SuperTrait"),
    loc: dummy_loc,
  }
  let method_binder = @ast.Binder::{ name: "method", loc: dummy_loc }
  let trait_method = @ast.TraitMethodDecl::{
    name: method_binder,
    has_error: None,
    is_async: false,
    quantifiers: @list.of([]),
    params: @list.of([]),
    return_type: None,
    error_type: @ast.NoErrorType,
    has_default: None,
    loc: dummy_loc,
  }
  let trait_decl = @ast.TraitDecl::{
    name: trait_binder,
    supers: @list.of([super_constraint]),
    methods: @list.of([trait_method]),
    vis: @ast.Pub(attr=None, loc=dummy_loc),
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
        "trait_": { "type": "Ident", "name": "SuperTrait" },
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
    "vis": { "type": "Pub", "attr": null, "loc": null },
    "loc": null,
    "attrs": [{ "loc": null, "raw": "test_attr" }],
    "doc": { "content": ["Test documentation"], "loc": null },
  })
}

///|
test "impl variants to_json" {
  let unit_expr = @ast.Expr::Unit(loc=dummy_loc, faked=false)
  let type_decl = @ast.TypeDecl::{
    tycon: "TestType",
    tycon_loc: dummy_loc,
    params: @list.of([]),
    components: @ast.Abstract,
    attrs: @list.of([]),
    doc: dummy_docstring,
    type_vis: @ast.Default,
    deriving: @list.of([]),
    loc: dummy_loc,
  }
  let binder = @ast.Binder::{ name: "test", loc: dummy_loc }
  let int_type = @ast.Name(
    constr_id=@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(
    @ast.TopExpr(
      expr=unit_expr,
      is_main=true,
      local_types=@list.of([]),
      loc=dummy_loc,
    ),
    content={
      "type": "Impl::TopExpr",
      "expr": { "type": "Unit", "loc": null, "faked": false },
      "is_main": true,
      "local_types": [],
      "loc": null,
    },
  )
  @json.inspect(
    @ast.TopTest(
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
      "expr": { "type": "Unit", "loc": null, "faked": false },
      "name": ["test_string", null],
      "params": null,
      "local_types": [],
      "loc": null,
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": null },
    },
  )
  @json.inspect(@ast.TopTypeDef(type_decl), content={
    "type": "Impl::TopTypeDef",
    "0": {
      "type": "TypeDecl",
      "tycon": "TestType",
      "tycon_loc": null,
      "params": [],
      "components": { "type": "TypeDesc::Abstract" },
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": null },
      "type_vis": { "type": "Default" },
      "deriving": [],
      "loc": null,
    },
  })
  let fun_decl = @ast.FunDecl::{
    type_name: None,
    name: binder,
    has_error: None,
    is_async: false,
    decl_params: None,
    params_loc: dummy_loc,
    quantifiers: @list.of([]),
    return_type: None,
    error_type: @ast.NoErrorType,
    vis: @ast.Default,
    attrs: @list.of([]),
    doc: dummy_docstring,
  }
  let decl_body = @ast.DeclBody(local_types=@list.of([]), expr=unit_expr)
  @json.inspect(@ast.TopFuncDef(fun_decl~, decl_body~, loc=dummy_loc), content={
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
      "vis": { "type": "Default" },
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": null },
    },
    "decl_body": {
      "type": "DeclBody::DeclBody",
      "local_types": [],
      "expr": { "type": "Unit", "loc": null, "faked": false },
    },
    "loc": null,
  })
  @json.inspect(
    @ast.TopLetDef(
      binder~,
      ty=Some(int_type),
      expr=unit_expr,
      vis=@ast.Default,
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
          "id": { "type": "Ident", "name": "Int" },
          "loc": null,
        },
        "tys": [],
        "loc": null,
      },
      "expr": { "type": "Unit", "loc": null, "faked": false },
      "vis": { "type": "Default" },
      "is_constant": true,
      "loc": null,
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": null },
    },
  )
}
