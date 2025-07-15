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
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.Pub(attr=Some("readonly"), loc=dummy_loc), content={
    "type": "Pub",
    "attr": "readonly",
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.Pub(attr=None, loc=dummy_loc), content={
    "type": "Pub",
    "attr": null,
    "loc": ":0:0-0:0",
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
  @json.inspect(@ast.String(dummy_string_literal), content={
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
    "loc": ":0:0-0:0",
  })
}

///|
test "constr_name to_json" {
  let constr_name = @ast.ConstrName::{ name: "TestConstructor", loc: dummy_loc }
  @json.inspect(constr_name, content={
    "type": "ConstrName",
    "name": "TestConstructor",
    "loc": ":0:0-0:0",
  })
}

///|
test "long_ident to_json" {
  @json.inspect(@ast.LongIdent::Ident(name="test_ident"), content={
    "type": "Ident",
    "name": "test_ident",
  })
  @json.inspect(@ast.LongIdent::Qualified(pkg="test_pkg", name="test_name"), content={
    "type": "Qualified",
    "pkg": "test_pkg",
    "name": "test_name",
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
    "loc": ":0:0-0:0",
  })
}

///|
test "constr_id to_json" {
  let constr_id = @ast.ConstrId::{
    id: @ast.Qualified(pkg="test_pkg", name="TestConstructor"),
    loc: dummy_loc,
  }
  @json.inspect(constr_id, content={
    "type": "ConstrId",
    "id": { "type": "Qualified", "pkg": "test_pkg", "name": "TestConstructor" },
    "loc": ":0:0-0:0",
  })
}

///|
test "error_type to_json" {
  @json.inspect(@ast.NoErrorType, content={ "type": "NoErrorType" })
  @json.inspect(@ast.DefaultErrorType(dummy_loc, is_old_syntax=false), content={
    "type": "DefaultErrorType",
    "0": ":0:0-0:0",
    "is_old_syntax": false,
  })
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@ast.ErrorType(ty=int_type, is_old_syntax=true), content={
    "type": "ErrorType",
    "ty": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
    "is_old_syntax": true,
  })
  @json.inspect(@ast.MaybeError(int_type, is_old_syntax=false), content={
    "type": "MaybeError",
    "0": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
    "is_old_syntax": false,
  })
}

///|
test "type to_json" {
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@ast.TAny(dummy_loc), content={
    "type": "TAny",
    "0": ":0:0-0:0",
  })
  @json.inspect(
    @ast.TArrow(
      args=@list.of([int_type]),
      res=int_type,
      err=@ast.NoErrorType,
      is_async=false,
      loc=dummy_loc,
    ),
    content={
      "type": "TArrow",
      "args": [
        {
          "type": "TName",
          "0": {
            "type": "ConstrId",
            "id": { "type": "Ident", "name": "Int" },
            "loc": ":0:0-0:0",
          },
          "1": [],
          "loc": ":0:0-0:0",
        },
      ],
      "res": {
        "type": "TName",
        "0": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": ":0:0-0:0",
        },
        "1": [],
        "loc": ":0:0-0:0",
      },
      "err": { "type": "NoErrorType" },
      "is_async": false,
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(@ast.TTuple(@list.of([int_type]), loc=dummy_loc), content={
    "type": "TTuple",
    "0": [
      {
        "type": "TName",
        "0": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": ":0:0-0:0",
        },
        "1": [],
        "loc": ":0:0-0:0",
      },
    ],
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.TOption(int_type, loc=dummy_loc, question_loc=dummy_loc), content={
    "type": "TOption",
    "0": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
    "loc": ":0:0-0:0",
    "question_loc": ":0:0-0:0",
  })
  @json.inspect(
    @ast.TObject(@ast.ConstrId::{
      id: @ast.LongIdent::Ident(name="TestObject"),
      loc: dummy_loc,
    }),
    content={
      "type": "TObject",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "TestObject" },
        "loc": ":0:0-0:0",
      },
    },
  )
}

///|
test "hole to_json" {
  @json.inspect(@ast.Synthesized, content={ "type": "Synthesized" })
  @json.inspect(@ast.Incomplete, content={ "type": "Incomplete" })
  @json.inspect(@ast.Todo, content={ "type": "Todo" })
}

///|
test "argument_kind to_json" {
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  @json.inspect(@ast.ArgumentKind::Positional, content={ "type": "Positional" })
  @json.inspect(@ast.ArgumentKind::Labelled(label), content={
    "type": "Labelled",
    "0": { "type": "Label", "name": "test_label", "loc": ":0:0-0:0" },
  })
  @json.inspect(@ast.ArgumentKind::LabelledPun(label), content={
    "type": "LabelledPun",
    "0": { "type": "Label", "name": "test_label", "loc": ":0:0-0:0" },
  })
  @json.inspect(
    @ast.ArgumentKind::LabelledOption(label~, question_loc=dummy_loc),
    content={
      "type": "LabelledOption",
      "label": { "type": "Label", "name": "test_label", "loc": ":0:0-0:0" },
      "question_loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.ArgumentKind::LabelledOptionPun(label~, question_loc=dummy_loc),
    content={
      "type": "LabelledOptionPun",
      "label": { "type": "Label", "name": "test_label", "loc": ":0:0-0:0" },
      "question_loc": ":0:0-0:0",
    },
  )
}

///|
test "fn_kind to_json" {
  @json.inspect(@ast.FnKind::Lambda, content={ "type": "Lambda" })
  @json.inspect(@ast.FnKind::Matrix, content={ "type": "Matrix" })
  @json.inspect(@ast.FnKind::Arrow, content={ "type": "Arrow" })
}

///|
test "group to_json" {
  @json.inspect(@ast.GroupBrace, content={ "type": "GroupBrace" })
  @json.inspect(@ast.GroupParen, content={ "type": "GroupParen" })
}

///|
test "trailing_mark to_json" {
  @json.inspect(@ast.TrailingComma, content={ "type": "TrailingComma" })
  @json.inspect(@ast.TrailingSemi, content={ "type": "TrailingSemi" })
  @json.inspect(@ast.TrailingNone, content={ "type": "TrailingNone" })
}

///|
test "apply_attr to_json" {
  @json.inspect(@ast.NoAttr, content={ "type": "NoAttr" })
  @json.inspect(@ast.Exclamation, content={ "type": "Exclamation" })
  @json.inspect(@ast.Question, content={ "type": "Question" })
}

///|
test "binder to_json" {
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  @json.inspect(binder, content={
    "type": "Binder",
    "name": "test_binder",
    "loc": ":0:0-0:0",
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
    "loc": ":0:0-0:0",
  })
}

///|
test "constructor_extra_info to_json" {
  let type_name = @ast.TypeName::{
    name: @ast.LongIdent::Ident(name="TestType"),
    is_object: false,
    loc: dummy_loc,
  }
  @json.inspect(@ast.NoExtraInfo, content={ "type": "NoExtraInfo" })
  @json.inspect(@ast.TypeName(type_name), content={
    "type": "TypeName",
    "0": {
      "type": "TypeName",
      "name": { "type": "Ident", "name": "TestType" },
      "is_object": false,
      "loc": ":0:0-0:0",
    },
  })
  @json.inspect(@ast.Package("test_package"), content={
    "type": "Package",
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
    "name": {
      "type": "ConstrName",
      "name": "TestConstructor",
      "loc": ":0:0-0:0",
    },
    "extra_info": { "type": "NoExtraInfo" },
    "loc": ":0:0-0:0",
  })
}

///|
test "accessor to_json" {
  let label = @ast.Label::{ name: "test_field", loc: dummy_loc }
  @json.inspect(@ast.Accessor::Label(label), content={
    "type": "Label",
    "0": { "type": "Label", "name": "test_field", "loc": ":0:0-0:0" },
  })
  @json.inspect(@ast.Accessor::Index(tuple_index=0, loc=dummy_loc), content={
    "type": "Index",
    "tuple_index": 0,
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.Accessor::Newtype(loc=dummy_loc), content={
    "type": "Newtype",
    "loc": ":0:0-0:0",
  })
}

///|
test "exception_decl to_json" {
  @json.inspect(@ast.NoPayload, content={ "type": "NoPayload" })
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@ast.SinglePayload(int_type), content={
    "type": "SinglePayload",
    "0": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
  })
  let constr_decl = @ast.ConstrDecl::{
    name: @ast.ConstrName::{ name: "TestConstr", loc: dummy_loc },
    args: None,
    tag: None,
    loc: dummy_loc,
  }
  @json.inspect(@ast.EnumPayload(@list.of([constr_decl])), content={
    "type": "EnumPayload",
    "0": [
      {
        "type": "ConstrDecl",
        "name": {
          "type": "ConstrName",
          "name": "TestConstr",
          "loc": ":0:0-0:0",
        },
        "loc": ":0:0-0:0",
      },
    ],
  })
}

///|
test "type_desc to_json" {
  @json.inspect(@ast.Abstract, content={ "type": "Abstract" })
  @json.inspect(@ast.Extern, content={ "type": "Extern" })
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  @json.inspect(@ast.TypeDesc::Newtype(int_type), content={
    "type": "Newtype",
    "0": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
  })
  @json.inspect(@ast.Error(@ast.NoPayload), content={
    "type": "Error",
    "0": { "type": "NoPayload" },
  })
  @json.inspect(@ast.TypeDesc::Alias(int_type), content={
    "type": "Alias",
    "0": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
  })
}

///|
test "dot_dot_binder to_json" {
  @json.inspect(@ast.Underscore, content={ "type": "Underscore" })
  @json.inspect(@ast.NoBinder, content={ "type": "NoBinder" })
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  @json.inspect(@ast.BinderAs(binder), content={
    "type": "BinderAs",
    "0": { "type": "Binder", "name": "test_binder", "loc": ":0:0-0:0" },
  })
  @json.inspect(@ast.Binder_(binder), content={
    "type": "Binder_",
    "0": { "type": "Binder", "name": "test_binder", "loc": ":0:0-0:0" },
  })
}

///|
test "array_pattern to_json" {
  let any_pattern = @ast.Any(loc=dummy_loc)
  @json.inspect(@ast.Pattern(any_pattern), content={
    "type": "Pattern",
    "0": { "type": "Any", "loc": ":0:0-0:0" },
  })
  @json.inspect(@ast.StringSpread(dummy_string_literal), content={
    "type": "StringSpread",
    "0": "test_string",
  })
  @json.inspect(@ast.BytesSpread(dummy_bytes_literal), content={
    "type": "BytesSpread",
    "0": "b\"ABC\"",
  })
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  @json.inspect(
    @ast.ConstSpread(binder~, pkg=Some("test_pkg"), loc=dummy_loc),
    content={
      "type": "ConstSpread",
      "binder": { "type": "Binder", "name": "test_binder", "loc": ":0:0-0:0" },
      "pkg": "test_pkg",
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(@ast.ConstSpread(binder~, pkg=None, loc=dummy_loc), content={
    "type": "ConstSpread",
    "binder": { "type": "Binder", "name": "test_binder", "loc": ":0:0-0:0" },
    "pkg": null,
    "loc": ":0:0-0:0",
  })
}

///|
test "array_patterns to_json" {
  let any_pattern = @ast.Pattern(@ast.Any(loc=dummy_loc))
  @json.inspect(@ast.Closed(@list.of([any_pattern])), content={
    "type": "Closed",
    "0": [{ "type": "Pattern", "0": { "type": "Any", "loc": ":0:0-0:0" } }],
  })
  @json.inspect(
    @ast.Open(@list.of([any_pattern]), @list.of([any_pattern]), @ast.Underscore),
    content={
      "type": "Open",
      "0": [{ "type": "Pattern", "0": { "type": "Any", "loc": ":0:0-0:0" } }],
      "1": [{ "type": "Pattern", "0": { "type": "Any", "loc": ":0:0-0:0" } }],
      "2": { "type": "Underscore" },
    },
  )
}

///|
test "embedded_code to_json" {
  @json.inspect(@ast.CodeString(dummy_string_literal), content={
    "type": "CodeString",
    "0": "test_string",
  })
  @json.inspect(
    @ast.CodeMultilineString(@list.of(["line1", "line2", "line3"])),
    content={ "type": "CodeMultilineString", "0": ["line1", "line2", "line3"] },
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
      "type": "Import",
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
      "type": "Embedded",
      "language": "test_string",
      "code": { "type": "CodeString", "0": "test_string" },
    },
  )
  @json.inspect(
    @ast.Embedded(language=None, code=@ast.CodeString(dummy_string_literal)),
    content={
      "type": "Embedded",
      "language": null,
      "code": { "type": "CodeString", "0": "test_string" },
    },
  )
}

///|
test "parameter to_json" {
  let binder = @ast.Binder::{ name: "param", loc: dummy_loc }
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  @json.inspect(
    @ast.Parameter::DiscardPositional(ty=Some(int_type), loc=dummy_loc),
    content={
      "type": "DiscardPositional",
      "ty": {
        "type": "TName",
        "0": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": ":0:0-0:0",
        },
        "1": [],
        "loc": ":0:0-0:0",
      },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(@ast.Parameter::DiscardPositional(ty=None, loc=dummy_loc), content={
    "type": "DiscardPositional",
    "ty": null,
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.Parameter::Positional(binder~, ty=Some(int_type)), content={
    "type": "Positional",
    "binder": { "type": "Binder", "name": "param", "loc": ":0:0-0:0" },
    "ty": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
  })
  @json.inspect(@ast.Parameter::Positional(binder~, ty=None), content={
    "type": "Positional",
    "binder": { "type": "Binder", "name": "param", "loc": ":0:0-0:0" },
    "ty": null,
  })
  @json.inspect(@ast.Parameter::Labelled(binder~, ty=Some(int_type)), content={
    "type": "Labelled",
    "binder": { "type": "Binder", "name": "param", "loc": ":0:0-0:0" },
    "ty": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
  })
  @json.inspect(
    @ast.Parameter::Optional(binder~, default=unit_expr, ty=Some(int_type)),
    content={
      "type": "Optional",
      "binder": { "type": "Binder", "name": "param", "loc": ":0:0-0:0" },
      "default": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "ty": {
        "type": "TName",
        "0": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": ":0:0-0:0",
        },
        "1": [],
        "loc": ":0:0-0:0",
      },
    },
  )
  @json.inspect(@ast.Parameter::QuestionOptional(binder~, ty=Some(int_type)), content={
    "type": "QuestionOptional",
    "binder": { "type": "Binder", "name": "param", "loc": ":0:0-0:0" },
    "ty": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
  })
}

///|
test "interp_elem to_json" {
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  @json.inspect(
    @ast.InterpElem::InterpLit(repr=dummy_string_literal, loc=dummy_loc),
    content={ "type": "InterpLit", "repr": "test_string", "loc": ":0:0-0:0" },
  )
  @json.inspect(@ast.InterpExpr(expr=unit_expr, loc=dummy_loc), content={
    "type": "InterpExpr",
    "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "loc": ":0:0-0:0",
  })
}

///|
test "multiline_string_elem to_json" {
  let interp_elem = @ast.InterpElem::InterpLit(
    repr=dummy_string_literal,
    loc=dummy_loc,
  )
  @json.inspect(@ast.MultilineStringElem::MultilineString("test string"), content={
    "type": "MultilineString",
    "0": "test string",
  })
  @json.inspect(@ast.MultilineInterp(@list.of([interp_elem])), content={
    "type": "MultilineInterp",
    "0": [{ "type": "InterpLit", "repr": "test_string", "loc": ":0:0-0:0" }],
  })
}

///|
test "spreadable_elem to_json" {
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  @json.inspect(@ast.ElemRegular(unit_expr), content={
    "type": "ElemRegular",
    "0": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
  })
  @json.inspect(@ast.ElemSpread(expr=unit_expr, loc=dummy_loc), content={
    "type": "ElemSpread",
    "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "loc": ":0:0-0:0",
  })
}

///|
test "map_expr_elem to_json" {
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
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
    "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "key_loc": ":0:0-0:0",
    "loc": ":0:0-0:0",
  })
}

///|
test "field_def to_json" {
  let label = @ast.Label::{ name: "field_name", loc: dummy_loc }
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  let field_def = @ast.FieldDef::{
    label,
    expr: unit_expr,
    is_pun: false,
    loc: dummy_loc,
  }
  @json.inspect(field_def, content={
    "type": "FieldDef",
    "label": { "type": "Label", "name": "field_name", "loc": ":0:0-0:0" },
    "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "is_pun": false,
    "loc": ":0:0-0:0",
  })
}

///|
test "argument to_json" {
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  let argument = @ast.Argument::{
    value: unit_expr,
    kind: @ast.ArgumentKind::Positional,
  }
  @json.inspect(argument, content={
    "type": "Argument",
    "value": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "kind": { "type": "Positional" },
  })
}

///|
test "case to_json" {
  let any_pattern = @ast.Any(loc=dummy_loc)
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
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
    "pattern": { "type": "Any", "loc": ":0:0-0:0" },
    "guard_": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "body": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
  })
  @json.inspect(case_without_guard, content={
    "type": "Case",
    "pattern": { "type": "Any", "loc": ":0:0-0:0" },
    "body": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
  })
}

///|
test "multi_arg_case to_json" {
  let any_pattern = @ast.Any(loc=dummy_loc)
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  let multi_case = @ast.MultiArgCase::{
    patterns: @list.of([any_pattern]),
    guard_: Some(unit_expr),
    body: unit_expr,
  }
  @json.inspect(multi_case, content={
    "type": "MultiArgCase",
    "patterns": [{ "type": "Any", "loc": ":0:0-0:0" }],
    "guard_": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "body": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
  })
}

///|
test "static_assertion to_json" {
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  let assertion = @ast.StaticAssertion::{
    assert_type: int_type,
    assert_trait: @ast.LongIdent::Ident(name="Show"),
    assert_loc: dummy_loc,
    assert_msg: "Test assertion message",
  }
  @json.inspect(assertion, content={
    "type": "StaticAssertion",
    "assert_type": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
    "assert_trait": { "type": "Ident", "name": "Show" },
    "assert_loc": ":0:0-0:0",
    "assert_msg": "Test assertion message",
  })
}

///|
test "func to_json" {
  let binder = @ast.Binder::{ name: "param", loc: dummy_loc }
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  let param = @ast.Parameter::Positional(binder~, ty=Some(int_type))
  let any_pattern = @ast.Any(loc=dummy_loc)
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
      "type": "Lambda",
      "parameters": [
        {
          "type": "Positional",
          "binder": { "type": "Binder", "name": "param", "loc": ":0:0-0:0" },
          "ty": {
            "type": "TName",
            "0": {
              "type": "ConstrId",
              "id": { "type": "Ident", "name": "Int" },
              "loc": ":0:0-0:0",
            },
            "1": [],
            "loc": ":0:0-0:0",
          },
        },
      ],
      "params_loc": ":0:0-0:0",
      "body": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "return_type": {
        "type": "TName",
        "0": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": ":0:0-0:0",
        },
        "1": [],
        "loc": ":0:0-0:0",
      },
      "error_type": { "type": "NoErrorType" },
      "kind": { "type": "Lambda" },
      "has_error": ":0:0-0:0",
      "is_async": false,
      "loc": ":0:0-0:0",
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
      "type": "Match",
      "cases": [
        {
          "type": "MultiArgCase",
          "patterns": [{ "type": "Any", "loc": ":0:0-0:0" }],
          "body": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
        },
      ],
      "has_error": null,
      "is_async": true,
      "fn_loc": ":0:0-0:0",
      "loc": ":0:0-0:0",
    },
  )
}

///|
test "expr basic types to_json" {
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  let int_constant = @ast.Int("42")
  let variable = @ast.Var::{
    name: @ast.LongIdent::Ident(name="test_var"),
    loc: dummy_loc,
  }
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  let argument = @ast.Argument::{ value: unit_expr, kind: @ast.Positional }
  @json.inspect(@ast.Unit_(loc=dummy_loc, faked=false), content={
    "type": "Unit_",
    "loc": ":0:0-0:0",
    "faked": false,
  })
  @json.inspect(@ast.Expr::Constant(c=int_constant, loc=dummy_loc), content={
    "type": "Constant",
    "c": { "type": "Int", "0": "42" },
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.Expr::Ident(id=variable, loc=dummy_loc), content={
    "type": "Ident",
    "id": {
      "type": "Var",
      "name": { "type": "Ident", "name": "test_var" },
      "loc": ":0:0-0:0",
    },
    "loc": ":0:0-0:0",
  })
  @json.inspect(
    @ast.Apply(
      func=unit_expr,
      args=@list.of([argument]),
      attr=@ast.NoAttr,
      loc=dummy_loc,
    ),
    content={
      "type": "Apply",
      "func": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "args": [
        {
          "type": "Argument",
          "value": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
          "kind": { "type": "Positional" },
        },
      ],
      "attr": { "type": "NoAttr" },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Infix(op=variable, lhs=unit_expr, rhs=unit_expr, loc=dummy_loc),
    content={
      "type": "Infix",
      "op": {
        "type": "Var",
        "name": { "type": "Ident", "name": "test_var" },
        "loc": ":0:0-0:0",
      },
      "lhs": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "rhs": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(@ast.Unary(op=variable, expr=unit_expr, loc=dummy_loc), content={
    "type": "Unary",
    "op": {
      "type": "Var",
      "name": { "type": "Ident", "name": "test_var" },
      "loc": ":0:0-0:0",
    },
    "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.Expr::Array_(exprs=@list.of([unit_expr]), loc=dummy_loc), content={
    "type": "Array_",
    "exprs": [{ "type": "Unit_", "loc": ":0:0-0:0", "faked": false }],
    "loc": ":0:0-0:0",
  })
  let spread_elem = @ast.ElemRegular(unit_expr)
  @json.inspect(
    @ast.ArraySpread(elems=@list.of([spread_elem]), loc=dummy_loc),
    content={
      "type": "ArraySpread",
      "elems": [
        {
          "type": "ElemRegular",
          "0": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
        },
      ],
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.ArrayGet(array=unit_expr, index=unit_expr, loc=dummy_loc),
    content={
      "type": "ArrayGet",
      "array": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "index": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Expr::Tuple(exprs=@list.of([unit_expr, unit_expr]), loc=dummy_loc),
    content={
      "type": "Tuple",
      "exprs": [
        { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
        { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      ],
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(@ast.Hole_(loc=dummy_loc, kind=@ast.Todo), content={
    "type": "Hole_",
    "loc": ":0:0-0:0",
    "kind": { "type": "Todo" },
  })
  @json.inspect(@ast.Return(return_value=Some(unit_expr), loc=dummy_loc), content={
    "type": "Return",
    "return_value": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.Return(return_value=None, loc=dummy_loc), content={
    "type": "Return",
    "return_value": null,
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.Raise(err_value=unit_expr, loc=dummy_loc), content={
    "type": "Raise",
    "err_value": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "loc": ":0:0-0:0",
  })
  @json.inspect(
    @ast.Break(arg=Some(unit_expr), label=Some(label), loc=dummy_loc),
    content={
      "type": "Break",
      "arg": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "label": { "type": "Label", "name": "test_label", "loc": ":0:0-0:0" },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Continue(args=@list.of([unit_expr]), label=None, loc=dummy_loc),
    content={
      "type": "Continue",
      "args": [{ "type": "Unit_", "loc": ":0:0-0:0", "faked": false }],
      "label": null,
      "loc": ":0:0-0:0",
    },
  )
}

///|
test "expr complex types to_json" {
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  let any_pattern = @ast.Any(loc=dummy_loc)
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
      "type": "If",
      "cond": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "ifso": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "ifnot": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "loc": ":0:0-0:0",
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
      "type": "Guard",
      "cond": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "otherwise": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "body": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(@ast.Is(expr=unit_expr, pat=any_pattern, loc=dummy_loc), content={
    "type": "Is",
    "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "pat": { "type": "Any", "loc": ":0:0-0:0" },
    "loc": ":0:0-0:0",
  })
  @json.inspect(
    @ast.Let(pattern=any_pattern, expr=unit_expr, body=unit_expr, loc=dummy_loc),
    content={
      "type": "Let",
      "pattern": { "type": "Any", "loc": ":0:0-0:0" },
      "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "body": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "loc": ":0:0-0:0",
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
      "type": "LetMut",
      "binder": { "type": "Binder", "name": "test_binder", "loc": ":0:0-0:0" },
      "ty": {
        "type": "TName",
        "0": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": ":0:0-0:0",
        },
        "1": [],
        "loc": ":0:0-0:0",
      },
      "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "body": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Sequence(
      exprs=@list.of([unit_expr]),
      last_expr=unit_expr,
      loc=dummy_loc,
    ),
    content={
      "type": "Sequence",
      "exprs": [{ "type": "Unit_", "loc": ":0:0-0:0", "faked": false }],
      "last_expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Expr::Record(
      type_name=Some(type_name),
      fields=@list.of([field_def]),
      trailing=@ast.TrailingComma,
      loc=dummy_loc,
    ),
    content={
      "type": "Record",
      "type_name": {
        "type": "TypeName",
        "name": { "type": "Ident", "name": "TestType" },
        "is_object": false,
        "loc": ":0:0-0:0",
      },
      "fields": [
        {
          "type": "FieldDef",
          "label": { "type": "Label", "name": "test_label", "loc": ":0:0-0:0" },
          "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
          "is_pun": false,
          "loc": ":0:0-0:0",
        },
      ],
      "trailing": { "type": "TrailingComma" },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Field(
      record=unit_expr,
      accessor=@ast.Accessor::Label(label),
      loc=dummy_loc,
    ),
    content={
      "type": "Field",
      "record": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "accessor": {
        "type": "Label",
        "0": { "type": "Label", "name": "test_label", "loc": ":0:0-0:0" },
      },
      "loc": ":0:0-0:0",
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
      "type": "Match",
      "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "cases": [
        {
          "type": "Case",
          "pattern": { "type": "Any", "loc": ":0:0-0:0" },
          "body": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
        },
      ],
      "match_loc": ":0:0-0:0",
      "using_": { "type": "Label", "name": "test_label", "loc": ":0:0-0:0" },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(@ast.Pipe(lhs=unit_expr, rhs=unit_expr, loc=dummy_loc), content={
    "type": "Pipe",
    "lhs": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "rhs": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    "loc": ":0:0-0:0",
  })
  @json.inspect(
    @ast.Assign(
      var_=variable,
      expr=unit_expr,
      augmented_by=Some(variable),
      loc=dummy_loc,
    ),
    content={
      "type": "Assign",
      "var_": {
        "type": "Var",
        "name": { "type": "Ident", "name": "test_var" },
        "loc": ":0:0-0:0",
      },
      "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "augmented_by": {
        "type": "Var",
        "name": { "type": "Ident", "name": "test_var" },
        "loc": ":0:0-0:0",
      },
      "loc": ":0:0-0:0",
    },
  )
}

///|
test "pattern to_json" {
  let binder = @ast.Binder::{ name: "test_binder", loc: dummy_loc }
  let any_pattern = @ast.Any(loc=dummy_loc)
  let int_constant = @ast.Int("42")
  let constant_pattern = @ast.Pattern::Constant(c=int_constant, loc=dummy_loc)
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
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
  let array_patterns = @ast.Closed(@list.of([]))
  @json.inspect(@ast.Any(loc=dummy_loc), content={
    "type": "Any",
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.Var(binder), content={
    "type": "Var",
    "0": { "type": "Binder", "name": "test_binder", "loc": ":0:0-0:0" },
  })
  @json.inspect(
    @ast.Pattern::Alias(pat=any_pattern, alias_=binder, loc=dummy_loc),
    content={
      "type": "Alias",
      "pat": { "type": "Any", "loc": ":0:0-0:0" },
      "alias_": { "type": "Binder", "name": "test_binder", "loc": ":0:0-0:0" },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(@ast.Pattern::Array_(pats=array_patterns, loc=dummy_loc), content={
    "type": "Array_",
    "pats": { "type": "Closed", "0": [] },
    "loc": ":0:0-0:0",
  })
  @json.inspect(@ast.Expr::Constant(c=int_constant, loc=dummy_loc), content={
    "type": "Constant",
    "c": { "type": "Int", "0": "42" },
    "loc": ":0:0-0:0",
  })
  @json.inspect(
    @ast.Pattern::Constraint(pat=any_pattern, ty=int_type, loc=dummy_loc),
    content={
      "type": "Constraint",
      "pat": { "type": "Any", "loc": ":0:0-0:0" },
      "ty": {
        "type": "TName",
        "0": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": ":0:0-0:0",
        },
        "1": [],
        "loc": ":0:0-0:0",
      },
      "loc": ":0:0-0:0",
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
      "type": "Constr",
      "constr": {
        "type": "Constructor",
        "name": {
          "type": "ConstrName",
          "name": "TestConstructor",
          "loc": ":0:0-0:0",
        },
        "extra_info": { "type": "NoExtraInfo" },
        "loc": ":0:0-0:0",
      },
      "args": [
        {
          "type": "ConstrPatArg",
          "pat": { "type": "Any", "loc": ":0:0-0:0" },
          "kind": { "type": "Positional" },
        },
      ],
      "is_open": false,
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Pattern::Constr(constr~, args=None, is_open=true, loc=dummy_loc),
    content={
      "type": "Constr",
      "constr": {
        "type": "Constructor",
        "name": {
          "type": "ConstrName",
          "name": "TestConstructor",
          "loc": ":0:0-0:0",
        },
        "extra_info": { "type": "NoExtraInfo" },
        "loc": ":0:0-0:0",
      },
      "args": null,
      "is_open": true,
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Or(pat1=any_pattern, pat2=constant_pattern, loc=dummy_loc),
    content={
      "type": "Or",
      "pat1": { "type": "Any", "loc": ":0:0-0:0" },
      "pat2": {
        "type": "Constant",
        "c": { "type": "Int", "0": "42" },
        "loc": ":0:0-0:0",
      },
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Pattern::Tuple(
      pats=@list.of([any_pattern, constant_pattern]),
      loc=dummy_loc,
    ),
    content={
      "type": "Tuple",
      "pats": [
        { "type": "Any", "loc": ":0:0-0:0" },
        {
          "type": "Constant",
          "c": { "type": "Int", "0": "42" },
          "loc": ":0:0-0:0",
        },
      ],
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Pattern::Record(
      fields=@list.of([field_pat]),
      is_closed=true,
      loc=dummy_loc,
    ),
    content={
      "type": "Record",
      "fields": [
        {
          "type": "FieldPat",
          "label": { "type": "Label", "name": "test_field", "loc": ":0:0-0:0" },
          "pattern": { "type": "Any", "loc": ":0:0-0:0" },
          "is_pun": false,
          "loc": ":0:0-0:0",
        },
      ],
      "is_closed": true,
      "loc": ":0:0-0:0",
    },
  )
  @json.inspect(
    @ast.Pattern::Map(
      elems=@list.of([map_elem]),
      is_closed=false,
      loc=dummy_loc,
    ),
    content={
      "type": "Map",
      "elems": [
        {
          "type": "MapPatElem",
          "key": { "type": "Int", "0": "42" },
          "pat": { "type": "Any", "loc": ":0:0-0:0" },
          "match_absent": false,
          "key_loc": ":0:0-0:0",
          "loc": ":0:0-0:0",
        },
      ],
      "is_closed": false,
      "loc": ":0:0-0:0",
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
      "type": "Range",
      "lhs": { "type": "Any", "loc": ":0:0-0:0" },
      "rhs": {
        "type": "Constant",
        "c": { "type": "Int", "0": "42" },
        "loc": ":0:0-0:0",
      },
      "inclusive": true,
      "loc": ":0:0-0:0",
    },
  )
}

///|
test "field_name to_json" {
  let field_name = @ast.FieldName::{ name: "test_field", loc: dummy_loc }
  @json.inspect(field_name, content={
    "type": "FieldName",
    "name": "test_field",
    "loc": ":0:0-0:0",
  })
}

///|
test "field_decl to_json" {
  let field_name = @ast.FieldName::{ name: "test_field", loc: dummy_loc }
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  let field_decl = @ast.FieldDecl::{
    name: field_name,
    type_: int_type,
    mut_: false,
    vis: @ast.Default,
    loc: dummy_loc,
  }
  @json.inspect(field_decl, content={
    "type": "FieldDecl",
    "name": { "type": "FieldName", "name": "test_field", "loc": ":0:0-0:0" },
    "type_": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
    "mut_": false,
    "vis": { "type": "Default" },
    "loc": ":0:0-0:0",
  })
}

///|
test "constr_param to_json" {
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  let param_with_label = @ast.ConstrParam::{
    type_: int_type,
    mut_: false,
    label: Some(label),
  }
  let param_without_label = @ast.ConstrParam::{
    type_: int_type,
    mut_: true,
    label: None,
  }
  @json.inspect(param_with_label, content={
    "type": "ConstrParam",
    "type_": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
    "mut_": false,
    "label": { "type": "Label", "name": "test_label", "loc": ":0:0-0:0" },
  })
  @json.inspect(param_without_label, content={
    "type": "ConstrParam",
    "type_": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
    "mut_": true,
  })
}

///|
test "constr_decl to_json" {
  let constr_name = @ast.ConstrName::{ name: "TestConstructor", loc: dummy_loc }
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
    loc=dummy_loc,
  )
  let label = @ast.Label::{ name: "test_label", loc: dummy_loc }
  let param = @ast.ConstrParam::{
    type_: int_type,
    mut_: false,
    label: Some(label),
  }
  let constr_with_args = @ast.ConstrDecl::{
    name: constr_name,
    args: Some(@list.of([param])),
    tag: Some(("tag_value", dummy_loc)),
    loc: dummy_loc,
  }
  let constr_without_args = @ast.ConstrDecl::{
    name: constr_name,
    args: None,
    tag: None,
    loc: dummy_loc,
  }
  @json.inspect(constr_with_args, content={
    "type": "ConstrDecl",
    "name": {
      "type": "ConstrName",
      "name": "TestConstructor",
      "loc": ":0:0-0:0",
    },
    "args": [
      {
        "type": "ConstrParam",
        "type_": {
          "type": "TName",
          "0": {
            "type": "ConstrId",
            "id": { "type": "Ident", "name": "Int" },
            "loc": ":0:0-0:0",
          },
          "1": [],
          "loc": ":0:0-0:0",
        },
        "mut_": false,
        "label": { "type": "Label", "name": "test_label", "loc": ":0:0-0:0" },
      },
    ],
    "tag": ["tag_value", ":0:0-0:0"],
    "loc": ":0:0-0:0",
  })
  @json.inspect(constr_without_args, content={
    "type": "ConstrDecl",
    "name": {
      "type": "ConstrName",
      "name": "TestConstructor",
      "loc": ":0:0-0:0",
    },
    "loc": ":0:0-0:0",
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
    "loc": ":0:0-0:0",
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
        "loc": ":0:0-0:0",
      },
    ],
    "loc": ":0:0-0:0",
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
    "loc": ":0:0-0:0",
  })
  @json.inspect(binder_without_name, content={
    "type": "TypeDeclBinder",
    "loc": ":0:0-0:0",
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
    "binder": { "type": "Binder", "name": "test_binder", "loc": ":0:0-0:0" },
    "target": { "type": "Label", "name": "target_label", "loc": ":0:0-0:0" },
  })
  @json.inspect(alias_without_target, content={
    "type": "AliasTarget",
    "binder": { "type": "Binder", "name": "test_binder", "loc": ":0:0-0:0" },
  })
}

///|
test "field_pat to_json" {
  let label = @ast.Label::{ name: "field_label", loc: dummy_loc }
  let any_pattern = @ast.Any(loc=dummy_loc)
  let field_pat = @ast.FieldPat::{
    label,
    pattern: any_pattern,
    is_pun: true,
    loc: dummy_loc,
  }
  @json.inspect(field_pat, content={
    "type": "FieldPat",
    "label": { "type": "Label", "name": "field_label", "loc": ":0:0-0:0" },
    "pattern": { "type": "Any", "loc": ":0:0-0:0" },
    "is_pun": true,
    "loc": ":0:0-0:0",
  })
}

///|
test "constr_pat_arg to_json" {
  let any_pattern = @ast.Any(loc=dummy_loc)
  let label = @ast.Label::{ name: "arg_label", loc: dummy_loc }
  let arg = @ast.ConstrPatArg::{
    pat: any_pattern,
    kind: @ast.ArgumentKind::Labelled(label),
  }
  @json.inspect(arg, content={
    "type": "ConstrPatArg",
    "pat": { "type": "Any", "loc": ":0:0-0:0" },
    "kind": {
      "type": "Labelled",
      "0": { "type": "Label", "name": "arg_label", "loc": ":0:0-0:0" },
    },
  })
}

///|
test "map_pat_elem to_json" {
  let int_constant = @ast.Int("42")
  let any_pattern = @ast.Any(loc=dummy_loc)
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
    "pat": { "type": "Any", "loc": ":0:0-0:0" },
    "match_absent": true,
    "key_loc": ":0:0-0:0",
    "loc": ":0:0-0:0",
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
    "tycon_loc": ":0:0-0:0",
    "components": { "type": "Abstract" },
    "deriving": [
      {
        "type": "DerivingDirective",
        "type_name": {
          "type": "TypeName",
          "name": { "type": "Ident", "name": "Show" },
          "is_object": false,
          "loc": ":0:0-0:0",
        },
        "args": [],
        "loc": ":0:0-0:0",
      },
    ],
  })
}

///|
test "deriving_directive to_json" {
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  let argument = @ast.Argument::{ value: unit_expr, kind: @ast.Positional }
  let deriving_directive = @ast.DerivingDirective::{
    type_name: @ast.TypeName::{
      name: @ast.Qualified(pkg="std", name="Show"),
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
      "name": { "type": "Qualified", "pkg": "std", "name": "Show" },
      "is_object": true,
      "loc": ":0:0-0:0",
    },
    "args": [
      {
        "type": "Argument",
        "value": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
        "kind": { "type": "Positional" },
      },
    ],
    "loc": ":0:0-0:0",
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
    deprecated_type_bang: false,
  }
  @json.inspect(type_decl, content={
    "type": "TypeDecl",
    "tycon": "TestType",
    "tycon_loc": ":0:0-0:0",
    "params": [{ "type": "TypeDeclBinder", "name": "T", "loc": ":0:0-0:0" }],
    "components": { "type": "Extern" },
    "attrs": [{ "loc": ":0:0-0:0", "raw": "test_attr" }],
    "doc": { "content": ["Test documentation"], "loc": ":0:0-0:0" },
    "type_vis": { "type": "Default" },
    "deriving": [],
    "loc": ":0:0-0:0",
    "deprecated_type_bang": false,
  })
}

///|
test "decl_body to_json" {
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
  let local_type_decl = @ast.LocalTypeDecl::{
    tycon: "LocalType",
    tycon_loc: dummy_loc,
    components: @ast.Abstract,
    deriving: @list.of([]),
  }
  @json.inspect(
    @ast.DeclBody(local_types=@list.of([local_type_decl]), expr=unit_expr),
    content={
      "type": "DeclBody",
      "local_types": [
        {
          "type": "LocalTypeDecl",
          "tycon": "LocalType",
          "tycon_loc": ":0:0-0:0",
          "components": { "type": "Abstract" },
          "deriving": [],
        },
      ],
      "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
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
      "type": "DeclStubs",
      "0": {
        "type": "Import",
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
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
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
      "loc": ":0:0-0:0",
    },
    "name": { "type": "Binder", "name": "test_func", "loc": ":0:0-0:0" },
    "has_error": ":0:0-0:0",
    "is_async": true,
    "decl_params": [
      {
        "type": "Positional",
        "binder": { "type": "Binder", "name": "test_func", "loc": ":0:0-0:0" },
        "ty": {
          "type": "TName",
          "0": {
            "type": "ConstrId",
            "id": { "type": "Ident", "name": "Int" },
            "loc": ":0:0-0:0",
          },
          "1": [],
          "loc": ":0:0-0:0",
        },
      },
    ],
    "params_loc": ":0:0-0:0",
    "quantifiers": [
      {
        "type": "TypeVarBinder",
        "name": "T",
        "constraints": [
          {
            "type": "TypeVarConstraint",
            "trait_": { "type": "Ident", "name": "Show" },
            "loc": ":0:0-0:0",
          },
        ],
        "loc": ":0:0-0:0",
      },
    ],
    "return_type": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
    "error_type": { "type": "NoErrorType" },
    "vis": { "type": "Priv", "loc": ":0:0-0:0" },
    "attrs": [{ "loc": ":0:0-0:0", "raw": "test_attr" }],
    "doc": { "content": ["Test documentation"], "loc": ":0:0-0:0" },
  })
}

///|
test "trait_method_decl to_json" {
  let binder = @ast.Binder::{ name: "trait_method", loc: dummy_loc }
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
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
    error_type: @ast.DefaultErrorType(dummy_loc, is_old_syntax=false),
    has_default: Some(dummy_loc),
    loc: dummy_loc,
  }
  @json.inspect(trait_method, content={
    "type": "TraitMethodDecl",
    "name": { "type": "Binder", "name": "trait_method", "loc": ":0:0-0:0" },
    "is_async": false,
    "quantifiers": [
      {
        "type": "TypeVarBinder",
        "name": "T",
        "constraints": [],
        "loc": ":0:0-0:0",
      },
    ],
    "params": [
      {
        "type": "Positional",
        "binder": {
          "type": "Binder",
          "name": "trait_method",
          "loc": ":0:0-0:0",
        },
        "ty": {
          "type": "TName",
          "0": {
            "type": "ConstrId",
            "id": { "type": "Ident", "name": "Int" },
            "loc": ":0:0-0:0",
          },
          "1": [],
          "loc": ":0:0-0:0",
        },
      },
    ],
    "return_type": {
      "type": "TName",
      "0": {
        "type": "ConstrId",
        "id": { "type": "Ident", "name": "Int" },
        "loc": ":0:0-0:0",
      },
      "1": [],
      "loc": ":0:0-0:0",
    },
    "error_type": {
      "type": "DefaultErrorType",
      "0": ":0:0-0:0",
      "is_old_syntax": false,
    },
    "has_default": ":0:0-0:0",
    "loc": ":0:0-0:0",
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
    trait_name: trait_binder,
    trait_supers: @list.of([super_constraint]),
    trait_methods: @list.of([trait_method]),
    trait_vis: @ast.Pub(attr=None, loc=dummy_loc),
    trait_loc: dummy_loc,
    trait_attrs: @list.of([dummy_attribute]),
    trait_doc: dummy_docstring,
  }
  @json.inspect(trait_decl, content={
    "type": "TraitDecl",
    "trait_name": { "type": "Binder", "name": "TestTrait", "loc": ":0:0-0:0" },
    "trait_supers": [
      {
        "type": "TypeVarConstraint",
        "trait_": { "type": "Ident", "name": "SuperTrait" },
        "loc": ":0:0-0:0",
      },
    ],
    "trait_methods": [
      {
        "type": "TraitMethodDecl",
        "name": { "type": "Binder", "name": "method", "loc": ":0:0-0:0" },
        "is_async": false,
        "quantifiers": [],
        "params": [],
        "error_type": { "type": "NoErrorType" },
        "loc": ":0:0-0:0",
      },
    ],
    "trait_vis": { "type": "Pub", "attr": null, "loc": ":0:0-0:0" },
    "trait_loc": ":0:0-0:0",
    "trait_attrs": [{ "loc": ":0:0-0:0", "raw": "test_attr" }],
    "trait_doc": { "content": ["Test documentation"], "loc": ":0:0-0:0" },
  })
}

///|
test "impl variants to_json" {
  let unit_expr = @ast.Unit_(loc=dummy_loc, faked=false)
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
    deprecated_type_bang: false,
  }
  let binder = @ast.Binder::{ name: "test", loc: dummy_loc }
  let int_type = @ast.TName(
    @ast.ConstrId::{ id: @ast.LongIdent::Ident(name="Int"), loc: dummy_loc },
    @list.of([]),
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
      "type": "TopExpr",
      "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "is_main": true,
      "local_types": [],
      "loc": ":0:0-0:0",
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
      "type": "TopTest",
      "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "name": [["test_string", ":0:0-0:0"]],
      "params": null,
      "local_types": [],
      "loc": ":0:0-0:0",
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": ":0:0-0:0" },
    },
  )
  @json.inspect(@ast.TopTypeDef(type_decl), content={
    "type": "TopTypeDef",
    "0": {
      "type": "TypeDecl",
      "tycon": "TestType",
      "tycon_loc": ":0:0-0:0",
      "params": [],
      "components": { "type": "Abstract" },
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": ":0:0-0:0" },
      "type_vis": { "type": "Default" },
      "deriving": [],
      "loc": ":0:0-0:0",
      "deprecated_type_bang": false,
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
    "type": "TopFuncDef",
    "fun_decl": {
      "type": "FunDecl",
      "name": { "type": "Binder", "name": "test", "loc": ":0:0-0:0" },
      "is_async": false,
      "params_loc": ":0:0-0:0",
      "quantifiers": [],
      "error_type": { "type": "NoErrorType" },
      "vis": { "type": "Default" },
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": ":0:0-0:0" },
    },
    "decl_body": {
      "type": "DeclBody",
      "local_types": [],
      "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
    },
    "loc": ":0:0-0:0",
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
      "type": "TopLetDef",
      "binder": { "type": "Binder", "name": "test", "loc": ":0:0-0:0" },
      "ty": {
        "type": "TName",
        "0": {
          "type": "ConstrId",
          "id": { "type": "Ident", "name": "Int" },
          "loc": ":0:0-0:0",
        },
        "1": [],
        "loc": ":0:0-0:0",
      },
      "expr": { "type": "Unit_", "loc": ":0:0-0:0", "faked": false },
      "vis": { "type": "Default" },
      "is_constant": true,
      "loc": ":0:0-0:0",
      "attrs": [],
      "doc": { "content": ["Test documentation"], "loc": ":0:0-0:0" },
    },
  )
}
