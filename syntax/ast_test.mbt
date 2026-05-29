///|
let dummy_loc : @basic.Location = {
  let pos = @basic.Position::{ fname: "", lnum: 0, bol: 0, cnum: 0 }
  { start: pos, end: pos }
}

///|
let null : Json = Json::null()

///|
let dummy_docstring : @syntax.DocString = {
  content: @list.from_array(["Test documentation"]),
  loc: dummy_loc,
}

///|
let dummy_string_literal : @tokens.StringLiteral = "test_string"

///|
test "impl variants to_json" {
  let unit_expr = @syntax.Expr::Unit(
    loc=dummy_loc,
    faked=false,
    lparen_loc=dummy_loc,
    rparen_loc=dummy_loc,
  )
  let type_decl = @syntax.TypeDecl::{
    tycon: "TestType",
    tycon_loc: dummy_loc,
    params: @list.from_array([]),
    components: @syntax.Abstract,
    attrs: @list.from_array([]),
    doc: dummy_docstring,
    type_vis: @syntax.Default,
    deriving: @list.from_array([]),
    declare_loc: None,
    keyword_loc: dummy_loc,
    params_lbracket_loc: None,
    params_rbracket_loc: None,
    param_comma_locs: @list.empty(),
    derive_loc: None,
    derive_lparen_loc: None,
    derive_rparen_loc: None,
    derive_comma_locs: @list.empty(),
    loc: dummy_loc,
    is_declare: false,
  }
  let binder = @syntax.Binder::{ name: "test", loc: dummy_loc }
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.from_array([]),
    type_args_lbracket_loc=None,
    type_args_rbracket_loc=None,
    type_arg_comma_locs=@list.empty(),
    loc=dummy_loc,
  )
  json_inspect(
    @syntax.TopExpr(expr=unit_expr, is_main=true, is_async=None, loc=dummy_loc).json_repr(),
    content={
      "kind": "Impl::TopExpr",
      "loc": null,
      "children": {
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "is_main": true,
        "local_types": {
          "kind": "Impl::TopExpr::LocalTypeList",
          "loc": null,
          "children": [],
        },
        "is_async": null,
      },
    },
  )
  json_inspect(
    @syntax.TopTest(
      expr=unit_expr,
      name=Some((dummy_string_literal, dummy_loc)),
      params=None,
      is_async=None,
      test_loc=dummy_loc,
      params_lparen_loc=None,
      params_rparen_loc=None,
      param_comma_locs=@list.from_array([]),
      body_lbrace_loc=dummy_loc,
      body_rbrace_loc=dummy_loc,
      loc=dummy_loc,
      attrs=@list.from_array([]),
      doc=dummy_docstring,
    ).json_repr(),
    content={
      "kind": "Impl::TopTest",
      "loc": null,
      "children": {
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "name": {
          "kind": "TestName",
          "loc": null,
          "children": { "value": "test_string" },
        },
        "params": null,
        "local_types": {
          "kind": "Impl::TopTest::LocalTypeList",
          "loc": null,
          "children": [],
        },
        "is_async": null,
        "attrs": {
          "kind": "Impl::TopTest::AttrList",
          "loc": null,
          "children": [],
        },
        "doc": "Test documentation",
      },
    },
  )
  json_inspect(@syntax.TopTypeDef(type_decl).json_repr(), content={
    "kind": "Impl::TopTypeDef",
    "loc": null,
    "children": {
      "value": {
        "kind": "TypeDecl",
        "loc": null,
        "children": {
          "tycon": "TestType",
          "params": {
            "kind": "TypeDecl::ParamList",
            "loc": null,
            "children": [],
          },
          "components": {
            "kind": "TypeDesc::Abstract",
            "loc": null,
            "children": {},
          },
          "attrs": { "kind": "TypeDecl::AttrList", "loc": null, "children": [] },
          "doc": "Test documentation",
          "type_vis": {
            "kind": "Visibility::Default",
            "loc": null,
            "children": {},
          },
          "is_declare": false,
          "deriving": {
            "kind": "TypeDecl::DerivingList",
            "loc": null,
            "children": [],
          },
        },
      },
    },
  })
  let fun_decl = @syntax.FunDecl::{
    type_name: None,
    name: binder,
    is_async: None,
    declare_loc: None,
    extern_loc: None,
    fn_loc: dummy_loc,
    coloncolon_loc: None,
    decl_params: None,
    params_loc: dummy_loc,
    type_params_lbracket_loc: None,
    type_params_rbracket_loc: None,
    type_param_comma_locs: @list.from_array([]),
    params_lparen_loc: None,
    params_rparen_loc: None,
    param_comma_locs: @list.from_array([]),
    quantifiers: @list.from_array([]),
    return_type: None,
    return_arrow_loc: None,
    error_type: @syntax.NoErrorType,
    vis: @syntax.Default,
    attrs: @list.from_array([]),
    doc: dummy_docstring,
    loc: dummy_loc,
  }
  let decl_body = @syntax.DeclBody(
    expr=unit_expr,
    lbrace_loc=None,
    rbrace_loc=None,
  )
  json_inspect(
    @syntax.TopFuncDef(
      fun_decl~,
      decl_body~,
      where_clause=None,
      equal_loc=None,
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Impl::TopFuncDef",
      "loc": null,
      "children": {
        "fun_decl": {
          "kind": "FunDecl",
          "loc": null,
          "children": {
            "type_name": null,
            "name": {
              "kind": "Binder",
              "loc": null,
              "children": { "name": "test" },
            },
            "has_error": null,
            "is_async": null,
            "decl_params": null,
            "quantifiers": {
              "kind": "FunDecl::QuantifierList",
              "loc": null,
              "children": [],
            },
            "return_type": null,
            "error_type": {
              "kind": "ErrorType::NoErrorType",
              "loc": null,
              "children": {},
            },
            "vis": {
              "kind": "Visibility::Default",
              "loc": null,
              "children": {},
            },
            "attrs": {
              "kind": "FunDecl::AttrList",
              "loc": null,
              "children": [],
            },
            "doc": "Test documentation",
          },
        },
        "where_clause": null,
        "decl_body": {
          "kind": "DeclBody::DeclBody",
          "loc": null,
          "children": {
            "local_types": {
              "kind": "DeclBody::DeclBody::LocalTypeList",
              "loc": null,
              "children": [],
            },
            "expr": {
              "kind": "Expr::Unit",
              "loc": null,
              "children": { "faked": false },
            },
          },
        },
      },
    },
  )
  json_inspect(
    @syntax.TopLetDef(
      binder~,
      ty=Some(int_type),
      expr=unit_expr,
      vis=@syntax.Default,
      is_constant=true,
      declare_loc=None,
      let_or_const_loc=dummy_loc,
      annot_colon_loc=Some(dummy_loc),
      equal_loc=dummy_loc,
      loc=dummy_loc,
      attrs=@list.from_array([]),
      doc=dummy_docstring,
      is_declare=false,
    ).json_repr(),
    content={
      "kind": "Impl::TopLetDef",
      "loc": null,
      "children": {
        "binder": {
          "kind": "Binder",
          "loc": null,
          "children": { "name": "test" },
        },
        "ty": {
          "kind": "Type::Name",
          "loc": null,
          "children": {
            "constr_id": {
              "kind": "ConstrId",
              "loc": null,
              "children": {
                "id": {
                  "kind": "LongIdent::Ident",
                  "loc": null,
                  "children": { "value": "Int" },
                },
              },
            },
            "tys": {
              "kind": "Type::Name::TypeList",
              "loc": null,
              "children": [],
            },
          },
        },
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "vis": { "kind": "Visibility::Default", "loc": null, "children": {} },
        "is_constant": true,
        "is_declare": false,
        "attrs": {
          "kind": "Impl::TopLetDef::AttrList",
          "loc": null,
          "children": [],
        },
        "doc": "Test documentation",
      },
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
    enumview_loc=dummy_loc,
    type_params_lbracket_loc=None,
    type_params_rbracket_loc=None,
    type_param_comma_locs=@list.empty(),
    lbrace_loc=dummy_loc,
    rbrace_loc=dummy_loc,
    constructor_semi_locs=@list.empty(),
    for_loc=dummy_loc,
    with_loc=dummy_loc,
    params_lparen_loc=dummy_loc,
    params_rparen_loc=dummy_loc,
    param_comma_locs=@list.empty(),
    body_lbrace_loc=dummy_loc,
    body_rbrace_loc=dummy_loc,
    attrs=@list.empty(),
    doc=dummy_docstring,
    loc=dummy_loc,
  )
  json_inspect(view.json_repr(), content={
    "kind": "Impl::TopView",
    "loc": null,
    "children": {
      "quantifiers": {
        "kind": "Impl::TopView::QuantifierList",
        "loc": null,
        "children": [],
      },
      "source_ty": {
        "kind": "Type::Name",
        "loc": null,
        "children": {
          "constr_id": {
            "kind": "ConstrId",
            "loc": null,
            "children": {
              "id": {
                "kind": "LongIdent::Ident",
                "loc": null,
                "children": { "value": "Int" },
              },
            },
          },
          "tys": { "kind": "Type::Name::TypeList", "loc": null, "children": [] },
        },
      },
      "view_constrs": {
        "kind": "Impl::TopView::ConstrList",
        "loc": null,
        "children": [],
      },
      "body": {
        "kind": "Expr::Unit",
        "loc": null,
        "children": { "faked": false },
      },
      "ty_name": "TestViewType",
      "parameters": {
        "kind": "Impl::TopView::ParameterList",
        "loc": null,
        "children": [],
      },
      "func_name": {
        "kind": "Binder",
        "loc": null,
        "children": { "name": "test" },
      },
      "vis": { "kind": "Visibility::Default", "loc": null, "children": {} },
      "attrs": {
        "kind": "Impl::TopView::AttrList",
        "loc": null,
        "children": [],
      },
      "doc": "Test documentation",
    },
  })
}

///|
test "expr basic types to_json" {
  let unit_expr = @syntax.Expr::Unit(
    loc=dummy_loc,
    faked=false,
    lparen_loc=dummy_loc,
    rparen_loc=dummy_loc,
  )
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
  json_inspect(
    @syntax.Expr::Unit(
      loc=dummy_loc,
      faked=false,
      lparen_loc=dummy_loc,
      rparen_loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Unit",
      "loc": null,
      "children": { "faked": false },
    },
  )
  json_inspect(
    @syntax.Expr::Constant(c=int_constant, loc=dummy_loc).json_repr(),
    content={
      "kind": "Expr::Constant",
      "loc": null,
      "children": {
        "constant": {
          "kind": "Constant::Int",
          "loc": null,
          "children": { "value": "42" },
        },
      },
    },
  )
  json_inspect(@syntax.Expr::Ident(id=variable, loc=dummy_loc).json_repr(), content={
    "kind": "Expr::Ident",
    "loc": null,
    "children": {
      "id": {
        "kind": "Var",
        "loc": null,
        "children": {
          "name": {
            "kind": "LongIdent::Ident",
            "loc": null,
            "children": { "value": "test_var" },
          },
        },
      },
    },
  })
  json_inspect(
    @syntax.Expr::Apply(
      func=unit_expr,
      args=@list.from_array([argument]),
      lparen_loc=dummy_loc,
      rparen_loc=dummy_loc,
      comma_locs=@list.from_array([]),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Apply",
      "loc": null,
      "children": {
        "func": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "args": {
          "kind": "Expr::Apply::ArgumentList",
          "loc": null,
          "children": [
            {
              "kind": "Argument",
              "loc": null,
              "children": {
                "value": {
                  "kind": "Expr::Unit",
                  "loc": null,
                  "children": { "faked": false },
                },
                "kind": {
                  "kind": "ArgumentKind::Positional",
                  "loc": null,
                  "children": {},
                },
              },
            },
          ],
        },
        "attr": { "kind": "ApplyAttr::NoAttr", "loc": null, "children": {} },
      },
    },
  )
  json_inspect(
    @syntax.Infix(op=variable, lhs=unit_expr, rhs=unit_expr, loc=dummy_loc).json_repr(),
    content={
      "kind": "Expr::Infix",
      "loc": null,
      "children": {
        "op": {
          "kind": "Var",
          "loc": null,
          "children": {
            "name": {
              "kind": "LongIdent::Ident",
              "loc": null,
              "children": { "value": "test_var" },
            },
          },
        },
        "lhs": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "rhs": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Unary(op=variable, expr=unit_expr, loc=dummy_loc).json_repr(),
    content={
      "kind": "Expr::Unary",
      "loc": null,
      "children": {
        "op": {
          "kind": "Var",
          "loc": null,
          "children": {
            "name": {
              "kind": "LongIdent::Ident",
              "loc": null,
              "children": { "value": "test_var" },
            },
          },
        },
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Expr::Array(
      exprs=@list.from_array([unit_expr]),
      lbracket_loc=dummy_loc,
      rbracket_loc=dummy_loc,
      comma_locs=@list.from_array([]),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Array",
      "loc": null,
      "children": {
        "exprs": {
          "kind": "Expr::Array::ExprList",
          "loc": null,
          "children": [
            {
              "kind": "Expr::Unit",
              "loc": null,
              "children": { "faked": false },
            },
          ],
        },
      },
    },
  )
  let spread_elem = @syntax.SpreadableElem::Regular(unit_expr)
  json_inspect(
    @syntax.ArraySpread(
      elems=@list.from_array([spread_elem]),
      lbracket_loc=dummy_loc,
      rbracket_loc=dummy_loc,
      comma_locs=@list.from_array([]),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::ArraySpread",
      "loc": null,
      "children": {
        "elems": {
          "kind": "Expr::ArraySpread::ElemList",
          "loc": null,
          "children": [
            {
              "kind": "SpreadableElem::Regular",
              "loc": null,
              "children": {
                "value": {
                  "kind": "Expr::Unit",
                  "loc": null,
                  "children": { "faked": false },
                },
              },
            },
          ],
        },
      },
    },
  )
  json_inspect(
    @syntax.ArrayGet(
      array=unit_expr,
      index=unit_expr,
      lbracket_loc=dummy_loc,
      rbracket_loc=dummy_loc,
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::ArrayGet",
      "loc": null,
      "children": {
        "array": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "index": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Expr::Tuple(
      exprs=@list.from_array([unit_expr, unit_expr]),
      lparen_loc=dummy_loc,
      rparen_loc=dummy_loc,
      comma_locs=@list.from_array([]),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Tuple",
      "loc": null,
      "children": {
        "exprs": {
          "kind": "Expr::Tuple::ExprList",
          "loc": null,
          "children": [
            {
              "kind": "Expr::Unit",
              "loc": null,
              "children": { "faked": false },
            },
            {
              "kind": "Expr::Unit",
              "loc": null,
              "children": { "faked": false },
            },
          ],
        },
      },
    },
  )
  json_inspect(
    @syntax.Expr::Hole(loc=dummy_loc, kind=@syntax.Todo).json_repr(),
    content={
      "kind": "Expr::Hole",
      "loc": null,
      "children": {
        "kind": { "kind": "Hole::Todo", "loc": null, "children": {} },
      },
    },
  )
  json_inspect(
    @syntax.Return(
      return_value=Some(unit_expr),
      return_loc=dummy_loc,
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Return",
      "loc": null,
      "children": {
        "return_value": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Return(return_value=None, return_loc=dummy_loc, loc=dummy_loc).json_repr(),
    content={
      "kind": "Expr::Return",
      "loc": null,
      "children": { "return_value": null },
    },
  )
  json_inspect(
    @syntax.Raise(err_value=unit_expr, raise_loc=dummy_loc, loc=dummy_loc).json_repr(),
    content={
      "kind": "Expr::Raise",
      "loc": null,
      "children": {
        "err_value": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Break(
      arg=Some(unit_expr),
      label=Some(label),
      break_loc=dummy_loc,
      post_label_loc=Some(dummy_loc),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Break",
      "loc": null,
      "children": {
        "arg": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "label": {
          "kind": "Label",
          "loc": null,
          "children": { "name": "test_label" },
        },
      },
    },
  )
  json_inspect(
    @syntax.Continue(
      args=@list.from_array([unit_expr]),
      label=None,
      continue_loc=dummy_loc,
      post_label_loc=None,
      comma_locs=@list.empty(),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Continue",
      "loc": null,
      "children": {
        "args": {
          "kind": "Expr::Continue::ExprList",
          "loc": null,
          "children": [
            {
              "kind": "Expr::Unit",
              "loc": null,
              "children": { "faked": false },
            },
          ],
        },
        "label": null,
      },
    },
  )
}

///|
test "expr complex types to_json" {
  let unit_expr = @syntax.Expr::Unit(
    loc=dummy_loc,
    faked=false,
    lparen_loc=dummy_loc,
    rparen_loc=dummy_loc,
  )
  let int_type = @syntax.Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Int"),
      loc: dummy_loc,
    },
    tys=@list.from_array([]),
    type_args_lbracket_loc=None,
    type_args_rbracket_loc=None,
    type_arg_comma_locs=@list.empty(),
    loc=dummy_loc,
  )
  let binder = @syntax.Binder::{ name: "test_binder", loc: dummy_loc }
  let label = @syntax.Label::{ name: "test_label", loc: dummy_loc }
  let any_pattern = @syntax.Pattern::Any(loc=dummy_loc)
  let case = @syntax.Case::{
    pattern: any_pattern,
    guard_: None,
    body: unit_expr,
    if_loc: None,
    fat_arrow_loc: None,
    ellipsis_loc: None,
  }
  let type_name = @syntax.TypeName::{
    name: @syntax.LongIdent::Ident(name="TestType"),
    is_object: false,
    object_amp_loc: None,
    loc: dummy_loc,
  }
  let field_def = @syntax.FieldDef::{
    label,
    expr: unit_expr,
    is_pun: false,
    colon_loc: None,
    loc: dummy_loc,
  }
  let variable = @syntax.Var::{
    name: @syntax.LongIdent::Ident(name="test_var"),
    loc: dummy_loc,
  }
  json_inspect(
    @syntax.If(
      cond=unit_expr,
      ifso=unit_expr,
      ifnot=Some(unit_expr),
      if_loc=dummy_loc,
      then_lbrace_loc=dummy_loc,
      then_rbrace_loc=dummy_loc,
      else_loc=Some(dummy_loc),
      else_lbrace_loc=Some(dummy_loc),
      else_rbrace_loc=Some(dummy_loc),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::If",
      "loc": null,
      "children": {
        "cond": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "ifso": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "ifnot": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Guard(
      cond=unit_expr,
      otherwise=Some(unit_expr),
      body=unit_expr,
      guard_loc=dummy_loc,
      else_loc=Some(dummy_loc),
      else_lbrace_loc=Some(dummy_loc),
      else_rbrace_loc=Some(dummy_loc),
      semi_loc=None,
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Guard",
      "loc": null,
      "children": {
        "cond": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "otherwise": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "body": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Defer(
      expr=unit_expr,
      body=unit_expr,
      defer_loc=dummy_loc,
      semi_loc=None,
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Defer",
      "loc": null,
      "children": {
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "body": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Is(expr=unit_expr, pat=any_pattern, is_loc=dummy_loc, loc=dummy_loc).json_repr(),
    content={
      "kind": "Expr::Is",
      "loc": null,
      "children": {
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "pat": { "kind": "Pattern::Any", "loc": null, "children": {} },
      },
    },
  )
  json_inspect(
    @syntax.Let(
      pattern=any_pattern,
      expr=unit_expr,
      body=unit_expr,
      let_loc=dummy_loc,
      annot_colon_loc=None,
      equal_loc=dummy_loc,
      semi_loc=None,
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Let",
      "loc": null,
      "children": {
        "pattern": { "kind": "Pattern::Any", "loc": null, "children": {} },
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "body": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.LetMut(
      binder~,
      ty=Some(int_type),
      expr=unit_expr,
      body=unit_expr,
      let_loc=dummy_loc,
      mut_loc=dummy_loc,
      annot_colon_loc=None,
      equal_loc=dummy_loc,
      semi_loc=None,
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::LetMut",
      "loc": null,
      "children": {
        "binder": {
          "kind": "Binder",
          "loc": null,
          "children": { "name": "test_binder" },
        },
        "ty": {
          "kind": "Type::Name",
          "loc": null,
          "children": {
            "constr_id": {
              "kind": "ConstrId",
              "loc": null,
              "children": {
                "id": {
                  "kind": "LongIdent::Ident",
                  "loc": null,
                  "children": { "value": "Int" },
                },
              },
            },
            "tys": {
              "kind": "Type::Name::TypeList",
              "loc": null,
              "children": [],
            },
          },
        },
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "body": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Expr::Sequence(
      exprs=@list.from_array([unit_expr]),
      last_expr=unit_expr,
      semi_locs=@list.empty(),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Sequence",
      "loc": null,
      "children": {
        "exprs": {
          "kind": "Expr::Sequence::ExprList",
          "loc": null,
          "children": [
            {
              "kind": "Expr::Unit",
              "loc": null,
              "children": { "faked": false },
            },
          ],
        },
        "last_expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Expr::Record(
      type_name=Some(type_name),
      fields=@list.from_array([field_def]),
      trailing=@syntax.TrailingMark::Comma,
      coloncolon_loc=Some(dummy_loc),
      lbrace_loc=dummy_loc,
      rbrace_loc=dummy_loc,
      comma_locs=@list.empty(),
      semi_locs=@list.empty(),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Record",
      "loc": null,
      "children": {
        "type_name": {
          "kind": "TypeName",
          "loc": null,
          "children": {
            "name": {
              "kind": "LongIdent::Ident",
              "loc": null,
              "children": { "value": "TestType" },
            },
            "is_object": false,
          },
        },
        "fields": {
          "kind": "Expr::Record::FieldList",
          "loc": null,
          "children": [
            {
              "kind": "FieldDef",
              "loc": null,
              "children": {
                "label": {
                  "kind": "Label",
                  "loc": null,
                  "children": { "name": "test_label" },
                },
                "expr": {
                  "kind": "Expr::Unit",
                  "loc": null,
                  "children": { "faked": false },
                },
                "is_pun": false,
              },
            },
          ],
        },
        "trailing": { "kind": "Trailing::Comma", "loc": null, "children": {} },
      },
    },
  )
  json_inspect(
    @syntax.Field(
      record=unit_expr,
      accessor=@syntax.Accessor::Label(label),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Field",
      "loc": null,
      "children": {
        "record": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "accessor": {
          "kind": "Accessor::Label",
          "loc": null,
          "children": {
            "value": {
              "kind": "Label",
              "loc": null,
              "children": { "name": "test_label" },
            },
          },
        },
      },
    },
  )
  json_inspect(
    @syntax.Expr::Match(
      expr=unit_expr,
      cases=@list.from_array([case]),
      match_loc=dummy_loc,
      lbrace_loc=dummy_loc,
      rbrace_loc=dummy_loc,
      semi_locs=@list.empty(),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Match",
      "loc": null,
      "children": {
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "cases": {
          "kind": "Expr::Match::CaseList",
          "loc": null,
          "children": [
            {
              "kind": "Case",
              "loc": null,
              "children": {
                "pattern": {
                  "kind": "Pattern::Any",
                  "loc": null,
                  "children": {},
                },
                "guard": null,
                "body": {
                  "kind": "Expr::Unit",
                  "loc": null,
                  "children": { "faked": false },
                },
              },
            },
          ],
        },
      },
    },
  )
  json_inspect(
    @syntax.Pipe(
      lhs=unit_expr,
      rhs=unit_expr,
      pipe_loc=dummy_loc,
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Pipe",
      "loc": null,
      "children": {
        "lhs": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "rhs": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
      },
    },
  )
  json_inspect(
    @syntax.Assign(
      var_=variable,
      expr=unit_expr,
      augmented_by=Some(variable),
      equal_loc=None,
      augmented_assign_loc=Some(dummy_loc),
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::Assign",
      "loc": null,
      "children": {
        "var": {
          "kind": "Var",
          "loc": null,
          "children": {
            "name": {
              "kind": "LongIdent::Ident",
              "loc": null,
              "children": { "value": "test_var" },
            },
          },
        },
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "augmented_by": {
          "kind": "Var",
          "loc": null,
          "children": {
            "name": {
              "kind": "LongIdent::Ident",
              "loc": null,
              "children": { "value": "test_var" },
            },
          },
        },
      },
    },
  )
}

///|
test "foreach json includes empty continue block" {
  let unit_expr = @syntax.Expr::Unit(
    loc=dummy_loc,
    faked=false,
    lparen_loc=dummy_loc,
    rparen_loc=dummy_loc,
  )
  json_inspect(
    @syntax.ForEach(
      binders=@list.from_array([]),
      expr=unit_expr,
      init=@list.from_array([]),
      continue_block=@list.from_array([]),
      body=unit_expr,
      else_block=None,
      where_clause=None,
      label=None,
      label_colon_loc=None,
      for_loc=dummy_loc,
      binder_comma_locs=@list.from_array([]),
      in_loc=dummy_loc,
      semi_locs=@list.from_array([]),
      init_comma_locs=@list.from_array([]),
      init_equal_locs=@list.from_array([]),
      continue_comma_locs=@list.from_array([]),
      continue_equal_locs=@list.from_array([]),
      body_lbrace_loc=dummy_loc,
      body_rbrace_loc=dummy_loc,
      nobreak_loc=None,
      nobreak_lbrace_loc=None,
      nobreak_rbrace_loc=None,
      loc=dummy_loc,
    ).json_repr(),
    content={
      "kind": "Expr::ForEach",
      "loc": null,
      "children": {
        "binders": {
          "kind": "Expr::ForEach::BinderList",
          "loc": null,
          "children": [],
        },
        "expr": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "init": { "kind": "Expr::ForEach::Init", "loc": null, "children": [] },
        "continue_block": {
          "kind": "Expr::ForEach::ContinueBlock",
          "loc": null,
          "children": [],
        },
        "body": {
          "kind": "Expr::Unit",
          "loc": null,
          "children": { "faked": false },
        },
        "else_block": null,
        "where_clause": null,
        "label": null,
      },
    },
  )
}
