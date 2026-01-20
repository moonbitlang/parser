///|
fn simple_parse_expr(source : String) -> @syntax.Expr raise {
  let tokens = @lexer.tokens_from_string(source, comment=true).tokens
  let (expr, reports) = parse_expr(tokens)
  if reports.is_empty() {
    return expr
  } else {
    fail(reports.to_string())
  }
}

///|
test {
  @basic.show_loc.val = @basic.LocStyle::Json
  let add : String =
    #| 1 + 2
  json_inspect(simple_parse_expr(add), content={
    "type": "Expr::Infix",
    "op": {
      "type": "Var",
      "name": { "type": "LongIdent::Ident", "0": "+" },
      "loc": {
        "file": "",
        "start": { "line": 1, "column": 4 },
        "end": { "line": 1, "column": 5 },
      },
    },
    "lhs": {
      "type": "Expr::Constant",
      "c": { "type": "Constant::Int", "0": "1" },
      "loc": {
        "file": "",
        "start": { "line": 1, "column": 2 },
        "end": { "line": 1, "column": 3 },
      },
    },
    "rhs": {
      "type": "Expr::Constant",
      "c": { "type": "Constant::Int", "0": "2" },
      "loc": {
        "file": "",
        "start": { "line": 1, "column": 6 },
        "end": { "line": 1, "column": 7 },
      },
    },
    "loc": {
      "file": "",
      "start": { "line": 1, "column": 2 },
      "end": { "line": 1, "column": 7 },
    },
  })
  let try_catch =
    #| try foo(1, 4 * 8) catch {
    #|   e => {
    #|     print(e)
    #|   }
    #| }
  json_inspect(simple_parse_expr(try_catch), content={
    "type": "Expr::Try",
    "body": {
      "type": "Expr::Apply",
      "func": {
        "type": "Expr::Ident",
        "id": {
          "type": "Var",
          "name": { "type": "LongIdent::Ident", "0": "foo" },
          "loc": {
            "file": "",
            "start": { "line": 1, "column": 6 },
            "end": { "line": 1, "column": 9 },
          },
        },
        "loc": {
          "file": "",
          "start": { "line": 1, "column": 6 },
          "end": { "line": 1, "column": 9 },
        },
      },
      "args": [
        {
          "type": "Argument",
          "value": {
            "type": "Expr::Constant",
            "c": { "type": "Constant::Int", "0": "1" },
            "loc": {
              "file": "",
              "start": { "line": 1, "column": 10 },
              "end": { "line": 1, "column": 11 },
            },
          },
          "kind": { "type": "ArgumentKind::Positional" },
        },
        {
          "type": "Argument",
          "value": {
            "type": "Expr::Infix",
            "op": {
              "type": "Var",
              "name": { "type": "LongIdent::Ident", "0": "*" },
              "loc": {
                "file": "",
                "start": { "line": 1, "column": 15 },
                "end": { "line": 1, "column": 16 },
              },
            },
            "lhs": {
              "type": "Expr::Constant",
              "c": { "type": "Constant::Int", "0": "4" },
              "loc": {
                "file": "",
                "start": { "line": 1, "column": 13 },
                "end": { "line": 1, "column": 14 },
              },
            },
            "rhs": {
              "type": "Expr::Constant",
              "c": { "type": "Constant::Int", "0": "8" },
              "loc": {
                "file": "",
                "start": { "line": 1, "column": 17 },
                "end": { "line": 1, "column": 18 },
              },
            },
            "loc": {
              "file": "",
              "start": { "line": 1, "column": 13 },
              "end": { "line": 1, "column": 18 },
            },
          },
          "kind": { "type": "ArgumentKind::Positional" },
        },
      ],
      "attr": { "type": "ApplyAttr::NoAttr" },
      "loc": {
        "file": "",
        "start": { "line": 1, "column": 6 },
        "end": { "line": 1, "column": 19 },
      },
    },
    "catch": [
      {
        "type": "Case",
        "pattern": {
          "type": "Pattern::Var",
          "0": {
            "type": "Binder",
            "name": "e",
            "loc": {
              "file": "",
              "start": { "line": 2, "column": 4 },
              "end": { "line": 2, "column": 5 },
            },
          },
        },
        "guard": null,
        "body": {
          "type": "Expr::Group",
          "expr": {
            "type": "Expr::Apply",
            "func": {
              "type": "Expr::Ident",
              "id": {
                "type": "Var",
                "name": { "type": "LongIdent::Ident", "0": "print" },
                "loc": {
                  "file": "",
                  "start": { "line": 3, "column": 6 },
                  "end": { "line": 3, "column": 11 },
                },
              },
              "loc": {
                "file": "",
                "start": { "line": 3, "column": 6 },
                "end": { "line": 3, "column": 11 },
              },
            },
            "args": [
              {
                "type": "Argument",
                "value": {
                  "type": "Expr::Ident",
                  "id": {
                    "type": "Var",
                    "name": { "type": "LongIdent::Ident", "0": "e" },
                    "loc": {
                      "file": "",
                      "start": { "line": 3, "column": 12 },
                      "end": { "line": 3, "column": 13 },
                    },
                  },
                  "loc": {
                    "file": "",
                    "start": { "line": 3, "column": 12 },
                    "end": { "line": 3, "column": 13 },
                  },
                },
                "kind": { "type": "ArgumentKind::Positional" },
              },
            ],
            "attr": { "type": "ApplyAttr::NoAttr" },
            "loc": {
              "file": "",
              "start": { "line": 3, "column": 6 },
              "end": { "line": 3, "column": 14 },
            },
          },
          "group": { "type": "Group::Brace" },
          "loc": {
            "file": "",
            "start": { "line": 2, "column": 9 },
            "end": { "line": 2, "column": 8 },
          },
        },
      },
    ],
    "catch_all": false,
    "try_else": null,
    "has_try": true,
    "try_loc": {
      "file": "",
      "start": { "line": 1, "column": 2 },
      "end": { "line": 1, "column": 5 },
    },
    "catch_loc": {
      "file": "",
      "start": { "line": 1, "column": 20 },
      "end": { "line": 1, "column": 27 },
    },
    "else_loc": {
      "file": "<no location>",
      "start": { "line": -1, "column": 1 },
      "end": { "line": -1, "column": 1 },
    },
    "loc": {
      "file": "",
      "start": { "line": 1, "column": 2 },
      "end": { "line": 5, "column": 3 },
    },
  })
}
