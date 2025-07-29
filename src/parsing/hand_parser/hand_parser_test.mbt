///|
fn lex(
  utf8 : Bytes,
) -> Array[(@tokens.Token, @syntax.Position, @syntax.Position)] {
  @lexer.tokens_from_bytes(utf8, comment=false).tokens
}

///|
test "lexing" {
  let tokens = lex("let a : TyCon[Ty] = 100 + 5")
    .map(triple => triple.0.to_string())
    .fold(init="", (a, b) => a + b + "\n")
  inspect(
    tokens,
    content=(
      #|LET
      #|LIDENT("a")
      #|COLON
      #|UIDENT("TyCon")
      #|LBRACKET
      #|UIDENT("Ty")
      #|RBRACKET
      #|EQUAL
      #|INT("100")
      #|PLUS
      #|INT("5")
      #|EOF
      #|
    ),
  )
}

///|
let null : Json = Json::null()

///|
test "" {
  let tokens = lex("let a : TyCon[Ty] = 100 + 5")
  let result = parse(tokens)
  @json.inspect(result, content=[
    [
      {
        "type": "Impl::TopLetDef",
        "binder": { "type": "Binder", "name": "a", "loc": null },
        "ty": {
          "type": "Type::Name",
          "constr_id": {
            "type": "ConstrId",
            "id": { "type": "LongIdent::Ident", "0": "TyCon" },
            "loc": null,
          },
          "tys": [
            {
              "type": "Type::Name",
              "constr_id": {
                "type": "ConstrId",
                "id": { "type": "LongIdent::Ident", "0": "Ty" },
                "loc": null,
              },
              "tys": [],
              "loc": null,
            },
          ],
          "loc": null,
        },
        "expr": {
          "type": "Expr::Infix",
          "op": {
            "type": "Var",
            "name": { "type": "LongIdent::Ident", "0": "+" },
            "loc": null,
          },
          "lhs": {
            "type": "Expr::Constant",
            "c": { "type": "Constant::Int", "0": "100" },
            "loc": null,
          },
          "rhs": {
            "type": "Expr::Constant",
            "c": { "type": "Constant::Int", "0": "5" },
            "loc": null,
          },
          "loc": null,
        },
        "vis": { "type": "Visibility::Default" },
        "is_constant": false,
        "loc": null,
        "attrs": [],
        "doc": "",
      },
    ],
    [],
  ])
}
