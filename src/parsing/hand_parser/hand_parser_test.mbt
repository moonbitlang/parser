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

test "" {
  let tokens = lex("let a : TyCon[Ty] = 100 + 5") 
  let result = parse(tokens)
  @json.inspect(result)
}
