///|
test "reference keyword handling: where is a keyword without lexical error" {
  let result = tokens_from_string("where", comment=false)
  guard result.errors.length() == 0 else {
    fail("where should not be reported as a reserved keyword")
  }
  guard result.tokens[0].0 is WHERE else {
    fail("where should tokenize as WHERE")
  }
}

///|
test "reference keyword handling: package remains a keyword" {
  let result = tokens_from_string("package", comment=false)
  guard result.errors.length() == 0 else {
    fail("package in normal source should not be a lexical error")
  }
  guard result.tokens[0].0 is PACKAGE else {
    fail("package should tokenize as PACKAGE")
  }
}

///|
test "reference byte literal recovery keeps a BYTE token after invalid contents" {
  let result = tokens_from_string("b'🐰'", comment=false)
  guard result.errors.length() == 1 else {
    fail("invalid byte literal should report exactly one lexical error")
  }
  guard result.errors[0].2 is InvalidByteLiteral(payload) && payload == "🐰" else {
    fail("invalid byte literal should report InvalidByteLiteral")
  }
  guard result.tokens[0].0 is BYTE(payload) && payload == "🐰" else {
    fail("invalid byte literal should still emit BYTE for parser recovery")
  }
}

///|
test "reference interpolation scanner handles nested bytes interpolation" {
  let source = "\"\\{b\"\\{\"}\"}\"}\""
  let result = tokens_from_string(source, comment=false)
  guard result.errors.length() == 0 else {
    fail("nested bytes interpolation should not produce lexer errors")
  }
  guard result.tokens[0].0 is INTERP(elems) else {
    fail("outer literal should tokenize as INTERP")
  }
  guard elems.length() == 1 else {
    fail("outer literal should contain one interpolation source")
  }
  guard elems[0] is InterpSource({ source, .. }) else {
    fail("outer interpolation should be captured as source")
  }
  guard source == "b\"\\{\"}\"}\"" else {
    fail("unexpected captured interpolation source: \{source}")
  }
}

///|
test "reference dot recovery reports a missing identifier after dot" {
  let result = tokens_from_string(".", comment=false)
  guard result.errors.length() == 1 else {
    fail("standalone dot should report exactly one lexical error")
  }
  guard result.errors[0].2 is MissingIdentifierAfterDot else {
    fail("standalone dot should report MissingIdentifierAfterDot")
  }
  guard result.tokens[0].0 is DOT_LIDENT("") else {
    fail("standalone dot should still emit an empty DOT_LIDENT for recovery")
  }
}
