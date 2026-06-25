///|
test "metavariable syntax is disabled by default" {
  let result = tokens_from_string("$exp:x", comment=false)
  guard result.errors[0].2 is IllegalCharacter('$') else {
    fail(
      "expected disabled metavariable syntax to use normal illegal-char path",
    )
  }
  let dot = tokens_from_string(".$id:x", comment=false)
  guard dot.tokens[0].0 is DOT_LIDENT("") else {
    fail("expected disabled dot metavariable syntax to keep the dot token")
  }
  guard dot.tokens[1].0 is LIDENT("id") else {
    fail("expected disabled dot metavariable syntax to resume after $")
  }
  guard dot.tokens[2].0 is COLON && dot.tokens[3].0 is LIDENT("x") else {
    fail("expected disabled dot metavariable syntax to lex the rest normally")
  }
}

///|
test "metavariable syntax encodes as ordinary tokens when enabled" {
  let exp = tokens_from_string("$exp:x", comment=false, enable_metavar=true)
  guard exp.tokens[0].0 is LIDENT("$exp:x") else {
    fail("expected LIDENT encoded subtree metavariable")
  }
  let id = tokens_from_string("$id:x", comment=false, enable_metavar=true)
  guard id.tokens[0].0 is LIDENT("$id:x") else {
    fail("expected LIDENT encoded identifier metavariable")
  }
  let pat = tokens_from_string("$pat:x", comment=false, enable_metavar=true)
  guard pat.tokens[0].0 is LIDENT("$pat:x") else {
    fail("expected LIDENT encoded arbitrary-kind metavariable")
  }
  let type_name = tokens_from_string(
    "$type:Foo",
    comment=false,
    enable_metavar=true,
  )
  guard type_name.tokens[0].0 is UIDENT("$type:Foo") else {
    fail("expected UIDENT encoded type metavariable")
  }
  let dot = tokens_from_string(".$id:field", comment=false, enable_metavar=true)
  guard dot.tokens[0].0 is DOT_LIDENT("$id:field") else {
    fail("expected DOT_LIDENT encoded identifier metavariable")
  }
  let dot_pat = tokens_from_string(
    ".$pat:field",
    comment=false,
    enable_metavar=true,
  )
  guard dot_pat.tokens[0].0 is DOT_LIDENT("$pat:field") else {
    fail("expected DOT_LIDENT encoded arbitrary-kind metavariable")
  }
  let post = tokens_from_string(
    "$id:label~",
    comment=false,
    enable_metavar=true,
  )
  guard post.tokens[0].0 is POST_LABEL("$id:label") else {
    fail("expected POST_LABEL encoded identifier metavariable")
  }
  let post_pat = tokens_from_string(
    "$pat:label~",
    comment=false,
    enable_metavar=true,
  )
  guard post_pat.tokens[0].0 is POST_LABEL("$pat:label") else {
    fail("expected POST_LABEL encoded arbitrary-kind metavariable")
  }
}

///|
test "metavariable malformed forms report lexical errors when enabled" {
  for source in ["$exp:", "$id:", ".$id:", "$id:~", "$:x"] {
    let result = tokens_from_string(source, comment=false, enable_metavar=true)
    guard result.errors[0].2 is InvalidMetavarSyntax(_) else {
      fail("expected InvalidMetavarSyntax for \{source}")
    }
  }
}

///|
test "multiline interpolation keeps precedence over metavariable syntax" {
  let result = tokens_from_string("$|hello", comment=false, enable_metavar=true)
  guard result.tokens[0].0 is MULTILINE_INTERP(_) else {
    fail("expected MULTILINE_INTERP")
  }
}
