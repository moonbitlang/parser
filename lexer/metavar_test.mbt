///|
test "metavariable syntax is disabled by default" {
  let result = tokens_from_string("$x", comment=false)
  guard result.errors[0].2 is IllegalCharacter('$') else {
    fail(
      "expected disabled metavariable syntax to use normal illegal-char path",
    )
  }
  let dot = tokens_from_string(".$field", comment=false)
  guard dot.tokens[0].0 is DOT_LIDENT("") else {
    fail("expected disabled dot metavariable syntax to keep the dot token")
  }
  guard dot.tokens[1].0 is LIDENT("field") else {
    fail("expected disabled dot metavariable syntax to resume after $")
  }
}

///|
test "metavariable syntax encodes as ordinary tokens when enabled" {
  let exp = tokens_from_string("$(x:exp)", comment=false, enable_metavar=true)
  guard exp.tokens[0].0 is LIDENT("$(x:exp)") else {
    fail("expected LIDENT encoded typed metavariable")
  }
  let spaced = tokens_from_string(
    "$( x : exp )",
    comment=false,
    enable_metavar=true,
  )
  guard spaced.tokens[0].0 is LIDENT("$(x:exp)") else {
    fail("expected typed metavariable payload to normalize whitespace")
  }
  let inferred = tokens_from_string("$x", comment=false, enable_metavar=true)
  guard inferred.tokens[0].0 is LIDENT("$x") else {
    fail("expected LIDENT encoded untyped metavariable")
  }
  let type_name = tokens_from_string(
    "$(Foo:type)",
    comment=false,
    enable_metavar=true,
  )
  guard type_name.tokens[0].0 is UIDENT("$(Foo:type)") else {
    fail("expected UIDENT encoded type metavariable")
  }
  let dot = tokens_from_string(
    ".$(field:id)",
    comment=false,
    enable_metavar=true,
  )
  guard dot.tokens[0].0 is DOT_LIDENT("$(field:id)") else {
    fail("expected DOT_LIDENT encoded typed metavariable")
  }
  let dot_inferred = tokens_from_string(
    ".$field",
    comment=false,
    enable_metavar=true,
  )
  guard dot_inferred.tokens[0].0 is DOT_LIDENT("$field") else {
    fail("expected DOT_LIDENT encoded untyped metavariable")
  }
  let post = tokens_from_string(
    "$(label:id)~",
    comment=false,
    enable_metavar=true,
  )
  guard post.tokens[0].0 is POST_LABEL("$(label:id)") else {
    fail("expected POST_LABEL encoded typed metavariable")
  }
  let post_inferred = tokens_from_string(
    "$label~",
    comment=false,
    enable_metavar=true,
  )
  guard post_inferred.tokens[0].0 is POST_LABEL("$label") else {
    fail("expected POST_LABEL encoded untyped metavariable")
  }
}

///|
test "inferred metavariable annotation does not require whitespace" {
  let bare = tokens_from_string("$x:Int", comment=false, enable_metavar=true)
  guard bare.errors.length() == 0 else {
    fail("expected no lexical errors for inferred metavariable annotation")
  }
  guard bare.tokens[0].0 is LIDENT("$x") &&
    bare.tokens[1].0 is COLON &&
    bare.tokens[2].0 is UIDENT("Int") else {
    fail("expected inferred metavariable annotation tokens")
  }
  let result = tokens_from_string(
    "($x:Foo)",
    comment=false,
    enable_metavar=true,
  )
  guard result.errors.length() == 0 else {
    fail("expected no lexical errors for inferred metavariable annotation")
  }
  guard result.tokens[0].0 is LPAREN &&
    result.tokens[1].0 is LIDENT("$x") &&
    result.tokens[2].0 is COLON &&
    result.tokens[3].0 is UIDENT("Foo") &&
    result.tokens[4].0 is RPAREN else {
    fail("expected parenthesized inferred metavariable annotation tokens")
  }
}

///|
test "metavariable malformed forms report lexical errors when enabled" {
  for source in ["$exp:x", "$(:exp)", "$(x:)", "$(x:exp", "$(\tx:exp)"] {
    let result = tokens_from_string(source, comment=false, enable_metavar=true)
    guard result.errors.length() > 0 &&
      result.errors[0].2 is InvalidMetavarSyntax(_) else {
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
