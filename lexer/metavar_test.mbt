///|
fn assert_single_metavar_result(
  result : LexResult,
  source : String,
  token_start : Int,
) -> Unit raise {
  guard result.errors.length() == 0 else {
    fail("expected no lexical errors for \{source}")
  }
  guard result.tokens.length() == 2 && result.tokens[1].0 is EOF else {
    fail(
      "expected exactly one metavariable token followed by EOF for \{source}",
    )
  }
  let (_, start, end) = result.tokens[0]
  guard start.cnum == token_start && end.cnum == source.length() else {
    fail(
      "unexpected metavariable location for \{source}: \{start.cnum}..\{end.cnum}",
    )
  }
  let (_, eof_start, eof_end) = result.tokens[1]
  guard eof_start.cnum == source.length() && eof_end.cnum == source.length() else {
    fail("unexpected EOF location for \{source}")
  }
}

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
  let triple = tokens_from_string("$$$x", comment=false)
  guard triple.errors.length() == 3 else {
    fail("expected each disabled $$$ prefix character to be rejected")
  }
  guard triple.tokens[0].0 is LIDENT("x") else {
    fail("expected disabled $$$ metavariable syntax to resume after the prefix")
  }
  let dot_triple = tokens_from_string(".$$$field", comment=false)
  guard dot_triple.errors.length() == 3 else {
    fail("expected each disabled dot $$$ prefix character to be rejected")
  }
  guard dot_triple.tokens[0].0 is DOT_LIDENT("") &&
    dot_triple.tokens[1].0 is LIDENT("field") else {
    fail(
      "expected disabled dot $$$ metavariable syntax to recover after the prefix",
    )
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
test "triple-dollar metavariable syntax preserves its prefix" {
  let exp = tokens_from_string("$$$(x:exp)", comment=false, enable_metavar=true)
  assert_single_metavar_result(exp, "$$$(x:exp)", 0)
  guard exp.tokens[0].0 is LIDENT("$$$(x:exp)") else {
    fail("expected LIDENT encoded typed $$$ metavariable")
  }
  let spaced = tokens_from_string(
    "$$$( x : exp )",
    comment=false,
    enable_metavar=true,
  )
  assert_single_metavar_result(spaced, "$$$( x : exp )", 0)
  guard spaced.tokens[0].0 is LIDENT("$$$(x:exp)") else {
    fail("expected typed $$$ metavariable payload to normalize whitespace")
  }
  let inferred = tokens_from_string("$$$x", comment=false, enable_metavar=true)
  assert_single_metavar_result(inferred, "$$$x", 0)
  guard inferred.tokens[0].0 is LIDENT("$$$x") else {
    fail("expected LIDENT encoded untyped $$$ metavariable")
  }
  let inferred_type = tokens_from_string(
    "$$$Foo",
    comment=false,
    enable_metavar=true,
  )
  assert_single_metavar_result(inferred_type, "$$$Foo", 0)
  guard inferred_type.tokens[0].0 is UIDENT("$$$Foo") else {
    fail("expected UIDENT encoded untyped $$$ metavariable")
  }
  let type_name = tokens_from_string(
    "$$$(Foo:type)",
    comment=false,
    enable_metavar=true,
  )
  assert_single_metavar_result(type_name, "$$$(Foo:type)", 0)
  guard type_name.tokens[0].0 is UIDENT("$$$(Foo:type)") else {
    fail("expected UIDENT encoded typed $$$ metavariable")
  }
  let dot = tokens_from_string(
    ".$$$(field:id)",
    comment=false,
    enable_metavar=true,
  )
  assert_single_metavar_result(dot, ".$$$(field:id)", 1)
  guard dot.tokens[0].0 is DOT_LIDENT("$$$(field:id)") else {
    fail("expected DOT_LIDENT encoded typed $$$ metavariable")
  }
  let dot_inferred = tokens_from_string(
    ".$$$field",
    comment=false,
    enable_metavar=true,
  )
  assert_single_metavar_result(dot_inferred, ".$$$field", 1)
  guard dot_inferred.tokens[0].0 is DOT_LIDENT("$$$field") else {
    fail("expected DOT_LIDENT encoded untyped $$$ metavariable")
  }
  let post = tokens_from_string(
    "$$$(label:id)~",
    comment=false,
    enable_metavar=true,
  )
  assert_single_metavar_result(post, "$$$(label:id)~", 0)
  guard post.tokens[0].0 is POST_LABEL("$$$(label:id)") else {
    fail("expected POST_LABEL encoded typed $$$ metavariable")
  }
  let post_inferred = tokens_from_string(
    "$$$label~",
    comment=false,
    enable_metavar=true,
  )
  assert_single_metavar_result(post_inferred, "$$$label~", 0)
  guard post_inferred.tokens[0].0 is POST_LABEL("$$$label") else {
    fail("expected POST_LABEL encoded untyped $$$ metavariable")
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
  let triple = tokens_from_string(
    "$$$x:Int",
    comment=false,
    enable_metavar=true,
  )
  guard triple.errors.length() == 0 else {
    fail("expected no lexical errors for inferred $$$ metavariable annotation")
  }
  guard triple.tokens[0].0 is LIDENT("$$$x") &&
    triple.tokens[1].0 is COLON &&
    triple.tokens[2].0 is UIDENT("Int") else {
    fail("expected inferred $$$ metavariable annotation tokens")
  }
}

///|
test "metavariable malformed forms report lexical errors when enabled" {
  for
    source in [
      "$exp:x", "$(:exp)", "$(x:)", "$(x:exp", "$(\tx:exp)", "$$$exp:x", "$$$(:exp)",
      "$$$(x:)", "$$$(x:exp", "$$$(\tx:exp)", ".$$$(:exp)",
    ] {
    let result = tokens_from_string(source, comment=false, enable_metavar=true)
    guard result.errors.length() > 0 &&
      result.errors[0].2 is InvalidMetavarSyntax(_) else {
      fail("expected InvalidMetavarSyntax for \{source}")
    }
  }
}

///|
test "double-dollar metavariable prefix remains unsupported" {
  for
    case in [
      ("$$x", "$$x", 0),
      ("$$(x:exp)", "$$(x:exp)", 0),
      ("$$label~", "$$label~", 0),
      (".$$field", ".$$field", 1),
      (".$$(field:id)", ".$$(field:id)", 1),
    ] {
    let (source, expected_raw, error_start) = case
    let result = tokens_from_string(source, comment=false, enable_metavar=true)
    guard result.errors.length() == 1 else {
      fail("expected one lexical error for unsupported prefix in \{source}")
    }
    let (start, end, error) = result.errors[0]
    guard error is InvalidMetavarSyntax(raw) &&
      raw == expected_raw &&
      start.cnum == error_start &&
      end.cnum == source.length() else {
      fail("unexpected double-dollar error or location for \{source}")
    }
    guard result.tokens.length() == 1 && result.tokens[0].0 is EOF else {
      fail("unsupported \{source} must not recover as a valid metavariable")
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
