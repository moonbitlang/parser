///|
fn required_syntax_cases() -> Array[(String, String, Bool)] {
  let cases = [
    ("proof_let x", "", true),
    ("proof_let x =", "", true),
    ("proof_let ", "= 1", true),
    ("proof_let ", "X = 1", false),
    ("guard true else", "", true),
    ("guard! true else", "", true),
    ("try f() catch", "", true),
    ("f() catch", "", true),
    ("try f() catch {} noraise", "", true),
    ("f() catch {} noraise", "", true),
    ("try f() catch ", "noraise {}", true),
    ("f() catch ", "noraise {}", true),
  ]
  for quantifier in ["∀", "∃"] {
    cases.push((quantifier + " ", ": Int, true", true))
    cases.push((quantifier + " ", "X: Int, true", false))
    cases.push((quantifier + " x ", "Int, true", true))
    cases.push((quantifier + " x: ", ", true", true))
    cases.push((quantifier + " x: Int ", "true", true))
    cases.push((quantifier + " x: Int,", "", true))
    cases.push(("∀ x: Int, " + quantifier + " y: ", ", true", true))
    cases.push(("∃ x: Int, " + quantifier + " y: Int,", "", true))
    for suffix in ["", " x", " x:", " x: Int", " x: Int,"] {
      cases.push((quantifier + suffix, "", true))
    }
  }
  for (open, close) in [("[", "]"), ("[|", "|]")] {
    for
      header in [
        "for ", "for i = 0; i < 2; ", "for x in xs; ", "for x in xs; i = 0; ",
      ] {
      let prefix = open + header
      cases.push((prefix, "= 0 => 1" + close, true))
      cases.push((prefix, "I = 0 => 1" + close, false))
      cases.push((prefix, "_ = 0 => 1" + close, true))
      cases.push((prefix + "i ", "0 => i" + close, true))
      cases.push((prefix + "i = ", "=> i" + close, true))
      cases.push((prefix + "i", "", true))
      cases.push((prefix + "i =", "", true))
      cases.push((prefix, ", i = 0 => i" + close, true))
      cases.push((prefix + "i = 0, ", ", j = 1 => i" + close, true))
    }
  }
  cases
}

///|
fn required_syntax_assert_missing(
  root : @node.CstNode,
  source : String,
) -> Unit raise {
  let _ = assert_missing_spans(root, source)
  guard api_find_node(root, node => {
      node.kind is Missing ||
      node.kind is Expr_MissingExpr ||
      node.kind is Type_MissingType
    })
    is Some(_) else {
    fail("expected missing syntax in: " + source)
  }
}

///|
fn required_syntax_comment_count(node : @node.CstNode) -> Int32 {
  // Recovery may retain a comment inside an Error node instead of as trivia.
  if node.kind is (Syntax_Token(TK_COMMENT) | Syntax_Unexpected(TK_COMMENT)) {
    return 1
  }
  let mut count = 0
  for (_, child) in node.children {
    count += required_syntax_comment_count(child)
  }
  count
}

///|
fn required_syntax_assert_position(
  source : String,
  prefix : String,
  report : @basic.Report,
) -> Unit raise {
  // Stop at an inserted semicolon as well as explicit source tokens.
  let prefix_tokens = @lexer.tokens_from_string(prefix, name="", comment=true).tokens
  let mut boundary = prefix_tokens[0].1
  for (token, _, end) in prefix_tokens {
    if !(token is (COMMENT(_) | NEWLINE | EOF | SEMI(false))) {
      boundary = end
    }
  }
  let tokens = @lexer.tokens_from_string(source, name="", comment=true).tokens
  for (token, start, end) in tokens {
    if token is (COMMENT(_) | NEWLINE) || start.cnum < boundary.cnum {
      continue
    }
    if report.loc != @node.mk_loc(start, end) {
      fail(
        "wrong first diagnostic position in: " +
        source +
        "\n" +
        Repr(report).to_string(),
      )
    }
    return
  }
  fail("missing expected diagnostic position in: " + source)
}

///|
test "required syntax rejects incomplete bindings quantifiers and branch blocks" {
  for (prefix, suffix, has_missing) in required_syntax_cases() {
    for gap in ["", " // keep 🐇\n", " // keep 🐇\r\n // next 🦀\r\n"] {
      let expr = prefix + gap + suffix
      assert_all_parsers_reject("fn f() { " + expr + " }")
      let parsed = @untyped_cst.parse_expression(expr)
      guard !parsed.diagnostics_view().is_empty() else {
        fail("CST expression unexpectedly accepted: " + expr)
      }
      boundary_assert_source(parsed.root(), expr)
      let expected_comments = if gap == "" {
        0
      } else if gap.contains("next") {
        2
      } else {
        1
      }
      if required_syntax_comment_count(parsed.root()) != expected_comments {
        fail(
          "lost or duplicated recovery comments (" +
          required_syntax_comment_count(parsed.root()).to_string() +
          ") in: " +
          expr,
        )
      }
      required_syntax_assert_position(
        expr,
        prefix,
        parsed.diagnostics_view()[0],
      )
      let (lowered, reports) = parsed.to_expr()
      @test.assert_eq(lowered is None, true)
      @test.assert_eq(reports.is_empty(), false)
      if has_missing {
        required_syntax_assert_missing(parsed.root(), expr)
      }
      let source = "fn f() { " + expr + " }\nfn after() {}"
      let structure = @untyped_cst.parse_structure(source)
      boundary_assert_source(structure.root(), source)
      let (impls, reports) = structure.to_impls()
      @test.assert_eq(impls.is_empty(), true)
      @test.assert_eq(reports.is_empty(), false)
      guard api_find_node(structure.root(), node => {
          node.kind.is_impl() && node.text(source) == "fn after() {}"[:]
        })
        is Some(_) else {
        fail("recovery lost the following declaration: " + source)
      }
    }
  }
}

///|
test "required syntax recovery stops at enclosing delimiters and declarations" {
  for
    expr in [
      "proof_let x =", "proof_let", "∀ x:", "∃ x: Int,", "[for i =", "[|for x in xs; i =",
      "guard true else", "guard! true else", "try f() catch", "f() catch", "try f() catch {} noraise",
      "f() catch {} noraise", "g([for i =", "g(∀ x:", "(∃ x: Int,",
    ] {
    for tail in [")", "]", "}", "", "\nfn after() {}"] {
      let source = "fn f() { " + expr + " // keep 🐇\n" + tail
      assert_all_parsers_reject(source)
      let parsed = @untyped_cst.parse_structure(source)
      boundary_assert_source(parsed.root(), source)
      required_syntax_assert_missing(parsed.root(), source)
      if tail.contains("fn after") {
        guard api_find_node(parsed.root(), node => {
            node.kind.is_impl() && node.text(source) == "fn after() {}"[:]
          })
          is Some(_) else {
          fail(
            "recovery swallowed declaration after truncated syntax: " + source,
          )
        }
      }
    }
  }
}

///|
test "each missing quantifier and proof binding component reports once" {
  for quantifier in ["∀", "∃"] {
    for
      (suffix, count) in [
        ("", 5),
        (" x", 4),
        (" x:", 3),
        (" x: Int", 2),
        (" x: Int,", 1),
      ] {
      let source = quantifier + suffix
      let parsed = @untyped_cst.parse_expression(source)
      @test.assert_eq(parsed.diagnostics_view().length(), count)
      required_syntax_assert_missing(parsed.root(), source)
    }
  }
  for
    (source, count) in [
      ("proof_let", 3),
      ("proof_let x", 2),
      ("proof_let x =", 1),
      ("proof_let = 1", 1),
    ] {
    let parsed = @untyped_cst.parse_expression(source)
    @test.assert_eq(parsed.diagnostics_view().length(), count)
    required_syntax_assert_missing(parsed.root(), source)
  }
}
