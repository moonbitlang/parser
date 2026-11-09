///|
fn consistency_reject_expr(
  source : String,
  missing~ : Bool = false,
) -> Unit raise {
  assert_all_parsers_reject("fn f() { " + source + " }")
  let lex = @lexer.tokens_from_string(source, comment=true)
  let (_, hand_reports) = @handrolled_parser.parse_expr(lex.tokens)
  guard !hand_reports.is_empty() else {
    fail("Handrolled expression unexpectedly accepted: " + source)
  }
  let parsed = @untyped_cst.parse_expression(source)
  guard !parsed.diagnostics_view().is_empty() else {
    fail("CST expression unexpectedly accepted: " + source)
  }
  boundary_assert_source(parsed.root(), source)
  let (expr, reports) = parsed.to_expr()
  @test.assert_eq(expr is None, true)
  @test.assert_eq(reports.is_empty(), false)
  if missing {
    required_syntax_assert_missing(parsed.root(), source)
  }
}

///|
fn consistency_recover_declaration(source : String) -> Unit raise {
  assert_all_parsers_reject(source)
  let parsed = @untyped_cst.parse_structure(source)
  boundary_assert_source(parsed.root(), source)
  let (impls, reports) = parsed.to_impls()
  @test.assert_eq(impls.is_empty(), true)
  @test.assert_eq(reports.is_empty(), false)
  guard api_find_node(parsed.root(), node => {
      node.kind is Impl_Function && node.text(source) == "fn after() {}"[:]
    })
    is Some(_) else {
    fail("CST recovery lost the next declaration: " + source)
  }
}

///|
test "consistency: parenthesized assignment recovery does not corrupt the sync stack" {
  for newline in ["\n", "\r\n"] {
    for
      expr in [
        "if (x = 1) {}", "if ((x = 1)) {}", "while (x += 1) {}", "match (x = 1) { _ => () }",
      ] {
      let source = "fn f() { " +
        expr +
        " // keep 🐇" +
        newline +
        "}" +
        newline +
        "fn after() {}"
      consistency_recover_declaration(source)
      let (impls, _) = @parser.parse_string(source, parser=Handrolled)
      guard impls_json(impls).stringify().contains("\"after\"") else {
        fail("Handrolled recovery lost the next declaration: " + source)
      }
    }
    for expr in ["if (x =", "if ((x = 1)", "while (x +="] {
      assert_all_parsers_reject("fn f() { " + expr + " // EOF 🦀" + newline)
    }
  }
}

///|
test "consistency: lexmatch starts every optional expression" {
  let expr = "lexmatch s { re\"a\" => 1 }"
  for
    source in [
      "return " + expr,
      "break " + expr,
      "continue " + expr,
      "(" + expr + ")",
      "xs[:" + expr + "]",
      "if (" + expr + ") {}",
    ] {
    ignore(boundary_expr(source))
  }
}

///|
test "consistency: break labels distinguish target from labelled return value" {
  for label in ["done~", "done~~"] {
    guard boundary_expr("break " + label + ": { 1 }")
      is Break(label=None, arg=Some(LabelledBlock(..)), ..) else {
      fail("label should belong to the break value")
    }
    guard boundary_expr("break " + label + " 1")
      is Break(label=Some(_), arg=Some(Constant(..)), ..) else {
      fail("label should belong to the jump")
    }
    ignore(boundary_expr("break " + label))
    ignore(boundary_expr("continue " + label + ": { 1 }"))
  }
  ignore(boundary_expr("break done~: while true {}"))
  ignore(boundary_expr("break done~: for x in xs {}"))
}

///|
test "consistency: records and maps require values and map colons" {
  for newline in ["\n", "\r\n"] {
    for
      (prefix, suffix) in [
        ("{x:", "}"),
        ("{\"x\":", "}"),
        ("{..", "}"),
        ("{x: 1, y:", "}"),
        ("{\"x\": 1, \"y\"", "}"),
        ("{\"x\": 1, \"y\" ", "2}"),
      ] {
      let source = prefix + " // value 🦀" + newline + suffix
      consistency_reject_expr(source, missing=suffix != "2}")
      consistency_recover_declaration(
        "fn f() { " + source + " }" + newline + "fn after() {}",
      )
    }
  }
  let parsed = @untyped_cst.parse_expression("{x:}")
  guard api_find_node(parsed.root(), node => node.kind is Field_Definition)
    is Some(field) else {
    fail("missing record field")
  }
  @test.assert_eq(
    field.first_child_with_label("pun").unwrap().bool_payload(),
    Some(false),
  )
  @test.assert_eq(
    field.first_child_with_label("expr").unwrap().kind is Expr_MissingExpr,
    true,
  )
  for
    source in [
      "{x}", "{x: 1}", "{..record}", "{..record, x: 1}", "{\"x\": 1}", "{\"x\": 1, \"y\": 2}",
    ] {
    ignore(boundary_expr(source))
  }
}

///|
test "consistency: comprehensions require iterator guard arrow and body" {
  for (open, close) in [("[", "]"), ("[|", "|]")] {
    for
      header in [
        "for x in xs", "for x in xs =>", "for x in", "for x in => x", "for x in xs if",
        "for x in xs if => x", "for i = 0", "for i = 0 =>", "for ;;", "for ;; if => 1",
      ] {
      let source = open + header + close
      consistency_reject_expr(source, missing=true)
      consistency_recover_declaration(
        "fn f() { " + source + " }\nfn after() {}",
      )
    }
    for
      header in [
        "for x in xs => x", "for x in xs if x > 0 => x |> f", "for i = 0; i < 3; i = i + 1 => i",
        "for ;; if true => 1",
      ] {
      ignore(boundary_expr(open + header + close))
    }
  }
}

///|
test "consistency: assignment is restricted to statement positions" {
  for
    source in [
      "let y = x = 1", "g(x += 1)", "g(x *= 1)", "x = y = 1", "x += y = 1", "return x = 1",
      "[x = 1]", "{value: x = 1}", "(x = 1)", "try x = 1 catch { _ => () }",
    ] {
    consistency_reject_expr(source)
    consistency_recover_declaration("fn f() { " + source + " }\nfn after() {}")
  }
  for
    source in [
      "x = 1", "x += 1", "x *= 2", "obj.x = value |> f", "xs[0] += 1", "g(x=1)",
      "g(x=1, y~)", "match x { _ => y = 1 }", "lexmatch s { re\"a\" => x += 1 }",
    ] {
    ignore(boundary_expr(source))
  }
}

///|
test "consistency: infix contexts require parentheses around pipelines" {
  for
    (prefix, suffix) in [
      ("if ", " {}"),
      ("while ", " {}"),
      ("match ", " { _ => () }"),
      ("lexmatch ", " { re\"a\" => () }"),
      ("lexscan ", " { re\"a\" => () }"),
      ("guard ", " else {}"),
      ("proof_assert ", ""),
      ("match x { _ if ", " => () }"),
    ] {
    for op in ["|>", "<|"] {
      consistency_reject_expr(prefix + "x " + op + " pred" + suffix)
      ignore(boundary_expr(prefix + "(x " + op + " pred)" + suffix))
    }
  }
  for (open, close) in [("[", "]"), ("[|", "|]")] {
    for
      header in [
        "for x in xs |> f => x", "for x in xs if x |> pred => x", "for i = xs |> f => i",
      ] {
      consistency_reject_expr(open + header + close)
    }
  }
  for
    source in [
      "try x |> f catch { _ => () }", "for x in xs |> f {}", "for x = value |> f; x > 0; x = x |> next {}",
      "[for x in (xs |> f) if (x |> pred) => x |> f]",
    ] {
    ignore(boundary_expr(source))
  }
}

///|
test "consistency: arrow parameters accept only positional binders and wildcards" {
  for
    params in [
      "(x~)", "(x?)", "(x=1)", "(x?=1)", "(@pkg.x)", "(x.y)", "(1)", "(x + y)", "(x~: Int)",
      "(x?: Int)",
    ] {
    let source = params + " => x"
    consistency_reject_expr(source)
    consistency_recover_declaration("fn f() { " + source + " }\nfn after() {}")
  }
  for
    params in [
      "x", "_", "()", "(x)", "(x,)", "(_,)", "(x, y)", "(x: Int)", "(_: Int,)", "(x: Int, y: Int,)",
    ] {
    for body in ["x", "x = 1", "x += 1", "{ x = 1; x }"] {
      ignore(boundary_expr(params + " => " + body))
    }
  }
  for params in ["@pkg.x", "x.y", "(x~)", "(x?)", "(x=1)"] {
    for op in ["|>", "<|"] {
      consistency_reject_expr("value " + op + " " + params + " => { x }")
    }
  }
  ignore(boundary_expr("fn(x~, y?, z? = 1) { x }"))
}

///|
test "consistency: singleton type and pattern commas remain visible and invalid" {
  for
    source in [
      "type T = (Int,)", "type T = (Int,)?", "type T = ((Int,), Bool)", "fn f(x: (Int,)) {}",
      "fn f() { let (x,) = y }", "fn f() { match y { (x,) => x } }",
    ] {
    consistency_recover_declaration(source + "\nfn after() {}")
  }
  for
    source in [
      "type T = (Int)", "type T = (Int, Bool,)", "type T = (Int,) -> Int", "type T = async (Int,) -> Int",
      "type T = ((Int,) -> Int)?", "fn f() { let (x) = y; let (a, b,) = y; (x,) }",
    ] {
    assert_ast_equiv(source, label=source)
  }
}

///|
test "consistency: missing values retain delimiters comments and following declarations" {
  for newline in ["\n", "\r\n"] {
    for
      prefix in [
        "{x:", "{\"x\":", "{..", "[for x in", "[for x in xs if", "[for x in xs =>",
        "[|for x in", "[|for x in xs if", "[|for x in xs =>", "g([for x in xs =>",
        "([|for x in",
      ] {
      for tail in ["", ")", "]", "|]", "}", "fn after() {}"] {
        let source = "fn f() { " + prefix + " // keep 🐇" + newline + tail
        if tail == "fn after() {}" {
          consistency_recover_declaration(source)
        } else {
          assert_all_parsers_reject(source)
        }
        let parsed = @untyped_cst.parse_structure(source)
        boundary_assert_source(parsed.root(), source)
        required_syntax_assert_missing(parsed.root(), source)
        @test.assert_eq(required_syntax_comment_count(parsed.root()), 1)
        let (impls, reports) = parsed.to_impls()
        @test.assert_eq(impls.is_empty(), true)
        @test.assert_eq(reports.is_empty(), false)
      }
    }
  }
}

///|
test "consistency: missing expression diagnostics point at the recovery boundary" {
  for gap in ["", " // keep 🐇\n", " // keep 🦀\r\n"] {
    for
      (prefix, suffix) in [
        ("{x:", "}"),
        ("{..", "}"),
        ("{\"x\":", "}"),
        ("[for x in ", "=> x]"),
        ("[for x in xs if ", "=> x]"),
        ("[for x in xs =>", "]"),
        ("[|for x in xs", "|]"),
      ] {
      let source = prefix + gap + suffix
      let parsed = @untyped_cst.parse_expression(source)
      guard !parsed.diagnostics_view().is_empty() else {
        fail("missing required syntax diagnostic: " + source)
      }
      required_syntax_assert_position(
        source,
        prefix,
        parsed.diagnostics_view()[0],
      )
    }
  }
}

///|
test "consistency: valid syntax preserves AST locations and Unicode trivia" {
  for newline in ["\n", "\r\n"] {
    for
      raw in [
        "return lexmatch s { // case 🐇\n re\"a\" => 1 }", "break done~: { // value 🦀\n1 }",
        "break done~~: { // value 🐇\n1 }", "{x: // value 🦀\n1, y: 2}", "[for x in // iterator 🐇\n(xs |> f) if (x |> pred) => x]",
        "[|for x in xs if // condition 🦀\n(x |> pred) => x|]", "(x: Int, // parameter 🐇\ny: Int,) => x += y",
        "if ( // pipeline 🦀\nx |> pred) {}",
      ] {
      let source = raw.replace_all(old="\n", new=newline)
      ignore(boundary_expr(source))
      boundary_assert_source(
        @untyped_cst.parse_expression(source).root(),
        source,
      )
    }
  }
}
