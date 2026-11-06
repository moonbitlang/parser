///|
test "mbtx body AST and CST retain original Unicode and CRLF locations" {
  for newline in ["\n", "\r\n"] {
    for bom in ["", "\u{FEFF}"] {
      let prefix = bom +
        "----" +
        newline +
        "// 中文😀" +
        newline +
        "experimental = true" +
        newline +
        "-----" +
        newline
      let body = "/// body doc" + newline + "let text = \"😀\"" + newline
      let source = prefix + body
      let (hand, hand_reports) = @parser.parse_mbtx_string(
        source,
        name="unicode.mbtx",
        parser=Handrolled,
      )
      let (yacc, yacc_reports) = @parser.parse_mbtx_string(
        source,
        name="unicode.mbtx",
        parser=MoonYacc,
      )
      let cst = @untyped_cst.parse_mbtx(source, name="unicode.mbtx")
      assert_eq(hand_reports.length(), 0)
      assert_eq(yacc_reports.length(), 0)
      assert_eq(cst.diagnostics.length(), 0)
      assert_true(hand.header is Some(_))
      let (impls, diagnostics) = cst.body.to_impls()
      assert_eq(diagnostics.length(), 0)
      @basic.show_loc.protect(String, () => {
        let expected = Json(hand.body.map(impl_ => impl_.json_repr()))
        assert_eq(Json(yacc.body.map(impl_ => impl_.json_repr())), expected)
        assert_eq(Json(impls.map(impl_ => impl_.json_repr())), expected)
      })
      guard hand.body.head()
        is Some(TopLetDef(binder~, expr=Constant(loc~, ..), ..)) else {
        fail("expected body declaration")
      }
      assert_eq(binder.loc.start.lnum, 6)
      assert_eq(binder.loc.start.column(), 5)
      assert_eq(
        binder.loc.start.cnum,
        prefix.length() - 1 + 12 + newline.length() + 4,
      )
      assert_eq(loc.start.lnum, 6)
      guard api_find_node(cst.body.root(), node => {
          node.kind is Syntax_Token(TK_STRING)
        })
        is Some(literal) else {
        fail("expected body string token")
      }
      let start = prefix.length() + 12 + newline.length() + 11
      assert_eq(literal.source_span, (start, start + 4))
      assert_eq(literal.text(source), "\"😀\""[:])
    }
  }
}

///|
test "mbtx distinguishes absent empty and unclosed headers and reports header errors" {
  for
    (source, present, body_count) in [
      ("let x = 1", false, 1),
      ("---\n---\nlet x = 1", true, 1),
      ("---", true, 0),
      ("---\nexperimental = true", true, 0),
    ] {
    let (hand, hand_reports) = @parser.parse_mbtx_string(source)
    let cst = @untyped_cst.parse_mbtx(source)
    assert_eq(hand.header is Some(_), present)
    assert_eq(cst.header is Some(_), present)
    assert_eq(hand.body.length(), body_count)
    assert_eq(hand_reports.length(), 0)
    assert_eq(cst.diagnostics.length(), 0)
    assert_eq(cst.body.to_impls().0.length(), body_count)
  }
  let source = "---\r\npermissions(network: )\r\n---\r\nlet x = 1"
  let (hand, hand_reports) = @parser.parse_mbtx_string(
    source,
    name="invalid.mbtx",
  )
  let cst = @untyped_cst.parse_mbtx(source, name="invalid.mbtx")
  assert_true(!hand_reports.is_empty())
  assert_eq(cst.diagnostics.length(), hand_reports.length())
  assert_eq(hand_reports[0].loc.start.lnum, 2)
  assert_eq(hand_reports[0].loc.start.column(), 22)
  assert_eq(hand_reports[0].loc.start.fname, "invalid.mbtx")
  assert_eq(hand.body.length(), 1)
  // The existing source entry point does not silently consume frontmatter.
  assert_true(!@parser.parse_string(source).1.is_empty())
}

///|
test "upstream mbtx header accepts script settings and retains body imports" {
  let source =
    #|----
    #|import {
    #|  "foo/module1@0.1.1/pkg" @header,
    #|  "foo/module2/pkg" @header
    #|}
    #|permissions(network: false)
    #|----
    #|import { "bar/module@0.2.0" @legacy }
    #|fn main { println("hi") }
  let (hand, hand_reports) = @parser.parse_mbtx_string(
    source,
    name="script.mbtx",
  )
  let cst = @untyped_cst.parse_mbtx(source, name="script.mbtx")
  assert_eq(hand_reports.length(), 0)
  assert_eq(cst.diagnostics.length(), 0)
  guard hand.header is Some(header) else {
    fail("expected configuration header")
  }
  assert_eq(header.ast.0.length(), 2)
  guard header.ast.0[1] is Expr(Apply(name="permissions", loc~, ..)) else {
    fail("script settings were lost")
  }
  assert_eq(loc.start.lnum, 6)
  assert_eq(loc.start.column(), 1)
  assert_eq(hand.body.length(), 2)
  assert_eq(cst.body.to_impls().0.length(), 2)
  guard hand.body.to_array() is [TopImport(..), main] else {
    fail("expected original body declarations")
  }
  assert_eq(main.loc().start.lnum, 9)
}
