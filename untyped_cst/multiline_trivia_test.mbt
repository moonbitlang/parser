///|
fn assert_multiline_trivia(lines : ArrayView[(String, Int)]) -> Unit raise {
  for in_function in [false, true] {
    let prefix = if in_function { "fn f() {\n" } else { "" }
    let indent = if in_function { "  " } else { "" }
    let mut source = prefix
    for i, (text, _) in lines {
      if i > 0 {
        source += "\n"
      }
      source += indent + text
    }
    if in_function {
      source += "\n}"
    }
    let parsed = if in_function {
      @untyped_cst.parse_structure(source)
    } else {
      @untyped_cst.parse_expression(source)
    }
    @test.assert_eq(parsed.diagnostics_view().length(), 0)
    @test.assert_eq(parsed.root().text(source), source[:])
    guard api_find_node(parsed.root(), node => node.kind is Expr_MultilineString)
      is Some(multiline) else {
      fail("expected multiline string in \{source}")
    }
    let first_line = if in_function { 2 } else { 1 }
    let start_offset = prefix.length() + indent.length()
    let mut offset = prefix.length()
    let mut cnum = offset
    let mut line = first_line
    let mut bol = offset
    let mut child_index = 0
    // Widths count Unicode characters; source spans count UTF-16 code units.
    for i, (text, width) in lines {
      offset += indent.length()
      cnum += indent.length()
      let labels = if text == "" {
        []
      } else if text.has_prefix("//") {
        ["trivia"]
      } else {
        ["token", "element"]
      }
      for label in labels {
        let (actual_label, child) = multiline.children[child_index]
        @test.assert_eq(actual_label, Some(label))
        refactor_assert_source_range(
          child,
          source,
          text,
          (offset, offset + text.length()),
          (line, bol, cnum),
          (line, bol, cnum + width),
        )
        let matches_kind = match label {
          "trivia" => child.kind is Syntax_Token(TK_COMMENT)
          "token" =>
            if text.has_prefix("$|") {
              child.kind is Syntax_Token(TK_MULTILINE_INTERP)
            } else {
              child.kind is Syntax_Token(TK_MULTILINE_STRING)
            }
          _ =>
            if text.has_prefix("$|") {
              child.kind is MultilineStringElement_Interp
            } else {
              child.kind is MultilineStringElement_String
            }
        }
        @test.assert_eq(matches_kind, true)
        child_index += 1
      }
      offset += text.length()
      cnum += width
      if i + 1 < lines.length() {
        offset += 1
        cnum += 1
        line += 1
        bol = cnum
      }
    }
    @test.assert_eq(multiline.children.length(), child_index)
    for node in [multiline] {
      refactor_assert_source_range(
        node,
        source,
        source[start_offset:offset].to_owned(),
        (start_offset, offset),
        (first_line, prefix.length(), start_offset),
        (line, bol, cnum),
      )
    }
    if in_function {
      assert_ast_equiv(source)
    } else {
      assert_expr_ast_equiv(source)
    }
  }
}

///|
test "multiline strings retain an intervening comment" {
  // The expression case is the original reproducer: #|a\n// comment\n#|b.
  assert_multiline_trivia([("#|a", 3), ("// comment", 10), ("#|b", 3)])
}

///|
test "multiline strings retain consecutive comments and blank lines" {
  for blank_lines in [false, true] {
    let lines = [("#|a", 3)]
    if blank_lines {
      lines.push(("", 0))
    }
    lines.append([("// first", 8), ("// second", 9)])
    if blank_lines {
      lines.append([("", 0), ("// third", 8), ("", 0)])
    }
    lines.push(("#|b", 3))
    assert_multiline_trivia(lines)
  }
}

///|
test "mixed multiline strings retain comments and UTF-16 spans" {
  assert_multiline_trivia([
    ("#|a", 3),
    ("// keep 🐇", 9),
    ("// next 🦀", 9),
    ("", 0),
    ("$|b \\{x}", 8),
    ("// tail", 7),
    ("#|c", 3),
  ])
}
