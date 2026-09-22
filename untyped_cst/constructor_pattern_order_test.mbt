///|
fn constructor_pattern_source(
  prefix : String,
  pieces : ArrayView[(String, String, Int)],
  suffix : String,
) -> String {
  let mut source = prefix
  for i, (label, text, _) in pieces {
    source += text
    if i + 1 < pieces.length() {
      source += if label == "attrs" || label == "trivia" { "\n " } else { " " }
    }
  }
  source + suffix
}

///|
fn assert_constructor_pattern_children(
  node : @untyped_cst.CstNode,
  source : String,
  prefix : String,
  pieces : ArrayView[(String, String, Int)],
) -> Unit raise {
  let mut offset = prefix.length()
  let mut cnum = offset
  let mut line = 1
  let mut bol = 0
  for i, (label, text, width) in pieces {
    let (actual_label, child) = node.children[i]
    @test.assert_eq(actual_label, Some(label))
    refactor_assert_source_range(
      child,
      source,
      text,
      (offset, offset + text.length()),
      (line, bol, cnum),
      (line, bol, cnum + width),
    )
    if label == "trivia" {
      @test.assert_eq(
        child.kind is @untyped_cst.NodeKind::Syntax_Token(TK_COMMENT),
        true,
      )
    }
    offset += text.length()
    cnum += width
    if i + 1 < pieces.length() {
      if label == "attrs" || label == "trivia" {
        line += 1
        bol = cnum + 1
        offset += 2
        cnum += 2
      } else {
        offset += 1
        cnum += 1
      }
    }
  }
  refactor_assert_source_range(
    node,
    source,
    source[prefix.length():offset].to_owned(),
    (prefix.length(), offset),
    (1, 0, prefix.length()),
    (line, bol, cnum),
  )
}

///|
test "enum and suberror constructors retain comments between and after attributes" {
  for declaration in ["enum E", "suberror E"] {
    for
      comments in [
        [("trivia", "// keep", 7)],
        [("trivia", "// keep", 7), ("trivia", "// next", 7)],
        [("trivia", "// keep 🐇", 9), ("trivia", "// next 🦀", 9)],
      ] {
      for multiple_attrs in [false, true] {
        let pieces = [("attrs", "#deprecated", 11)]
        pieces.append(comments)
        if multiple_attrs {
          pieces.push(("attrs", "#deprecated", 11))
          pieces.append(comments)
        }
        pieces.push(("name", "A", 1))
        let prefix = declaration + " { "
        let source = constructor_pattern_source(prefix, pieces, " }")
        let node = refactor_find_node(source, kind => {
          kind is Clause_ConstructorDecl
        })
        @test.assert_eq(node.children.length(), pieces.length() + 1)
        assert_constructor_pattern_children(node, source, prefix, pieces)
        let (label, semantic_name) = node.children[pieces.length()]
        @test.assert_eq(label, Some("semantic_name"))
        let name = node.first_child_with_label("name").unwrap()
        @test.assert_eq(semantic_name.text(source), "A"[:])
        @test.assert_eq(semantic_name.loc, name.loc)
        @test.assert_eq(semantic_name.source_span, name.source_span)
        assert_ast_equiv(source)
      }
    }
  }
}

///|
test "enum and suberror constructor visibility retains following comments" {
  for declaration in ["enum E", "suberror E"] {
    for vis in ["pub", "priv"] {
      for
        comments in [
          [("trivia", "// keep", 7)],
          [("trivia", "// keep 🐇", 9), ("trivia", "// next 🦀", 9)],
        ] {
        let pieces = [("attrs", "#deprecated", 11)]
        pieces.append(comments)
        pieces.push(("vis", vis, vis.length()))
        pieces.append(comments)
        pieces.push(("name", "A", 1))
        let prefix = declaration + " { "
        let source = constructor_pattern_source(prefix, pieces, " }")
        let parsed = @untyped_cst.parse_structure(source)
        let reports = parsed.diagnostics_view()
        @test.assert_eq(reports.length(), 1)
        @test.assert_eq(
          reports[0].msg,
          "No individual visibility for enum constructor.",
        )
        let node = api_find_node(parsed.root(), node => {
          node.kind is Clause_ConstructorDecl
        }).unwrap()
        @test.assert_eq(node.children.length(), pieces.length() + 1)
        assert_constructor_pattern_children(node, source, prefix, pieces)
        @test.assert_eq(
          reports[0].loc,
          node.first_child_with_label("vis").unwrap().loc,
        )
        assert_structure_acceptance("constructor visibility", source, false)
      }
    }
  }
}

///|
test "constructor recovery retains visibility before an invalid name" {
  for declaration in ["enum E", "suberror E"] {
    let prefix = declaration + " { "
    let source = prefix + "pub x }"
    let parsed = @untyped_cst.parse_structure(source)
    let reports = reports_from_view(parsed.diagnostics_view())
    @test.assert_eq(reports.length(), 2)
    @test.assert_eq(
      reports[0].msg,
      "No individual visibility for enum constructor.",
    )
    @test.assert_eq(
      reports[1].msg,
      "Unexpected token id (lowercase start), you may expect id (lowercase start).",
    )
    let node = api_find_node(parsed.root(), node => {
      node.kind is Clause_ConstructorDecl
    }).unwrap()
    @test.assert_eq(refactor_labels(node), [Some("vis"), Some("error")])
    assert_constructor_pattern_children(node, source, prefix, [
      ("vis", "pub", 3),
      ("error", "x", 1),
    ])
    let error = node.children[1].1
    @test.assert_eq(error.kind is @untyped_cst.NodeKind::Error, true)
    @test.assert_eq(refactor_labels(error), [Some("item")])
    @test.assert_eq(error.children[0].1.text(source), "x"[:])
    @test.assert_eq(reports[0].loc, node.children[0].1.loc)
    @test.assert_eq(reports[1].loc, error.loc)
    let (_, lowering_reports) = parsed.to_impls()
    @test.assert_eq(
      diagnostics_equiv_reports_json(lowering_reports),
      diagnostics_equiv_reports_json(reports),
    )
  }
}

///|
test "with patterns retain every comment after the opening parenthesis" {
  for
    comments in [
      [("trivia", "// keep", 7)],
      [("trivia", "// keep", 7), ("trivia", "// next", 7)],
      [("trivia", "// keep 🐇", 9), ("trivia", "// next 🦀", 9)],
    ] {
    let pieces = [("open", "(", 1)]
    pieces.append(comments)
    pieces.append([
      ("pattern", "x", 1),
      ("op", "with", 4),
      ("body", "y = 1", 5),
      ("close", ")", 1),
    ])
    let prefix = "match value { "
    let source = constructor_pattern_source(prefix, pieces, " => y }")
    let node = refactor_find_node(
      source,
      kind => kind is Pattern_With,
      expression=true,
    )
    @test.assert_eq(node.children.length(), pieces.length())
    assert_constructor_pattern_children(node, source, prefix, pieces)
    assert_expr_ast_equiv(source)
    assert_ast_equiv("fn f() { " + source + " }")
  }
}

///|
test "missing with patterns retain an end-anchored placeholder before with" {
  for comments in ["", "// keep\n ", "// keep 🐇\n // next 🦀\n "] {
    let source = "match value { (" + comments + "with y = 1) => y }"
    let parsed = @untyped_cst.parse_expression(source)
    @test.assert_eq(parsed.diagnostics_view().length(), 0)
    let node = api_find_node(parsed.root(), node => node.kind is Pattern_With).unwrap()
    let labels = [Some("open")]
    let comment_count = if comments == "" {
      0
    } else if comments == "// keep\n " {
      1
    } else {
      2
    }
    for _ in 0..<comment_count {
      labels.push(Some("trivia"))
    }
    labels.append([Some("pattern"), Some("op"), Some("body"), Some("close")])
    @test.assert_eq(refactor_labels(node), labels)
    let missing = node.children[comment_count + 1].1
    @test.assert_eq(missing.kind is @untyped_cst.NodeKind::Missing, true)
    @test.assert_eq(missing.loc, @node.empty_loc(node.loc.end))
    @test.assert_eq(
      missing.source_span,
      (node.source_span.1, node.source_span.1),
    )
    @test.assert_eq(missing.text(source), ""[:])
    @test.assert_eq(
      node.source_span,
      (14, source.length() - " => y }".length()),
    )
    let utf16_extra = if comment_count == 2 { 2 } else { 0 }
    @test.assert_eq(missing.loc.end.cnum, node.source_span.1 - utf16_extra)
    @test.assert_eq(node.children[comment_count + 2].1.text(source), "with"[:])
    @test.assert_eq(assert_missing_spans(parsed.root(), source), 1)
  }
}
