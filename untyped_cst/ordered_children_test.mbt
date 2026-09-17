///|
fn assert_regex_binding_comments(
  before : ArrayView[(String, Int)],
  after : ArrayView[(String, Int)],
) -> Unit raise {
  // Explicit character widths distinguish locations from UTF-16 source spans.
  let pieces = [("name", "before", 6)]
  for (text, width) in before {
    pieces.push(("trivia", text, width))
  }
  pieces.push(("op", "=", 1))
  for (text, width) in after {
    pieces.push(("trivia", text, width))
  }
  pieces.push(("binder", "head", 4))
  let prefix = "s =~ (re\"a\", "
  let mut source = prefix
  for i, (label, text, _) in pieces {
    source += text
    if i + 1 < pieces.length() {
      source += if label == "trivia" { "\n " } else { " " }
    }
  }
  source += ")"
  let binding = refactor_find_node(
    source,
    kind => kind is Binding_Regex,
    expression=true,
  )
  assert_expr_ast_equiv(source)
  let labels = pieces.map(piece => Some(piece.0))
  labels.append([Some("semantic_label"), Some("semantic_binder")])
  @test.assert_eq(refactor_labels(binding), labels)
  let mut offset = prefix.length()
  let mut cnum = offset
  let mut line = 1
  let mut bol = 0
  for i, (label, text, width) in pieces {
    let child = binding.children[i].1
    refactor_assert_source_range(
      child,
      source,
      text,
      (offset, offset + text.length()),
      (line, bol, cnum),
      (line, bol, cnum + width),
    )
    offset += text.length()
    cnum += width
    if label == "trivia" {
      @test.assert_eq(child.kind is @untyped_cst.NodeKind::Syntax_Comment, true)
      line += 1
      bol = cnum + 1
      offset += 2
      cnum += 2
    } else if i + 1 < pieces.length() {
      offset += 1
      cnum += 1
    }
  }
  refactor_assert_source_range(
    binding,
    source,
    source[prefix.length():offset].to_owned(),
    (prefix.length(), offset),
    (1, 0, prefix.length()),
    (line, bol, cnum),
  )
  for
    (semantic, syntax) in [
      ("semantic_label", "name"),
      ("semantic_binder", "binder"),
    ] {
    let metadata = binding.first_child_with_label(semantic).unwrap()
    let token = binding.first_child_with_label(syntax).unwrap()
    @test.assert_eq(metadata.text(source), token.text(source))
    @test.assert_eq(metadata.loc, token.loc)
    @test.assert_eq(metadata.source_span, token.source_span)
  }
}

///|
test "regex binding retains comments around the equals sign" {
  assert_regex_binding_comments([("// keep", 7)], [("// next", 7)])
  assert_regex_binding_comments([("// keep 🐇", 9), ("// next 🦀", 9)], [
    ("// keep 🐇", 9),
    ("// next 🦀", 9),
  ])
}

///|
test "regex binding retains single and consecutive comments before equals" {
  for
    comments in [
      [("// keep", 7)],
      [("// keep", 7), ("// next", 7)],
      [("// keep 🐇", 9), ("// next 🦀", 9)],
    ] {
    assert_regex_binding_comments(comments, [])
  }
}

///|
test "regex binding retains single and consecutive comments after equals" {
  for
    comments in [
      [("// keep", 7)],
      [("// keep", 7), ("// next", 7)],
      [("// keep 🐇", 9), ("// next 🦀", 9)],
    ] {
    assert_regex_binding_comments([], comments)
  }
}

///|
fn assert_type_parameter_recovery(
  parameter : String,
  expected : ArrayView[(String, String, Int)],
) -> Unit raise {
  let source = "type T[" + parameter + "] = Int"
  let parsed = @untyped_cst.parse_structure(source)
  let reports = reports_from_view(parsed.diagnostics_view())
  @test.assert_eq(reports.length(), 1)
  @test.assert_eq(
    reports[0].msg,
    "Unexpected token `:`, you may expect `,`, `]`.",
  )
  @test.assert_eq(
    reports[0].loc,
    @node.mk_loc(@basic.Position::{ fname: "", lnum: 1, bol: 0, cnum: 9, }, @basic.Position::{
      fname: "",
      lnum: 1,
      bol: 0,
      cnum: 10,
    }),
  )
  let (impls, lowering_reports) = parsed.to_impls()
  @test.assert_eq(impls.length(), 0)
  @test.assert_eq(
    diagnostics_equiv_reports_json(lowering_reports),
    diagnostics_equiv_reports_json(reports),
  )
  guard api_find_node(parsed.root(), node => node.kind is Parameter_TypeDecl)
    is Some(param) else {
    fail("expected type parameter")
  }
  @test.assert_eq(param.children.length(), expected.length())
  for i, (label, text, start) in expected {
    @test.assert_eq(param.children[i].0, Some(label))
    refactor_assert_source_range(
      param.children[i].1,
      source,
      text,
      (start, start + text.length()),
      (1, 0, start),
      (1, 0, start + text.length()),
    )
  }
  let (_, last_text, last_start) = expected[expected.length() - 1]
  let end = last_start + last_text.length()
  refactor_assert_source_range(
    param,
    source,
    source[7:end].to_owned(),
    (7, end),
    (1, 0, 7),
    (1, 0, end),
  )
}

///|
test "type parameter recovery retains every constraint" {
  assert_type_parameter_recovery("A : X + Y", [
    ("name", "A", 7),
    ("op", ":", 9),
    ("type", "X", 11),
    ("op", "+", 13),
    ("type", "Y", 15),
  ])
}

///|
test "type parameter recovery retains multiple constraint separators" {
  assert_type_parameter_recovery("A : X + Y + Z", [
    ("name", "A", 7),
    ("op", ":", 9),
    ("type", "X", 11),
    ("op", "+", 13),
    ("type", "Y", 15),
    ("op", "+", 17),
    ("type", "Z", 19),
  ])
}

///|
test "type parameter recovery retains a separator without a following type" {
  assert_type_parameter_recovery("A : X + ", [
    ("name", "A", 7),
    ("op", ":", 9),
    ("type", "X", 11),
    ("op", "+", 13),
  ])
  assert_type_parameter_recovery("A : X + Y + ", [
    ("name", "A", 7),
    ("op", ":", 9),
    ("type", "X", 11),
    ("op", "+", 13),
    ("type", "Y", 15),
    ("op", "+", 17),
  ])
}

///|
fn assert_incomplete_constructor_package(
  comments : ArrayView[(String, Int)],
) -> Unit raise {
  let mut source = if comments.is_empty() { "T::" } else { "T:: " }
  for (text, _) in comments {
    source += text + "\n "
  }
  source += "@p"
  let parsed = @untyped_cst.parse_expression(source)
  let reports = reports_from_view(parsed.diagnostics_view())
  @test.assert_eq(reports.length(), 1)
  @test.assert_eq(
    reports[0].msg,
    "Unexpected end of file, missing id (uppercase start) here.",
  )
  let (expr, lowering_reports) = parsed.to_expr()
  @test.assert_eq(expr is None, true)
  @test.assert_eq(
    diagnostics_equiv_reports_json(lowering_reports),
    diagnostics_equiv_reports_json(reports),
  )
  let node = parsed.root().first_child_with_label("expr").unwrap()
  @test.assert_eq(node.kind is @untyped_cst.NodeKind::Expr_Method, true)
  let labels = [Some("type"), Some("op")]
  for _ in comments {
    labels.push(Some("trivia"))
  }
  labels.append([Some("pkg"), Some("field")])
  @test.assert_eq(refactor_labels(node), labels)
  refactor_assert_source_range(
    node.children[0].1,
    source,
    "T",
    (0, 1),
    (1, 0, 0),
    (1, 0, 1),
  )
  refactor_assert_source_range(
    node.children[1].1,
    source,
    "::",
    (1, 3),
    (1, 0, 1),
    (1, 0, 3),
  )
  let mut offset = if comments.is_empty() { 3 } else { 4 }
  let mut cnum = offset
  let mut bol = 0
  for i, (text, width) in comments {
    let comment = node.children[i + 2].1
    @test.assert_eq(comment.kind is @untyped_cst.NodeKind::Syntax_Comment, true)
    refactor_assert_source_range(
      comment,
      source,
      text,
      (offset, offset + text.length()),
      (i + 1, bol, cnum),
      (i + 1, bol, cnum + width),
    )
    offset += text.length() + 2
    cnum += width + 2
    bol = cnum - 1
  }
  let line = comments.length() + 1
  refactor_assert_source_range(
    node.first_child_with_label("pkg").unwrap(),
    source,
    "@p",
    (offset, offset + 2),
    (line, bol, cnum),
    (line, bol, cnum + 2),
  )
  let field = node.first_child_with_label("field").unwrap()
  @test.assert_eq(field.kind is @untyped_cst.NodeKind::Missing, true)
  refactor_assert_source_range(
    field,
    source,
    "",
    (offset + 2, offset + 2),
    (line, bol, cnum + 2),
    (line, bol, cnum + 2),
  )
  @test.assert_eq(reports[0].loc, field.loc)
  refactor_assert_source_range(
    node,
    source,
    source,
    (0, source.length()),
    (1, 0, 0),
    (line, bol, cnum + 2),
  )
}

///|
test "incomplete qualified constructor retains its package" {
  assert_incomplete_constructor_package([])
}

///|
test "incomplete qualified constructor retains trivia before its package" {
  for
    comments in [
      [("// keep", 7)],
      [("// keep", 7), ("// next", 7)],
      [("// keep 🐇", 9), ("// next 🦀", 9)],
    ] {
    assert_incomplete_constructor_package(comments)
  }
}
