///|
fn assert_missing_spans(
  node : @untyped_cst.CstNode,
  source : String,
) -> Int raise {
  let mut count = 0
  if node.kind is @untyped_cst.NodeKind::Missing {
    @test.assert_eq(node.loc.start, node.loc.end)
    let (start, end) = node.source_span
    @test.assert_eq(start, end)
    @test.assert_eq(start >= 0 && end <= source.length(), true)
    @test.assert_eq(source[start:end], ""[:])
    @test.assert_eq(node.text(source), ""[:])
    count += 1
  }
  for (_, child) in node.children {
    count += assert_missing_spans(child, source)
  }
  count
}

///|
test "missing spread expressions use the end of the spread as fallback" {
  for prefix in ["", "// 🐇\n", "{ \"🐇\"; "] {
    for record in ["{ .. }", "{ x: y, z, .. }"] {
      let suffix = if prefix == "{ \"🐇\"; " { " }" } else { "" }
      let source = prefix + record + suffix
      let parsed = @untyped_cst.parse_expression(source)
      // Preserve the existing diagnostics, including the silent missing value.
      @test.assert_eq(
        parsed.diagnostics_view().length(),
        if record == "{ .. }" {
          0
        } else {
          1
        },
      )
      @test.assert_eq(assert_missing_spans(parsed.root(), source) > 0, true)
      let spread = api_find_node(parsed.root(), node => {
        node.kind is Field_RecordSpread
      }).unwrap()
      let missing = spread.first_child_with_label("expr").unwrap()
      @test.assert_eq(missing.kind is @untyped_cst.NodeKind::Missing, true)
      @test.assert_eq(missing.loc, @node.empty_loc(spread.loc.end))
      @test.assert_eq(
        missing.source_span,
        @node.empty_span(spread.source_span.1),
      )
      if prefix != "" {
        @test.assert_eq(missing.source_span.0, missing.loc.start.cnum + 1)
      }
    }
  }
}

///|
test "missing range patterns use the end of the range as fallback" {
  for value in ["x", "\"🐇\""] {
    for op in ["..<", "..="] {
      let source = "match \{value} { 1\{op} => 0 }"
      let parsed = @untyped_cst.parse_expression(source)
      @test.assert_eq(parsed.diagnostics_view().length(), 0)
      @test.assert_eq(assert_missing_spans(parsed.root(), source) > 0, true)
      let range = api_find_node(parsed.root(), node => {
        node.kind is Pattern_Range
      }).unwrap()
      let missing = range.first_child_with_label("rhs").unwrap()
      @test.assert_eq(missing.kind is @untyped_cst.NodeKind::Missing, true)
      @test.assert_eq(missing.loc, @node.empty_loc(range.loc.end))
      @test.assert_eq(
        missing.source_span,
        @node.empty_span(range.source_span.1),
      )
      if value != "x" {
        @test.assert_eq(missing.source_span.0, missing.loc.start.cnum + 1)
      }
    }
  }
}

///|
test "missing delimiters and syntax retain zero width ranges recursively" {
  for
    (source, structure) in [
      ("f(1, 2", false),
      ("f(\"🐇\"", false),
      ("[1, 2", false),
      ("{ let x = 1; x", false),
      ("fn f(", true),
      ("// 🐇\nfn f(", true),
      ("let = 1", true),
    ] {
    let parsed = if structure {
      @untyped_cst.parse_structure(source)
    } else {
      @untyped_cst.parse_expression(source)
    }
    @test.assert_eq(parsed.diagnostics_view().is_empty(), false)
    guard assert_missing_spans(parsed.root(), source) > 0 else {
      fail("expected Missing node in \{source}")
    }
  }
}

///|
test "missing call delimiter retains its precise UTF-16 recovery position" {
  let source = "f(\"🐇\""
  let parsed = @untyped_cst.parse_expression(source)
  let args = api_find_node(parsed.root(), node => node.kind is List_ArgumentList).unwrap()
  let missing = args.first_child_with_label("close").unwrap()
  @test.assert_eq(missing.kind is @untyped_cst.NodeKind::Missing, true)
  refactor_assert_source_range(
    missing,
    source,
    "",
    (6, 6),
    (1, 0, 5),
    (1, 0, 5),
  )
}
