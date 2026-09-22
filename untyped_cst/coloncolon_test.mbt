///|
fn assert_coloncolon_trivia(
  suffix : String,
  pred : (@untyped_cst.NodeKind) -> Bool,
  tail_label : String,
) -> Unit raise {
  for
    (texts, char_widths) in [
      (["// keep"], [7]),
      (["// keep", "// keep"], [7, 7]),
      (["// keep 🐇", "// next 🦀"], [9, 9]),
    ] {
    let source = "T:: " + texts.join("\n ") + "\n " + suffix
    let node = refactor_find_node(source, pred, expression=true)
    let labels = [Some("type"), Some("op")]
    for _ in texts {
      labels.push(Some("trivia"))
    }
    labels.push(Some(tail_label))
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
    let comments = node.children_with_label("trivia")
    @test.assert_eq(comments.length(), texts.length())
    let mut offset = 4
    let mut cnum = 4
    let mut bol = 0
    for i, comment in comments {
      @test.assert_eq(
        comment.kind is @untyped_cst.NodeKind::Syntax_Comment,
        true,
      )
      refactor_assert_source_range(
        comment,
        source,
        texts[i],
        (offset, offset + texts[i].length()),
        (i + 1, bol, cnum),
        (i + 1, bol, cnum + char_widths[i]),
      )
      offset += texts[i].length() + 2
      cnum += char_widths[i] + 2
      bol = cnum - 1
    }
    let line = texts.length() + 1
    refactor_assert_source_range(
      node.first_child_with_label(tail_label).unwrap(),
      source,
      suffix,
      (offset, source.length()),
      (line, bol, cnum),
      (line, bol, cnum + suffix.length()),
    )
    refactor_assert_source_range(
      node,
      source,
      source,
      (0, source.length()),
      (1, 0, 0),
      (line, bol, cnum + suffix.length()),
    )
    assert_expr_ast_equiv(source)
  }
}

///|
test "method references preserve all trivia after ::" {
  assert_coloncolon_trivia("f", kind => kind is Expr_Method, "field")
}

///|
test "record expressions preserve all trivia after ::" {
  assert_coloncolon_trivia("{ x: 1 }", kind => kind is Expr_Record, "body")
}

///|
test "record updates preserve all trivia after ::" {
  assert_coloncolon_trivia(
    "{ ..r, x: 1 }",
    kind => kind is Expr_RecordUpdate,
    "body",
  )
}
