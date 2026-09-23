///|
fn collect_recovered_regex_literals(
  pat : @syntax.RegexPattern,
  literals : Array[(String, Bool)],
) -> Unit {
  match pat {
    Literal(lit~, recovered_~, loc=_) => literals.push((lit, recovered_))
    Reference(_) => ()
    Sequence(pat1~, pat2~, loc=_) | Alternation(pat1~, pat2~, loc=_) => {
      collect_recovered_regex_literals(pat1, literals)
      collect_recovered_regex_literals(pat2, literals)
    }
    Alias(pat~, binder=_, loc=_) =>
      collect_recovered_regex_literals(pat, literals)
  }
}

///|
fn recovered_regex_literals_from_impls(
  impls : @syntax.Impls,
) -> Array[(String, Bool)] {
  let literals : Array[(String, Bool)] = []
  for impl_ in impls {
    match impl_ {
      TopLetDef(expr=RegexMatch(pat~, ..), ..) =>
        collect_recovered_regex_literals(pat, literals)
      TopLetDef(expr=LexScan(cases~, ..), ..) =>
        for case in cases {
          match case.pat {
            Pattern(pat) => collect_recovered_regex_literals(pat, literals)
            Binder(_) | Wildcard(_) => ()
          }
        }
      _ => ()
    }
  }
  literals
}

///|
fn cst_leaf_string(node : @node.CstNode) -> String? {
  for entry in node.children {
    let (label, child) = entry
    if label is Some("payload") {
      match child.kind {
        @node.NodeKind::Leaf(@node.NodePayload::String(value)) =>
          return Some(value)
        _ => ()
      }
    }
  }
  None
}

///|
fn collect_cst_recovered_regex_literals(
  node : @node.CstNode,
  literals : Array[String],
) -> Unit {
  if node.kind is @node.NodeKind::RegexPattern_Literal {
    for entry in node.children {
      let (label, child) = entry
      if label is Some("lex_literal") &&
        child.kind is @node.NodeKind::LexLiteral_String {
        if cst_leaf_string(child) is Some(value) {
          literals.push(value)
        }
      }
    }
  }
  for entry in node.children {
    let (_, child) = entry
    collect_cst_recovered_regex_literals(child, literals)
  }
}

///|
test "bare strings recover as regex literals" {
  let source =
    #|let simple = subject =~ "simple"
    #|let binding = subject =~ ("bound", before=head)
    #|let sequence = subject =~ ("left" + "right",)
    #|let scanner = lexscan input {
    #|  "scan" => ()
    #|  _ => ()
    #|}
  let expected_diagnostic = "Unexpected token string, you may expect regex."
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    name="bare_regex_recovery.mbt",
    parser=Handrolled,
  )
  @test.assert_eq(hand_reports.length(), 5)
  for report in hand_reports {
    @test.assert_eq(report.msg, expected_diagnostic)
  }
  @test.assert_eq(recovered_regex_literals_from_impls(hand_impls), [
    ("simple", true),
    ("bound", true),
    ("left", true),
    ("right", true),
    ("scan", true),
  ])
  let (_, yacc_reports) = @parser.parse_string(
    source,
    name="bare_regex_recovery.mbt",
    parser=MoonYacc,
  )
  if yacc_reports.is_empty() {
    fail("moonyacc parser unexpectedly accepted a bare string regex")
  }
  let cst = @untyped_cst.parse_structure(source, name="bare_regex_recovery.mbt")
  @test.assert_eq(cst.diagnostics_view().length(), 5)
  for report in cst.diagnostics_view() {
    @test.assert_eq(report.msg, expected_diagnostic)
  }
  let cst_literals : Array[String] = []
  collect_cst_recovered_regex_literals(cst.root(), cst_literals)
  @test.assert_eq(cst_literals, ["simple", "bound", "left", "right", "scan"])
}
