///|
fn find_regex_literal_node(node : CstNode) -> CstNode? {
  if node.kind is RegexPattern_Literal {
    return Some(node)
  }
  for entry in node.children {
    let (_, child) = entry
    if find_regex_literal_node(child) is Some(found) {
      return Some(found)
    }
  }
  None
}

///|
fn lowered_regex_literal(source : String) -> @syntax.RegexPattern raise Error {
  let result = parse_expression(source)
  guard find_regex_literal_node(result.root()) is Some(node) else {
    fail("expected a regex literal CST node")
  }
  lower_regex_pattern(node)
}

///|
test "lower recovered bare string regex literal" {
  guard lowered_regex_literal("subject =~ \"bare\"")
    is @syntax.RegexPattern::Literal(lit~, recovered_~, loc=_) else {
    fail("expected a lowered regex literal")
  }
  @test.assert_eq(lit, "bare")
  @test.assert_eq(recovered_, true)
}

///|
test "lower regular regex literal" {
  guard lowered_regex_literal("subject =~ re\"regular\"")
    is @syntax.RegexPattern::Literal(lit~, recovered_~, loc=_) else {
    fail("expected a lowered regex literal")
  }
  @test.assert_eq(lit, "regular")
  @test.assert_eq(recovered_, false)
}
