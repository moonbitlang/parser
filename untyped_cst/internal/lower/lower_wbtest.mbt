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
  guard find_regex_literal_node(result.root) is Some(node) else {
    fail("expected a regex literal CST node")
  }
  lower_regex_pattern(node)
}

///|
test "lower recovered bare string regex literal" {
  guard lowered_regex_literal("subject =~ \"bare\"")
    is Literal(lit~, recovered_~, loc=_) else {
    fail("expected a lowered regex literal")
  }
  @test.assert_eq(lit, "bare")
  @test.assert_eq(recovered_, true)
}

///|
test "lower regular regex literal" {
  guard lowered_regex_literal("subject =~ re\"regular\"")
    is Literal(lit~, recovered_~, loc=_) else {
    fail("expected a lowered regex literal")
  }
  @test.assert_eq(lit, "regular")
  @test.assert_eq(recovered_, false)
}

///|
test "pattern trivia is transparent but annotations retain their types" {
  for annotated in [false, true] {
    let suffix = if annotated { " : Int" } else { "" }
    let source = "subject is // keep 🐇\n (x" + suffix + ")"
    let result = parse_expression(source)
    @test.assert_eq(result.diagnostics.length(), 0)
    let constraints = []
    collect_kind(result.root, kind => kind is Pattern_Constraint, constraints)
    @test.assert_eq(constraints.length(), if annotated { 2 } else { 1 })
    let pattern = lower_pattern(constraints[0])
    if annotated {
      guard pattern
        is Constraint(
          pat=Var(binder),
          ty=Name(constr_id={ id: Ident(name="Int"), .. }, ..),
          ..
        ) else {
        fail("expected a real Int constraint around the variable")
      }
      @test.assert_eq(binder.name, "x")
      let constraint = constraints[1]
      // Error recovery can retain the colon without a type child.
      let missing_type = CstNode::pattern_constraint(
        loc=constraint.loc,
        source_span=constraint.source_span,
        entries=[
          Pattern(constraint.first_child_with_label("pattern").unwrap()),
          Op(constraint.first_child_with_label("op").unwrap()),
        ],
      )
      guard lower_pattern(missing_type)
        is Constraint(pat=Var(_), ty=Any(..), ..) else {
        fail("an incomplete annotation must still lower as a constraint")
      }
    } else {
      guard pattern is Var(binder) else { fail("unexpected trivia constraint") }
      @test.assert_eq(binder.name, "x")
      @test.assert_eq(binder.loc.start.lnum, 2)
    }
  }
}
