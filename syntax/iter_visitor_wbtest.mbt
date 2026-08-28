///|
struct BinderCounter {
  mut count : Int
}

///|
extend BinderCounter with IterVisitor::{visit_Pattern}

///|
impl IterVisitor for BinderCounter with fn visit_Binder(self, _) {
  self.count += 1
}

///|
test "IterVisitor defaults delegate through base" {
  let visitor : BinderCounter = { count: 0, }
  let pattern : Pattern = Alias(
    pat=Any(loc=dummy_loc),
    alias_={ name: "alias", loc: dummy_loc, },
    loc=dummy_loc,
  )
  visitor.visit_Pattern(pattern)
  @test.assert_eq(visitor.count, 1)
}
