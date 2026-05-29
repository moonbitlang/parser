///|
priv struct RemoveGroup {}

///|
impl MapVisitor for RemoveGroup with fn visit_Expr_Group(
  self,
  expr~,
  group~,
  left_delim_loc~,
  right_delim_loc~,
  loc~,
) {
  ignore((group, left_delim_loc, right_delim_loc, loc))
  self.visit_Expr(expr)
}

///|
fn RemoveGroup::process(impls : @syntax.Impls) -> @syntax.Impls {
  let self = RemoveGroup::{  }
  impls.map(x => self.visit_Impl(x))
}
