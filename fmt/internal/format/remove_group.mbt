///|
priv struct RemoveGroup {}

///|
extend RemoveGroup with MapVisitor::{visit_Expr, visit_Impl}

///|
impl MapVisitor for RemoveGroup with fn visit_Expr_Group(
  self,
  expr~,
  group~,
  loc~,
) {
  ignore((group, loc))
  self.visit_Expr(expr)
}

///|
fn RemoveGroup::process(impls : @syntax.Impls) -> @syntax.Impls {
  let self = RemoveGroup::{  }
  impls.map(x => self.visit_Impl(x))
}
