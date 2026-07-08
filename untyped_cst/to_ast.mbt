///|
fn ParseResult::diagnostics_array(self : ParseResult) -> Array[Report] {
  let reports : Array[Report] = []
  for report in self.diagnostics_view() {
    reports.push(report)
  }
  reports
}

///|
fn conversion_report(root : CstNode, msg : String) -> Report {
  Report::{ loc: root.loc, msg }
}

///|
pub fn ParseResult::to_impls(
  self : ParseResult,
) -> (@syntax.Impls, Array[Report]) {
  let diagnostics = self.diagnostics_array()
  if diagnostics.length() > 0 {
    return (@list.empty(), diagnostics)
  }
  match self.root().kind {
    SourceFile => ()
    _ =>
      return (
        @list.empty(),
        [conversion_report(self.root(), "expected structure parse result")],
      )
  }
  let impls = lower_impls_from_cst(self.root())
  attach_docstrings_to_impls(self.docstrings_view(), impls)
  (impls, [])
}

///|
pub fn ParseResult::to_expr(
  self : ParseResult,
) -> (@syntax.Expr?, Array[Report]) {
  let diagnostics = self.diagnostics_array()
  if diagnostics.length() > 0 {
    return (None, diagnostics)
  }
  match self.root().kind {
    ExpressionFragment => ()
    _ =>
      return (
        None,
        [conversion_report(self.root(), "expected expression parse result")],
      )
  }
  (Some(lower_expr(self.root())), [])
}
