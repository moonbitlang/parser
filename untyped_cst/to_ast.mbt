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
  Report::{ loc: root.loc, msg, }
}

///|
/// Lowers a successful structure parse result to the syntax AST.
///
/// When this result was produced by `parse_structure` and contains no parse
/// diagnostics, the returned implementation list has its collected docstrings
/// attached and the returned diagnostics are empty. Otherwise, this returns an
/// empty implementation list together with the existing parse diagnostics or
/// a diagnostic reporting that the root has the wrong kind.
pub fn ParseResult::to_impls(
  self : ParseResult,
) -> (@syntax.Impls, Array[Report]) {
  let diagnostics = self.diagnostics_array()
  if diagnostics.length() > 0 {
    return (@list.empty(), diagnostics)
  }
  match self.root().kind {
    Impls => ()
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
/// Lowers a successful expression parse result to the syntax AST.
///
/// When this result was produced by `parse_expression` and contains no parse
/// diagnostics, this returns `Some(expr)` and an empty diagnostics array.
/// Otherwise, it returns `None` together with the existing parse diagnostics or
/// a diagnostic reporting that the root has the wrong kind.
pub fn ParseResult::to_expr(
  self : ParseResult,
) -> (@syntax.Expr?, Array[Report]) {
  let diagnostics = self.diagnostics_array()
  if diagnostics.length() > 0 {
    return (None, diagnostics)
  }
  match self.root().kind {
    Expression => ()
    _ =>
      return (
        None,
        [conversion_report(self.root(), "expected expression parse result")],
      )
  }
  (Some(lower_expr(self.root())), [])
}
