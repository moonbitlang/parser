///|
fn loc_to_test_json(loc : Location) -> Json {
  { "start": loc.start.cnum, "end": loc.end.cnum }
}

///|
fn span_to_test_json(span : (Int, Int)) -> Json {
  { "start": span.0, "end": span.1 }
}

///|
fn label_to_test_json(label : String?) -> Json {
  if label is Some(label) {
    Json::string(label)
  } else {
    Json::null()
  }
}

///|
fn NodeKind::test_name(self : NodeKind) -> String {
  match self {
    SourceFile => "SourceFile"
    ExpressionFragment => "ExpressionFragment"
    Token(_) => "Token"
    Missing(_) => "Missing"
    Error => "Error"
    kind if kind.is_impl() => "Impl"
    kind if kind.is_expr() => "Expr"
    kind if kind.is_pattern() => "Pattern"
    kind if kind.is_type() => "Type"
    kind if kind.is_lex_pattern() => "LexPattern"
    kind if kind.is_regex_pattern() => "RegexPattern"
    kind if kind.is_case() => "Case"
    kind if kind.is_argument() => "Argument"
    kind if kind.is_stmt() => "Stmt"
    kind if kind.is_decl() => "Decl"
    kind if kind.is_clause() => "Clause"
    kind if kind.is_name() => "Name"
    kind if kind.is_list() => "List"
    kind if kind.is_interp_segment() => "InterpSegment"
    _ => "Error"
  }
}

///|
fn CstNode::to_test_json(self : CstNode, source? : String = "") -> Json {
  let _ = source
  let children : Array[Json] = []
  for i in 0..<self.children.length() {
    let (label, child) = self.children[i]
    children.push(
      Json::object({
        "label": label_to_test_json(label),
        "node": child.to_test_json(source~),
      }),
    )
  }
  match self.kind {
    Missing(expected) =>
      Json::object({
        "kind": Json::string(self.kind.test_name()),
        "expected": Json::string(expected.to_string()),
        "loc": loc_to_test_json(self.loc),
        "source_span": span_to_test_json(self.source_span),
        "children": Json::array(children),
      })
    Token(token) =>
      Json::object({
        "kind": Json::string(self.kind.test_name()),
        "token": token.to_json(),
        "loc": loc_to_test_json(self.loc),
        "source_span": span_to_test_json(self.source_span),
        "children": Json::array(children),
      })
    _ =>
      Json::object({
        "kind": Json::string(self.kind.test_name()),
        "loc": loc_to_test_json(self.loc),
        "source_span": span_to_test_json(self.source_span),
        "children": Json::array(children),
      })
  }
}
