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
    Missing => "Missing"
    Leaf(_) => "Leaf"
    _ => "\{Repr(self)}"
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
    Leaf(payload) =>
      Json::object({
        "kind": Json::string(self.kind.test_name()),
        "payload": Json::string("\{Repr(payload)}"),
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
