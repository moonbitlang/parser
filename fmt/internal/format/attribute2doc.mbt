///|
fn attr_id2doc(id : @attribute.Id) -> Doc {
  let { qual, name } = id
  let qual = match qual {
    None => empty
    Some(q) => text(q) + text(".")
  }
  qual + text(name)
}

///|
fn attr_prop2doc(prop : @attribute.Prop) -> Doc {
  match prop {
    Labeled(label, expr) => text(label) + char('=') + attr_expr2doc(expr)
    Expr(expr) => attr_expr2doc(expr)
  }
}

///|
fn attr_expr2doc(expr : @attribute.Expr) -> Doc {
  match expr {
    Apply(id, props) =>
      attr_id2doc(id) +
      char('(') +
      separate_map(props, char(',') + space, attr_prop2doc) +
      char(')')
    String(s) => @pp.string(s)
    Ident(id) => attr_id2doc(id)
    Bool(bool) => if bool { text("true") } else { text("false") }
  }
}

///|
fn attribute2doc(attr : Attribute) -> Doc {
  let { raw, parsed, .. } = attr
  match parsed {
    None => text(raw)
    Some(expr) => text("#") + attr_expr2doc(expr)
  }
}

///|
fn Fmt::attributes(_ : Fmt, attrs : List[Attribute]) -> Doc {
  concat_map(attrs, x => attribute2doc(x) + hardline)
}
