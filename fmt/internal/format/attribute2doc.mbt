///|
fn attr_id2doc(id : @attribute.Id, attribute_name~ : Bool = false) -> Doc {
  let { qual, name, } = id
  let qual = if qual is Some(q) {
    (if attribute_name { empty } else { text("@") }) + text(q) + text(".")
  } else {
    empty
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
fn attr_expr2doc(expr : @attribute.Expr, attribute_name~ : Bool = false) -> Doc {
  match expr {
    Apply(id, props) =>
      attr_id2doc(id, attribute_name~) +
      char('(') +
      separate_map(props, char(',') + space, attr_prop2doc) +
      char(')')
    String(s) => @pp.string(s)
    Ident(id) => attr_id2doc(id, attribute_name~)
    Array(elems) =>
      char('[') +
      separate_map(elems, char(',') + space, fn(expr) { attr_expr2doc(expr) }) +
      char(']')
    Bool(bool) => if bool { text("true") } else { text("false") }
  }
}

///|
fn attribute2doc(attr : Attribute) -> Doc {
  let { raw, parsed, .. } = attr
  if parsed is Some(expr) {
    text("#") + attr_expr2doc(expr, attribute_name=true)
  } else {
    text(raw)
  }
}

///|
fn Fmt::attributes(_ : Fmt, attrs : List[Attribute]) -> Doc {
  concat_map(attrs, x => attribute2doc(x) + hardline)
}
