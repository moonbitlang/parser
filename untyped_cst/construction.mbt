///|
fn CstNode::first_child_with_label_from(
  children : Array[(String?, CstNode)],
  label : String,
) -> CstNode? {
  for entry in children {
    match entry {
      (Some(actual), child) if actual == label => return Some(child)
      _ => ()
    }
  }
  None
}

///|
fn CstNode::children_with_label_from(
  children : Array[(String?, CstNode)],
  label : String,
) -> Array[CstNode] {
  let result : Array[CstNode] = []
  for entry in children {
    match entry {
      (Some(actual), child) if actual == label => result.push(child)
      _ => ()
    }
  }
  result
}

///|
fn CstNode::payload_from_children(
  children : Array[(String?, CstNode)],
) -> NodePayload? {
  match CstNode::first_child_with_label_from(children, "payload") {
    Some({ kind: Leaf(payload), .. }) => Some(payload)
    _ => None
  }
}

///|
fn node(
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  builder(loc, source_span, children)
}

///|
fn node_like(
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  source : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for entry in source.children {
    children.push(entry)
  }
  builder(source.loc, source.source_span, children)
}

///|
fn leaf_node(
  payload : NodePayload,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  CstNode::leaf(payload~, loc~, source_span~)
}

///|
fn push_payload_leaf(
  children : Array[(String?, CstNode)],
  payload : NodePayload,
  loc : Location,
  source_span : (Int, Int),
) -> Unit {
  children.push((Some("payload"), leaf_node(payload, loc, source_span)))
}

///|
fn node_with_payload(
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
  payload : NodePayload,
) -> CstNode {
  push_payload_leaf(children, payload, loc, source_span)
  builder(loc, source_span, children)
}

///|
fn scalar_node(
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  loc : Location,
  source_span : (Int, Int),
  payload : NodePayload,
) -> CstNode {
  node_with_payload(builder, loc, source_span, [], payload)
}

///|
fn marker_node(
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  pos : Position,
  offset : Int,
) -> CstNode {
  builder(empty_loc(pos), empty_span(offset), [])
}

///|
fn bool_marker_node(
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  pos : Position,
  offset : Int,
  value : Bool,
) -> CstNode {
  scalar_node(
    builder,
    empty_loc(pos),
    empty_span(offset),
    NodePayload::Bool(value),
  )
}

///|
fn missing_node(_kind : TokenKind, pos : Position, offset : Int) -> CstNode {
  CstNode::missing(loc=empty_loc(pos), source_span=empty_span(offset))
}

///|
// Constructor fallbacks have no precise token position. Anchor them at the
// parent's end, keeping character positions separate from UTF-16 offsets.
fn missing_at_end(loc~ : Location, source_span~ : (Int, Int)) -> CstNode {
  CstNode::missing(
    loc=empty_loc(loc.end),
    source_span=empty_span(source_span.1),
  )
}

///|
fn error_node(
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  (fn(loc, source_span, children) {
    let entries = recovery_children(children)
    @node.CstNode::error(loc~, source_span~, entries~)
  })(loc, source_span, children)
}

///|
fn push_child(
  children : Array[(String?, CstNode)],
  label : String?,
  child : CstNode,
) -> Unit {
  children.push((label, child))
}

///|
fn push_children(
  children : Array[(String?, CstNode)],
  more : Array[(String?, CstNode)],
) -> Unit {
  for child in more {
    children.push(child)
  }
}

///|
fn token_node_with_children(
  token : Token,
  start : Position,
  end : Position,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  let entries : Array[@node.CstTokenChild] = []
  for (label, child) in children {
    match label {
      Some("flag") => entries.push(Flag(child))
      Some("name_value") => entries.push(NameValue(child))
      Some("operator") => entries.push(Operator(child))
      Some("constant") => entries.push(Constant(child))
      Some("accessor") => entries.push(Accessor(child))
      Some("trailing") => entries.push(Trailing(child))
      Some("attribute") => entries.push(Attribute(child))
      Some("comment") => entries.push(Comment(child))
      Some("segment") => entries.push(Segment(child))
      Some("payload") =>
        match child.kind {
          Leaf(payload) => entries.push(Payload(payload))
          _ => abort("expected a token payload leaf")
        }
      _ => abort("unsupported token child label")
    }
  }
  if @node.token_payload(token) is Some(payload) {
    entries.push(Payload(payload))
  }
  CstNode::syntax_token(
    token_kind=token.kind(),
    loc=mk_loc(start, end),
    source_span~,
    entries~,
  )
}

///|
fn root_children(children : Array[(String?, CstNode)]) -> Array[CstRootChild] {
  let entries : Array[CstRootChild] = []
  for entry in children {
    let (label, child) = entry
    match label {
      Some("eof") => entries.push(CstRootChild::Eof(child))
      Some("error") => entries.push(CstRootChild::Error(child))
      Some("expr") => entries.push(CstRootChild::Expr(child))
      Some("item") => entries.push(CstRootChild::Item(child))
      Some("sep") => entries.push(CstRootChild::Sep(child))
      Some("trivia") => entries.push(CstRootChild::Trivia(child))
      _ => abort("unsupported CST child label")
    }
  }
  entries
}

///|
fn declaration_children(
  children : Array[(String?, CstNode)],
) -> Array[CstDeclarationChild] {
  let entries : Array[CstDeclarationChild] = []
  for entry in children {
    let (label, child) = entry
    match label {
      Some("async") => entries.push(CstDeclarationChild::Async(child))
      Some("attrs") => entries.push(CstDeclarationChild::Attrs(child))
      Some("body") => entries.push(CstDeclarationChild::Body(child))
      Some("clause") => entries.push(CstDeclarationChild::Clause(child))
      Some("decl_body_kind") =>
        entries.push(CstDeclarationChild::DeclBodyKind(child))
      Some("declare") => entries.push(CstDeclarationChild::Declare(child))
      Some("deriving") => entries.push(CstDeclarationChild::Deriving(child))
      Some("effect_op") => entries.push(CstDeclarationChild::EffectOp(child))
      Some("error") => entries.push(CstDeclarationChild::Error(child))
      Some("expr") => entries.push(CstDeclarationChild::Expr(child))
      Some("kind") => entries.push(CstDeclarationChild::Kind(child))
      Some("kw") => entries.push(CstDeclarationChild::Kw(child))
      Some("language") => entries.push(CstDeclarationChild::Language(child))
      Some("language_token") =>
        entries.push(CstDeclarationChild::LanguageToken(child))
      Some("method") => entries.push(CstDeclarationChild::Method(child))
      Some("name") => entries.push(CstDeclarationChild::Name(child))
      Some("nocancel") => entries.push(CstDeclarationChild::Nocancel(child))
      Some("object") => entries.push(CstDeclarationChild::Object(child))
      Some("op") => entries.push(CstDeclarationChild::Op(child))
      Some("params") => entries.push(CstDeclarationChild::Params(child))
      Some("pattern") => entries.push(CstDeclarationChild::Pattern(child))
      Some("quantifier") => entries.push(CstDeclarationChild::Quantifier(child))
      Some("semantic_name") =>
        entries.push(CstDeclarationChild::SemanticName(child))
      Some("semantic_type_name") =>
        entries.push(CstDeclarationChild::SemanticTypeName(child))
      Some("stub") => entries.push(CstDeclarationChild::Stub(child))
      Some("stub_token") => entries.push(CstDeclarationChild::StubToken(child))
      Some("test_name") => entries.push(CstDeclarationChild::TestName(child))
      Some("trait") => entries.push(CstDeclarationChild::Trait(child))
      Some("trivia") => entries.push(CstDeclarationChild::Trivia(child))
      Some("type") => entries.push(CstDeclarationChild::Type_(child))
      Some("type_desc") => entries.push(CstDeclarationChild::TypeDesc(child))
      Some("type_name") => entries.push(CstDeclarationChild::TypeName(child))
      Some("vis") => entries.push(CstDeclarationChild::Vis(child))
      Some("where") => entries.push(CstDeclarationChild::Where(child))
      _ => abort("unsupported CST child label")
    }
  }
  entries
}

///|
fn list_children(children : Array[(String?, CstNode)]) -> Array[CstListChild] {
  let entries : Array[CstListChild] = []
  for entry in children {
    let (label, child) = entry
    match label {
      Some("case") => entries.push(CstListChild::Case(child))
      Some("close") => entries.push(CstListChild::Close(child))
      Some("error") => entries.push(CstListChild::Error(child))
      Some("item") => entries.push(CstListChild::Item(child))
      Some("open") => entries.push(CstListChild::Open(child))
      Some("sep") => entries.push(CstListChild::Sep(child))
      Some("trailing") => entries.push(CstListChild::Trailing(child))
      Some("trivia") => entries.push(CstListChild::Trivia(child))
      _ => abort("unsupported CST child label")
    }
  }
  entries
}

///|
fn multiline_children(
  children : Array[(String?, CstNode)],
) -> Array[CstMultilineChild] {
  let entries : Array[CstMultilineChild] = []
  for entry in children {
    let (label, child) = entry
    match label {
      Some("element") => entries.push(CstMultilineChild::Element(child))
      Some("token") => entries.push(CstMultilineChild::Token(child))
      Some("trivia") => entries.push(CstMultilineChild::Trivia(child))
      _ => abort("unsupported CST child label")
    }
  }
  entries
}

///|
fn recovery_children(
  children : Array[(String?, CstNode)],
) -> Array[CstRecoveryChild] {
  let entries : Array[CstRecoveryChild] = []
  for entry in children {
    let (label, child) = entry
    match label {
      Some("accessor") => entries.push(CstRecoveryChild::Accessor(child))
      Some("alias") => entries.push(CstRecoveryChild::Alias(child))
      Some("arg") => entries.push(CstRecoveryChild::Arg(child))
      Some("array_kind") => entries.push(CstRecoveryChild::ArrayKind(child))
      Some("as") => entries.push(CstRecoveryChild::As(child))
      Some("async") => entries.push(CstRecoveryChild::Async(child))
      Some("attr") => entries.push(CstRecoveryChild::Attr(child))
      Some("attribute") => entries.push(CstRecoveryChild::Attribute(child))
      Some("attrs") => entries.push(CstRecoveryChild::Attrs(child))
      Some("augmented") => entries.push(CstRecoveryChild::Augmented(child))
      Some("binder") => entries.push(CstRecoveryChild::Binder(child))
      Some("binding") => entries.push(CstRecoveryChild::Binding(child))
      Some("binding_type") => entries.push(CstRecoveryChild::BindingType(child))
      Some("bindings") => entries.push(CstRecoveryChild::Bindings(child))
      Some("body") => entries.push(CstRecoveryChild::Body(child))
      Some("bytes") => entries.push(CstRecoveryChild::Bytes(child))
      Some("case") => entries.push(CstRecoveryChild::Case(child))
      Some("clause") => entries.push(CstRecoveryChild::Clause(child))
      Some("close") => entries.push(CstRecoveryChild::Close(child))
      Some("comment") => entries.push(CstRecoveryChild::Comment(child))
      Some("component") => entries.push(CstRecoveryChild::Component(child))
      Some("cond") => entries.push(CstRecoveryChild::Cond(child))
      Some("conditional") => entries.push(CstRecoveryChild::Conditional(child))
      Some("const") => entries.push(CstRecoveryChild::Const(child))
      Some("constant") => entries.push(CstRecoveryChild::Constant(child))
      Some("constructor_extra_info") =>
        entries.push(CstRecoveryChild::ConstructorExtraInfo(child))
      Some("constructor_tag") =>
        entries.push(CstRecoveryChild::ConstructorTag(child))
      Some("consumed_by_docstring") =>
        entries.push(CstRecoveryChild::ConsumedByDocstring(child))
      Some("content") => entries.push(CstRecoveryChild::Content(child))
      Some("continue") => entries.push(CstRecoveryChild::Continue(child))
      Some("decl_body_kind") =>
        entries.push(CstRecoveryChild::DeclBodyKind(child))
      Some("declare") => entries.push(CstRecoveryChild::Declare(child))
      Some("default") => entries.push(CstRecoveryChild::Default(child))
      Some("deriving") => entries.push(CstRecoveryChild::Deriving(child))
      Some("dot_dot_binder") =>
        entries.push(CstRecoveryChild::DotDotBinder(child))
      Some("effect_op") => entries.push(CstRecoveryChild::EffectOp(child))
      Some("element") => entries.push(CstRecoveryChild::Element(child))
      Some("else") => entries.push(CstRecoveryChild::Else_(child))
      Some("else_kw") => entries.push(CstRecoveryChild::ElseKw(child))
      Some("end") => entries.push(CstRecoveryChild::End(child))
      Some("eof") => entries.push(CstRecoveryChild::Eof(child))
      Some("error") => entries.push(CstRecoveryChild::Error(child))
      Some("exclamation") => entries.push(CstRecoveryChild::Exclamation(child))
      Some("expr") => entries.push(CstRecoveryChild::Expr(child))
      Some("faked") => entries.push(CstRecoveryChild::Faked(child))
      Some("field") => entries.push(CstRecoveryChild::Field(child))
      Some("flag") => entries.push(CstRecoveryChild::Flag(child))
      Some("fn_kind") => entries.push(CstRecoveryChild::FnKind(child))
      Some("func") => entries.push(CstRecoveryChild::Func(child))
      Some("group") => entries.push(CstRecoveryChild::Group(child))
      Some("guard") => entries.push(CstRecoveryChild::Guard(child))
      Some("has_try") => entries.push(CstRecoveryChild::HasTry(child))
      Some("head") => entries.push(CstRecoveryChild::Head(child))
      Some("hole_kind") => entries.push(CstRecoveryChild::HoleKind(child))
      Some("index") => entries.push(CstRecoveryChild::Index(child))
      Some("init") => entries.push(CstRecoveryChild::Init(child))
      Some("item") => entries.push(CstRecoveryChild::Item(child))
      Some("key") => entries.push(CstRecoveryChild::Key(child))
      Some("kind") => entries.push(CstRecoveryChild::Kind(child))
      Some("kw") => entries.push(CstRecoveryChild::Kw(child))
      Some("label") => entries.push(CstRecoveryChild::Label(child))
      Some("language") => entries.push(CstRecoveryChild::Language(child))
      Some("language_token") =>
        entries.push(CstRecoveryChild::LanguageToken(child))
      Some("leading_blank_line") =>
        entries.push(CstRecoveryChild::LeadingBlankLine(child))
      Some("lex_literal") => entries.push(CstRecoveryChild::LexLiteral(child))
      Some("lex_scan_case_pattern") =>
        entries.push(CstRecoveryChild::LexScanCasePattern(child))
      Some("lhs") => entries.push(CstRecoveryChild::Lhs(child))
      Some("list_comprehension_kind") =>
        entries.push(CstRecoveryChild::ListComprehensionKind(child))
      Some("long_ident") => entries.push(CstRecoveryChild::LongIdent(child))
      Some("match_absent") => entries.push(CstRecoveryChild::MatchAbsent(child))
      Some("method") => entries.push(CstRecoveryChild::Method(child))
      Some("mut") => entries.push(CstRecoveryChild::Mut(child))
      Some("mutable") => entries.push(CstRecoveryChild::Mutable(child))
      Some("name") => entries.push(CstRecoveryChild::Name(child))
      Some("name_value") => entries.push(CstRecoveryChild::NameValue(child))
      Some("nocancel") => entries.push(CstRecoveryChild::Nocancel(child))
      Some("object") => entries.push(CstRecoveryChild::Object(child))
      Some("op") => entries.push(CstRecoveryChild::Op(child))
      Some("open") => entries.push(CstRecoveryChild::Open(child))
      Some("openness") => entries.push(CstRecoveryChild::Openness(child))
      Some("operator") => entries.push(CstRecoveryChild::Operator(child))
      Some("param") => entries.push(CstRecoveryChild::Param(child))
      Some("params") => entries.push(CstRecoveryChild::Params(child))
      Some("path") => entries.push(CstRecoveryChild::Path(child))
      Some("pattern") => entries.push(CstRecoveryChild::Pattern(child))
      Some("pattern_group") =>
        entries.push(CstRecoveryChild::PatternGroup(child))
      Some("payload") => entries.push(CstRecoveryChild::Payload(child))
      Some("pipe") => entries.push(CstRecoveryChild::Pipe(child))
      Some("pkg") => entries.push(CstRecoveryChild::Pkg(child))
      Some("pun") => entries.push(CstRecoveryChild::Pun(child))
      Some("qualifier") => entries.push(CstRecoveryChild::Qualifier(child))
      Some("quantifier") => entries.push(CstRecoveryChild::Quantifier(child))
      Some("quantifier_kind") =>
        entries.push(CstRecoveryChild::QuantifierKind(child))
      Some("question") => entries.push(CstRecoveryChild::Question(child))
      Some("range_kind") => entries.push(CstRecoveryChild::RangeKind(child))
      Some("raw_payload") => entries.push(CstRecoveryChild::RawPayload(child))
      Some("result") => entries.push(CstRecoveryChild::Result(child))
      Some("return_self") => entries.push(CstRecoveryChild::ReturnSelf(child))
      Some("rhs") => entries.push(CstRecoveryChild::Rhs(child))
      Some("segment") => entries.push(CstRecoveryChild::Segment(child))
      Some("self") => entries.push(CstRecoveryChild::Self_(child))
      Some("semantic_binder") =>
        entries.push(CstRecoveryChild::SemanticBinder(child))
      Some("semantic_constant") =>
        entries.push(CstRecoveryChild::SemanticConstant(child))
      Some("semantic_label") =>
        entries.push(CstRecoveryChild::SemanticLabel(child))
      Some("semantic_name") =>
        entries.push(CstRecoveryChild::SemanticName(child))
      Some("semantic_type_name") =>
        entries.push(CstRecoveryChild::SemanticTypeName(child))
      Some("sep") => entries.push(CstRecoveryChild::Sep(child))
      Some("start") => entries.push(CstRecoveryChild::Start(child))
      Some("str") => entries.push(CstRecoveryChild::Str(child))
      Some("stub") => entries.push(CstRecoveryChild::Stub(child))
      Some("stub_token") => entries.push(CstRecoveryChild::StubToken(child))
      Some("syntax") => entries.push(CstRecoveryChild::Syntax(child))
      Some("tag") => entries.push(CstRecoveryChild::Tag(child))
      Some("target") => entries.push(CstRecoveryChild::Target(child))
      Some("template") => entries.push(CstRecoveryChild::Template(child))
      Some("test_name") => entries.push(CstRecoveryChild::TestName(child))
      Some("token") => entries.push(CstRecoveryChild::Token(child))
      Some("trailing") => entries.push(CstRecoveryChild::Trailing(child))
      Some("trailing_blank_line") =>
        entries.push(CstRecoveryChild::TrailingBlankLine(child))
      Some("trait") => entries.push(CstRecoveryChild::Trait(child))
      Some("trivia") => entries.push(CstRecoveryChild::Trivia(child))
      Some("try_operator_kind") =>
        entries.push(CstRecoveryChild::TryOperatorKind(child))
      Some("type") => entries.push(CstRecoveryChild::Type_(child))
      Some("type_desc") => entries.push(CstRecoveryChild::TypeDesc(child))
      Some("type_name") => entries.push(CstRecoveryChild::TypeName(child))
      Some("using_kind") => entries.push(CstRecoveryChild::UsingKind(child))
      Some("vis") => entries.push(CstRecoveryChild::Vis(child))
      Some("where") => entries.push(CstRecoveryChild::Where(child))
      Some("wildcard") => entries.push(CstRecoveryChild::Wildcard(child))
      _ => abort("unsupported CST child label")
    }
  }
  entries
}
