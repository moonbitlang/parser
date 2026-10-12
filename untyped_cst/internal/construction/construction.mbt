///|
pub fn first_child_with_label_from(
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
pub fn children_with_label_from(
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
pub fn payload_from_children(
  children : Array[(String?, CstNode)],
) -> NodePayload? {
  match first_child_with_label_from(children, "payload") {
    Some({ kind: Leaf(payload), .. }) => Some(payload)
    _ => None
  }
}

///|
pub fn node(
  builder : (Location, (Int32, Int32), Array[(String?, CstNode)]) -> CstNode,
  loc : Location,
  source_span : (Int32, Int32),
  children : Array[(String?, CstNode)],
) -> CstNode {
  builder(loc, source_span, children)
}

///|
pub fn node_like(
  builder : (Location, (Int32, Int32), Array[(String?, CstNode)]) -> CstNode,
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
  source_span : (Int32, Int32),
) -> CstNode {
  CstNode::leaf(payload~, loc~, source_span~)
}

///|
fn push_payload_leaf(
  children : Array[(String?, CstNode)],
  payload : NodePayload,
  loc : Location,
  source_span : (Int32, Int32),
) -> Unit {
  children.push((Some("payload"), leaf_node(payload, loc, source_span)))
}

///|
pub fn node_with_payload(
  builder : (Location, (Int32, Int32), Array[(String?, CstNode)]) -> CstNode,
  loc : Location,
  source_span : (Int32, Int32),
  children : Array[(String?, CstNode)],
  payload : NodePayload,
) -> CstNode {
  push_payload_leaf(children, payload, loc, source_span)
  builder(loc, source_span, children)
}

///|
pub fn scalar_node(
  builder : (Location, (Int32, Int32), Array[(String?, CstNode)]) -> CstNode,
  loc : Location,
  source_span : (Int32, Int32),
  payload : NodePayload,
) -> CstNode {
  node_with_payload(builder, loc, source_span, [], payload)
}

///|
pub fn marker_node(
  builder : (Location, (Int32, Int32), Array[(String?, CstNode)]) -> CstNode,
  pos : Position,
  offset : Int32,
) -> CstNode {
  builder(empty_loc(pos), empty_span(offset), [])
}

///|
pub fn bool_marker_node(
  builder : (Location, (Int32, Int32), Array[(String?, CstNode)]) -> CstNode,
  pos : Position,
  offset : Int32,
  value : Bool,
) -> CstNode {
  scalar_node(builder, empty_loc(pos), empty_span(offset), Bool(value))
}

///|
pub fn missing_node(
  _kind : TokenKind,
  pos : Position,
  offset : Int32,
) -> CstNode {
  CstNode::missing(loc=empty_loc(pos), source_span=empty_span(offset))
}

///|
// Constructor fallbacks have no precise token position. Anchor them at the
// parent's end, keeping character positions separate from UTF-16 offsets.
pub fn missing_at_end(
  loc~ : Location,
  source_span~ : (Int32, Int32),
) -> CstNode {
  CstNode::missing(
    loc=empty_loc(loc.end),
    source_span=empty_span(source_span.1),
  )
}

///|
pub fn error_node(
  loc : Location,
  source_span : (Int32, Int32),
  children : Array[(String?, CstNode)],
) -> CstNode {
  (fn(loc, source_span, children) {
    let entries = recovery_children(children)
    @node.CstNode::error(loc~, source_span~, entries~)
  })(loc, source_span, children)
}

///|
pub fn push_child(
  children : Array[(String?, CstNode)],
  label : String?,
  child : CstNode,
) -> Unit {
  children.push((label, child))
}

///|
pub fn push_children(
  children : Array[(String?, CstNode)],
  more : Array[(String?, CstNode)],
) -> Unit {
  for child in more {
    children.push(child)
  }
}

///|
pub fn token_node_with_children(
  token : Token,
  start : Position,
  end : Position,
  source_span : (Int32, Int32),
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
pub fn root_children(
  children : Array[(String?, CstNode)],
) -> Array[CstRootChild] {
  let entries : Array[CstRootChild] = []
  for entry in children {
    let (label, child) = entry
    match label {
      Some("eof") => entries.push(Eof(child))
      Some("error") => entries.push(Error(child))
      Some("expr") => entries.push(Expr(child))
      Some("item") => entries.push(Item(child))
      Some("sep") => entries.push(Sep(child))
      Some("trivia") => entries.push(Trivia(child))
      _ => abort("unsupported CST child label")
    }
  }
  entries
}

///|
pub fn declaration_children(
  children : Array[(String?, CstNode)],
) -> Array[CstDeclarationChild] {
  let entries : Array[CstDeclarationChild] = []
  for entry in children {
    let (label, child) = entry
    match label {
      Some("async") => entries.push(Async(child))
      Some("attrs") => entries.push(Attrs(child))
      Some("body") => entries.push(Body(child))
      Some("clause") => entries.push(Clause(child))
      Some("decl_body_kind") => entries.push(DeclBodyKind(child))
      Some("declare") => entries.push(Declare(child))
      Some("deriving") => entries.push(Deriving(child))
      Some("effect_op") => entries.push(EffectOp(child))
      Some("error") => entries.push(Error(child))
      Some("expr") => entries.push(Expr(child))
      Some("kind") => entries.push(Kind(child))
      Some("kw") => entries.push(Kw(child))
      Some("language") => entries.push(Language(child))
      Some("language_token") => entries.push(LanguageToken(child))
      Some("method") => entries.push(Method(child))
      Some("name") => entries.push(Name(child))
      Some("nocancel") => entries.push(Nocancel(child))
      Some("object") => entries.push(Object(child))
      Some("op") => entries.push(Op(child))
      Some("params") => entries.push(Params(child))
      Some("pattern") => entries.push(Pattern(child))
      Some("quantifier") => entries.push(Quantifier(child))
      Some("semantic_name") => entries.push(SemanticName(child))
      Some("semantic_type_name") => entries.push(SemanticTypeName(child))
      Some("stub") => entries.push(Stub(child))
      Some("stub_token") => entries.push(StubToken(child))
      Some("test_name") => entries.push(TestName(child))
      Some("trait") => entries.push(Trait(child))
      Some("trivia") => entries.push(Trivia(child))
      Some("type") => entries.push(Type_(child))
      Some("type_desc") => entries.push(TypeDesc(child))
      Some("type_name") => entries.push(TypeName(child))
      Some("vis") => entries.push(Vis(child))
      Some("where") => entries.push(Where(child))
      _ => abort("unsupported CST child label")
    }
  }
  entries
}

///|
pub fn list_children(
  children : Array[(String?, CstNode)],
) -> Array[CstListChild] {
  let entries : Array[CstListChild] = []
  for entry in children {
    let (label, child) = entry
    match label {
      Some("case") => entries.push(Case(child))
      Some("close") => entries.push(Close(child))
      Some("error") => entries.push(Error(child))
      Some("item") => entries.push(Item(child))
      Some("open") => entries.push(Open(child))
      Some("sep") => entries.push(Sep(child))
      Some("trailing") => entries.push(Trailing(child))
      Some("trivia") => entries.push(Trivia(child))
      _ => abort("unsupported CST child label")
    }
  }
  entries
}

///|
pub fn multiline_children(
  children : Array[(String?, CstNode)],
) -> Array[CstMultilineChild] {
  let entries : Array[CstMultilineChild] = []
  for entry in children {
    let (label, child) = entry
    match label {
      Some("element") => entries.push(Element(child))
      Some("token") => entries.push(Token(child))
      Some("trivia") => entries.push(Trivia(child))
      _ => abort("unsupported CST child label")
    }
  }
  entries
}

///|
pub fn recovery_children(
  children : Array[(String?, CstNode)],
) -> Array[CstRecoveryChild] {
  let entries : Array[CstRecoveryChild] = []
  for entry in children {
    let (label, child) = entry
    match label {
      Some("accessor") => entries.push(Accessor(child))
      Some("alias") => entries.push(Alias(child))
      Some("arg") => entries.push(Arg(child))
      Some("array_kind") => entries.push(ArrayKind(child))
      Some("as") => entries.push(As(child))
      Some("async") => entries.push(Async(child))
      Some("attr") => entries.push(Attr(child))
      Some("attribute") => entries.push(Attribute(child))
      Some("attrs") => entries.push(Attrs(child))
      Some("augmented") => entries.push(Augmented(child))
      Some("binder") => entries.push(Binder(child))
      Some("binding") => entries.push(Binding(child))
      Some("binding_type") => entries.push(BindingType(child))
      Some("bindings") => entries.push(Bindings(child))
      Some("body") => entries.push(Body(child))
      Some("bytes") => entries.push(Bytes(child))
      Some("case") => entries.push(Case(child))
      Some("clause") => entries.push(Clause(child))
      Some("close") => entries.push(Close(child))
      Some("comment") => entries.push(Comment(child))
      Some("component") => entries.push(Component(child))
      Some("cond") => entries.push(Cond(child))
      Some("conditional") => entries.push(Conditional(child))
      Some("const") => entries.push(Const(child))
      Some("constant") => entries.push(Constant(child))
      Some("constructor_extra_info") =>
        entries.push(ConstructorExtraInfo(child))
      Some("constructor_tag") => entries.push(ConstructorTag(child))
      Some("consumed_by_docstring") => entries.push(ConsumedByDocstring(child))
      Some("content") => entries.push(Content(child))
      Some("continue") => entries.push(Continue(child))
      Some("decl_body_kind") => entries.push(DeclBodyKind(child))
      Some("declare") => entries.push(Declare(child))
      Some("default") => entries.push(Default(child))
      Some("deriving") => entries.push(Deriving(child))
      Some("dot_dot_binder") => entries.push(DotDotBinder(child))
      Some("effect_op") => entries.push(EffectOp(child))
      Some("element") => entries.push(Element(child))
      Some("else") => entries.push(Else_(child))
      Some("else_kw") => entries.push(ElseKw(child))
      Some("end") => entries.push(End(child))
      Some("eof") => entries.push(Eof(child))
      Some("error") => entries.push(Error(child))
      Some("exclamation") => entries.push(Exclamation(child))
      Some("expr") => entries.push(Expr(child))
      Some("faked") => entries.push(Faked(child))
      Some("field") => entries.push(Field(child))
      Some("flag") => entries.push(Flag(child))
      Some("fn_kind") => entries.push(FnKind(child))
      Some("func") => entries.push(Func(child))
      Some("group") => entries.push(Group(child))
      Some("guard") => entries.push(Guard(child))
      Some("has_try") => entries.push(HasTry(child))
      Some("head") => entries.push(Head(child))
      Some("hole_kind") => entries.push(HoleKind(child))
      Some("index") => entries.push(Index(child))
      Some("init") => entries.push(Init(child))
      Some("item") => entries.push(Item(child))
      Some("key") => entries.push(Key(child))
      Some("kind") => entries.push(Kind(child))
      Some("kw") => entries.push(Kw(child))
      Some("label") => entries.push(Label(child))
      Some("language") => entries.push(Language(child))
      Some("language_token") => entries.push(LanguageToken(child))
      Some("leading_blank_line") => entries.push(LeadingBlankLine(child))
      Some("lex_literal") => entries.push(LexLiteral(child))
      Some("lex_scan_case_pattern") => entries.push(LexScanCasePattern(child))
      Some("lhs") => entries.push(Lhs(child))
      Some("list_comprehension_kind") =>
        entries.push(ListComprehensionKind(child))
      Some("long_ident") => entries.push(LongIdent(child))
      Some("match_absent") => entries.push(MatchAbsent(child))
      Some("method") => entries.push(Method(child))
      Some("mut") => entries.push(Mut(child))
      Some("mutable") => entries.push(Mutable(child))
      Some("name") => entries.push(Name(child))
      Some("name_value") => entries.push(NameValue(child))
      Some("nocancel") => entries.push(Nocancel(child))
      Some("object") => entries.push(Object(child))
      Some("op") => entries.push(Op(child))
      Some("open") => entries.push(Open(child))
      Some("openness") => entries.push(Openness(child))
      Some("operator") => entries.push(Operator(child))
      Some("param") => entries.push(Param(child))
      Some("params") => entries.push(Params(child))
      Some("path") => entries.push(Path(child))
      Some("pattern") => entries.push(Pattern(child))
      Some("pattern_group") => entries.push(PatternGroup(child))
      Some("payload") => entries.push(Payload(child))
      Some("pipe") => entries.push(Pipe(child))
      Some("pkg") => entries.push(Pkg(child))
      Some("pun") => entries.push(Pun(child))
      Some("qualifier") => entries.push(Qualifier(child))
      Some("quantifier") => entries.push(Quantifier(child))
      Some("quantifier_kind") => entries.push(QuantifierKind(child))
      Some("question") => entries.push(Question(child))
      Some("range_kind") => entries.push(RangeKind(child))
      Some("raw_payload") => entries.push(RawPayload(child))
      Some("result") => entries.push(Result(child))
      Some("return_self") => entries.push(ReturnSelf(child))
      Some("rhs") => entries.push(Rhs(child))
      Some("segment") => entries.push(Segment(child))
      Some("self") => entries.push(Self_(child))
      Some("semantic_binder") => entries.push(SemanticBinder(child))
      Some("semantic_constant") => entries.push(SemanticConstant(child))
      Some("semantic_label") => entries.push(SemanticLabel(child))
      Some("semantic_name") => entries.push(SemanticName(child))
      Some("semantic_type_name") => entries.push(SemanticTypeName(child))
      Some("sep") => entries.push(Sep(child))
      Some("start") => entries.push(Start(child))
      Some("str") => entries.push(Str(child))
      Some("stub") => entries.push(Stub(child))
      Some("stub_token") => entries.push(StubToken(child))
      Some("syntax") => entries.push(Syntax(child))
      Some("tag") => entries.push(Tag(child))
      Some("target") => entries.push(Target(child))
      Some("template") => entries.push(Template(child))
      Some("test_name") => entries.push(TestName(child))
      Some("token") => entries.push(Token(child))
      Some("trailing") => entries.push(Trailing(child))
      Some("trailing_blank_line") => entries.push(TrailingBlankLine(child))
      Some("trait") => entries.push(Trait(child))
      Some("trivia") => entries.push(Trivia(child))
      Some("try_operator_kind") => entries.push(TryOperatorKind(child))
      Some("type") => entries.push(Type_(child))
      Some("type_desc") => entries.push(TypeDesc(child))
      Some("type_name") => entries.push(TypeName(child))
      Some("using_kind") => entries.push(UsingKind(child))
      Some("vis") => entries.push(Vis(child))
      Some("where") => entries.push(Where(child))
      Some("wildcard") => entries.push(Wildcard(child))
      _ => abort("unsupported CST child label")
    }
  }
  entries
}
