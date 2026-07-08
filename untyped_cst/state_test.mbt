///|
fn any_node(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.NodeKind) -> Bool,
) -> Bool {
  if pred(node.kind) {
    return true
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) => if any_node(child, pred) { return true }
      None => ()
    }
  }
  false
}

///|
fn count_nodes(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.NodeKind) -> Bool,
) -> Int {
  let mut count = if pred(node.kind) { 1 } else { 0 }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) => count += count_nodes(child, pred)
      None => ()
    }
  }
  count
}

///|
fn first_node(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.NodeKind) -> Bool,
) -> @untyped_cst.CstNode? {
  if pred(node.kind) {
    return Some(node)
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) =>
        if first_node(child, pred) is Some(found) {
          return Some(found)
        } else {
          ()
        }
      None => ()
    }
  }
  None
}

///|
fn is_comment(kind : @untyped_cst.NodeKind) -> Bool {
  match kind {
    @untyped_cst.Token(@tokens.COMMENT(_)) => true
    _ => false
  }
}

///|
fn is_newline(kind : @untyped_cst.NodeKind) -> Bool {
  match kind {
    @untyped_cst.Token(@tokens.NEWLINE) => true
    _ => false
  }
}

///|
fn is_asi_semi(kind : @untyped_cst.NodeKind) -> Bool {
  match kind {
    @untyped_cst.Token(@tokens.SEMI(false)) => true
    _ => false
  }
}

///|
fn is_any_semi(kind : @untyped_cst.NodeKind) -> Bool {
  match kind {
    @untyped_cst.Token(@tokens.SEMI(_)) => true
    _ => false
  }
}

///|
fn is_eof(kind : @untyped_cst.NodeKind) -> Bool {
  match kind {
    @untyped_cst.Token(@tokens.EOF) => true
    _ => false
  }
}

///|
fn is_missing_eof(kind : @untyped_cst.NodeKind) -> Bool {
  match kind {
    @untyped_cst.Missing(@tokens.TokenKind::TK_EOF) => true
    _ => false
  }
}

///|
fn is_lident(name : String) -> (@untyped_cst.NodeKind) -> Bool {
  fn(kind) {
    match kind {
      @untyped_cst.Token(@tokens.LIDENT(id)) => id == name
      _ => false
    }
  }
}

///|
test "comments are retained and newline tokens are skipped" {
  let source = "// c\nlet x = 1"
  let result = @untyped_cst.parse_structure(source)
  let root = result.root()
  @test.assert_eq(result.diagnostics_view().length(), 0)
  @test.assert_eq(root.text(source), source[:])
  @test.assert_eq(any_node(root, is_comment), true)
  @test.assert_eq(any_node(root, is_newline), false)
}

///|
test "asi semicolon is zero width and explicit semicolon spans source" {
  let asi_source = "let x = 1\nlet y = 2"
  let asi = @untyped_cst.parse_structure(asi_source).root()
  guard first_node(asi, is_asi_semi) is Some(asi_semi) else {
    fail("expected ASI semicolon")
  }
  @test.assert_eq(asi_semi.source_span.0, asi_semi.source_span.1)
  let explicit_source = "let x = 1; let y = 2"
  let explicit = @untyped_cst.parse_structure(explicit_source).root()
  guard first_node(explicit, is_any_semi) is Some(explicit_semi) else {
    fail("expected explicit semicolon")
  }
  @test.assert_eq(explicit_semi.text(explicit_source), ";"[:])
}

///|
test "default entry preserves text after emoji" {
  let source = "let s = \"😀\"\nlet x = 1"
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.root().text(source), source[:])
  guard first_node(result.root(), is_lident("x")) is Some(x) else {
    fail("expected x token")
  }
  @test.assert_eq(x.text(source), "x"[:])
}

///|
test "source-backed roots cover leading and trailing layout" {
  let source = "  let x = 1  "
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.root().source_span.0, 0)
  @test.assert_eq(result.root().source_span.1, source.length())
  @test.assert_eq(result.root().text(source), source[:])
}
