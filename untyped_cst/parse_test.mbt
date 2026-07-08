///|
fn root_is_source_file(root : @untyped_cst.CstNode) -> Bool {
  match root.kind {
    @untyped_cst.SourceFile => true
    _ => false
  }
}

///|
fn root_is_expression_fragment(root : @untyped_cst.CstNode) -> Bool {
  match root.kind {
    @untyped_cst.ExpressionFragment => true
    _ => false
  }
}

///|
fn is_top_item(node : @untyped_cst.CstNode) -> Bool {
  let kind = node.kind
  kind.is_impl() || kind.is_decl()
}

///|
fn is_expr(node : @untyped_cst.CstNode) -> Bool {
  let kind = node.kind
  kind.is_stmt() || kind.is_expr()
}

///|
fn has_balanced_decl_shape(node : @untyped_cst.CstNode) -> Bool {
  let kind = node.kind
  kind.is_decl() || kind.is_impl()
}

///|
fn count_root_label(root : @untyped_cst.CstNode, label : String) -> Int {
  let mut count = 0
  for i in 0..<root.children.length() {
    match root.children.get(i) {
      Some((Some(child_label), _)) if child_label == label => count += 1
      _ => ()
    }
  }
  count
}

///|
fn count_root_items(root : @untyped_cst.CstNode) -> Int {
  let mut count = 0
  for i in 0..<root.children.length() {
    match root.children.get(i) {
      Some((Some("item"), child)) if is_top_item(child) => count += 1
      _ => ()
    }
  }
  count
}

///|
fn root_contains_expr(root : @untyped_cst.CstNode) -> Bool {
  for i in 0..<root.children.length() {
    match root.children.get(i) {
      Some((_, child)) if is_expr(child) => return true
      _ => ()
    }
  }
  false
}

///|
fn find_node(
  root : @untyped_cst.CstNode,
  pred : (@untyped_cst.CstNode) -> Bool,
) -> @untyped_cst.CstNode? {
  if pred(root) {
    return Some(root)
  }
  for i in 0..<root.children.length() {
    match root.children.get(i) {
      Some((_, child)) =>
        if find_node(child, pred) is Some(found) {
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
fn is_argument_list(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.List_ArgumentList => true
    _ => false
  }
}

///|
fn is_statement_list(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.List_StatementList => true
    _ => false
  }
}

///|
fn has_missing_close(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.Missing(@tokens.TokenKind::TK_RPAREN) => true
    _ => false
  }
}

///|
fn is_type_node(node : @untyped_cst.CstNode) -> Bool {
  node.kind.is_type()
}

///|
fn is_pattern_node(node : @untyped_cst.CstNode) -> Bool {
  node.kind.is_pattern()
}

///|
fn is_parameter_clause(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.Clause_Parameter => true
    _ => false
  }
}

///|
fn is_error_node(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.Error => true
    _ => false
  }
}

///|
fn is_multiline_string_expr(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.Expr_MultilineString => true
    _ => false
  }
}

///|
fn is_interp_literal_segment(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.InterpSegment_InterpLiteralSegment => true
    _ => false
  }
}

///|
fn is_interp_source_segment(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.InterpSegment_InterpSourceSegment => true
    _ => false
  }
}

///|
fn is_regex_interp_lex_pattern(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.LexPattern_RegexInterp => true
    _ => false
  }
}

///|
fn is_interp_source_segment_with_expr(node : @untyped_cst.CstNode) -> Bool {
  is_interp_source_segment(node) && find_node(node, is_expr) is Some(_)
}

///|
fn is_error_annotation(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.Clause_ErrorAnnotation => true
    _ => false
  }
}

///|
fn is_return_type_clause(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.Clause_ReturnType => true
    _ => false
  }
}

///|
fn has_node_text(
  root : @untyped_cst.CstNode,
  source : String,
  pred : (@untyped_cst.CstNode) -> Bool,
  text : String,
) -> Bool {
  if find_node(root, fn(node) { pred(node) && node.text(source) == text[:] })
    is Some(_) {
    true
  } else {
    false
  }
}

///|
fn count_label(node : @untyped_cst.CstNode, label : String) -> Int {
  let mut count = 0
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some(child_label), _)) if child_label == label => count += 1
      _ => ()
    }
  }
  count
}

///|
fn has_label_recursive(node : @untyped_cst.CstNode, label : String) -> Bool {
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some(child_label), _)) if child_label == label => return true
      Some((_, child)) if has_label_recursive(child, label) => return true
      _ => ()
    }
  }
  false
}

///|
fn has_node_kind(
  root : @untyped_cst.CstNode,
  pred : (@untyped_cst.NodeKind) -> Bool,
) -> Bool {
  find_node(root, fn(node) { pred(node.kind) }) is Some(_)
}

///|
fn has_type_kind(root : @untyped_cst.CstNode, expected : String) -> Bool {
  find_node(root, fn(node) {
    match node.kind {
      @untyped_cst.Type_Name => expected == "Name"
      @untyped_cst.Type_Tuple => expected == "Tuple"
      @untyped_cst.Type_Function => expected == "Function"
      @untyped_cst.Type_Application => expected == "Application"
      @untyped_cst.Type_Option => expected == "Option"
      @untyped_cst.Type_Object => expected == "Object"
      @untyped_cst.Type_Group => expected == "Group"
      _ => false
    }
  })
  is Some(_)
}

///|
fn is_placeholder_node(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.RegexPattern_Placeholder => true
    _ => false
  }
}

///|
fn has_no_placeholder_nodes(root : @untyped_cst.CstNode) -> Bool {
  find_node(root, is_placeholder_node) is None
}

///|
test "empty structure returns source file" {
  let source = ""
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length(), 0)
  @test.assert_eq(root_is_source_file(result.root()), true)
  @test.assert_eq(result.root().text(source), source[:])
}

///|
test "let structure returns one top-level item" {
  let source = "let x = 1"
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length(), 0)
  @test.assert_eq(root_is_source_file(result.root()), true)
  @test.assert_eq(count_root_items(result.root()), 1)
}

///|
test "top-level separators are preserved without empty trailing item" {
  let explicit = @untyped_cst.parse_structure("let x = 1; let y = 2")
  @test.assert_eq(count_root_items(explicit.root()), 2)
  @test.assert_eq(count_root_label(explicit.root(), "sep"), 1)
  let asi = @untyped_cst.parse_structure("let x = 1\nlet y = 2")
  @test.assert_eq(count_root_items(asi.root()), 2)
  @test.assert_eq(count_root_label(asi.root(), "sep"), 1)
  let trailing = @untyped_cst.parse_structure("let x = 1;")
  @test.assert_eq(count_root_items(trailing.root()), 1)
  @test.assert_eq(count_root_label(trailing.root(), "sep"), 1)
}

///|
test "expression entry returns expression fragment" {
  let source = "1 + 2"
  let result = @untyped_cst.parse_expression(source)
  @test.assert_eq(root_is_expression_fragment(result.root()), true)
  @test.assert_eq(root_contains_expr(result.root()), true)
  @test.assert_eq(result.root().text(source), source[:])
}

///|
test "successful expressions are grammar shaped without placeholders" {
  let cases = [
    "a + b * c", "object.method(1).field[0]", "if ok { yes } else { no }", "match value { Some(x) => x; _ => y }",
    "{ let x = 1; x }",
  ]
  for source in cases {
    let result = @untyped_cst.parse_expression(source)
    @test.assert_eq(result.diagnostics_view().length(), 0)
    @test.assert_eq(result.root().text(source), source[:])
    @test.assert_eq(has_no_placeholder_nodes(result.root()), true)
  }
}

///|
test "grammar shaped expression labels are semantic not positional" {
  let infix = @untyped_cst.parse_expression("a + b * c")
  @test.assert_eq(infix.diagnostics_view().length(), 0)
  @test.assert_eq(has_label_recursive(infix.root(), "lhs"), true)
  @test.assert_eq(has_label_recursive(infix.root(), "rhs"), true)
  @test.assert_eq(has_label_recursive(infix.root(), "op"), true)
  let control = @untyped_cst.parse_expression("if ok { yes } else { no }")
  @test.assert_eq(control.diagnostics_view().length(), 0)
  @test.assert_eq(has_label_recursive(control.root(), "cond"), true)
  @test.assert_eq(has_label_recursive(control.root(), "body"), true)
  @test.assert_eq(has_label_recursive(control.root(), "else"), true)
  let call = @untyped_cst.parse_expression("object.method(1).field[0]")
  @test.assert_eq(call.diagnostics_view().length(), 0)
  @test.assert_eq(has_label_recursive(call.root(), "self"), true)
  @test.assert_eq(has_label_recursive(call.root(), "arg"), true)
  @test.assert_eq(has_label_recursive(call.root(), "field"), true)
  @test.assert_eq(has_label_recursive(call.root(), "index"), true)
}

///|
test "expression entries return expression fragment" {
  let source = "1 + 2"
  let bare = @untyped_cst.parse_expression(source)
  @test.assert_eq(root_is_expression_fragment(bare.root()), true)
}

///|
test "argument list preserves open items separators and close" {
  let source = "f(1, 2,)"
  let result = @untyped_cst.parse_expression(source)
  guard find_node(result.root(), is_argument_list) is Some(args) else {
    fail("expected argument list")
  }
  @test.assert_eq(count_label(args, "open"), 1)
  @test.assert_eq(count_label(args, "item"), 2)
  @test.assert_eq(count_label(args, "sep"), 2)
  @test.assert_eq(count_label(args, "close"), 1)
  @test.assert_eq(args.text(source), "(1, 2,)"[:])
}

///|
test "block statement list preserves braces and semicolon" {
  let source = "{ let x = 1; x }"
  let result = @untyped_cst.parse_expression(source)
  guard find_node(result.root(), is_statement_list) is Some(block) else {
    fail("expected statement list")
  }
  @test.assert_eq(count_label(block, "open"), 1)
  @test.assert_eq(count_label(block, "sep"), 1)
  @test.assert_eq(count_label(block, "close"), 1)
  @test.assert_eq(block.text(source), source[:])
}

///|
test "missing list close inserts missing node and diagnostic" {
  let source = "f(1, 2"
  let result = @untyped_cst.parse_expression(source)
  guard find_node(result.root(), is_argument_list) is Some(args) else {
    fail("expected argument list")
  }
  @test.assert_eq(find_node(args, has_missing_close) is Some(_), true)
  @test.assert_eq(result.diagnostics_view().length(), 1)
}

///|
test "top-level starts have balanced exterior nodes" {
  let cases = [
    "fn f() {}", "async fn f() {}", "let x = 1", "const X = 1", "type T = Int", "struct S {}",
    "enum E { A }", "suberror E { A }", "extenum E { A }", "enumview V { C } for A with view(x) { C }",
    "trait T {}", "impl T with f(self) { self }", "test \"t\" {}", "using @pkg {func as func2}",
    "extern \"js\" fn f() = \"f\"", "declare fn f()", "pub fn f() {}", "priv type T = Int",
    "#deprecated\nfn f() {}",
  ]
  for source in cases {
    let result = @untyped_cst.parse_structure(source)
    if result.diagnostics_view().length() != 0 {
      fail(source)
    }
    @test.assert_eq(result.root().text(source), source[:])
    @test.assert_eq(
      find_node(result.root(), has_balanced_decl_shape) is Some(_),
      true,
    )
    @test.assert_eq(count_root_items(result.root()), 1)
  }
}

///|
test "balanced declaration body keeps nested semicolon inside item" {
  let source = "fn f() { let x = 1; x }\nlet y = 2"
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length(), 0)
  @test.assert_eq(result.root().text(source), source[:])
  @test.assert_eq(count_root_label(result.root(), "item"), 2)
  @test.assert_eq(count_root_items(result.root()), 2)
  @test.assert_eq(count_root_label(result.root(), "sep"), 1)
}

///|
test "top-level splitter ignores fn tokens inside let bodies" {
  let source = "let f = fn(x) { let z = 1; z }\nlet y = 1"
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length(), 0)
  @test.assert_eq(result.root().text(source), source[:])
  @test.assert_eq(count_root_label(result.root(), "item"), 2)
  @test.assert_eq(count_root_items(result.root()), 2)
  @test.assert_eq(count_root_label(result.root(), "sep"), 1)
}

///|
test "type grammar cases produce type nodes" {
  let cases = [
    ("fn f(x : Int) {}", "Int"),
    ("fn f(x : @pkg.T[A]) {}", "@pkg.T[A]"),
    ("fn f(x : (Int, String)) {}", "(Int, String)"),
    ("fn f(x : (Int) -> String) {}", "(Int) -> String"),
    ("fn f(x : Int?) {}", "Int?"),
    ("fn f(x : Self) {}", "Self"),
    ("fn f(x : _) {}", "_"),
    ("fn f(x : &Name) {}", "&Name"),
    ("type T = Array[Int]", "Array[Int]"),
  ]
  for case in cases {
    let (source, expected_type) = case
    let result = @untyped_cst.parse_structure(source)
    if result.diagnostics_view().length() != 0 {
      fail(source)
    }
    @test.assert_eq(
      has_node_text(result.root(), source, is_type_node, expected_type),
      true,
    )
  }
}

///|
test "successful type grammar cases are shape-specific without placeholders" {
  let cases = [
    ("fn f(x : Int) {}", "Name"),
    ("fn f(x : Array[Int]) {}", "Application"),
    ("fn f(x : A?) {}", "Option"),
    ("fn f(x : (A, B)) {}", "Tuple"),
    ("fn f(x : (A) -> B) {}", "Function"),
    ("fn f(x : &T) {}", "Object"),
  ]
  for case in cases {
    let (source, expected_kind) = case
    let result = @untyped_cst.parse_structure(source)
    if result.diagnostics_view().length() != 0 {
      fail(source)
    }
    @test.assert_eq(result.root().text(source), source[:])
    @test.assert_eq(has_type_kind(result.root(), expected_kind), true)
    @test.assert_eq(has_no_placeholder_nodes(result.root()), true)
  }
}

///|
test "return type and error annotation cases" {
  let cases = [
    ("fn f() -> T raise E { x }", "T"),
    ("fn f() -> T noraise { x }", "T"),
    ("fn f() -> T raise ? { x }", "T"),
    (
      "fn f() -> async () -> async () -> Int { x }", "async () -> async () -> Int",
    ),
    ("fn f() -> &Name { x }", "&Name"),
    ("fn f() -> @pkg.T[A] { x }", "@pkg.T[A]"),
    ("fn[N : Name] f(x : Int) -> T { x }", "Name"),
  ]
  for case in cases {
    let (source, expected_type) = case
    let result = @untyped_cst.parse_structure(source)
    @test.assert_eq(result.diagnostics_view().length(), 0)
    @test.assert_eq(
      has_node_text(result.root(), source, is_type_node, expected_type),
      true,
    )
  }
  let with_raise = @untyped_cst.parse_structure("fn f() -> T raise E { x }")
  @test.assert_eq(
    find_node(with_raise.root(), is_return_type_clause) is Some(_),
    true,
  )
  @test.assert_eq(
    find_node(with_raise.root(), is_error_annotation) is Some(_),
    true,
  )
  let with_noraise = @untyped_cst.parse_structure("fn f() -> T noraise { x }")
  @test.assert_eq(
    find_node(with_noraise.root(), is_return_type_clause) is Some(_),
    true,
  )
  @test.assert_eq(
    find_node(with_noraise.root(), is_error_annotation) is Some(_),
    true,
  )
}

///|
test "pattern grammar cases produce pattern nodes" {
  let cases = [
    ("fn f() { let x = value; () }", "x"),
    ("fn f() { let _ = value; () }", "_"),
    ("fn f() { let 1 = value; () }", "1"),
    ("fn f() { let (a, _) = value; () }", "(a, _)"),
    ("fn f() { let [a, .. rest,] = value; () }", "[a, .. rest,]"),
    ("fn f() { let Some(x) = value; () }", "Some(x)"),
    ("fn f() { let { x: y, z, .. } = value; () }", "{ x: y, z, .. }"),
    ("fn f() { let { \"x\"? : v, .. } = value; () }", "{ \"x\"? : v, .. }"),
    ("fn f() { let A | B = value; () }", "A | B"),
    ("fn f() { let A as a = value; () }", "A as a"),
    ("fn f() { let (x : Int) = value; () }", "(x : Int)"),
    ("fn f() { let 1..<3 = value; () }", "1..<3"),
    ("fn f() { let W::A(bv, d) = value; () }", "W::A(bv, d)"),
    ("fn f() { let @pkg.Constr = value; () }", "@pkg.Constr"),
    ("fn f() { let Ty::Constr = value; () }", "Ty::Constr"),
    ("fn f() { let Ty::@pkg.Constr = value; () }", "Ty::@pkg.Constr"),
    ("fn f() { let C(x=pat, y~, ..) = value; () }", "C(x=pat, y~, ..)"),
    ("fn f() { let ['c', .. b,] = value; () }", "['c', .. b,]"),
    ("fn f() { let [.. as rest] = value; () }", "[.. as rest]"),
    ("fn f() { let [.. _] = value; () }", "[.. _]"),
    ("fn f() { let [.. \"lit\"] = value; () }", "[.. \"lit\"]"),
    ("fn f() { let [.. b\"lit\"] = value; () }", "[.. b\"lit\"]"),
    ("fn f() { let i6(x) = value; () }", "i6(x)"),
  ]
  for case in cases {
    let (source, expected_pattern) = case
    let result = @untyped_cst.parse_structure(source)
    if result.diagnostics_view().length() != 0 {
      fail(source)
    }
    @test.assert_eq(
      has_node_text(result.root(), source, is_pattern_node, expected_pattern),
      true,
    )
  }
}

///|
test "match expression cases produce pattern nodes" {
  let source = "match value { Some(x) => x; _ => y }"
  let result = @untyped_cst.parse_expression(source)
  if result.diagnostics_view().length() != 0 {
    fail(source)
  }
  @test.assert_eq(
    has_node_text(result.root(), source, is_pattern_node, "Some(x)"),
    true,
  )
  @test.assert_eq(
    has_node_text(result.root(), source, is_pattern_node, "_"),
    true,
  )
}

///|
test "function parameters reject destructuring patterns" {
  let source = "fn f((a, _) : Pair, [x, .. rest] : Array[Int]) { x }"
  assert_structure_acceptance(
    "destructuring function parameters", source, false,
  )
}

///|
test "local function optional parameters parse without diagnostics" {
  let local_fn_source =
    #|fn init {
    #|  fn question(a? : Int, b? : Int) -> Int? { Some(0) }
    #|  question()
    #|}
  let lambda_source =
    #|fn init {
    #|  let question = fn(a? : Int, b? : Int) { Some(0) }
    #|  question()
    #|}
  let sources = [local_fn_source, lambda_source]
  for source in sources {
    let result = @untyped_cst.parse_structure(source)
    @test.assert_eq(result.diagnostics_view().length(), 0)
    @test.assert_eq(result.root().text(source), source[:])
    @test.assert_eq(
      has_node_text(result.root(), source, is_parameter_clause, "a? : Int"),
      true,
    )
    @test.assert_eq(
      has_node_text(result.root(), source, is_parameter_clause, "b? : Int"),
      true,
    )
  }
}

///|
test "parenthesized expression type annotation accepts optional type marker" {
  let source =
    #|fn init {
    #|  let g0 = (None : Int?)
    #|  ()
    #|}
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length(), 0)
  @test.assert_eq(result.root().text(source), source[:])
  @test.assert_eq(
    has_node_text(result.root(), source, is_type_node, "Int?"),
    true,
  )
}

///|
test "optional argument forwarding in call list parses without diagnostics" {
  let sources = [
    "f(label~)", "f(label?, other?=value)", "extend(extend?, extend?=value)",
  ]
  let argument_texts = [
    "(label~)", "(label?, other?=value)", "(extend?, extend?=value)",
  ]
  for i, source in sources {
    let result = @untyped_cst.parse_expression(source)
    @test.assert_eq(result.diagnostics_view().length(), 0)
    @test.assert_eq(result.root().text(source), source[:])
    @test.assert_eq(
      has_node_text(result.root(), source, is_argument_list, argument_texts[i]),
      true,
    )
  }
}

///|
test "expression cases preserve source text" {
  let cases = [
    "42", "name", "f(1, 2)", "object.method(1)", "object.field", "array[0]", "(a, b)",
    "[1, 2]", "{ \"x\": 1 }", "T::{ x: 1 }", "x => x + 1", "if ok { yes } else { no }",
    "match value { Some(x) => x; _ => y }", "try f() catch { _ => g }", "try? f()",
    "try! f()", "raise err", "return value", "break value", "continue", "while ok { work() } nobreak { done }",
    "for i in xs { i }", "defer cleanup()", "guard ok else { return }", "a |> b + c",
    "a <| b + c", "-x", "a + b * c", "x = y", "x += y", "0..<n", "x as T", "x is Some(_)",
    "{ let x = 1; x }", "lexmatch s { re\"a\" => 1 }", "lexscan s { re\"a\" => 1 }",
    "s =~ re\"a\"", "∀ i : Int, i >= 0", "∃ i : Int, i >= 0", "a → b", "letrec f = fn () { 0 }",
    "proof_assert a == b", "proof_let witness = mk_proof()",
  ]
  for source in cases {
    assert_expression_acceptance(source, source, true)
    let result = @untyped_cst.parse_expression(source)
    if result.diagnostics_view().length() != 0 {
      fail(source)
    }
    @test.assert_eq(result.root().text(source), source[:])
    if !root_contains_expr(result.root()) {
      fail(source)
    }
  }
}

///|
test "unsupported expression tokens produce recovery diagnostics" {
  let cases = [
    "loop 0 { break }", "throw err", "s lexmatch? \"a\"", "s lexmatch? \"a\" with longest",
    "pub", "else", "catch", "with",
  ]
  for source in cases {
    assert_expression_acceptance(source, source, false)
    let result = @untyped_cst.parse_expression(source)
    @test.assert_eq(result.root().text(source), source[:])
    @test.assert_eq(result.diagnostics_view().length() > 0, true)
    @test.assert_eq(find_node(result.root(), is_error_node) is Some(_), true)
  }
  let structure_cases = [
    "fn f() { pub }", "fn f() { else }", "fn f() { catch }", "fn f() { with }",
  ]
  for source in structure_cases {
    assert_structure_acceptance(source, source, false)
    let result = @untyped_cst.parse_structure(source)
    @test.assert_eq(result.root().text(source), source[:])
    @test.assert_eq(result.diagnostics_view().length() > 0, true)
    @test.assert_eq(find_node(result.root(), is_error_node) is Some(_), true)
  }
  let where_source = "while ok { step() } where { proof_assert ok }"
  assert_expression_acceptance(where_source, where_source, false)
  let where_result = @untyped_cst.parse_expression(where_source)
  @test.assert_eq(where_result.root().text(where_source), where_source[:])
  @test.assert_eq(where_result.diagnostics_view().length() > 0, true)
}

///|
test "string literal cases preserve source text" {
  let cases = ["\"hello\"", "b\"hello\"", "re\"a+\""]
  for source in cases {
    let result = @untyped_cst.parse_expression(source)
    if result.diagnostics_view().length() != 0 {
      fail(source)
    }
    @test.assert_eq(result.root().text(source), source[:])
    @test.assert_eq(root_contains_expr(result.root()), true)
  }
}

///|
test "interpolation segments include parsed expression children" {
  let cases = [
    "\"hello \\{name}\"", "b\"hello \\{name}\"", "$| hello \\{name}\n#| tail",
  ]
  for source in cases {
    let result = @untyped_cst.parse_expression(source)
    if result.diagnostics_view().length() != 0 {
      fail(source)
    }
    @test.assert_eq(result.root().text(source), source[:])
    @test.assert_eq(
      find_node(result.root(), is_interp_literal_segment) is Some(_),
      true,
    )
    @test.assert_eq(
      find_node(result.root(), is_interp_source_segment_with_expr) is Some(_),
      true,
    )
  }
}

///|
test "consecutive multiline string tokens form one multiline expression" {
  let source = "#| Hello\n#| World"
  let result = @untyped_cst.parse_expression(source)
  if result.diagnostics_view().length() != 0 {
    fail(source)
  }
  @test.assert_eq(result.root().text(source), source[:])
  @test.assert_eq(
    find_node(result.root(), is_multiline_string_expr) is Some(_),
    true,
  )
}

///|
test "regex interpolation is represented as lex pattern segment" {
  let source = "lexmatch s { re\"a\\{x}\" => 1 }"
  let result = @untyped_cst.parse_expression(source)
  if result.diagnostics_view().length() != 0 {
    fail(source)
  }
  @test.assert_eq(result.root().text(source), source[:])
  @test.assert_eq(
    find_node(result.root(), is_regex_interp_lex_pattern) is Some(_),
    true,
  )
  @test.assert_eq(
    find_node(result.root(), is_interp_source_segment_with_expr) is Some(_),
    true,
  )
}

///|
test "top-level declarations expose semantic children" {
  let fn_source = "fn f(x : Int) -> Int { x }"
  let fn_result = @untyped_cst.parse_structure(fn_source)
  @test.assert_eq(fn_result.diagnostics_view().length(), 0)
  @test.assert_eq(fn_result.root().text(fn_source), fn_source[:])
  @test.assert_eq(
    has_node_kind(fn_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_Function
    }),
    true,
  )
  @test.assert_eq(has_label_recursive(fn_result.root(), "name"), true)
  @test.assert_eq(has_label_recursive(fn_result.root(), "type"), true)
  @test.assert_eq(has_label_recursive(fn_result.root(), "clause"), true)
  @test.assert_eq(has_label_recursive(fn_result.root(), "body"), true)
  let let_source = "let x = 1"
  let let_result = @untyped_cst.parse_structure(let_source)
  @test.assert_eq(let_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(let_result.root(), fn(kind) { kind is @untyped_cst.Decl_Let }),
    true,
  )
  @test.assert_eq(has_label_recursive(let_result.root(), "pattern"), true)
  let const_source = "const X = 1"
  let const_result = @untyped_cst.parse_structure(const_source)
  @test.assert_eq(const_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(const_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_Const
    }),
    true,
  )
  @test.assert_eq(has_label_recursive(const_result.root(), "name"), true)
  let type_source = "type T = Array[Int]"
  let type_result = @untyped_cst.parse_structure(type_source)
  @test.assert_eq(type_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(type_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_Type
    }),
    true,
  )
  @test.assert_eq(has_label_recursive(type_result.root(), "name"), true)
  @test.assert_eq(has_label_recursive(type_result.root(), "type"), true)
  let struct_source = "struct S { x : Int }"
  let struct_result = @untyped_cst.parse_structure(struct_source)
  @test.assert_eq(struct_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(struct_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_Struct
    }),
    true,
  )
  @test.assert_eq(has_label_recursive(struct_result.root(), "name"), true)
  @test.assert_eq(has_label_recursive(struct_result.root(), "body"), true)
  let enum_source = "enum E { A }"
  let enum_result = @untyped_cst.parse_structure(enum_source)
  @test.assert_eq(enum_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(enum_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_Enum
    }),
    true,
  )
  @test.assert_eq(has_label_recursive(enum_result.root(), "name"), true)
  @test.assert_eq(has_label_recursive(enum_result.root(), "body"), true)
  let trait_source = "trait T { f(Self) -> Unit }"
  let trait_result = @untyped_cst.parse_structure(trait_source)
  @test.assert_eq(trait_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(trait_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_Trait
    }),
    true,
  )
  @test.assert_eq(has_label_recursive(trait_result.root(), "name"), true)
  @test.assert_eq(has_label_recursive(trait_result.root(), "body"), true)
  let impl_source = "impl T with f(self) { self }"
  let impl_result = @untyped_cst.parse_structure(impl_source)
  @test.assert_eq(impl_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(impl_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_Impl
    }),
    true,
  )
  @test.assert_eq(has_label_recursive(impl_result.root(), "name"), true)
  @test.assert_eq(has_label_recursive(impl_result.root(), "body"), true)
  let test_source = "test { 1 }"
  let test_result = @untyped_cst.parse_structure(test_source)
  @test.assert_eq(test_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(test_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_Test
    }),
    true,
  )
  @test.assert_eq(has_label_recursive(test_result.root(), "body"), true)
  let using_source = "using @pkg {}"
  let using_result = @untyped_cst.parse_structure(using_source)
  @test.assert_eq(using_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(using_result.root(), fn(kind) {
      kind is @untyped_cst.Impl_Using
    }),
    true,
  )
  let extern_source = "extern \"js\" fn f() = \"f\""
  let extern_result = @untyped_cst.parse_structure(extern_source)
  @test.assert_eq(extern_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(extern_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_Function
    }),
    true,
  )
  @test.assert_eq(has_label_recursive(extern_result.root(), "name"), true)
  @test.assert_eq(has_label_recursive(extern_result.root(), "language"), true)
  @test.assert_eq(has_label_recursive(extern_result.root(), "stub"), true)
}

///|
test "successful top-level declarations are shaped without placeholders" {
  let cases = [
    "fn f(x : Int) -> Int { x }", "let x : Int = 1", "const X : Int = 1", "type T = Array[Int]",
    "struct S { x : Int }", "enum E { A(Int) }", "trait T { f(Self) -> Unit }", "impl T with f(self) { self }",
    "test { inspect(1, content=\"1\") }",
  ]
  for source in cases {
    let result = @untyped_cst.parse_structure(source)
    if result.diagnostics_view().length() != 0 {
      fail(source)
    }
    @test.assert_eq(result.root().text(source), source[:])
    @test.assert_eq(has_no_placeholder_nodes(result.root()), true)
  }
}

///|
test "prefixed declarations expose resolved declaration kind" {
  let fn_cases = ["pub fn f() {}", "declare fn f()", "#deprecated\nfn f() {}"]
  for source in fn_cases {
    let result = @untyped_cst.parse_structure(source)
    if result.diagnostics_view().length() != 0 {
      fail(source)
    }
    @test.assert_eq(
      has_node_kind(result.root(), fn(kind) {
        kind is @untyped_cst.Decl_Function
      }),
      true,
    )
    @test.assert_eq(
      has_node_kind(result.root(), fn(kind) {
        kind is @untyped_cst.Decl_BalancedDecl
      }),
      false,
    )
  }
  let type_source = "priv type T = Int"
  let type_result = @untyped_cst.parse_structure(type_source)
  @test.assert_eq(type_result.diagnostics_view().length(), 0)
  @test.assert_eq(
    has_node_kind(type_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_Type
    }),
    true,
  )
  @test.assert_eq(
    has_node_kind(type_result.root(), fn(kind) {
      kind is @untyped_cst.Decl_BalancedDecl
    }),
    false,
  )
}

///|
test "optional type markers in declaration bodies do not produce recovery diagnostics" {
  let source =
    #|struct S {
    #|  x : String?
    #|}
    #|
    #|fn f(x : String??) -> String? { x }
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length(), 0)
  @test.assert_eq(result.root().text(source), source[:])
}

///|
test "optional type markers in block let annotations do not produce recovery diagnostics" {
  let source = "{ let opt : _? = Some(42); opt }"
  let result = @untyped_cst.parse_expression(source)
  @test.assert_eq(result.diagnostics_view().length(), 0)
  @test.assert_eq(result.root().text(source), source[:])
}
