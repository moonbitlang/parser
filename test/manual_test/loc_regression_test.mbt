///|
using @syntax {trait IterVisitor}

///|
let loc_regression_source =
  #| let global: Int = {
  #|   1
  #| }
  #|
  #| fn init {
  #|   let _ = global
  #| }

///|
fn parse_loc_regression(
  parser : @parser.Parser,
) -> (@syntax.Impls, @basic.Location, @basic.Location, Json) raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    loc_regression_source,
    name="loc_regression.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail("\{Repr(diagnostics)}")
  }
  let mut brace_group_loc = None
  let mut params_loc = None
  for impl_ in impls {
    match impl_ {
      TopLetDef(expr=Group(group=Brace, loc~, ..), ..) =>
        brace_group_loc = Some(loc)
      TopFuncDef(fun_decl~, ..) => params_loc = Some(fun_decl.params_loc)
      _ => ()
    }
  }
  let json = @basic.show_loc.protect(String, fn() {
    impls.map(impl_ => impl_.json_repr()).to_json()
  })
  match (brace_group_loc, params_loc) {
    (Some(brace_group_loc), Some(params_loc)) =>
      (impls, brace_group_loc, params_loc, json)
    _ => fail("failed to find loc regression fixtures in parsed AST")
  }
}

///|
test "loc parity for brace group and empty fn params" {
  let (_, hand_group_loc, hand_params_loc, hand_json) = parse_loc_regression(
    Handrolled,
  )
  let (_, yacc_group_loc, yacc_params_loc, yacc_json) = parse_loc_regression(
    MoonYacc,
  )
  inspect(hand_group_loc, content="loc_regression.mbt-1:20-3:3")
  inspect(hand_params_loc, content="loc_regression.mbt-5:9-5:9")
  inspect(yacc_group_loc, content="loc_regression.mbt-1:20-3:3")
  inspect(yacc_params_loc, content="loc_regression.mbt-5:9-5:9")
  if hand_json != yacc_json {
    fail(
      "loc-sensitive AST mismatch between handrolled and yacc parsers.\nhandrolled:\n\{hand_json.stringify(indent=2)}\n\nyacc:\n\{yacc_json.stringify(indent=2)}",
    )
  }
}

///|
let grouped_pattern_loc_regression_source =
  #| fn grouped_patterns {
  #|   let (a, b) = (1, 2)
  #|   let ((a, b)) = (1, 2)
  #|   let (x : Int) = 1
  #|   let ((x : Int)) = 1
  #| }

///|
fn parse_grouped_pattern_loc_regression(
  parser : @parser.Parser,
) -> Json raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    grouped_pattern_loc_regression_source,
    name="grouped_pattern_loc_regression.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail("\{Repr(diagnostics)}")
  }
  @basic.show_loc.protect(String, fn() {
    impls.map(impl_ => impl_.json_repr()).to_json()
  })
}

///|
test "loc parity for grouped tuple and constraint patterns" {
  let hand_json = parse_grouped_pattern_loc_regression(Handrolled)
  let yacc_json = parse_grouped_pattern_loc_regression(MoonYacc)
  if hand_json != yacc_json {
    fail(
      "loc-sensitive AST mismatch between handrolled and yacc parsers.\nhandrolled:\n\{hand_json.stringify(indent=2)}\n\nyacc:\n\{yacc_json.stringify(indent=2)}",
    )
  }
}

///|
let modern_syntax_loc_regression_source =
  #|import { "pkg/core", "pkg/other" @other } for "native"
  #|
  #|#deprecated
  #|pub extend @pkg.Type with @traits.Trait::{f, old as renamed}
  #|
  #|fn modern(value : Int?, lexbuf : @lexbuf.Lexbuf) -> Int {
  #|  let normal = done~: {
  #|    if false { continue done~ }
  #|    break done~ 1
  #|  }
  #|  let transparent = inserted~~: {
  #|    if false { continue inserted~~ }
  #|    break inserted~~ 2
  #|  }
  #|  guard! value is Some(inner)
  #|  guard !false
  #|  let matched = match value {
  #|    Some(n) | (None with n = 0) => n
  #|  }
  #|  let scanned = lexscan lexbuf with longest {
  #|    ((re"if" | re"else") as keyword, before=prefix, after~) => prefix
  #|    @tokens.Token => 0
  #|  }
  #|  normal + transparent + inner + matched + scanned
  #|}

///|
fn parse_modern_syntax_loc_regression(
  parser : @parser.Parser,
) -> Json raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    modern_syntax_loc_regression_source,
    name="modern_syntax_loc_regression.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail(
      "modern syntax fixture was rejected by {parser}: {diagnostics.to_json().stringify(indent=2)}",
    )
  }
  @basic.show_loc.protect(String, fn() {
    impls.map(impl_ => impl_.json_repr()).to_json()
  })
}

///|
fn parse_modern_syntax_cst_loc_regression() -> Json raise Error {
  @basic.show_loc.protect(String, fn() raise {
    cst_impls_json(
      modern_syntax_loc_regression_source, "modern_syntax_loc_regression.mbt",
    )
  })
}

///|
test "exact modern syntax locations agree across all parsers" (t : @test.Test) {
  let hand_json = parse_modern_syntax_loc_regression(Handrolled)
  let yacc_json = parse_modern_syntax_loc_regression(MoonYacc)
  let cst_json = parse_modern_syntax_cst_loc_regression()
  if hand_json != yacc_json || hand_json != cst_json {
    @fs.create_dir(debug_dir)
    @fs.write_string_to_file(
      "\{debug_dir}/modern_syntax_hand.json",
      hand_json.stringify(indent=2),
    )
    @fs.write_string_to_file(
      "\{debug_dir}/modern_syntax_yacc.json",
      yacc_json.stringify(indent=2),
    )
    @fs.write_string_to_file(
      "\{debug_dir}/modern_syntax_cst.json",
      cst_json.stringify(indent=2),
    )
    fail(
      (
        $|loc-sensitive modern AST mismatch.
        $|handrolled:
        $|\{hand_json.stringify(indent=2)}
        $|
        $|yacc:
        $|\{yacc_json.stringify(indent=2)}
        $|
        $|cst:
        $|\{cst_json.stringify(indent=2)}
      ),
    )
  }
  t.write(hand_json.stringify(indent=2))
  t.snapshot(filename="modern_syntax_locations.json")
}

///|
priv struct LabelledPunLocationCollector {
  locations : Array[(@basic.Location, @basic.Location)]
  labels : Array[@basic.Location]
}

///|
extend LabelledPunLocationCollector with IterVisitor::{base, visit_Impl}

///|
impl IterVisitor for LabelledPunLocationCollector with fn visit_Argument(
  self,
  argument,
) {
  match argument.kind {
    LabelledPun(label) => self.locations.push((argument.value.loc(), label.loc))
    _ => ()
  }
  self.base().visit_Argument(argument)
}

///|
impl IterVisitor for LabelledPunLocationCollector with fn visit_Label(
  self,
  label,
) {
  self.labels.push(label.loc)
}

///|
fn find_labelled_pun_locations(
  impls : @syntax.Impls,
) -> (@basic.Location, @basic.Location) raise Error {
  let collector : LabelledPunLocationCollector = { locations: [], labels: [] }
  impls.each(impl_ => collector.visit_Impl(impl_))
  match collector.locations.get(0) {
    Some(locations) if collector.locations.length() == 1 => locations
    _ => fail("expected exactly one labelled-pun argument")
  }
}

///|
fn find_single_label_location(
  impls : @syntax.Impls,
) -> @basic.Location raise Error {
  let collector : LabelledPunLocationCollector = { locations: [], labels: [] }
  impls.each(impl_ => collector.visit_Impl(impl_))
  match collector.labels.get(0) {
    Some(loc) if collector.labels.length() == 1 => loc
    _ => fail("expected exactly one label")
  }
}

///|
fn parse_labelled_pun_loc_regression(
  parser : @parser.Parser,
) -> (@basic.Location, @basic.Location, Json) raise Error {
  let source = "fn f() { g(label~) }"
  let name = "labelled_pun_loc_regression.mbt"
  let (impls, diagnostics) = @parser.parse_string(source, name~, parser~)
  if !diagnostics.is_empty() {
    fail("labelled-pun fixture was rejected by {parser}: {diagnostics}")
  }
  let (value_loc, label_loc) = find_labelled_pun_locations(impls)
  let json = @basic.show_loc.protect(String, fn() {
    impls.map(impl_ => impl_.json_repr()).to_json()
  })
  (value_loc, label_loc, json)
}

///|
fn parse_labelled_pun_cst_loc_regression() -> (
  @basic.Location,
  @basic.Location,
  Json,
) raise Error {
  let source = "fn f() { g(label~) }"
  let name = "labelled_pun_loc_regression.mbt"
  let result = @untyped_cst.parse_structure(source, name~)
  if !result.diagnostics_view().is_empty() {
    fail("labelled-pun fixture was rejected by CST parser")
  }
  let (impls, diagnostics) = result.to_impls()
  if !diagnostics.is_empty() {
    fail("labelled-pun CST lowering produced diagnostics: {diagnostics}")
  }
  let (value_loc, label_loc) = find_labelled_pun_locations(impls)
  let json = @basic.show_loc.protect(String, fn() {
    impls.map(impl_ => impl_.json_repr()).to_json()
  })
  (value_loc, label_loc, json)
}

///|
test "labelled-pun value and label locations agree across all parsers" {
  let (hand_value_loc, hand_label_loc, hand_json) = parse_labelled_pun_loc_regression(
    Handrolled,
  )
  let (yacc_value_loc, yacc_label_loc, yacc_json) = parse_labelled_pun_loc_regression(
    MoonYacc,
  )
  let (cst_value_loc, cst_label_loc, cst_json) = parse_labelled_pun_cst_loc_regression()
  inspect(hand_value_loc, content="labelled_pun_loc_regression.mbt-1:12-1:17")
  inspect(hand_label_loc, content="labelled_pun_loc_regression.mbt-1:12-1:18")
  inspect(yacc_value_loc, content="labelled_pun_loc_regression.mbt-1:12-1:17")
  inspect(yacc_label_loc, content="labelled_pun_loc_regression.mbt-1:12-1:18")
  inspect(cst_value_loc, content="labelled_pun_loc_regression.mbt-1:12-1:17")
  inspect(cst_label_loc, content="labelled_pun_loc_regression.mbt-1:12-1:18")
  if hand_json != yacc_json || hand_json != cst_json {
    fail("labelled-pun loc-sensitive AST mismatch between parsers")
  }
}

///|
fn find_top_let_constant_loc(
  impls : @syntax.Impls,
) -> @basic.Location raise Error {
  for impl_ in impls {
    match impl_ {
      TopLetDef(expr=Constant(loc~, ..), ..) => return loc
      _ => ()
    }
  }
  fail("expected a top-level constant definition")
}

///|
fn parse_prefix_constant_loc_regression(
  source_expr : String,
  parser : @parser.Parser,
) -> @basic.Location raise Error {
  let source = "let result = \{source_expr}"
  let (impls, diagnostics) = @parser.parse_string(
    source,
    name="prefix_constant_loc_regression.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail("prefix-constant fixture was rejected: \{Repr(diagnostics)}")
  }
  find_top_let_constant_loc(impls)
}

///|
fn parse_prefix_constant_cst_loc_regression(
  source_expr : String,
) -> @basic.Location raise Error {
  let source = "let result = \{source_expr}"
  let result = @untyped_cst.parse_structure(
    source,
    name="prefix_constant_loc_regression.mbt",
  )
  if !result.diagnostics_view().is_empty() {
    fail("prefix-constant fixture was rejected by CST parser")
  }
  let (impls, diagnostics) = result.to_impls()
  if !diagnostics.is_empty() {
    fail(
      "prefix-constant CST lowering produced diagnostics: \{Repr(diagnostics)}",
    )
  }
  find_top_let_constant_loc(impls)
}

///|
test "folded prefix constant locations agree across all parsers" {
  let base = "let result = ".length()
  let cases : Array[(String, Int, Int)] = [
    ("+1", 1, 2),
    ("++1", 2, 3),
    ("+-1", 1, 3),
    ("-+1", 0, 3),
    ("--1", 0, 3),
  ]
  for case in cases {
    let (source, start, end) = case
    let hand = parse_prefix_constant_loc_regression(source, Handrolled)
    let yacc = parse_prefix_constant_loc_regression(source, MoonYacc)
    let cst = parse_prefix_constant_cst_loc_regression(source)
    for loc in [hand, yacc, cst] {
      @test.assert_eq(loc.start.cnum, base + start)
      @test.assert_eq(loc.end.cnum, base + end)
    }
  }
}

///|
test "nested array-set locations agree across all parsers" {
  let source =
    #|fn nested(
    #|  foo : Array[Array[Int]],
    #|  i : Int,
    #|  j : Int,
    #|  x : Int,
    #|) {
    #|  foo[i][j] = x
    #|}
  let name = "nested_array_set_loc_regression.mbt"
  let (hand_impls, hand_diagnostics) = @parser.parse_string(
    source,
    name~,
    parser=Handrolled,
  )
  let (yacc_impls, yacc_diagnostics) = @parser.parse_string(
    source,
    name~,
    parser=MoonYacc,
  )
  if !hand_diagnostics.is_empty() || !yacc_diagnostics.is_empty() {
    fail("nested array-set fixture was rejected")
  }
  let hand_json = @basic.show_loc.protect(String, fn() {
    hand_impls.map(impl_ => impl_.json_repr()).to_json()
  })
  let yacc_json = @basic.show_loc.protect(String, fn() {
    yacc_impls.map(impl_ => impl_.json_repr()).to_json()
  })
  let cst_json = @basic.show_loc.protect(String, fn() raise {
    cst_impls_json(source, name)
  })
  if hand_json != yacc_json || hand_json != cst_json {
    fail("nested array-set loc-sensitive AST mismatch between parsers")
  }
}

///|
fn parse_loop_label_loc_regression(
  parser : @parser.Parser,
) -> @basic.Location raise Error {
  let source = "fn f() { label~: while true {} }"
  let name = "loop_label_loc_regression.mbt"
  let (impls, diagnostics) = @parser.parse_string(source, name~, parser~)
  if !diagnostics.is_empty() {
    fail("loop-label fixture was rejected by {parser}: {diagnostics}")
  }
  find_single_label_location(impls)
}

///|
fn parse_loop_label_cst_loc_regression() -> @basic.Location raise Error {
  let source = "fn f() { label~: while true {} }"
  let name = "loop_label_loc_regression.mbt"
  let result = @untyped_cst.parse_structure(source, name~)
  if !result.diagnostics_view().is_empty() {
    fail("loop-label fixture was rejected by CST parser")
  }
  let (impls, diagnostics) = result.to_impls()
  if !diagnostics.is_empty() {
    fail("loop-label CST lowering produced diagnostics: {diagnostics}")
  }
  find_single_label_location(impls)
}

///|
test "loop label excludes the colon in every parser" {
  let hand_loc = parse_loop_label_loc_regression(Handrolled)
  let yacc_loc = parse_loop_label_loc_regression(MoonYacc)
  let cst_loc = parse_loop_label_cst_loc_regression()
  inspect(hand_loc, content="loop_label_loc_regression.mbt-1:10-1:16")
  inspect(yacc_loc, content="loop_label_loc_regression.mbt-1:10-1:16")
  inspect(cst_loc, content="loop_label_loc_regression.mbt-1:10-1:16")
}

///|
let labelled_loop_expr_loc_regression_source =
  #|fn labelled_loops(items : Array[Int]) {
  #|  while_loop~: while false {}
  #|  c_style_loop~: for ;; {}
  #|  foreach_loop~: for item in items {}
  #|}

///|
fn parse_labelled_loop_expr_loc_regression(
  parser : @parser.Parser,
) -> Json raise Error {
  let name = "labelled_loop_expr_loc_regression.mbt"
  let (impls, diagnostics) = @parser.parse_string(
    labelled_loop_expr_loc_regression_source,
    name~,
    parser~,
  )
  if !diagnostics.is_empty() {
    fail("labelled-loop fixture was rejected by {parser}: {diagnostics}")
  }
  @basic.show_loc.protect(String, fn() {
    impls.map(impl_ => impl_.json_repr()).to_json()
  })
}

///|
fn parse_labelled_loop_expr_cst_loc_regression() -> Json raise Error {
  @basic.show_loc.protect(String, fn() raise {
    cst_impls_json(
      labelled_loop_expr_loc_regression_source, "labelled_loop_expr_loc_regression.mbt",
    )
  })
}

///|
test "labelled loop expression locations agree across all parsers" {
  let hand_json = parse_labelled_loop_expr_loc_regression(Handrolled)
  let yacc_json = parse_labelled_loop_expr_loc_regression(MoonYacc)
  let cst_json = parse_labelled_loop_expr_cst_loc_regression()
  if hand_json != yacc_json || hand_json != cst_json {
    fail(
      (
        $|loc-sensitive labelled-loop AST mismatch.
        $|handrolled:
        $|\{hand_json.stringify(indent=2)}
        $|
        $|yacc:
        $|\{yacc_json.stringify(indent=2)}
        $|
        $|cst:
        $|\{cst_json.stringify(indent=2)}
      ),
    )
  }
}
