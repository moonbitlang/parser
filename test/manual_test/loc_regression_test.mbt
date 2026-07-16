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
    fail("\{to_repr(diagnostics)}")
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
    fail("\{to_repr(diagnostics)}")
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
