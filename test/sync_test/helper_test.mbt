///|
fn reports_from_view(view : ArrayView[@basic.Report]) -> Array[@basic.Report] {
  let reports : Array[@basic.Report] = []
  for report in view {
    reports.push(report)
  }
  reports
}

///|
/// Verifies that both the hand-rolled parser and the moonyacc parser produce identical ASTs,
/// and that no diagnostics are generated for all test cases in `__snapshot__`.
fn @test.Test::run(t : Self) -> Unit raise Error {
  let name = t.name()
  let source_file = "test/sync_test/__snapshot__/\{name}.mbt"
  let source = @fs.read_file_to_string(source_file) catch {
    err => fail("failed to read file \{name}.mbt: \{Repr(err)}")
  }
  let hand_result = @parser.parse_string(source, name~, parser=Handrolled)
  let yacc_result = @parser.parse_string(source, name~, parser=MoonYacc)
  let cst_result = @untyped_cst.parse_structure(source, name~)
  let cst_diagnostics = reports_from_view(cst_result.diagnostics_view())
  if !(hand_result.1.is_empty() &&
    yacc_result.1.is_empty() &&
    cst_diagnostics.is_empty()) {
    let buf = StringBuilder()
    let _ : Unit = @basic.show_loc.protect(String, fn() {
      buf
      ..write_string("parsing \{name}.mbt failed due to diagnostics.\n")
      ..write_string("Handrolled parser diagnostics: ")
      ..write_string(hand_result.1.to_json().stringify(indent=2))
      ..write_string("\nYacc parser diagnostics: ")
      ..write_string(yacc_result.1.to_json().stringify(indent=2))
      ..write_string("\nCST parser diagnostics: ")
      ..write_string(cst_diagnostics.to_json().stringify(indent=2))
      .write_string("\n")
    })
    fail(buf.to_string())
  }
  let hand_json = hand_result.0.map(impl_ => impl_.json_repr()).to_json()
  let yacc_json = yacc_result.0.map(impl_ => impl_.json_repr()).to_json()
  let (cst_impls, cst_reports) = cst_result.to_impls()
  if !cst_reports.is_empty() {
    let rendered = @basic.show_loc.protect(String, fn() {
      cst_reports.to_json().stringify(indent=2)
    })
    @fs.create_dir(debug_dir)
    @fs.write_string_to_file("\{debug_dir}/\{name}.mbt", source)
    @fs.write_string_to_file("\{debug_dir}/\{name}_cst_reports.json", rendered)
    fail("converting CST for \{name}.mbt produced diagnostics.")
  }
  let cst_json = cst_impls.map(impl_ => impl_.json_repr()).to_json()
  if hand_json != yacc_json || hand_json != cst_json {
    @fs.create_dir(debug_dir)
    @fs.write_string_to_file("\{debug_dir}/\{name}.mbt", source)
    @fs.write_string_to_file(
      "\{debug_dir}/\{name}_hand.json",
      hand_json.stringify(indent=2),
    )
    @fs.write_string_to_file(
      "\{debug_dir}/\{name}_yacc.json",
      yacc_json.stringify(indent=2),
    )
    @fs.write_string_to_file(
      "\{debug_dir}/\{name}_cst.json",
      cst_json.stringify(indent=2),
    )
    fail(
      "parsing \{name}.mbt produced different ASTs between moonyacc, handrolled, and CST parsers.",
    )
  }
  let (hand_loc_json, cst_loc_json) = @basic.show_loc.protect(String, fn() {
    (
      hand_result.0.map(impl_ => impl_.json_repr()).to_json(),
      cst_impls.map(impl_ => impl_.json_repr()).to_json(),
    )
  })
  if hand_loc_json != cst_loc_json {
    @fs.create_dir(debug_dir)
    @fs.write_string_to_file("\{debug_dir}/\{name}.mbt", source)
    @fs.write_string_to_file(
      "\{debug_dir}/\{name}_hand_loc.json",
      hand_loc_json.stringify(indent=2),
    )
    @fs.write_string_to_file(
      "\{debug_dir}/\{name}_cst_loc.json",
      cst_loc_json.stringify(indent=2),
    )
    fail(
      "parsing \{name}.mbt produced location-sensitive AST differences between handrolled and CST parsers.",
    )
  }
  let json : Json = hand_json
  t.write(json.stringify(indent=2))
  t.snapshot(filename="\{name}.json")
}

///|
fn lexer_test(t : @test.Test) -> Unit raise Error {
  let name = match t.name() {
    ['l', 'e', 'x', 'i', 'n', 'g', ' ', .. rest] => rest
    otherwise => fail("wrong test label: \{otherwise}")
  }
  @basic.show_loc.val = String
  let source_file = "test/sync_test/__snapshot__/\{name}.mbt"
  let source = @fs.read_file_to_string(source_file) catch {
    err => fail("failed to read file \{name}.mbt: \{Repr(err)}")
  }
  let lex_result = @lexer.tokens_from_string(source, comment=true)
  let errors = lex_result.errors.filter(triple => {
    match triple.2 {
      Reserved_keyword(_) => false // need to be a warning
      _ => true
    }
  })
  if errors.length() > 0 {
    let errors = errors.map(triple => (triple.0, triple.1, triple.2.to_string()))
    fail(
      "lexing \{name}.mbt failed due to errors: \{errors.to_json().stringify(indent=2)}",
    )
  }
  let tokens : Array[Json] = lex_result.tokens.map(triple => {
    "token": triple.0,
    "loc": @basic.Location::{ start: triple.1, end: triple.2, }.to_json(),
  })
  t.write(tokens.to_json().stringify(indent=2))
  t.snapshot(filename="\{name}.mbt.tokens.json")
}

///|
let debug_dir = "test/sync_test/debug"

///|
fn init {
  try {
    if @fs.path_exists(debug_dir) {
      @fs.remove_dir(debug_dir)
    }
  } catch {
    e => println(Repr(e))
  }
}
