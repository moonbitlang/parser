///|
fn source_extension(name : String) -> String raise {
  let prefix = "test/sync_test/__snapshot__/" + name
  let mbt = @fs.path_exists(prefix + ".mbt")
  let mbtx = @fs.path_exists(prefix + ".mbtx")
  if mbt == mbtx {
    fail("expected exactly one .mbt or .mbtx source for " + name)
  }
  if mbtx {
    ".mbtx"
  } else {
    ".mbt"
  }
}

///|
fn reports_from_view(view : ArrayView[@basic.Report]) -> Array[@basic.Report] {
  let reports : Array[@basic.Report] = []
  for report in view {
    reports.push(report)
  }
  reports
}

///|
/// Verifies that handrolled, MoonYacc and CST lowering produce identical ASTs,
/// and that no diagnostics are generated for all test cases in `__snapshot__`.
fn @test.Test::run(t : Self) -> Unit raise Error {
  let name = t.name()
  let extension = source_extension(name)
  let source_file = "test/sync_test/__snapshot__/\{name}\{extension}"
  let source = @fs.read_file_to_string(source_file) catch {
    err => fail("failed to read file \{name}\{extension}: \{Repr(err)}")
  }
  let hand_result = if extension == ".mbtx" {
    let result = @parser.parse_mbtx(source, name~, parser=Handrolled)
    (result.body, result.diagnostics)
  } else {
    @parser.parse_string(source, name~, parser=Handrolled)
  }
  let yacc_result = if extension == ".mbtx" {
    let result = @parser.parse_mbtx(source, name~, parser=MoonYacc)
    (result.body, result.diagnostics)
  } else {
    @parser.parse_string(source, name~, parser=MoonYacc)
  }
  let (cst_result, cst_diagnostics) = if extension == ".mbtx" {
    let result = @untyped_cst.parse_mbtx(source, name~)
    (result.body, result.diagnostics)
  } else {
    let result = @untyped_cst.parse_structure(source, name~)
    (result, reports_from_view(result.diagnostics_view()))
  }
  if !(hand_result.1.is_empty() &&
    yacc_result.1.is_empty() &&
    cst_diagnostics.is_empty()) {
    let buf = StringBuilder()
    let _ : Unit = @basic.show_loc.protect(String, fn() {
      buf
      ..write_string("parsing \{name}\{extension} failed due to diagnostics.\n")
      ..write_string("Handrolled parser diagnostics: ")
      ..write_string(Json(hand_result.1).stringify(indent=2))
      ..write_string("\nYacc parser diagnostics: ")
      ..write_string(Json(yacc_result.1).stringify(indent=2))
      ..write_string("\nCST parser diagnostics: ")
      ..write_string(Json(cst_diagnostics).stringify(indent=2))
      .write_string("\n")
    })
    fail(buf.to_string())
  }
  let hand_json = Json(hand_result.0.map(impl_ => impl_.json_repr()))
  let yacc_json = Json(yacc_result.0.map(impl_ => impl_.json_repr()))
  let (cst_impls, cst_reports) = cst_result.to_impls()
  if !cst_reports.is_empty() {
    let rendered = @basic.show_loc.protect(String, fn() {
      Json(cst_reports).stringify(indent=2)
    })
    let debug_error = try {
      if !@fs.path_exists(debug_dir) {
        @fs.create_dir(debug_dir)
      }
      @fs.write_string_to_file("\{debug_dir}/\{name}\{extension}", source)
      @fs.write_string_to_file(
        "\{debug_dir}/\{name}_cst_reports.json",
        rendered,
      )
      ""
    } catch {
      err => "\nAdditionally, writing debug files failed: \{Repr(err)}"
    }
    fail(
      "converting CST for \{name}\{extension} produced diagnostics.\{debug_error}",
    )
  }
  let cst_json = Json(cst_impls.map(impl_ => impl_.json_repr()))
  if hand_json != yacc_json || hand_json != cst_json {
    let debug_error = try {
      if !@fs.path_exists(debug_dir) {
        @fs.create_dir(debug_dir)
      }
      @fs.write_string_to_file("\{debug_dir}/\{name}\{extension}", source)
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
      ""
    } catch {
      err => "\nAdditionally, writing debug files failed: \{Repr(err)}"
    }
    fail(
      "parsing \{name}\{extension} produced different ASTs between moonyacc, handrolled, and CST parsers.\{debug_error}",
    )
  }
  let (hand_loc_json, cst_loc_json) = @basic.show_loc.protect(String, fn() {
    (
      Json(hand_result.0.map(impl_ => impl_.json_repr())),
      Json(cst_impls.map(impl_ => impl_.json_repr())),
    )
  })
  if hand_loc_json != cst_loc_json {
    let debug_error = try {
      if !@fs.path_exists(debug_dir) {
        @fs.create_dir(debug_dir)
      }
      @fs.write_string_to_file("\{debug_dir}/\{name}\{extension}", source)
      @fs.write_string_to_file(
        "\{debug_dir}/\{name}_hand_loc.json",
        hand_loc_json.stringify(indent=2),
      )
      @fs.write_string_to_file(
        "\{debug_dir}/\{name}_cst_loc.json",
        cst_loc_json.stringify(indent=2),
      )
      ""
    } catch {
      err => "\nAdditionally, writing debug files failed: \{Repr(err)}"
    }
    fail(
      "parsing \{name}\{extension} produced location-sensitive AST differences between handrolled and CST parsers.\{debug_error}",
    )
  }
  let json : Json = hand_json
  t.write(json.stringify(indent=2))
  t.snapshot(filename="\{name}.json")
}

///|
fn lexer_test(t : @test.Test) -> Unit raise Error {
  let name = match t.name() {
    ['l', 'e', 'x', 'i', 'n', 'g', ' ', .. rest] => rest.to_owned()
    otherwise => fail("wrong test label: \{otherwise}")
  }
  @basic.show_loc.val = String
  let extension = source_extension(name)
  let source_file = "test/sync_test/__snapshot__/\{name}\{extension}"
  let source = @fs.read_file_to_string(source_file) catch {
    err => fail("failed to read file \{name}\{extension}: \{Repr(err)}")
  }
  let lex_result = if extension == ".mbtx" {
    let split = @frontmatter.split(source)
    @lexer.tokens_from_string(
      source[split.body_offset:],
      start_pos=split.body_start,
      comment=true,
    )
  } else {
    @lexer.tokens_from_string(source, comment=true)
  }
  let errors = lex_result.errors.filter(triple => {
    match triple.2 {
      Reserved_keyword(_) => false // need to be a warning
      _ => true
    }
  })
  if errors.length() > 0 {
    let errors = errors.map(triple => {
      (triple.0, triple.1, Show::to_string(triple.2))
    })
    fail(
      "lexing \{name}\{extension} failed due to errors: \{Json(errors).stringify(indent=2)}",
    )
  }
  let tokens : Array[Json] = lex_result.tokens.map(triple => {
    // These imported snapshots predate the separate STAR token.
    "token": match triple.0 {
      STAR => ["INFIX3", "*"]
      token => ToJson::to_json(token)
    },
    "loc": ToJson::to_json(@basic.Location::{ start: triple.1, end: triple.2, }),
  })
  t.write(Json(tokens).stringify(indent=2))
  t.snapshot(filename="\{name}\{extension}.tokens.json")
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
