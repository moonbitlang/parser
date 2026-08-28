///|
fn run_modified_sync_lexer_test(t : @test.Test) -> Unit raise Error {
  let name = match t.name() {
    ['l', 'e', 'x', 'i', 'n', 'g', ' ', .. rest] => rest
    otherwise => fail("wrong test label: \{otherwise}")
  }
  @basic.show_loc.val = String
  let source_file = "test/manual_test/__snapshot__/\{name}.mbt"
  let source = @fs.read_file_to_string(source_file) catch {
    err => fail("failed to read file \{name}.mbt: \{err}")
  }
  let lex_result = @lexer.tokens_from_string(source, comment=true)
  let errors = lex_result.errors.filter(triple => {
    match triple.2 {
      Reserved_keyword(_) => false
      _ => true
    }
  })
  if !errors.is_empty() {
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
test "parse_test_try_simple_modified" (t : @test.Test) {
  t.run()
}

///|
test "parse_test_tuples_modified" (t : @test.Test) {
  t.run()
}

///|
test "pipeline_test_foreach_modified" (t : @test.Test) {
  t.run()
}

///|
test "lexing parse_test_error_type_modified" (t : @test.Test) {
  run_modified_sync_lexer_test(t)
}

///|
test "lexing parse_test_invalid_error_type_modified" (t : @test.Test) {
  run_modified_sync_lexer_test(t)
}

///|
test "lexing parse_test_try_simple_modified" (t : @test.Test) {
  run_modified_sync_lexer_test(t)
}

///|
test "lexing parse_test_tuples_modified" (t : @test.Test) {
  run_modified_sync_lexer_test(t)
}

///|
test "lexing parse_test_tuples_err10_modified" (t : @test.Test) {
  run_modified_sync_lexer_test(t)
}

///|
test "lexing parse_test_tuples_err9_modified" (t : @test.Test) {
  run_modified_sync_lexer_test(t)
}

///|
test "lexing pipeline_test_foreach_modified" (t : @test.Test) {
  run_modified_sync_lexer_test(t)
}

///|
test "lexing pipeline_test_lexmatch_first_modified" (t : @test.Test) {
  run_modified_sync_lexer_test(t)
}

///|
test "lexing pipeline_test_match_using_regex_modified" (t : @test.Test) {
  run_modified_sync_lexer_test(t)
}
