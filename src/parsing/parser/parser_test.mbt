///|
fn[T : ToJson] list_to_json(list : @list.T[T]) -> Json {
  let array = []
  list.each(fn(item) { array.push(item.to_json()) })
  Json::array(array)
}

///|
fn impls_to_json(impls : @list.T[@syntax.Impl]) -> Json {
  { "type": "Program", "body": list_to_json(impls) }
}

///|
fn test_parse(
  t : @test.T,
  label : String,
  loc~ : SourceLoc = _,
) -> Unit raise Error {
  let filename = "src/parsing/parser/fixtures/\{label}.mbt_"
  let bytes = @fs.read_file_to_bytes(filename) catch {
    err => fail("failed to read file \{filename}: \{err}", loc~)
  }
  let tokens = @lexer.tokens_from_bytes(bytes, comment=false, name=filename).tokens.filter(fn(
      triple,
    ) {
      !(triple.0 is @tokens.Token::NEWLINE ||
      triple.0 is @tokens.Token::COMMENT(_))
    },
  )
  t.writeln(impls_to_json(@parser.structure(tokens)).stringify(indent=2))
  t.snapshot(filename="\{label}.mbt.json")
}

///|
test "parse abbreviation_let" (t : @test.T) {
  test_parse(t, "abbreviation_let")
}

///|
test "parse allow_if_match_in_opr" (t : @test.T) {
  test_parse(t, "allow_if_match_in_opr")
}

///|
test "parse allow_trailing_comma" (t : @test.T) {
  test_parse(t, "allow_trailing_comma")
}

// TODO: align CharLiteral in lexer with ideas
// test "parse array_pat_trailing_comma" (t : @test.T) {
//   test_parse(t, "array_pat_trailing_comma")
// }

// ignore array_rest_pat for moonyacc parser
// test "parse array_rest_pat" (t : @test.T) {
//   test_parse(t, "array_rest_pat")
// }

///|
test "parse array_spread" (t : @test.T) {
  test_parse(t, "array_spread")
}

///|
test "parse array_spread2" (t : @test.T) {
  test_parse(t, "array_spread2")
}

///|
test "parse array2" (t : @test.T) {
  test_parse(t, "array2")
}

// test "parse arrow_fn_error" (t : @test.T) {
//   test_parse(t, "arrow_fn_error")
// }

///|
test "parse arrow_fn" (t : @test.T) {
  test_parse(t, "arrow_fn")
}

///|
test "parse arrow_fn4" (t : @test.T) {
  test_parse(t, "arrow_fn4")
}

///|
test "parse async_fn" (t : @test.T) {
  test_parse(t, "async_fn")
}

// TODO: fix parse async_lambda
// test "parse async_lambda" (t : @test.T) {
//   test_parse(t, "async_lambda")
// }

///|
test "parse async_type" (t : @test.T) {
  test_parse(t, "async_type")
}

///|
test "parse attribute_name" (t : @test.T) {
  test_parse(t, "attribute_name")
}

// TODO: fix parse attribute
// test "parse attribute" (t : @test.T) {
//   test_parse(t, "attribute")
// }

///|
test "parse augmented" (t : @test.T) {
  test_parse(t, "augmented")
}

// TODO: fill doc in AST
// test "parse brace_grouped_ident" (t : @test.T) {
//   test_parse(t, "brace_grouped_ident")
// }

///|
test "parse byte_literal_in_parens" (t : @test.T) {
  test_parse(t, "byte_literal_in_parens")
}

// TODO: fix parse callback_test
// test "parse callback_test" (t : @test.T) {
//   test_parse(t, "callback_test")
// }
