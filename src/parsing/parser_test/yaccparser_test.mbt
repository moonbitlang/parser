///|

///|
fn test_yacc_parser(
  t : @test.T,
  label : String,
  loc~ : SourceLoc = _,
) -> Unit raise Error {
  let filename = "src/parsing/parser_test/fixtures/\{label}.mbt_"
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
  t.writeln(
    @syntax.impls_to_json(@parser.structure(tokens)).stringify(indent=2),
  )
  t.snapshot(filename="\{label}.mbt.json")
}

///|
test "parse abbreviation_let" (t : @test.T) {
  test_yacc_parser(t, "abbreviation_let")
}

///|
test "parse allow_if_match_in_opr" (t : @test.T) {
  test_yacc_parser(t, "allow_if_match_in_opr")
}

///|
test "parse allow_trailing_comma" (t : @test.T) {
  test_yacc_parser(t, "allow_trailing_comma")
}

// TODO: align CharLiteral in lexer with ideas
// test "parse array_pat_trailing_comma" (t : @test.T) {
//   test_yacc_parser(t, "array_pat_trailing_comma")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse array_rest_pat" (t : @test.T) {
//   test_yacc_parser(t, "array_rest_pat")
// }

///|
test "parse array_spread" (t : @test.T) {
  test_yacc_parser(t, "array_spread")
}

///|
test "parse array_spread2" (t : @test.T) {
  test_yacc_parser(t, "array_spread2")
}

///|
test "parse array2" (t : @test.T) {
  test_yacc_parser(t, "array2")
}

// TODO: fix parse arrow_fn_error
// test "parse arrow_fn_error" (t : @test.T) {
//   test_yacc_parser(t, "arrow_fn_error")
// }

///|
test "parse arrow_fn" (t : @test.T) {
  test_yacc_parser(t, "arrow_fn")
}

///|
test "parse arrow_fn4" (t : @test.T) {
  test_yacc_parser(t, "arrow_fn4")
}

///|
test "parse async_fn" (t : @test.T) {
  test_yacc_parser(t, "async_fn")
}

// TODO: fix parse async_lambda
// test "parse async_lambda" (t : @test.T) {
//   test_yacc_parser(t, "async_lambda")
// }

///|
test "parse async_type" (t : @test.T) {
  test_yacc_parser(t, "async_type")
}

///|
test "parse attribute_name" (t : @test.T) {
  test_yacc_parser(t, "attribute_name")
}

// TODO: fix parse attribute
// test "parse attribute" (t : @test.T) {
//   test_yacc_parser(t, "attribute")
// }

///|
test "parse augmented" (t : @test.T) {
  test_yacc_parser(t, "augmented")
}

// TODO: fill doc in AST
// test "parse brace_grouped_ident" (t : @test.T) {
//   test_yacc_parser(t, "brace_grouped_ident")
// }

///|
test "parse byte_literal_in_parens" (t : @test.T) {
  test_yacc_parser(t, "byte_literal_in_parens")
}

// TODO: fix parse callback_test
// test "parse callback_test" (t : @test.T) {
//   test_yacc_parser(t, "callback_test")
// }

///|
test "parse cascade" (t : @test.T) {
  test_yacc_parser(t, "cascade")
}

///|
test "parse catch_all" (t : @test.T) {
  test_yacc_parser(t, "catch_all")
}

///|
test "parse colon_recovery" (t : @test.T) {
  test_yacc_parser(t, "colon_recovery")
}

///|
test "parse const" (t : @test.T) {
  test_yacc_parser(t, "const")
}

///|
test "parse constr_mut_field" (t : @test.T) {
  test_yacc_parser(t, "constr_mut_field")
}

///|
test "parse constr_unit" (t : @test.T) {
  test_yacc_parser(t, "constr_unit")
}

// TODO: fix parse
// test "parse constrained_type_param" (t : @test.T) {
//   test_yacc_parser(t, "constrained_type_param")
// }

///|
test "parse continue_labeled_loop" (t : @test.T) {
  test_yacc_parser(t, "continue_labeled_loop")
}

///|
test "parse default_impl" (t : @test.T) {
  test_yacc_parser(t, "default_impl")
}

///|
test "parse defer" (t : @test.T) {
  test_yacc_parser(t, "defer")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse derive_arguments" (t : @test.T) {
//   test_yacc_parser(t, "derive_arguments")
// }

// TODO: fix parse derive
// test "parse derive" (t : @test.T) {
//   test_yacc_parser(t, "derive")
// }

// TODO: fill doc in AST
// test "parse docstring" (t : @test.T) {
//   test_yacc_parser(t, "docstring")
// }

// TODO: fill doc in AST
// test "parse docstring2" (t : @test.T) {
//   test_yacc_parser(t, "docstring2")
// }

///|
test "parse enum_custom_tag" (t : @test.T) {
  test_yacc_parser(t, "enum_custom_tag")
}

///|
test "parse enumview" (t : @test.T) {
  test_yacc_parser(t, "enumview")
}

// TODO: wait for astserde
// test "parse error_polymorphism" (t : @test.T) {
//   test_yacc_parser(t, "error_polymorphism")
// }

// TODO: fix parse error_recovery
// test "parse error_recovery" (t : @test.T) {
//   test_yacc_parser(t, "error_recovery")
// }

// TODO: interp parser && CharLiteral
// test "parse escape" (t : @test.T) {
//   test_yacc_parser(t, "escape")
// }

///|
test "parse explicit_method" (t : @test.T) {
  test_yacc_parser(t, "explicit_method")
}

///|
test "parse explicit_record" (t : @test.T) {
  test_yacc_parser(t, "explicit_record")
}

///|
test "parse extern_type" (t : @test.T) {
  test_yacc_parser(t, "extern_type")
}

// ignore error recovery related testcase for moonyacc parser
// test "failed_error_recover" (t : @test.T) {
//   test_yacc_parser(t, "failed_error_recover")
// }

///|
test "parse ffi" (t : @test.T) {
  test_yacc_parser(t, "ffi")
}

///|
test "parse field_vs_method" (t : @test.T) {
  test_yacc_parser(t, "field_vs_method")
}

///|
test "parse float_crlf" (t : @test.T) {
  test_yacc_parser(t, "float_crlf")
}

///|
test "parse fn_arguments" (t : @test.T) {
  test_yacc_parser(t, "fn_arguments")
}

///|
test "parse fnname_bang" (t : @test.T) {
  test_yacc_parser(t, "fnname_bang")
}

// TODO: fix parse for_else
// test "parse for_else" (t : @test.T) {
//   test_yacc_parser(t, "for_else")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse for" (t : @test.T) {
//   test_yacc_parser(t, "for")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse foreach" (t : @test.T) {
//   test_yacc_parser(t, "foreach")
// }
