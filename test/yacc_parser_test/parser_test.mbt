///|

///|
#callsite(autofill(loc))
fn test_yacc_parser(
  t : @test.T,
  label : String,
  loc~ : SourceLoc,
) -> Unit raise Error {
  let filename = "test/handrolled_parser_test/fixtures/\{label}.mbt"
  let bytes = @fs.read_file_to_string(filename) catch {
    err => fail("failed to read file \{filename}: \{err}", loc~)
  }
  let tokens = @lexer.tokens_from_string(bytes, comment=false, name=filename).tokens.filter(fn(
      triple,
    ) {
      !(triple.0 is @tokens.Token::NEWLINE ||
      triple.0 is @tokens.Token::COMMENT(_))
    },
  )
  t.writeln(
    @syntax.impls_to_json(@yacc_parser.structure(tokens)).stringify(indent=2),
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

///|
test "parse array_pat_trailing_comma" (t : @test.T) {
  test_yacc_parser(t, "array_pat_trailing_comma")
}

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

// ignore error recovery related testcase for moonyacc parser
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

// ignore error recovery related testcase for moonyacc parser
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

// ignore testcase that contain oldsyntax
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

// ignore error recovery related testcase for moonyacc parser
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

// ignore error recovery related testcase for moonyacc parser
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

// ignore error recovery related testcase for moonyacc parser
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

///|
test "parse error_polymorphism" (t : @test.T) {
  test_yacc_parser(t, "error_polymorphism")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse error_recovery" (t : @test.T) {
//   test_yacc_parser(t, "error_recovery")
// }

///|
test "parse escape" (t : @test.T) {
  test_yacc_parser(t, "escape")
}

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

// ignore error recovery related testcase for moonyacc parser
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

///|
test "parse foreign_constr" (t : @test.T) {
  test_yacc_parser(t, "foreign_constr")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse fun_binder" (t : @test.T) {
//   test_yacc_parser(t, "fun_binder")
// }

///|
test "parse function_type_with_trailing_comma" (t : @test.T) {
  test_yacc_parser(t, "function_type_with_trailing_comma")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse generic_fn" (t : @test.T) {
//   test_yacc_parser(t, "generic_fn")
// }

///|
test "parse get_set" (t : @test.T) {
  test_yacc_parser(t, "get_set")
}

///|
test "parse get_slice" (t : @test.T) {
  test_yacc_parser(t, "get_slice")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse gitlab_issue_1612" (t : @test.T) {
//   test_yacc_parser(t, "gitlab_issue_1612")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse guard_error" (t : @test.T) {
//   test_yacc_parser(t, "guard_error")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse guard_if" (t : @test.T) {
//   test_yacc_parser(t, "guard_if")
// }

///|
test "hello" (t : @test.T) {
  test_yacc_parser(t, "hello")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse if_match_recovery" (t : @test.T) {
//   test_yacc_parser(t, "if_match_recovery")
// }

// ignore error recovery related testcase for moonyacc parser
// test "impl" (t : @test.T) {
//   test_yacc_parser(t, "impl")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse interp_test" (t : @test.T) {
//   test_yacc_parser(t, "interp_test")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse invalid_error_type" (t : @test.T) {
//   test_yacc_parser(t, "invalid_error_type")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse invalid_expr_in_condition" (t : @test.T) {
//   test_yacc_parser(t, "invalid_expr_in_condition")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse invalid_token" (t : @test.T) {
//   test_yacc_parser(t, "invalid_token")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse invalid_trait_def" (t : @test.T) {
//   test_yacc_parser(t, "invalid_trait_def")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse invalid_type" (t : @test.T) {
//   test_yacc_parser(t, "invalid_type")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse invalid_unicode" (t : @test.T) {
//   test_yacc_parser(t, "invalid_unicode")
// }

///|
test "parse is_expr" (t : @test.T) {
  test_yacc_parser(t, "is_expr")
}

///|
test "parse iter_range" (t : @test.T) {
  test_yacc_parser(t, "iter_range")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse labelled_arg" (t : @test.T) {
//   test_yacc_parser(t, "labelled_arg")
// }

///|
test "parse let_mut" (t : @test.T) {
  test_yacc_parser(t, "let_mut")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse letrec_err" (t : @test.T) {
//   test_yacc_parser(t, "letrec_err")
// }

///|
test "parse list_example" (t : @test.T) {
  test_yacc_parser(t, "list_example")
}

// TODO: fill doc in AST
// test "parse local_docs" (t : @test.T) {
//   test_yacc_parser(t, "local_docs")
// }

///|
test "parse local_types" (t : @test.T) {
  test_yacc_parser(t, "local_types")
}

///|
test "parse local_types2" (t : @test.T) {
  test_yacc_parser(t, "local_types2")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse local_types3" (t : @test.T) {
//   test_yacc_parser(t, "local_types3")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse local_types_error" (t : @test.T) {
//   test_yacc_parser(t, "local_types_error")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse loop" (t : @test.T) {
//   test_yacc_parser(t, "loop")
// }

// TODO: fill doc in AST
// test "parse main_fn_with_return_unit" (t : @test.T) {
//   test_yacc_parser(t, "main_fn_with_return_unit")
// }

///|
test "parse map_literal" (t : @test.T) {
  test_yacc_parser(t, "map_literal")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse map_pattern" (t : @test.T) {
//   test_yacc_parser(t, "map_pattern")
// }

///|
test "parse match_arm" (t : @test.T) {
  test_yacc_parser(t, "match_arm")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse match_match" (t : @test.T) {
//   test_yacc_parser(t, "match_match")
// }

// TODO: support regex match
// test "parse match_using" (t : @test.T) {
//   test_yacc_parser(t, "match_using")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse missing_type_after_thin_arrow" (t : @test.T) {
//   test_yacc_parser(t, "missing_type_after_thin_arrow")
// }

// TODO: fix parse interp
// test "parse multiline_string_literals" (t : @test.T) {
//   test_yacc_parser(t, "multiline_string_literals")
// }

///|
test "parse negative_literal_pattern" (t : @test.T) {
  test_yacc_parser(t, "negative_literal_pattern")
}

///|
test "parse nested_exp" (t : @test.T) {
  test_yacc_parser(t, "nested_exp")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse newtype" (t : @test.T) {
//   test_yacc_parser(t, "newtype")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse no_trailing_newline_enum" (t : @test.T) {
//   test_yacc_parser(t, "no_trailing_newline_enum")
// }

///|
test "parse no_trailing_newline_extern" (t : @test.T) {
  test_yacc_parser(t, "no_trailing_newline_extern")
}

///|
test "parse no_trailing_newline_ffi1" (t : @test.T) {
  test_yacc_parser(t, "no_trailing_newline_ffi1")
}

///|
test "parse no_trailing_newline_ffi2" (t : @test.T) {
  test_yacc_parser(t, "no_trailing_newline_ffi2")
}

///|
test "parse no_trailing_newline_fn" (t : @test.T) {
  test_yacc_parser(t, "no_trailing_newline_fn")
}

///|
test "parse no_trailing_newline_let" (t : @test.T) {
  test_yacc_parser(t, "no_trailing_newline_let")
}

///|
test "parse no_trailing_newline_struct" (t : @test.T) {
  test_yacc_parser(t, "no_trailing_newline_struct")
}

///|
test "parse no_trailing_newline_type" (t : @test.T) {
  test_yacc_parser(t, "no_trailing_newline_type")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse not_syntax" (t : @test.T) {
//   test_yacc_parser(t, "not_syntax")
// }

///|
test "parse num_literal" (t : @test.T) {
  test_yacc_parser(t, "num_literal")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse open_constr_pat" (t : @test.T) {
//   test_yacc_parser(t, "open_constr_pat")
// }

///|
test "parse operator_bitwise_and" (t : @test.T) {
  test_yacc_parser(t, "operator_bitwise_and")
}

///|
test "parse operator_bitwise_or" (t : @test.T) {
  test_yacc_parser(t, "operator_bitwise_or")
}

///|
test "parse operator_bitwise_xor" (t : @test.T) {
  test_yacc_parser(t, "operator_bitwise_xor")
}

///|
test "parse optional_type_abbrv" (t : @test.T) {
  test_yacc_parser(t, "optional_type_abbrv")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse or_alias_pattern_prec" (t : @test.T) {
//   test_yacc_parser(t, "or_alias_pattern_prec")
// }

///|
test "parse package" (t : @test.T) {
  test_yacc_parser(t, "package")
}

///|
test "parse package1" (t : @test.T) {
  test_yacc_parser(t, "package1")
}

///|
test "parse package2" (t : @test.T) {
  test_yacc_parser(t, "package2")
}

///|
test "parse package3" (t : @test.T) {
  test_yacc_parser(t, "package3")
}

///|
test "parse package4" (t : @test.T) {
  test_yacc_parser(t, "package4")
}

///|
test "parse parse_float32_literal" (t : @test.T) {
  test_yacc_parser(t, "parse_float32_literal")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse partial_type_error" (t : @test.T) {
//   test_yacc_parser(t, "partial_type_error")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse path_pattern_test" (t : @test.T) {
//   test_yacc_parser(t, "path_pattern_test")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse pattern_guard" (t : @test.T) {
//   test_yacc_parser(t, "pattern_guard")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse pattern_tuple" (t : @test.T) {
//   test_yacc_parser(t, "pattern_tuple")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse pipe" (t : @test.T) {
//   test_yacc_parser(t, "pipe")
// }

///|
test "parse postfix_label" (t : @test.T) {
  test_yacc_parser(t, "postfix_label")
}

///|
test "parse pub_test0" (t : @test.T) {
  test_yacc_parser(t, "pub_test0")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse pub_test1" (t : @test.T) {
//   test_yacc_parser(t, "pub_test1")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse pub_test2" (t : @test.T) {
//   test_yacc_parser(t, "pub_test2")
// }

///|
test "parse pub_test3" (t : @test.T) {
  test_yacc_parser(t, "pub_test3")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse pub_test4" (t : @test.T) {
//   test_yacc_parser(t, "pub_test4")
// }

///|
test "parse pub_test5" (t : @test.T) {
  test_yacc_parser(t, "pub_test5")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse question_optional_arg" (t : @test.T) {
//   test_yacc_parser(t, "question_optional_arg")
// }

///|
test "parse range_pattern" (t : @test.T) {
  test_yacc_parser(t, "range_pattern")
}

///|
test "parse recand" (t : @test.T) {
  test_yacc_parser(t, "recand")
}

///|
test "parse record_pat_pun" (t : @test.T) {
  test_yacc_parser(t, "record_pat_pun")
}

///|
test "parse record_punning" (t : @test.T) {
  test_yacc_parser(t, "record_punning")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse record_punning_fail" (t : @test.T) {
//   test_yacc_parser(t, "record_punning_fail")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse recover_wrong_deliminator" (t : @test.T) {
//   test_yacc_parser(t, "recover_wrong_deliminator")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse relaxed_for" (t : @test.T) {
//   test_yacc_parser(t, "relaxed_for")
// }

///|
test "parse return" (t : @test.T) {
  test_yacc_parser(t, "return")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse same_sync_multi_times" (t : @test.T) {
//   test_yacc_parser(t, "same_sync_multi_times")
// }

// TODO: check this
// test "parse semi_insertion" (t : @test.T) {
//   test_yacc_parser(t, "semi_insertion")
// }

///|
test "parse semi_pattern_test" (t : @test.T) {
  test_yacc_parser(t, "semi_pattern_test")
}

///|
test "parse string_escape_error" (t : @test.T) {
  test_yacc_parser(t, "string_escape_error")
}

///|
test "parse string_pattern" (t : @test.T) {
  test_yacc_parser(t, "string_pattern")
}

///|
test "parse struct_spread" (t : @test.T) {
  test_yacc_parser(t, "struct_spread")
}

// TODO: check this
// test "parse super_trait" (t : @test.T) {
//   test_yacc_parser(t, "super_trait")
// }

// TODO: fill doc in AST
// test "parse text_segment_test" (t : @test.T) {
//   test_yacc_parser(t, "text_segment_test")
// }

// TODO: fill doc in AST
// test "parse toplevel_test" (t : @test.T) {
//   test_yacc_parser(t, "toplevel_test")
// }

// TODO: fill doc in AST
// test "parse trait" (t : @test.T) {
//   test_yacc_parser(t, "trait")
// }

///|
test "parse trait_lint" (t : @test.T) {
  test_yacc_parser(t, "trait_lint")
}

///|
test "parse trait_method_param_name" (t : @test.T) {
  test_yacc_parser(t, "trait_method_param_name")
}

///|
test "parse trait_obj_fn_apply" (t : @test.T) {
  test_yacc_parser(t, "trait_obj_fn_apply")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse trait_object" (t : @test.T) {
//   test_yacc_parser(t, "trait_object")
// }

///|
test "parse try" (t : @test.T) {
  test_yacc_parser(t, "try")
}

// TODO: check this
// test "parse try_error" (t : @test.T) {
//   test_yacc_parser(t, "try_error")
// }

///|
test "parse try_question" (t : @test.T) {
  test_yacc_parser(t, "try_question")
}

///|
test "parse try_simple" (t : @test.T) {
  test_yacc_parser(t, "try_simple")
}

///|
test "parse try_with_parentheses" (t : @test.T) {
  test_yacc_parser(t, "try_with_parentheses")
}

///|
test "parse tuple" (t : @test.T) {
  test_yacc_parser(t, "tuple")
}

///|
test "parse tuple_multiline_str" (t : @test.T) {
  test_yacc_parser(t, "tuple_multiline_str")
}

///|
test "parse tuples" (t : @test.T) {
  test_yacc_parser(t, "tuples")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse tuples_err1" (t : @test.T) {
//   test_yacc_parser(t, "tuples_err1")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse tuples_err10" (t : @test.T) {
//   test_yacc_parser(t, "tuples_err10")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse tuples_err2" (t : @test.T) {
//   test_yacc_parser(t, "tuples_err2")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse tuples_err3" (t : @test.T) {
//   test_yacc_parser(t, "tuples_err3")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse tuples_err4" (t : @test.T) {
//   test_yacc_parser(t, "tuples_err4")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse tuples_err5" (t : @test.T) {
//   test_yacc_parser(t, "tuples_err5")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse tuples_err6" (t : @test.T) {
//   test_yacc_parser(t, "tuples_err6")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse tuples_err7" (t : @test.T) {
//   test_yacc_parser(t, "tuples_err7")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse tuples_err8" (t : @test.T) {
//   test_yacc_parser(t, "tuples_err8")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse tuples_err9" (t : @test.T) {
//   test_yacc_parser(t, "tuples_err9")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse type_alias" (t : @test.T) {
//   test_yacc_parser(t, "type_alias")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse type_exclamation" (t : @test.T) {
//   test_yacc_parser(t, "type_exclamation")
// }

///|
test "parse uint_literal" (t : @test.T) {
  test_yacc_parser(t, "uint_literal")
}

///|
test "parse uminus" (t : @test.T) {
  test_yacc_parser(t, "uminus")
}

///|
test "parse underscore_test" (t : @test.T) {
  test_yacc_parser(t, "underscore_test")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse unfinished_string" (t : @test.T) {
//   test_yacc_parser(t, "unfinished_string")
// }

///|
test "parse unicode_test" (t : @test.T) {
  test_yacc_parser(t, "unicode_test")
}

///|
test "parse uplus" (t : @test.T) {
  test_yacc_parser(t, "uplus")
}

///|
test "parse utf16_escape" (t : @test.T) {
  test_yacc_parser(t, "utf16_escape")
}

// ignore error recovery related testcase for moonyacc parser
// test "parse while" (t : @test.T) {
//   test_yacc_parser(t, "while")
// }

// ignore error recovery related testcase for moonyacc parser
// test "parse while_else" (t : @test.T) {
//   test_yacc_parser(t, "while_else")
// }

///|
test "parse cfg_test_main_init" (t : @test.T) {
  test_yacc_parser(t, "cfg_test_main_init")
}
