///|
test "selected upstream syntax formats and reparses without fallback" {
  for
    name in [
      "parse_test_bytes_regex_literal.mbt", "pipeline_test_bytes_regex_value.mbt",
      "pipeline_test_bytes_regex_match.mbt", "pipeline_test_bytes_regex_automaton.mbt",
      "pipeline_test_range_pattern.mbt", "parse_test_attribute.mbt", "format_test_attribute2.mbt",
      "pipeline_test_derive_debug.mbt", "pipeline_test_derive_from_json.mbtx", "pipeline_test_derive_to_json.mbtx",
      "parse_test_return.mbt", "parse_test_try_simple.mbt",
    ] {
    let source = @fs.read_file_to_string("test/sync_test/__snapshot__/" + name)
    let split = @frontmatter.split(source)
    let body = source[split.body_offset:].to_owned()
    guard strict_parse(body) is Some(original) else {
      fail("rejected upstream source: " + name)
    }
    guard format_once(original.impls, original.tokens, block_line=false)
      is Some(first) else {
      fail("formatting rejected upstream source: " + name)
    }
    guard strict_parse(first) is Some(reparsed) else {
      fail("formatted source failed to parse: " + name)
    }
    assert_true(same_semantics(original, reparsed), msg=name)
    guard format_once(reparsed.impls, reparsed.tokens, block_line=false)
      is Some(second) else {
      fail("formatting rejected reparsed source: " + name)
    }
    assert_eq(second, first, msg=name)
    assert_eq(format(body, block_line=false), first, msg=name)
  }
}
