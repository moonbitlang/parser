///|
test "inclusive range patterns accept ..<= across all source parsers" {
  let source =
    #|fn ranges(x, chars, bytes) {
    #|  match x {
    #|    -3..<=0 | 1..<=1 => true
    #|    _..<=-4 => false
    #|    _ => false
    #|  }
    #|  let _ = x is (0..<=9 as digit)
    #|  let _ = x is Some(0..<=9)
    #|  let _ = x is (0..<=9 with value = 1)
    #|  let _ = chars is ['a'..<='z', ..]
    #|  let _ = bytes is [b'0'..<=b'9', ..]
    #|  let _ = x is Lower..<=Upper
    #|}
  let name = "inclusive_range_patterns.mbt"
  @basic.show_loc.protect(String, fn() raise {
    assert_parser_sync(source, name)
    // Keep offsets unchanged so this also checks every AST source location.
    let (expected, reports) = @parser.parse_string(
      source.replace(old="..<=", new="..= "),
      name~,
      parser=Handrolled,
    )
    @test.assert_eq(reports.length(), 0)
    let (actual, _) = @parser.parse_string(source, name~, parser=Handrolled)
    @test.assert_eq(
      Json(actual.map(impl_ => impl_.json_repr())),
      Json(expected.map(impl_ => impl_.json_repr())),
    )
  })
}
