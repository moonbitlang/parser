///|
let fixtures_path = "test/mbti_parser_test/fixtures"

///|
fn @test.Test::run(t : Self, filename : String) -> Unit raise {
  let content = @fs.read_file_to_string(
    fixtures_path + "/" + filename + ".mbti",
  )
  let (mbti, reports) = @mbti_parser.parse_string(content, name=filename)
  if !reports.is_empty() {
    fail("parsing \{filename}.mbti failed with \{reports.length()} diagnostics")
  }
  ignore(mbti)
  // t.write(mbti.to_json().stringify(indent=2))
  t.snapshot(filename=filename + ".json")
}
