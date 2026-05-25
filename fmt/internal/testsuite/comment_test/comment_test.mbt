///|
fn read_fixture(path : String) -> String {
  @fs.read_file_to_string(path) catch {
    _ => @fs.read_file_to_string(path) catch { _ => "" }
  }
}

///|
fn @test.Test::run(t : Self) -> Unit raise {
  let name = t.name()
  let source = read_fixture(
    "./fmt/internal/testsuite/comment_test/__snapshot__/\{name}.input",
  )
  // Keep parser locations stable across LF and CRLF checkouts.
  let source = source
    .replace_all(old="\r\n", new="\n")
    .replace_all(old="\r", new="\n")
  let (comments_info, formatted) = @format.debug_comment(source)
  t.write(formatted)
  t.writeln("==========================================================")
  t.writeln("// generated file, do not edit!")
  t.writeln(comments_info)
  t.snapshot(filename="\{name}.output")
}

///|
test "expr_sequence" (t : @test.Test) {
  t.run()
}

///|
test "expr_sequence_array" (t : @test.Test) {
  t.run()
}

///|
test "expr_sequence_array2" (t : @test.Test) {
  t.run()
}

///|
test "expr_sequence_array3" (t : @test.Test) {
  t.run()
}

///|
test "top_func" (t : @test.Test) {
  t.run()
}
