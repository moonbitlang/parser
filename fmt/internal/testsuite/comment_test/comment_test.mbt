///|
fn read_fixture(path : String) -> String raise {
  @fs.read_file_to_string(path)
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
test "block line style normalizes top-level docstrings" {
  let source =
    #|
    #|enum Foo {
    #|  Bar
    #|  Baz
    #|}
    #|
    #|/// comment
    #|fn f(x: Int) -> Unit {
    #|  println(x)
    #|}
    #|
    #|__DOC_MARKER__
    #|/// This is a comment
    #|fn g(x: Foo) -> Unit {
    #|  println(x)
    #|}
    #|
    #|fn h(x: Foo) -> Unit {
    #|  println(x)
    #|}
    #|
    #|///| This is a comment
    #|fn h2(x: Foo) -> Unit {
    #|  println(x)
    #|}
  let source = source.replace_all(old="__DOC_MARKER__", new="///| ")
  inspect(
    @format.format(source, block_line=true).trim_end(),
    content=(
      #|///|
      #|enum Foo {
      #|  Bar
      #|  Baz
      #|}
      #|
      #|///|
      #|/// comment
      #|fn f(x : Int) -> Unit {
      #|  println(x)
      #|}
      #|
      #|///|
      #|/// This is a comment
      #|fn g(x : Foo) -> Unit {
      #|  println(x)
      #|}
      #|
      #|///|
      #|fn h(x : Foo) -> Unit {
      #|  println(x)
      #|}
      #|
      #|///|
      #|/// This is a comment
      #|fn h2(x : Foo) -> Unit {
      #|  println(x)
      #|}
    ),
  )
}

///|
test "block line style handles doc comments without license header" {
  let source =
    #|/// doc1
    #|/// doc2
    #|enum Foo {
    #|  Bar
    #|  Baz
    #|}
  inspect(
    @format.format(source, block_line=true).trim_end(),
    content=(
      #|///|
      #|/// doc1
      #|/// doc2
      #|enum Foo {
      #|  Bar
      #|  Baz
      #|}
    ),
  )
}

///|
test "block line style handles legacy inline doc marker" {
  let source =
    #|///| doc1
    #|/// doc2
    #|enum Foo {
    #|  Bar
    #|  Baz
    #|}
  inspect(
    @format.format(source, block_line=true).trim_end(),
    content=(
      #|///|
      #|/// doc1
      #|/// doc2
      #|enum Foo {
      #|  Bar
      #|  Baz
      #|}
    ),
  )
}

///|
test "block line style attaches doc comments separated by blank line" {
  let source =
    #|/// doc comment
    #|/// doc comment
    #|
    #|enum Foo {
    #|  Bar
    #|  Baz
    #|}
  inspect(
    @format.format(source, block_line=true).trim_end(),
    content=(
      #|///|
      #|/// doc comment
      #|/// doc comment
      #|enum Foo {
      #|  Bar
      #|  Baz
      #|}
    ),
  )
}

///|
test "block line style adds marker to bare top-level" {
  let source =
    #|enum Foo {
    #|  Bar
    #|  Baz
    #|}
  inspect(
    @format.format(source, block_line=true).trim_end(),
    content=(
      #|///|
      #|enum Foo {
      #|  Bar
      #|  Baz
      #|}
    ),
  )
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
