///|
fn read_fixture(path : String) -> String raise {
  @fs.read_file_to_string(path)
}

///|
fn inspect_nth(
  source : String,
  index : Int,
  is_markdown? : Bool = false,
) -> String {
  match @format.format_nth_impl_from_source(source, index, is_markdown~) {
    None => "none"
    Some((line_start, line_end, formatted)) =>
      "old loc: (\{line_start}-\{line_end})\n\{formatted}"
  }
}

///|
fn @test.Test::run(t : Self) -> Unit raise {
  let name = t.name()
  let source = read_fixture(
    "./fmt/internal/testsuite/style_test/fixtures/\{name}.input",
  )
  let round1 = @format.format(source)
  ast_check(name~, source~, round1~)
  let round2 = @format.format(round1)
  idempotent_check(name~, round1~, round2~)
  t.writeln("// generated file, do not edit!")
  t.write(round1)
  t.snapshot(filename="\{name}.output")
}

///|
fn @test.Test::run_expect_change(t : Self) -> Unit raise {
  let source = read_fixture(
    "./fmt/internal/testsuite/style_test/fixtures/\{t.name()}.input",
  )
  if @format.format(source) == source {
    fail("\{t.name()} was returned without being formatted")
  }
  t.run()
}

///|
fn idempotent_check(
  name~ : String,
  round1~ : String,
  round2~ : String,
) -> Unit raise {
  if round1 != round2 {
    fail(
      (
        $| format(\{name}.mbt) is not idempotent. 
        $|================= round1 =================
        $|\{round1}
        $|================= round2 =================
        $|\{round2}
      ),
    )
  }
}

///|
fn ast_check(name~ : String, source~ : String, round1~ : String) -> Unit raise {
  @basic.show_loc.val = String
  defer {
    @basic.show_loc.val = Hidden
  }
  let (source_ast, report1, _) = @format.parse(source)
  let (round1_ast, report2, _) = @format.parse(round1)
  if report1.length() != 0 {
    fail(
      (
        $| format(\{name}.mbt) produced diagnostics on the original source.
        $|================= diagnostics =================
        $|\{report1.to_json().stringify(indent=2)})}
      ),
    )
  }
  if report2.length() != 0 {
    fail(
      (
        $| format(\{name}.mbt) produced diagnostics on the formatted source.
        $|================= diagnostics =================
        $|\{report2.to_json().stringify(indent=2)}
      ),
    )
  }
  if source == round1 {
    return
  }
  if !@format.semantically_equivalent(source, round1) {
    fail(
      (
        $| format(\{name}.mbt) changed the AST.
        $|================= source AST =================
        $|\{source_ast.to_json().stringify(indent=2)}
        $|================= round1 AST =================
        $|\{round1_ast.to_json().stringify(indent=2)}
      ),
    )
  }
}

///|
test "format nth impl returns one-based source range" {
  let source =
    #|
    #|enum Foo {
    #|  Bar; Baz
    #|}
    #|
    #|/// doc
    #|fn f(x: Int) -> Unit { println(x) }
    #|
    #|fn h(x: Foo) -> Unit { //comment4
    #|      println(x)  //comment5
    #|}
  inspect(
    inspect_nth(source, 0),
    content=(
      #|old loc: (2-4)
      #|///|
      #|enum Foo {
      #|  Bar
      #|  Baz
      #|}
    ),
  )
  inspect(
    inspect_nth(source, 1),
    content=(
      #|old loc: (6-7)
      #|///|
      #|/// doc
      #|fn f(x : Int) -> Unit {
      #|  println(x)
      #|}
    ),
  )
  inspect(
    inspect_nth(source, 2),
    content=(
      #|old loc: (9-11)
      #|///|
      #|fn h(x : Foo) -> Unit { //comment4
      #|  println(x) //comment5
      #|}
    ),
  )
  inspect(inspect_nth(source, 3), content="none")
}

///|
test "format nth impl rejects any full-file parse diagnostic" {
  let source =
    #|enum Foo {
    #|  Bar
    #|  Baz
    #|}
    #|
    #|fn f(x: Int*syntax error!!!*) -> Unit { println(x) }
  inspect(inspect_nth(source, 0), content="none")
  inspect(inspect_nth(source, 1), content="none")
}

///|
test "format nth impl does not treat URL text as a comment" {
  let source =
    #|fn url( )->String { "https://example.com/a//b" } // real comment
  match @format.format_nth_impl_from_source(source, 0) {
    Some((_, _, formatted)) =>
      if !formatted.contains("\"https://example.com/a//b\"") ||
        !formatted.contains("// real comment") {
        fail("nth formatting corrupted URL/comment boundaries: \{formatted}")
      }
    None => fail("valid URL implementation was rejected")
  }
}

///|
test "format keeps invalid source unchanged" {
  let source =
    #|fn broken() -> Unit {
    #|  let x =
    #|}
  inspect(@format.format(source, block_line=false), content=source)
}

///|
test "format keeps source unchanged when parser reports diagnostics" {
  let source =
    #|enum Foo {
    #|  Bar
    #|  Baz
    #|}
    #|
    #|fn f(x: Int*syntax error!!!*) -> Unit { println(x) }
  inspect(@format.format(source, block_line=false), content=source)
}

///|
test "format keeps lexically invalid source unchanged" {
  let source =
    #|fn broken() -> Unit {
    #|  let s = "unterminated
    #|}
  inspect(@format.format(source, block_line=false), content=source)
}

///|
test "format nth impl in markdown check fences keeps indentation" {
  let quote =
    #|> Quote
    #|>
    #|> ```mbt check
    #|> fn f(x: Int) -> Unit { println(x) }
    #|> ```
  inspect(
    inspect_nth(quote, 0, is_markdown=true),
    content=(
      #|old loc: (4-4)
      #|> ///|
      #|> fn f(x : Int) -> Unit {
      #|>   println(x)
      #|> }
    ),
  )
  let list =
    #|- item
    #|
    #|  ```mbt check
    #|  fn g(x: Int) -> Unit { println(x) }
    #|  ```
  inspect(
    inspect_nth(list, 0, is_markdown=true),
    content=(
      #|old loc: (4-4)
      #|  ///|
      #|  fn g(x : Int) -> Unit {
      #|    println(x)
      #|  }
    ),
  )
}

///|
test "format markdown formats closed mbt check fences only" {
  let source =
    #|# Heading
    #|
    #|```mbt check
    #|fn f(x: Int) -> Unit { println(x) }
    #|```
    #|
    #|```moonbit
    #|fn g(x: Int) -> Unit { println(x) }
    #|```
  inspect(
    @format.format_markdown(source).trim_end(),
    content=(
      #|# Heading
      #|
      #|```mbt check
      #|fn f(x : Int) -> Unit {
      #|  println(x)
      #|}
      #|```
      #|
      #|```moonbit
      #|fn g(x: Int) -> Unit { println(x) }
      #|```
    ),
  )
}

///|
test "format json moon.pkg migrates known keys and options" {
  let input =
    #|{
    #|  "is-main": true,
    #|  "warn-list": "+s1+s2-s3-s4@s5",
    #|  "supported-targets": "+wasm"
    #|}
  inspect(
    @format.format_json_to_moon_pkg(input),
    content=(
      #|warnings = "+s1+s2-s3-s4@s5"
      #|
      #|supported_targets = "+wasm"
      #|
      #|options(
      #|  "is-main": true,
      #|)
    ),
  )
}

///|
test "format json moon.mod migrates deps and metadata" {
  let input =
    #|{
    #|  "name": "path/to/module",
    #|  "version": "0.1.0",
    #|  "deps": {
    #|    "path/to/dependency1": "1.2.3",
    #|    "path/to/dependency2": "4.5.6",
    #|    "module/name": {
    #|      "path": "../local_path/to/dependency"
    #|    }
    #|  },
    #|  "warn-list": "+w1-w2@w3",
    #|  "readme": "README.md",
    #|  "include": ["src/", "lib/"],
    #|  "license": "MIT"
    #|}
  inspect(
    @format.format_json_to_moon_mod(input),
    content=(
      #|name = "path/to/module"
      #|
      #|version = "0.1.0"
      #|
      #|import {
      #|  "path/to/dependency1@1.2.3",
      #|  "path/to/dependency2@4.5.6",
      #|  "module/name",
      #|}
      #|
      #|warnings = "+w1-w2@w3"
      #|
      #|readme = "README.md"
      #|
      #|license = "MIT"
      #|
      #|options(
      #|  include: [ "src/", "lib/" ],
      #|)
    ),
  )
}

///|
test "format current moon.mod applies dependency edits" {
  let source =
    #|name = "example/app"
    #|version = "0.1.0"
    #|
    #|import {
    #|  "a@1.0.0",
    #|  "b@2.0.0",
    #|}
  inspect(
    @format.format_moon_mod(
      source,
      add_dependencies=["c@3.0.0"][:],
      remove_dependencies=["a"][:],
    ).trim_end(),
    content=(
      #|name = "example/app"
      #|
      #|version = "0.1.0"
      #|
      #|import {
      #|  "b@2.0.0",
      #|  "c@3.0.0",
      #|}
    ),
  )
}

///|
test "format current moon.pkg normalizes DSL config" {
  let source =
    #|warnings = "+w1"
    #|supported_targets = "+wasm"
    #|options(
    #|  "is-main": true,
    #|)
  inspect(
    @format.format_moon_pkg(source).trim_end(),
    content=(
      #|warnings = "+w1"
      #|
      #|supported_targets = "+wasm"
      #|
      #|options(
      #|  "is-main": true,
      #|)
    ),
  )
}

///|
test "compose_case_rhs" (t : @test.Test) {
  t.run_expect_change()
}

///|
test "compose_let_rhs" (t : @test.Test) {
  t.run_expect_change()
}

///|
test "chained_dot" (t : @test.Test) {
  t.run_expect_change()
}

///|
test "mixed_infix" (t : @test.Test) {
  t.run_expect_change()
}

///|
test "compose_arrow_rhs" (t : @test.Test) {
  t.run_expect_change()
}

///|
test "trailing_block" (t : @test.Test) {
  t.run_expect_change()
}

///|
test "cases" (t : @test.Test) {
  t.run()
}

///|
test "typedecl_enum" (t : @test.Test) {
  t.run()
}

///|
test "typedecl_struct" (t : @test.Test) {
  t.run()
}

///|
test "typedecl_derive" (t : @test.Test) {
  t.run()
}

///|
test "typedecl_suberror" (t : @test.Test) {
  t.run()
}

///|
test "typedecl_type" (t : @test.Test) {
  t.run()
}

///|
test "typedecl_tuplestruct" (t : @test.Test) {
  t.run()
}

///|
test "typedecl_alias" (t : @test.Test) {
  t.run()
}

///|
test "trait_impl_decl" (t : @test.Test) {
  t.run_expect_change()
}

///|
test "top_function" (t : @test.Test) {
  t.run()
}

///|
test "attribute" (t : @test.Test) {
  t.run()
}

///|
test "ffi" (t : @test.Test) {
  t.run()
}

///|
test "arrow_function" (t : @test.Test) {
  t.run()
}

///|
test "docstring" (t : @test.Test) {
  t.run()
}

///|
test "constant_literals" (t : @test.Test) {
  t.run()
}

///|
test "using_decl" (t : @test.Test) {
  t.run()
}

///|
test "for_where" (t : @test.Test) {
  t.run()
}

///|
test "loop_nobreak" (t : @test.Test) {
  t.run()
}

///|
test "declare_keyword" (t : @test.Test) {
  t.run()
}

///|
test "lexmatch" (t : @test.Test) {
  t.run()
}

///|
test "regex_precedence" (t : @test.Test) {
  t.run()
}

///|
test "async_toplevel" (t : @test.Test) {
  t.run()
}

///|
test "new_parser_syntax" (t : @test.Test) {
  let source = read_fixture(
    "./fmt/internal/testsuite/style_test/fixtures/new_parser_syntax.input",
  )
  if @format.format(source) == source {
    fail("new parser syntax fixture was returned without being formatted")
  }
  t.run()
}
