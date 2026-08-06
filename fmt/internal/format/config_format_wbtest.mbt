///|
test "moon.pkg keeps ordered fields, duplicate applies, and all imports" {
  let source =
    #|import { "normal" }
    #|dev_build(rule: "first", input: "a", output: "b")
    #|import { "test/pkg" @tp } for "test"
    #|warnings = "+all"
    #|rule(name: "r1", command: "one")
    #|supported_targets = "+wasm"
    #|import { "wb/pkg" } for "wbtest"
    #|pkgtype(kind: "lib")
    #|formatter(command: "fmt")
    #|dev_build(rule: "second", input: "c", output: "d")
    #|rule(name: "r2", command: "two")
    #|options(
    #|  "is-main": true,
    #|  custom: [1, 2],
    #|)
  inspect(
    format_moon_pkg(source),
    content=(
      #|import {
      #|  "normal",
      #|}
      #|
      #|dev_build(rule: "first", input: "a", output: "b")
      #|
      #|import {
      #|  "test/pkg" @tp,
      #|} for "test"
      #|
      #|warnings = "+all"
      #|
      #|rule(name: "r1", command: "one")
      #|
      #|supported_targets = "+wasm"
      #|
      #|import {
      #|  "wb/pkg",
      #|} for "wbtest"
      #|
      #|pkgtype(kind: "lib")
      #|
      #|formatter(command: "fmt")
      #|
      #|dev_build(rule: "second", input: "c", output: "d")
      #|
      #|rule(name: "r2", command: "two")
      #|
      #|options(
      #|  "is-main": true,
      #|  custom: [ 1, 2 ],
      #|)
    ),
  )
}

///|
test "moon.mod keeps metadata order, source, rule, deps, and options" {
  let source =
    #|description = "desc"
    #|name = "example/app"
    #|import { "a@1", "b" }
    #|source = "src"
    #|rule(name: "first", command: "one")
    #|version = "0.1.0"
    #|warnings = "+all"
    #|readme = "README.md"
    #|repository = "repo"
    #|license = "MIT"
    #|keywords = ["a", "b"]
    #|supported_targets = ["native", "wasm-gc"]
    #|preferred_target = "native"
    #|rule(name: "second", command: "two")
    #|options(extra: true)
  inspect(
    format_moon_mod(source),
    content=(
      #|description = "desc"
      #|
      #|name = "example/app"
      #|
      #|import {
      #|  "a@1",
      #|  "b",
      #|}
      #|
      #|source = "src"
      #|
      #|rule(name: "first", command: "one")
      #|
      #|version = "0.1.0"
      #|
      #|warnings = "+all"
      #|
      #|readme = "README.md"
      #|
      #|repository = "repo"
      #|
      #|license = "MIT"
      #|
      #|keywords = [ "a", "b" ]
      #|
      #|supported_targets = [ "native", "wasm-gc" ]
      #|
      #|preferred_target = "native"
      #|
      #|rule(name: "second", command: "two")
      #|
      #|options(
      #|  extra: true,
      #|)
    ),
  )
}

///|
test "configuration comments are conserved and formatting is idempotent" {
  let source =
    #|// header
    #|warnings = "+all" // warnings
    #|
    #|// before import
    #|import {
    #|  // before a
    #|  "a@1", // a
    #|  "b", // b
    #|} // imports
    #|options(
    #|  // before option
    #|  extra: [
    #|    1, // one
    #|    2,
    #|  ], // option
    #|)
    #|// eof
  let formatted = format_moon_mod(source)
  assert_not_eq(formatted, source)
  assert_eq(format_moon_mod(formatted), formatted)
  let source_comments = config_comment_contents(
    @lexer.tokens_from_string(comment=true, source).tokens,
  )
  let formatted_comments = config_comment_contents(
    @lexer.tokens_from_string(comment=true, formatted).tokens,
  )
  assert_eq(formatted_comments, source_comments)
}

///|
test "dependency edits preserve order and transfer only leading comments" {
  let source =
    #|name = "app"
    #|import {
    #|  // belongs to the next surviving item
    #|  "remove@1", // removed inline
    #|  "keep@2", // kept inline
    #|}
  let formatted = format_moon_mod(
    source,
    remove_dependencies=["remove"][:],
    add_dependencies=["keep@3", "new@4"][:],
  )
  assert_true(formatted.contains("// belongs to the next surviving item"))
  assert_false(formatted.contains("// removed inline"))
  assert_true(formatted.contains("// kept inline"))
  assert_true(formatted.contains("\"keep@3\""))
  assert_true(formatted.contains("\"new@4\""))
  assert_true(
    formatted.find("\"keep@3\"").unwrap() < formatted.find("\"new@4\"").unwrap(),
  )
}

///|
test "removing one same-line dependency keeps the survivor comment" {
  let source =
    #|name = "app"
    #|import { "remove@1", "keep@2" } // belongs to keep
  let formatted = format_moon_mod(source, remove_dependencies=["remove"][:])
  assert_true(formatted.contains("\"keep@2\""))
  assert_true(formatted.contains("// belongs to keep"))
}
