# moonbitlang/parser/moon_config

`moonbitlang/parser/moon_config` parses MoonBit project configuration files
into a small JSON-like AST plus diagnostics.

It provides `parse_moon_mod`, `parse_moon_pkg`, and `parse_moon_work` for
`moon.mod`, `moon.pkg`, and `moon.work` content.

The parse functions also run source-level configuration validation while
post-processing the raw AST, so their diagnostics include duplicate and
unexpected top-level keys. Use `validate_moon_mod`, `validate_moon_pkg`, or
`validate_moon_work` only when validating an already post-processed `Ast`.

## Examples

Parse a `moon.mod` file and convert the returned `Ast` to JSON:

```mbt check
///|
test "parse moon.mod" {
  let source =
    #|name = "example/app"
    #|version = "0.1.0"
    #|import { "moonbitlang/x@0.4.6" }
    #|
  let (ast, reports) = @moon_config.parse_moon_mod(source)
  assert_eq(reports.length(), 0) // no error
  json_inspect(ast.to_json(), content={
    "name": "example/app",
    "version": "0.1.0",
    "deps": { "moonbitlang/x": "0.4.6" },
  })
}
```

Parse a `moon.pkg` file:

```mbt check
///|
test "parse moon.pkg" {
  let source =
    #|import {
    #|  "moonbitlang/core/json" @json,
    #|}
    #|options(
    #|  "is-main": true,
    #|)
    #|
  let (ast, reports) = @moon_config.parse_moon_pkg(source)
  assert_eq(reports.length(), 0) // no error
  json_inspect(ast.to_json(), content={
    "import": [{ "path": "moonbitlang/core/json", "alias": "json" }],
    "is-main": true,
  })
}
```

Parse a `moon.work` file and check diagnostics:

```mbt check
///|
test "parse moon.work" {
  let source =
    #|members = ["./app", "./shared"]
    #|preferred_target = "native"
    #|
  let (ast, reports) = @moon_config.parse_moon_work(source)
  assert_eq(reports.length(), 0) // no error
  json_inspect(ast.to_json(), content={
    "members": ["./app", "./shared"],
    "preferred_target": "native",
  })
}
```
