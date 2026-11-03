# moonbitlang/moon_config

`moonbitlang/moon_config` parses MoonBit project configuration files
into a source-preserving syntax tree with locations, tokens and diagnostics.

It provides `parse_moon_mod`, `parse_moon_pkg`, and `parse_moon_work` for
`moon.mod`, `moon.pkg`, and `moon.work` content.

The parse functions validate without rewriting syntax. Errors and warnings are
separate; use `validate=false` for syntax only. `Ast::to_json()` preserves syntax except that repeated `key(...)` calls become `key = [{...}, {...}]`.

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
  let result = @moon_config.parse_moon_mod(source)
  assert_eq(result.diagnostics.errors.length(), 0) // no error
  assert_true(result.ast.to_json() is Array([_, _, _]))
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
  let result = @moon_config.parse_moon_pkg(source)
  assert_eq(result.diagnostics.errors.length(), 0) // no error
  assert_eq(result.ast.0.length(), 2)
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
  let result = @moon_config.parse_moon_work(source)
  assert_eq(result.diagnostics.errors.length(), 0) // no error
  assert_eq(result.ast.0.length(), 2)
}
```
