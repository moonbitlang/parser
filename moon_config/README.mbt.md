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

## Conditional imports

`moon.pkg` accepts repeated import blocks, optionally preceded by one or more
`#cfg(...)` attributes. Each import retains its `conditions`, including the raw
attribute, parsed `Condition`, and source location. Conditions support booleans,
`target = "wasm" | "wasm-gc" | "js" | "native" | "llvm"`, `all(...)`, `any(...)`,
and single-argument `not(...)`. No target selection is performed. `moon.mod`
rejects conditional imports. `experimental` accepts boolean values only.

```mbt check
///|
test "conditional imports" {
  let source = "#cfg(target = \"js\")\nimport { \"platform/js\" }\nexperimental = true"
  let result = @moon_config.parse_moon_pkg(source)
  assert_eq(result.diagnostics.errors.length(), 0)
  guard result.ast.0[0] is Import(conditions~, ..) else {
    fail("expected import")
  }
  assert_eq(conditions.length(), 1)
  assert_eq(conditions[0].parsed, Some(@moon_config.Condition::Target("js")))
}
```
