///|
fn legacy_loc() -> @basic.Location {
  let pos = @basic.Position::{ fname: "", lnum: 0, bol: 0, cnum: 0 }
  { start: pos, end: pos }
}

///|
fn legacy_ast_null(loc : @basic.Location) -> Ast {
  Null(loc~)
}

///|
fn legacy_bool(value : Bool, loc : @basic.Location) -> Ast {
  Bool(value, loc~)
}

///|
fn legacy_str(value : String, loc : @basic.Location) -> Ast {
  Str(value, loc~)
}

///|
fn legacy_float(value : String, loc : @basic.Location) -> Ast {
  Float(value, loc~)
}

///|
fn legacy_arr(items : Array[Ast], loc : @basic.Location) -> Ast {
  Arr(Vector(items), loc~)
}

///|
fn legacy_obj(fields : Array[(String, Ast)], loc : @basic.Location) -> Ast {
  Obj(Vector(fields), loc~)
}

///|
fn legacy_parse_pkg(source : String) -> Ast {
  let (ast, _) = parse_moon_pkg(source)
  ast
}

///|
fn legacy_parse_mod(source : String) -> Ast {
  let (ast, _) = parse_moon_mod(source)
  ast
}

///|
fn legacy_parse_work(source : String) -> Ast {
  let (ast, _) = parse_moon_work(source)
  ast
}

///|
test "legacy JSON converts Ast values" {
  let loc = legacy_loc()
  json_inspect(
    legacy_obj(
      [
        ("null", legacy_ast_null(loc)),
        ("bool", legacy_bool(true, loc)),
        ("str", legacy_str("text", loc)),
        ("float", legacy_float("1.5", loc)),
        (
          "array",
          legacy_arr([legacy_float("1", loc), legacy_str("x", loc)], loc),
        ),
        ("object", legacy_obj([("nested", legacy_bool(false, loc))], loc)),
      ],
      loc,
    ).to_json(),
    content={
      "null": null,
      "bool": true,
      "str": "text",
      "float": 1.5,
      "array": [1, "x"],
      "object": { "nested": false },
    },
  )
}

///|
test "legacy JSON folds repeated top-level entries only" {
  let loc = legacy_loc()
  let ast = legacy_obj(
    [
      (
        "rule",
        legacy_obj(
          [
            ("name", legacy_str("rule1", loc)),
            ("command", legacy_str("exe", loc)),
            (
              "env",
              legacy_obj(
                [
                  ("k", legacy_str("first", loc)),
                  ("k", legacy_str("second", loc)),
                ],
                loc,
              ),
            ),
          ],
          loc,
        ),
      ),
      (
        "rule",
        legacy_obj(
          [
            ("name", legacy_str("rule2", loc)),
            ("command", legacy_str("exe", loc)),
          ],
          loc,
        ),
      ),
    ],
    loc,
  )
  json_inspect(ast.to_json(), content={
    "rule": [
      { "name": "rule1", "command": "exe", "env": { "k": "second" } },
      { "name": "rule2", "command": "exe" },
    ],
  })
}

///|
test "legacy JSON preserves numeric repr" {
  let loc = legacy_loc()
  json_inspect(
    legacy_obj([("big", legacy_float("1N", loc))], loc).to_json(),
    content=Json::object(
      Map::from_array([("big", Json::number(0.0, repr="1N"))]),
    ),
  )
}

///|
test "legacy JSON postprocesses moon.pkg output" {
  let source =
    #|warnings = "+w1"
    #|supported_targets = "+wasm"
    #|options(
    #|  "is-main": true,
    #|)
    #|rule(name: "rule1", command: "exe")
    #|rule(name: "rule2", command: "exe")
    #|dev_build(rule: "rule1", input: "a", output: "b")
    #|dev_build(rule: "rule2", input: "c", output: "d")
    #|
  json_inspect(legacy_parse_pkg(source).to_json(), content={
    "warn-list": "+w1",
    "supported-targets": "+wasm",
    "is-main": true,
    "rule": [
      { "name": "rule1", "command": "exe" },
      { "name": "rule2", "command": "exe" },
    ],
    "dev_build": [
      { "rule": "rule1", "input": "a", "output": "b" },
      { "rule": "rule2", "input": "c", "output": "d" },
    ],
  })
}

///|
test "legacy JSON handles package imports aliases and targets" {
  let source =
    #|import {
    #|  "moonbit-community/fullstack-one-project-doc/shared" @shared,
    #|  "moonbitlang/core/json" @json,
    #|  "moonbitlang/async",
    #|}
    #|
    #|import {
    #|  "moonbitlang/core/test",
    #|} for "test"
    #|
    #|warnings = "-test_unqualified_package"
    #|supported_targets = "native"
    #|
    #|options(
    #|  "is-main": true,
    #|  targets: {
    #|    "main.mbt": [ "native" ],
    #|    "browser.mbt": [ "js" ],
    #|  },
    #|)
    #|
  json_inspect(legacy_parse_pkg(source).to_json(), content={
    "import": [
      {
        "path": "moonbit-community/fullstack-one-project-doc/shared",
        "alias": "shared",
      },
      { "path": "moonbitlang/core/json", "alias": "json" },
      "moonbitlang/async",
    ],
    "test-import": ["moonbitlang/core/test"],
    "warn-list": "-test_unqualified_package",
    "supported-targets": "native",
    "is-main": true,
    "targets": { "main.mbt": ["native"], "browser.mbt": ["js"] },
  })
}

///|
test "legacy JSON handles link options" {
  let source =
    #|options(
    #|  link: {
    #|    "wasm-gc": {
    #|      "use-js-builtin-string": true,
    #|      "import-memory": { "module": "env", "name": "memory" },
    #|      "memory-limits": { "min": 1, "max": 4 },
    #|      "shared-memory": true,
    #|    },
    #|  },
    #|)
    #|
  json_inspect(legacy_parse_pkg(source).to_json(), content={
    "link": {
      "wasm-gc": {
        "use-js-builtin-string": true,
        "import-memory": { "module": "env", "name": "memory" },
        "memory-limits": { "min": 1, "max": 4 },
        "shared-memory": true,
      },
    },
  })
}

///|
test "legacy JSON postprocesses moon.mod output" {
  let source =
    #|name = "user/mod"
    #|version = "0.1.0"
    #|import { "dep/a@1.2.3", "dep/b" }
    #|warnings = "+w1"
    #|supported_targets = "+wasm"
    #|options(
    #|  license: "MIT",
    #|  repository: "https://example.com/repo",
    #|)
    #|rule(name: "rule1", command: "exe")
    #|rule(name: "rule2", command: "exe")
    #|
  json_inspect(legacy_parse_mod(source).to_json(), content={
    "name": "user/mod",
    "version": "0.1.0",
    "deps": { "dep/a": "1.2.3", "dep/b": "" },
    "warn-list": "+w1",
    "supported-targets": "+wasm",
    "license": "MIT",
    "repository": "https://example.com/repo",
    "rule": [
      { "name": "rule1", "command": "exe" },
      { "name": "rule2", "command": "exe" },
    ],
  })
}

///|
test "legacy JSON handles moon.mod metadata options" {
  let source =
    #|name = "example/dsl_only"
    #|version = "0.1.0"
    #|import { "moonbitlang/x@0.4.6", "../dep" }
    #|options(
    #|  "bin-deps": {
    #|    "tool/cli": { "bin-pkg": [ "cli" ], "path": "../bin" },
    #|  },
    #|  "compile-flags": [ "-DDEBUG", "-Wall" ],
    #|  description: "A DSL fixture module",
    #|  exclude: [ "target/**", "**/*.tmp" ],
    #|  include: [ "src/**", "README.mbt.md" ],
    #|  keywords: [ "dsl", "fixture" ],
    #|  license: "Apache-2.0",
    #|  "link-flags": [ "-lm" ],
    #|  "preferred-target": "wasm-gc",
    #|  readme: "README.mbt.md",
    #|  repository: "https://example.com/repo",
    #|  scripts: { "prebuild": "node ./prebuild.js", "postbuild": "echo done" },
    #|  source: "src",
    #|)
    #|
  json_inspect(legacy_parse_mod(source).to_json(), content={
    "name": "example/dsl_only",
    "version": "0.1.0",
    "deps": { "moonbitlang/x": "0.4.6", "../dep": "" },
    "bin-deps": { "tool/cli": { "bin-pkg": ["cli"], "path": "../bin" } },
    "compile-flags": ["-DDEBUG", "-Wall"],
    "description": "A DSL fixture module",
    "exclude": ["target/**", "**/*.tmp"],
    "include": ["src/**", "README.mbt.md"],
    "keywords": ["dsl", "fixture"],
    "license": "Apache-2.0",
    "link-flags": ["-lm"],
    "preferred-target": "wasm-gc",
    "readme": "README.mbt.md",
    "repository": "https://example.com/repo",
    "scripts": { "prebuild": "node ./prebuild.js", "postbuild": "echo done" },
    "source": "src",
  })
}

///|
test "legacy JSON parses moon.work output" {
  let source =
    #|members = ["./app", "./shared"]
    #|preferred_target = "wasm-gc"
    #|
  json_inspect(legacy_parse_work(source).to_json(), content={
    "members": ["./app", "./shared"],
    "preferred_target": "wasm-gc",
  })
}

///|
test "legacy JSON folds duplicate moon.mod keys" {
  let source =
    #|name = "user/first"
    #|name = "user/second"
    #|
  json_inspect(legacy_parse_mod(source).to_json(), content={
    "name": ["user/first", "user/second"],
  })
}
