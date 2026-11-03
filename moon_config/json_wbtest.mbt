///|
fn json_without_locations(value : Json) -> Json {
  match value {
    Object(fields) => {
      if fields.contains("start") && fields.contains("end") {
        return Json::null()
      }
      let result : Map[String, Json] = Map([])
      for key, value in fields {
        if key != "loc" && !key.has_suffix("_loc") {
          result[key] = json_without_locations(value)
        }
      }
      Json::object(result)
    }
    Array(items) => Json::array(items.map(json_without_locations))
    _ => value
  }
}

///|
fn json_test_loc() -> @basic.Location {
  let pos = @basic.Position::{ fname: "", lnum: 0, bol: 0, cnum: 0, }
  { start: pos, end: pos, }
}

///|
fn json_test_hole(loc : @basic.Location) -> Expr {
  Hole(loc)
}

///|
fn json_test_bool(value : Bool, loc : @basic.Location) -> Expr {
  Bool(value~, loc~)
}

///|
fn json_test_str(value : String, loc : @basic.Location) -> Expr {
  String(str=value, loc~)
}

///|
fn json_test_int(value : String, loc : @basic.Location) -> Expr {
  Int(repr=value, loc~)
}

///|
fn json_test_arr(items : Array[Expr], loc : @basic.Location) -> Expr {
  Array(items~, loc~)
}

///|
fn json_test_map(fields : Array[(String, Expr)], loc : @basic.Location) -> Expr {
  Map(
    fields=fields.map(pair => { key: pair.0, key_loc: loc, value: pair.1, }),
    loc~,
  )
}

///|
fn syntax_parse_pkg(source : String) -> Json {
  json_without_locations(parse_moon_pkg(source).ast.to_json())
}

///|
fn syntax_parse_mod(source : String) -> Json {
  json_without_locations(parse_moon_mod(source).ast.to_json())
}

///|
fn syntax_parse_work(source : String) -> Json {
  json_without_locations(parse_moon_work(source).ast.to_json())
}

///|
test "Ast JSON preserves expression variants" {
  let loc = json_test_loc()
  let located = Ast(Vector([Expr(Hole(loc))])).to_json()
  json_inspect(located, content=[
    {
      "$tag": "Expr",
      "0": {
        "$tag": "Hole",
        "0": {
          "start": { "fname": "", "lnum": 0, "bol": 0, "cnum": 0 },
          "end": { "fname": "", "lnum": 0, "bol": 0, "cnum": 0 },
        },
      },
    },
  ])
  json_inspect(
    json_without_locations(
      Ast(
        Vector([
          Expr(
            json_test_map(
              [
                ("null", json_test_hole(loc)),
                ("bool", json_test_bool(true, loc)),
                ("str", json_test_str("text", loc)),
                ("int", json_test_int("15", loc)),
                (
                  "array",
                  json_test_arr(
                    [json_test_int("1", loc), json_test_str("x", loc)],
                    loc,
                  ),
                ),
                (
                  "object",
                  json_test_map([("nested", json_test_bool(false, loc))], loc),
                ),
              ],
              loc,
            ),
          ),
        ]),
      ).to_json(),
    ),
    content=[
      {
        "$tag": "Expr",
        "0": {
          "$tag": "Map",
          "fields": [
            { "key": "null", "value": { "$tag": "Hole", "0": null } },
            { "key": "bool", "value": { "$tag": "Bool", "value": true } },
            { "key": "str", "value": { "$tag": "String", "str": "text" } },
            { "key": "int", "value": { "$tag": "Int", "repr": "15" } },
            {
              "key": "array",
              "value": {
                "$tag": "Array",
                "items": [
                  { "$tag": "Int", "repr": "1" },
                  { "$tag": "String", "str": "x" },
                ],
              },
            },
            {
              "key": "object",
              "value": {
                "$tag": "Map",
                "fields": [
                  {
                    "key": "nested",
                    "value": { "$tag": "Bool", "value": false },
                  },
                ],
              },
            },
          ],
        },
      },
    ],
  )
}

///|
test "Ast JSON merges repeated calls and preserves nested fields" {
  let parsed = parse_moon_pkg(
    (
      #|rule(name: "rule1", command: "exe", env: { "k": "first", "k": "second" })
      #|options("is-main": true)
      #|rule(name: "rule2", command: "exe")
      #|rule(name: "rule3", command: "exe")
    ),
  )
  let statements = parsed.ast.0.to_array()
  let exported = parsed.ast.to_json()
  assert_eq(parsed.ast.0.to_array(), statements)
  assert_eq(parsed.ast.to_json(), exported)
  let ast = json_without_locations(exported)
  json_inspect(ast, content=[
    {
      "$tag": "Assign",
      "0": {
        "key": "rule",
        "value": {
          "$tag": "Array",
          "items": [
            {
              "$tag": "Map",
              "fields": [
                { "key": "name", "value": { "$tag": "String", "str": "rule1" } },
                {
                  "key": "command",
                  "value": { "$tag": "String", "str": "exe" },
                },
                {
                  "key": "env",
                  "value": {
                    "$tag": "Map",
                    "fields": [
                      {
                        "key": "k",
                        "value": { "$tag": "String", "str": "first" },
                      },
                      {
                        "key": "k",
                        "value": { "$tag": "String", "str": "second" },
                      },
                    ],
                  },
                },
              ],
            },
            {
              "$tag": "Map",
              "fields": [
                { "key": "name", "value": { "$tag": "String", "str": "rule2" } },
                {
                  "key": "command",
                  "value": { "$tag": "String", "str": "exe" },
                },
              ],
            },
            {
              "$tag": "Map",
              "fields": [
                { "key": "name", "value": { "$tag": "String", "str": "rule3" } },
                {
                  "key": "command",
                  "value": { "$tag": "String", "str": "exe" },
                },
              ],
            },
          ],
        },
      },
    },
    {
      "$tag": "Expr",
      "0": {
        "$tag": "Apply",
        "name": "options",
        "args": [
          { "key": "is-main", "value": { "$tag": "Bool", "value": true } },
        ],
      },
    },
  ])
}

///|
test "Ast JSON preserves numeric repr" {
  let loc = json_test_loc()
  json_inspect(
    json_without_locations(
      Ast(
        Vector([Expr(json_test_map([("big", json_test_int("1N", loc))], loc))]),
      ).to_json(),
    ),
    content=[
      {
        "$tag": "Expr",
        "0": {
          "$tag": "Map",
          "fields": [{ "key": "big", "value": { "$tag": "Int", "repr": "1N" } }],
        },
      },
    ],
  )
}

///|
test "Ast JSON preserves moon.pkg output" {
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
  json_inspect(syntax_parse_pkg(source), content=[
    {
      "$tag": "Assign",
      "0": { "key": "warnings", "value": { "$tag": "String", "str": "+w1" } },
    },
    {
      "$tag": "Assign",
      "0": {
        "key": "supported_targets",
        "value": { "$tag": "String", "str": "+wasm" },
      },
    },
    {
      "$tag": "Expr",
      "0": {
        "$tag": "Apply",
        "name": "options",
        "args": [
          { "key": "is-main", "value": { "$tag": "Bool", "value": true } },
        ],
      },
    },
    {
      "$tag": "Assign",
      "0": {
        "key": "rule",
        "value": {
          "$tag": "Array",
          "items": [
            {
              "$tag": "Map",
              "fields": [
                { "key": "name", "value": { "$tag": "String", "str": "rule1" } },
                {
                  "key": "command",
                  "value": { "$tag": "String", "str": "exe" },
                },
              ],
            },
            {
              "$tag": "Map",
              "fields": [
                { "key": "name", "value": { "$tag": "String", "str": "rule2" } },
                {
                  "key": "command",
                  "value": { "$tag": "String", "str": "exe" },
                },
              ],
            },
          ],
        },
      },
    },
    {
      "$tag": "Assign",
      "0": {
        "key": "dev_build",
        "value": {
          "$tag": "Array",
          "items": [
            {
              "$tag": "Map",
              "fields": [
                { "key": "rule", "value": { "$tag": "String", "str": "rule1" } },
                { "key": "input", "value": { "$tag": "String", "str": "a" } },
                { "key": "output", "value": { "$tag": "String", "str": "b" } },
              ],
            },
            {
              "$tag": "Map",
              "fields": [
                { "key": "rule", "value": { "$tag": "String", "str": "rule2" } },
                { "key": "input", "value": { "$tag": "String", "str": "c" } },
                { "key": "output", "value": { "$tag": "String", "str": "d" } },
              ],
            },
          ],
        },
      },
    },
  ])
}

///|
test "Ast JSON preserves package imports aliases and targets" {
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
  json_inspect(syntax_parse_pkg(source), content=[
    {
      "$tag": "Import",
      "items": [
        {
          "path": "moonbit-community/fullstack-one-project-doc/shared",
          "alias_": ["shared", null],
        },
        { "path": "moonbitlang/core/json", "alias_": ["json", null] },
        { "path": "moonbitlang/async", "alias_": null },
      ],
      "kind": { "$tag": "Normal" },
    },
    {
      "$tag": "Import",
      "items": [{ "path": "moonbitlang/core/test", "alias_": null }],
      "kind": { "$tag": "Test" },
    },
    {
      "$tag": "Assign",
      "0": {
        "key": "warnings",
        "value": { "$tag": "String", "str": "-test_unqualified_package" },
      },
    },
    {
      "$tag": "Assign",
      "0": {
        "key": "supported_targets",
        "value": { "$tag": "String", "str": "native" },
      },
    },
    {
      "$tag": "Expr",
      "0": {
        "$tag": "Apply",
        "name": "options",
        "args": [
          { "key": "is-main", "value": { "$tag": "Bool", "value": true } },
          {
            "key": "targets",
            "value": {
              "$tag": "Map",
              "fields": [
                {
                  "key": "main.mbt",
                  "value": {
                    "$tag": "Array",
                    "items": [{ "$tag": "String", "str": "native" }],
                  },
                },
                {
                  "key": "browser.mbt",
                  "value": {
                    "$tag": "Array",
                    "items": [{ "$tag": "String", "str": "js" }],
                  },
                },
              ],
            },
          },
        ],
      },
    },
  ])
}

///|
test "Ast JSON preserves link options" {
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
  json_inspect(syntax_parse_pkg(source), content=[
    {
      "$tag": "Expr",
      "0": {
        "$tag": "Apply",
        "name": "options",
        "args": [
          {
            "key": "link",
            "value": {
              "$tag": "Map",
              "fields": [
                {
                  "key": "wasm-gc",
                  "value": {
                    "$tag": "Map",
                    "fields": [
                      {
                        "key": "use-js-builtin-string",
                        "value": { "$tag": "Bool", "value": true },
                      },
                      {
                        "key": "import-memory",
                        "value": {
                          "$tag": "Map",
                          "fields": [
                            {
                              "key": "module",
                              "value": { "$tag": "String", "str": "env" },
                            },
                            {
                              "key": "name",
                              "value": { "$tag": "String", "str": "memory" },
                            },
                          ],
                        },
                      },
                      {
                        "key": "memory-limits",
                        "value": {
                          "$tag": "Map",
                          "fields": [
                            {
                              "key": "min",
                              "value": { "$tag": "Int", "repr": "1" },
                            },
                            {
                              "key": "max",
                              "value": { "$tag": "Int", "repr": "4" },
                            },
                          ],
                        },
                      },
                      {
                        "key": "shared-memory",
                        "value": { "$tag": "Bool", "value": true },
                      },
                    ],
                  },
                },
              ],
            },
          },
        ],
      },
    },
  ])
}

///|
test "Ast JSON preserves moon.mod output" {
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
  json_inspect(syntax_parse_mod(source), content=[
    {
      "$tag": "Assign",
      "0": { "key": "name", "value": { "$tag": "String", "str": "user/mod" } },
    },
    {
      "$tag": "Assign",
      "0": { "key": "version", "value": { "$tag": "String", "str": "0.1.0" } },
    },
    {
      "$tag": "Import",
      "items": [
        { "path": "dep/a@1.2.3", "alias_": null },
        { "path": "dep/b", "alias_": null },
      ],
      "kind": { "$tag": "Normal" },
    },
    {
      "$tag": "Assign",
      "0": { "key": "warnings", "value": { "$tag": "String", "str": "+w1" } },
    },
    {
      "$tag": "Assign",
      "0": {
        "key": "supported_targets",
        "value": { "$tag": "String", "str": "+wasm" },
      },
    },
    {
      "$tag": "Expr",
      "0": {
        "$tag": "Apply",
        "name": "options",
        "args": [
          { "key": "license", "value": { "$tag": "String", "str": "MIT" } },
          {
            "key": "repository",
            "value": { "$tag": "String", "str": "https://example.com/repo" },
          },
        ],
      },
    },
    {
      "$tag": "Assign",
      "0": {
        "key": "rule",
        "value": {
          "$tag": "Array",
          "items": [
            {
              "$tag": "Map",
              "fields": [
                { "key": "name", "value": { "$tag": "String", "str": "rule1" } },
                {
                  "key": "command",
                  "value": { "$tag": "String", "str": "exe" },
                },
              ],
            },
            {
              "$tag": "Map",
              "fields": [
                { "key": "name", "value": { "$tag": "String", "str": "rule2" } },
                {
                  "key": "command",
                  "value": { "$tag": "String", "str": "exe" },
                },
              ],
            },
          ],
        },
      },
    },
  ])
}

///|
test "Ast JSON preserves moon.mod metadata options" {
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
  json_inspect(syntax_parse_mod(source), content=[
    {
      "$tag": "Assign",
      "0": {
        "key": "name",
        "value": { "$tag": "String", "str": "example/dsl_only" },
      },
    },
    {
      "$tag": "Assign",
      "0": { "key": "version", "value": { "$tag": "String", "str": "0.1.0" } },
    },
    {
      "$tag": "Import",
      "items": [
        { "path": "moonbitlang/x@0.4.6", "alias_": null },
        { "path": "../dep", "alias_": null },
      ],
      "kind": { "$tag": "Normal" },
    },
    {
      "$tag": "Expr",
      "0": {
        "$tag": "Apply",
        "name": "options",
        "args": [
          {
            "key": "bin-deps",
            "value": {
              "$tag": "Map",
              "fields": [
                {
                  "key": "tool/cli",
                  "value": {
                    "$tag": "Map",
                    "fields": [
                      {
                        "key": "bin-pkg",
                        "value": {
                          "$tag": "Array",
                          "items": [{ "$tag": "String", "str": "cli" }],
                        },
                      },
                      {
                        "key": "path",
                        "value": { "$tag": "String", "str": "../bin" },
                      },
                    ],
                  },
                },
              ],
            },
          },
          {
            "key": "compile-flags",
            "value": {
              "$tag": "Array",
              "items": [
                { "$tag": "String", "str": "-DDEBUG" },
                { "$tag": "String", "str": "-Wall" },
              ],
            },
          },
          {
            "key": "description",
            "value": { "$tag": "String", "str": "A DSL fixture module" },
          },
          {
            "key": "exclude",
            "value": {
              "$tag": "Array",
              "items": [
                { "$tag": "String", "str": "target/**" },
                { "$tag": "String", "str": "**/*.tmp" },
              ],
            },
          },
          {
            "key": "include",
            "value": {
              "$tag": "Array",
              "items": [
                { "$tag": "String", "str": "src/**" },
                { "$tag": "String", "str": "README.mbt.md" },
              ],
            },
          },
          {
            "key": "keywords",
            "value": {
              "$tag": "Array",
              "items": [
                { "$tag": "String", "str": "dsl" },
                { "$tag": "String", "str": "fixture" },
              ],
            },
          },
          {
            "key": "license",
            "value": { "$tag": "String", "str": "Apache-2.0" },
          },
          {
            "key": "link-flags",
            "value": {
              "$tag": "Array",
              "items": [{ "$tag": "String", "str": "-lm" }],
            },
          },
          {
            "key": "preferred-target",
            "value": { "$tag": "String", "str": "wasm-gc" },
          },
          {
            "key": "readme",
            "value": { "$tag": "String", "str": "README.mbt.md" },
          },
          {
            "key": "repository",
            "value": { "$tag": "String", "str": "https://example.com/repo" },
          },
          {
            "key": "scripts",
            "value": {
              "$tag": "Map",
              "fields": [
                {
                  "key": "prebuild",
                  "value": { "$tag": "String", "str": "node ./prebuild.js" },
                },
                {
                  "key": "postbuild",
                  "value": { "$tag": "String", "str": "echo done" },
                },
              ],
            },
          },
          { "key": "source", "value": { "$tag": "String", "str": "src" } },
        ],
      },
    },
  ])
}

///|
test "Ast JSON preserves moon.work output" {
  let source =
    #|members = ["./app", "./shared"]
    #|preferred_target = "wasm-gc"
    #|
  json_inspect(syntax_parse_work(source), content=[
    {
      "$tag": "Assign",
      "0": {
        "key": "members",
        "value": {
          "$tag": "Array",
          "items": [
            { "$tag": "String", "str": "./app" },
            { "$tag": "String", "str": "./shared" },
          ],
        },
      },
    },
    {
      "$tag": "Assign",
      "0": {
        "key": "preferred_target",
        "value": { "$tag": "String", "str": "wasm-gc" },
      },
    },
  ])
}

///|
test "Ast JSON preserves duplicate moon.mod keys" {
  let source =
    #|name = "user/first"
    #|name = "user/second"
    #|
  json_inspect(syntax_parse_mod(source), content=[
    {
      "$tag": "Assign",
      "0": { "key": "name", "value": { "$tag": "String", "str": "user/first" } },
    },
    {
      "$tag": "Assign",
      "0": {
        "key": "name",
        "value": { "$tag": "String", "str": "user/second" },
      },
    },
  ])
}
