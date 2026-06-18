///|
struct AstNoLoc(Ast)

///|
impl @debug.Debug for AstNoLoc with fn to_repr(self) {
  fn go(ast : Ast) -> @debug.Repr {
    match ast {
      Null(_) => @debug.Repr::ctor("Null", [])
      Bool(value, ..) =>
        @debug.Repr::ctor("Bool", [(None, @debug.to_repr(value))])
      Str(str, ..) => @debug.Repr::ctor("Str", [(None, @debug.to_repr(str))])
      Float(float, ..) =>
        @debug.Repr::ctor("Float", [(None, @debug.to_repr(float))])
      Arr(content, ..) =>
        @debug.Repr::ctor("Arr", [
          (None, @debug.Repr::array(content.to_array().map(item => go(item)))),
        ])
      Obj(fields, ..) =>
        @debug.Repr::ctor("Obj", [
          (
            None,
            @debug.Repr::array(
              fields
              .to_array()
              .map(field => {
                let (key, value) = field
                @debug.Repr::tuple([@debug.to_repr(key), go(value)])
              }),
            ),
          ),
        ])
    }
  }

  match self {
    AstNoLoc(ast) => go(ast)
  }
}

///|
test "parse_moon_pkg postprocesses options and repeatable fields" {
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
  let (ast, reports) = parse_moon_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj(
      #|  [
      #|    ("warn-list", Str("+w1")),
      #|    ("supported-targets", Str("+wasm")),
      #|    ("is-main", Bool(true)),
      #|    ("rule", Obj([("name", Str("rule1")), ("command", Str("exe"))])),
      #|    ("rule", Obj([("name", Str("rule2")), ("command", Str("exe"))])),
      #|    (
      #|      "dev_build",
      #|      Obj([("rule", Str("rule1")), ("input", Str("a")), ("output", Str("b"))]),
      #|    ),
      #|    (
      #|      "dev_build",
      #|      Obj([("rule", Str("rule2")), ("input", Str("c")), ("output", Str("d"))]),
      #|    ),
      #|  ],
      #|)
    ),
  )
}

///|
test "parse_moon_pkg keeps repeated top-level rule entries in Ast" {
  let source =
    #|rule(name:"rule1", command:"exe")
    #|rule(name:"rule2", command:"exe")
    #|
  let (ast, reports) = parse_moon_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj(
      #|  [
      #|    ("rule", Obj([("name", Str("rule1")), ("command", Str("exe"))])),
      #|    ("rule", Obj([("name", Str("rule2")), ("command", Str("exe"))])),
      #|  ],
      #|)
    ),
  )
}

///|
test "parse_moon_pkg handles package imports aliases and targets" {
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
  let (ast, reports) = parse_moon_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj(
      #|  [
      #|    (
      #|      "import",
      #|      Arr(
      #|        [
      #|          Obj(
      #|            [
      #|              ("path", Str("moonbit-community/fullstack-one-project-doc/shared")),
      #|              ("alias", Str("shared")),
      #|            ],
      #|          ),
      #|          Obj([("path", Str("moonbitlang/core/json")), ("alias", Str("json"))]),
      #|          Str("moonbitlang/async"),
      #|        ],
      #|      ),
      #|    ),
      #|    ("test-import", Arr([Str("moonbitlang/core/test")])),
      #|    ("warn-list", Str("-test_unqualified_package")),
      #|    ("supported-targets", Str("native")),
      #|    ("is-main", Bool(true)),
      #|    (
      #|      "targets",
      #|      Obj(
      #|        [
      #|          ("main.mbt", Arr([Str("native")])),
      #|          ("browser.mbt", Arr([Str("js")])),
      #|        ],
      #|      ),
      #|    ),
      #|  ],
      #|)
    ),
  )
}

///|
test "parse_moon_pkg handles link options" {
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
  let (ast, reports) = parse_moon_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj(
      #|  [
      #|    (
      #|      "link",
      #|      Obj(
      #|        [
      #|          (
      #|            "wasm-gc",
      #|            Obj(
      #|              [
      #|                ("use-js-builtin-string", Bool(true)),
      #|                (
      #|                  "import-memory",
      #|                  Obj([("module", Str("env")), ("name", Str("memory"))]),
      #|                ),
      #|                ("memory-limits", Obj([("min", Float("1")), ("max", Float("4"))])),
      #|                ("shared-memory", Bool(true)),
      #|              ],
      #|            ),
      #|          ),
      #|        ],
      #|      ),
      #|    ),
      #|  ],
      #|)
    ),
  )
}

///|
test "parse_moon_mod postprocesses imports and options" {
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
  let (ast, reports) = parse_moon_mod(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj(
      #|  [
      #|    ("name", Str("user/mod")),
      #|    ("version", Str("0.1.0")),
      #|    ("deps", Obj([("dep/a", Str("1.2.3")), ("dep/b", Str(""))])),
      #|    ("warn-list", Str("+w1")),
      #|    ("supported-targets", Str("+wasm")),
      #|    ("license", Str("MIT")),
      #|    ("repository", Str("https://example.com/repo")),
      #|    ("rule", Obj([("name", Str("rule1")), ("command", Str("exe"))])),
      #|    ("rule", Obj([("name", Str("rule2")), ("command", Str("exe"))])),
      #|  ],
      #|)
    ),
  )
}

///|
test "parse_moon_mod handles metadata options" {
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
  let (ast, reports) = parse_moon_mod(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj(
      #|  [
      #|    ("name", Str("example/dsl_only")),
      #|    ("version", Str("0.1.0")),
      #|    ("deps", Obj([("moonbitlang/x", Str("0.4.6")), ("../dep", Str(""))])),
      #|    (
      #|      "bin-deps",
      #|      Obj(
      #|        [
      #|          (
      #|            "tool/cli",
      #|            Obj([("bin-pkg", Arr([Str("cli")])), ("path", Str("../bin"))]),
      #|          ),
      #|        ],
      #|      ),
      #|    ),
      #|    ("compile-flags", Arr([Str("-DDEBUG"), Str("-Wall")])),
      #|    ("description", Str("A DSL fixture module")),
      #|    ("exclude", Arr([Str("target/**"), Str("**/*.tmp")])),
      #|    ("include", Arr([Str("src/**"), Str("README.mbt.md")])),
      #|    ("keywords", Arr([Str("dsl"), Str("fixture")])),
      #|    ("license", Str("Apache-2.0")),
      #|    ("link-flags", Arr([Str("-lm")])),
      #|    ("preferred-target", Str("wasm-gc")),
      #|    ("readme", Str("README.mbt.md")),
      #|    ("repository", Str("https://example.com/repo")),
      #|    (
      #|      "scripts",
      #|      Obj(
      #|        [
      #|          ("prebuild", Str("node ./prebuild.js")),
      #|          ("postbuild", Str("echo done")),
      #|        ],
      #|      ),
      #|    ),
      #|    ("source", Str("src")),
      #|  ],
      #|)
    ),
  )
}

///|
test "parse_moon_mod accepts documented top-level fields" {
  let source =
    #|name = "example/top_level"
    #|version = "0.1.0"
    #|readme = "README.md"
    #|repository = "https://example.com/repo"
    #|license = "MIT"
    #|keywords = ["moonbit", "parser"]
    #|description = "A module config fixture"
    #|preferred_target = "js"
    #|supported_targets = ["js", "wasm-gc"]
    #|
  let (ast, reports) = parse_moon_mod(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj(
      #|  [
      #|    ("name", Str("example/top_level")),
      #|    ("version", Str("0.1.0")),
      #|    ("readme", Str("README.md")),
      #|    ("repository", Str("https://example.com/repo")),
      #|    ("license", Str("MIT")),
      #|    ("keywords", Arr([Str("moonbit"), Str("parser")])),
      #|    ("description", Str("A module config fixture")),
      #|    ("preferred-target", Str("js")),
      #|    ("supported-targets", Arr([Str("js"), Str("wasm-gc")])),
      #|  ],
      #|)
    ),
  )
}

///|
test "parse_moon_work parses workspace config" {
  let source =
    #|members = ["./app", "./shared"]
    #|preferred_target = "wasm-gc"
    #|
  let (ast, reports) = parse_moon_work(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj(
      #|  [
      #|    ("members", Arr([Str("./app"), Str("./shared")])),
      #|    ("preferred_target", Str("wasm-gc")),
      #|  ],
      #|)
    ),
  )
}

///|
test "parse_moon_work handles multiline members" {
  let source =
    #|members = [
    #|  "./mod_a",
    #|  "./mod_b",
    #|]
    #|
  let (ast, reports) = parse_moon_work(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("members", Arr([Str("./mod_a"), Str("./mod_b")]))])
    ),
  )
}

///|
test "parse_moon_mod reports duplicate non-repeatable keys" {
  let source =
    #|name = "user/first"
    #|name = "user/second"
    #|
  let (ast, reports) = parse_moon_mod(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.mod", lnum: 2, bol: 20, cnum: 27 },
      #|      end: { fname: "moon.mod", lnum: 2, bol: 20, cnum: 40 },
      #|    },
      #|    msg: "Duplicate key `name` found in moon.mod.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("name", Str("user/first")), ("name", Str("user/second"))])
    ),
  )
}
