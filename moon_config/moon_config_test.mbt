///|
struct AstNoLoc(@moon_config.Ast)

///|
impl @debug.Debug for AstNoLoc with fn to_repr(self) {
  fn expr_repr(expr : @moon_config.Expr) -> @debug.Repr {
    match expr {
      Hole(_) => @debug.Repr::ctor("Hole", [])
      Bool(value~, ..) => @debug.Repr::ctor("Bool", [(None, Repr(value))])
      Int(repr~, ..) => @debug.Repr::ctor("Int", [(None, Repr(repr))])
      String(str~, ..) => @debug.Repr::ctor("String", [(None, Repr(str))])
      Array(items~, ..) =>
        @debug.Repr::ctor("Array", [
          (None, @debug.Repr::array(items.map(expr_repr))),
        ])
      Map(fields~, ..) =>
        @debug.Repr::ctor("Map", [
          (
            None,
            @debug.Repr::array(
              fields.map(field => {
                @debug.Repr::tuple([Repr(field.key), expr_repr(field.value)])
              }),
            ),
          ),
        ])
      Apply(name~, args~, ..) =>
        @debug.Repr::ctor("Apply", [
          (None, Repr(name)),
          (
            None,
            @debug.Repr::array(
              args.map(field => {
                @debug.Repr::tuple([Repr(field.key), expr_repr(field.value)])
              }),
            ),
          ),
        ])
    }
  }
  fn field_repr(field : @moon_config.Field) -> @debug.Repr {
    @debug.Repr::tuple([Repr(field.key), expr_repr(field.value)])
  }
  @debug.Repr::array(
    self.0.0
    .to_array()
    .map(statement => {
      match statement {
        Assign(field) =>
          @debug.Repr::ctor("Assign", [(None, field_repr(field))])
        Expr(expr) => expr_repr(expr)
        Import(items~, kind~, ..) =>
          @debug.Repr::ctor("Import", [
            (None, Repr(kind)),
            (
              None,
              @debug.Repr::array(
                items.map(item => {
                  @debug.Repr::tuple([
                    Repr(item.path),
                    Repr(item.alias_.map(pair => pair.0)),
                  ])
                }),
              ),
            ),
          ])
      }
    }),
  )
}

///|
test "validate_moon_pkg accepts Ast returned by parse_moon_pkg" {
  let source =
    #|import {
    #|  "moonbitlang/core/json" @json,
    #|  "moonbitlang/core/test",
    #|}
    #|import {
    #|  "moonbitlang/core/debug",
    #|} for "wbtest"
    #|import {
    #|  "moonbitlang/core/test",
    #|} for "test"
    #|warnings = "+w1"
    #|supported_targets = "native"
    #|options(
    #|  "is-main": true,
    #|  targets: { "main.mbt": [ "native" ] },
    #|  link: { "native": { "cc-flags": [ "-O2" ] } },
    #|  "bin-deps": { "tool/cli": { "bin-pkg": [ "cli" ], "path": "../bin" } },
    #|)
    #|rule(name: "rule1", command: "moon run")
    #|dev_build(rule: "rule1", input: "a", output: "b")
    #|
  let { ast, diagnostics: { errors: parse_reports, .. }, .. } = @moon_config.parse_moon_pkg(
    source,
  )
  debug_inspect(
    parse_reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    @moon_config.validate_moon_pkg(ast).errors.map(report => report.msg),
    content=(
      #|[]
    ),
  )
}

///|
test "validate_moon_mod accepts Ast returned by parse_moon_mod" {
  let source =
    #|name = "user/mod"
    #|version = "0.1.0"
    #|import { "dep/a@1.2.3", "dep/b" }
    #|warnings = "+w1"
    #|supported_targets = "+js+wasm-gc"
    #|preferred_target = "js"
    #|options(
    #|  "bin-deps": { "tool/cli": { "bin-pkg": [ "cli" ], "path": "../bin" } },
    #|  "compile-flags": [ "-DDEBUG", "-Wall" ],
    #|  source: "src",
    #|)
    #|rule(name: "rule1", command: "moon run")
    #|
  let { ast, diagnostics: { errors: parse_reports, .. }, .. } = @moon_config.parse_moon_mod(
    source,
  )
  debug_inspect(
    parse_reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    @moon_config.validate_moon_mod(ast).errors.map(report => report.msg),
    content=(
      #|[]
    ),
  )
}

///|
test "validate_moon_work accepts Ast returned by parse_moon_work" {
  let source =
    #|members = ["./app", "./shared"]
    #|preferred_target = "native"
    #|
  let { ast, diagnostics: { errors: parse_reports, .. }, .. } = @moon_config.parse_moon_work(
    source,
  )
  debug_inspect(
    parse_reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    @moon_config.validate_moon_work(ast).errors.map(report => report.msg),
    content=(
      #|[]
    ),
  )
}

///|
test "validate_moon_pkg reports duplicate declarations" {
  let ast = @moon_config.parse_moon_pkg(
      validate=false,
      (
        #|warnings = "+w1"
        #|warnings = "+w2"
        #|rule(name: "r1")
        #|rule(name: "r2")
        #|dev_build(rule: "r1")
        #|dev_build(rule: "r2")
        #|options("is-main": true, "is-main": true)
      ),
    ).ast
  let reports : Array[@basic.Report] = @moon_config.validate_moon_pkg(ast).errors
  debug_inspect(
    reports.map(report => report.msg),
    content=(
      #|[
      #|  "Invalid configuration `warnings` in moon.pkg file.",
      #|  "Invalid configuration `pkgtype` in moon.pkg file.",
      #|]
    ),
  )
}

///|
test "validate_moon_mod reports duplicate declarations" {
  let ast = @moon_config.parse_moon_mod(
      validate=false,
      (
        #|rule(name: "r1")
        #|rule(name: "r2")
        #|import { "dep/a@1.0.0" }
        #|import { "dep/b@2.0.0" }
        #|preferred_target = "js"
        #|preferred_target = "native"
      ),
    ).ast
  let reports : Array[@basic.Report] = @moon_config.validate_moon_mod(ast).errors
  debug_inspect(
    reports.map(report => report.msg),
    content=(
      #|[
      #|  "Invalid configuration `import` in moon.mod file.",
      #|  "Invalid configuration `preferred_target` in moon.mod file.",
      #|]
    ),
  )
}

///|
test "validate_moon_pkg validates syntax without postprocessing" {
  let ast = @moon_config.parse_moon_pkg(
      validate=false,
      (
        #|warnings = "+w1"
        #|supported_targets = "native"
        #|options = { "is-main": true }
      ),
    ).ast
  let reports : Array[@basic.Report] = @moon_config.validate_moon_pkg(ast).errors
  debug_inspect(
    reports.map(report => report.msg),
    content=(
      #|["Invalid configuration `options` in moon.pkg file."]
    ),
  )
}

///|
test "validate_moon_mod validates syntax without postprocessing" {
  let ast = @moon_config.parse_moon_mod(
      validate=false,
      (
        #|warnings = "+w1"
        #|supported_targets = ["js"]
        #|preferred_target = "js"
        #|import { "dep/a@1.0.0" }
        #|options = { "source": "src" }
      ),
    ).ast
  let reports : Array[@basic.Report] = @moon_config.validate_moon_mod(ast).errors
  debug_inspect(
    reports.map(report => report.msg),
    content=(
      #|[
      #|  "Invalid configuration `supported_targets` in moon.mod file.",
      #|  "Invalid configuration `options` in moon.mod file.",
      #|]
    ),
  )
}

///|
test "validate_moon_work reports duplicate and unexpected fields" {
  let ast = @moon_config.parse_moon_work(
      validate=false,
      (
        #|members = ["./app"]
        #|members = ["./lib"]
        #|warnings = "+w1"
      ),
    ).ast
  let reports : Array[@basic.Report] = @moon_config.validate_moon_work(ast).errors
  debug_inspect(
    reports.map(report => report.msg),
    content=(
      #|[
      #|  "Invalid configuration `members` in moon.work file.",
      #|  "Invalid configuration `warnings` in moon.work file.",
      #|]
    ),
  )
}

///|
test "parse_moon_pkg reports raw unexpected top-level keys" {
  let source =
    #|unexpected = "value"
    #|
  let { ast: _, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_pkg(
    source,
  )
  debug_inspect(
    reports.map(report => report.msg),
    content=(
      #|["Invalid configuration `unexpected` in moon.pkg file."]
    ),
  )
}

///|
test "parse_moon_mod reports duplicate options blocks" {
  let source =
    #|options(
    #|  source: "src",
    #|)
    #|options(
    #|  repository: "https://example.com/repo",
    #|)
    #|
  let { ast: _, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_mod(
    source,
  )
  debug_inspect(
    reports.map(report => report.msg),
    content=(
      #|["Invalid configuration `options` in moon.mod file."]
    ),
  )
}

///|
test "parse_moon_pkg reports duplicate raw key names" {
  let source =
    #|warnings = "+w1"
    #|warnings = "+w2"
    #|
  let { ast: _, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_pkg(
    source,
  )
  debug_inspect(
    reports.map(report => report.msg),
    content=(
      #|["Invalid configuration `warnings` in moon.pkg file."]
    ),
  )
}

///|
test "parse_moon_pkg preserves options and repeatable fields" {
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
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_pkg(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Assign(("warnings", String("+w1"))),
      #|  Assign(("supported_targets", String("+wasm"))),
      #|  Apply("options", [("is-main", Bool(true))]),
      #|  Apply("rule", [("name", String("rule1")), ("command", String("exe"))]),
      #|  Apply("rule", [("name", String("rule2")), ("command", String("exe"))]),
      #|  Apply(
      #|    "dev_build",
      #|    [
      #|      ("rule", String("rule1")),
      #|      ("input", String("a")),
      #|      ("output", String("b")),
      #|    ],
      #|  ),
      #|  Apply(
      #|    "dev_build",
      #|    [
      #|      ("rule", String("rule2")),
      #|      ("input", String("c")),
      #|      ("output", String("d")),
      #|    ],
      #|  ),
      #|]
    ),
  )
}

///|
test "parse_moon_pkg keeps repeated top-level rule entries in Ast" {
  let source =
    #|rule(name:"rule1", command:"exe")
    #|rule(name:"rule2", command:"exe")
    #|
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_pkg(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Apply("rule", [("name", String("rule1")), ("command", String("exe"))]),
      #|  Apply("rule", [("name", String("rule2")), ("command", String("exe"))]),
      #|]
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
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_pkg(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Import(
      #|    Normal,
      #|    [
      #|      ("moonbit-community/fullstack-one-project-doc/shared", Some("shared")),
      #|      ("moonbitlang/core/json", Some("json")),
      #|      ("moonbitlang/async", None),
      #|    ],
      #|  ),
      #|  Import(Test, [("moonbitlang/core/test", None)]),
      #|  Assign(("warnings", String("-test_unqualified_package"))),
      #|  Assign(("supported_targets", String("native"))),
      #|  Apply(
      #|    "options",
      #|    [
      #|      ("is-main", Bool(true)),
      #|      (
      #|        "targets",
      #|        Map(
      #|          [
      #|            ("main.mbt", Array([String("native")])),
      #|            ("browser.mbt", Array([String("js")])),
      #|          ],
      #|        ),
      #|      ),
      #|    ],
      #|  ),
      #|]
    ),
  )
}

///|
test "parse_moon_pkg records import all aliases" {
  let source = "import { \"plain\" *, \"renamed\" @alias * }"
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_pkg(
    source,
  )
  assert_true(reports.is_empty())
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[Import(Normal, [("plain", Some("*")), ("renamed", Some("*alias"))])]
    ),
  )
}

///|
test "pkgtype kind is validated during parse and explicit validation" {
  for kind in ["library", "executable", "foreign_library"] {
    let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_pkg(
      "pkgtype(kind: \"\{kind}\")",
    )
    assert_true(reports.is_empty())
    assert_true(@moon_config.validate_moon_pkg(ast).errors.is_empty())
  }
  for source in ["pkgtype(kind: \"lib\")", "pkgtype(kind: true)", "pkgtype()"] {
    let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_pkg(
      source,
    )
    assert_eq(reports.length(), 1)
    assert_eq(
      reports[0].msg,
      "Invalid `pkgtype` in moon.pkg file. Expected `pkgtype(kind: \"library\" | \"executable\" | \"foreign_library\")`.",
    )
    let validated = @moon_config.validate_moon_pkg(ast).errors
    assert_eq(validated.length(), 1)
    assert_eq(validated[0].msg, reports[0].msg)
  }
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
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_pkg(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Apply(
      #|    "options",
      #|    [
      #|      (
      #|        "link",
      #|        Map(
      #|          [
      #|            (
      #|              "wasm-gc",
      #|              Map(
      #|                [
      #|                  ("use-js-builtin-string", Bool(true)),
      #|                  (
      #|                    "import-memory",
      #|                    Map([("module", String("env")), ("name", String("memory"))]),
      #|                  ),
      #|                  ("memory-limits", Map([("min", Int("1")), ("max", Int("4"))])),
      #|                  ("shared-memory", Bool(true)),
      #|                ],
      #|              ),
      #|            ),
      #|          ],
      #|        ),
      #|      ),
      #|    ],
      #|  ),
      #|]
    ),
  )
}

///|
test "parse_moon_mod preserves imports and options" {
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
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_mod(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Assign(("name", String("user/mod"))),
      #|  Assign(("version", String("0.1.0"))),
      #|  Import(Normal, [("dep/a@1.2.3", None), ("dep/b", None)]),
      #|  Assign(("warnings", String("+w1"))),
      #|  Assign(("supported_targets", String("+wasm"))),
      #|  Apply(
      #|    "options",
      #|    [
      #|      ("license", String("MIT")),
      #|      ("repository", String("https://example.com/repo")),
      #|    ],
      #|  ),
      #|  Apply("rule", [("name", String("rule1")), ("command", String("exe"))]),
      #|  Apply("rule", [("name", String("rule2")), ("command", String("exe"))]),
      #|]
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
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_mod(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Assign(("name", String("example/dsl_only"))),
      #|  Assign(("version", String("0.1.0"))),
      #|  Import(Normal, [("moonbitlang/x@0.4.6", None), ("../dep", None)]),
      #|  Apply(
      #|    "options",
      #|    [
      #|      (
      #|        "bin-deps",
      #|        Map(
      #|          [
      #|            (
      #|              "tool/cli",
      #|              Map([("bin-pkg", Array([String("cli")])), ("path", String("../bin"))]),
      #|            ),
      #|          ],
      #|        ),
      #|      ),
      #|      ("compile-flags", Array([String("-DDEBUG"), String("-Wall")])),
      #|      ("description", String("A DSL fixture module")),
      #|      ("exclude", Array([String("target/**"), String("**/*.tmp")])),
      #|      ("include", Array([String("src/**"), String("README.mbt.md")])),
      #|      ("keywords", Array([String("dsl"), String("fixture")])),
      #|      ("license", String("Apache-2.0")),
      #|      ("link-flags", Array([String("-lm")])),
      #|      ("preferred-target", String("wasm-gc")),
      #|      ("readme", String("README.mbt.md")),
      #|      ("repository", String("https://example.com/repo")),
      #|      (
      #|        "scripts",
      #|        Map(
      #|          [
      #|            ("prebuild", String("node ./prebuild.js")),
      #|            ("postbuild", String("echo done")),
      #|          ],
      #|        ),
      #|      ),
      #|      ("source", String("src")),
      #|    ],
      #|  ),
      #|]
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
    #|supported_targets = "+js+wasm-gc"
    #|
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_mod(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Assign(("name", String("example/top_level"))),
      #|  Assign(("version", String("0.1.0"))),
      #|  Assign(("readme", String("README.md"))),
      #|  Assign(("repository", String("https://example.com/repo"))),
      #|  Assign(("license", String("MIT"))),
      #|  Assign(("keywords", Array([String("moonbit"), String("parser")]))),
      #|  Assign(("description", String("A module config fixture"))),
      #|  Assign(("preferred_target", String("js"))),
      #|  Assign(("supported_targets", String("+js+wasm-gc"))),
      #|]
    ),
  )
}

///|
test "parse_moon_work parses workspace config" {
  let source =
    #|members = ["./app", "./shared"]
    #|preferred_target = "wasm-gc"
    #|
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_work(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Assign(("members", Array([String("./app"), String("./shared")]))),
      #|  Assign(("preferred_target", String("wasm-gc"))),
      #|]
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
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_work(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[Assign(("members", Array([String("./mod_a"), String("./mod_b")])))]
    ),
  )
}

///|
test "parse_moon_pkg reports config validation diagnostics" {
  let source =
    #|warnings = true
    #|unexpected = "value"
    #|
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_pkg(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 11 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 15 },
      #|    },
      #|    msg: "Invalid configuration `warnings` in moon.pkg file.",
      #|  },
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 2, bol: 16, cnum: 29 },
      #|      end: { fname: "moon.pkg", lnum: 2, bol: 16, cnum: 36 },
      #|    },
      #|    msg: "Invalid configuration `unexpected` in moon.pkg file.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Assign(("warnings", Bool(true))),
      #|  Assign(("unexpected", String("value"))),
      #|]
    ),
  )
}

///|
test "parse_moon_mod reports config validation diagnostics" {
  let source =
    #|name = "user/first"
    #|name = "user/second"
    #|
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_mod(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.mod", lnum: 2, bol: 20, cnum: 27 },
      #|      end: { fname: "moon.mod", lnum: 2, bol: 20, cnum: 40 },
      #|    },
      #|    msg: "Invalid configuration `name` in moon.mod file.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Assign(("name", String("user/first"))),
      #|  Assign(("name", String("user/second"))),
      #|]
    ),
  )
}

///|
test "parse_moon_work reports config validation diagnostics" {
  let source =
    #|members = ["./app"]
    #|members = ["./lib"]
    #|warnings = "+w1"
    #|
  let { ast, diagnostics: { errors: reports, .. }, .. } = @moon_config.parse_moon_work(
    source,
  )
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.work", lnum: 2, bol: 20, cnum: 30 },
      #|      end: { fname: "moon.work", lnum: 2, bol: 20, cnum: 39 },
      #|    },
      #|    msg: "Invalid configuration `members` in moon.work file.",
      #|  },
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.work", lnum: 3, bol: 40, cnum: 51 },
      #|      end: { fname: "moon.work", lnum: 3, bol: 40, cnum: 56 },
      #|    },
      #|    msg: "Invalid configuration `warnings` in moon.work file.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Assign(("members", Array([String("./app")]))),
      #|  Assign(("members", Array([String("./lib")]))),
      #|  Assign(("warnings", String("+w1"))),
      #|]
    ),
  )
}
