///|
fn parse_pkg(source : String) -> (Ast, Array[@basic.Report]) {
  parse_string(source, name="moon.pkg")
}

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
test "Ast::to_json folds repeated top-level rule entries into arrays" {
  fn loc() -> Location {
    { start: dummy_pos, end: dummy_pos }
  }

  let loc = loc()
  let ast = Obj(
    Vector([
      (
        "rule",
        Obj(
          Vector([
            ("name", Str("rule1", loc~)),
            ("command", Str("exe", loc~)),
            (
              "env",
              Obj(
                Vector([("k", Str("first", loc~)), ("k", Str("second", loc~))]),
                loc~,
              ),
            ),
          ]),
          loc~,
        ),
      ),
      (
        "rule",
        Obj(
          Vector([("name", Str("rule2", loc~)), ("command", Str("exe", loc~))]),
          loc~,
        ),
      ),
    ]),
    loc~,
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj(
      #|  [
      #|    (
      #|      "rule",
      #|      Obj(
      #|        [
      #|          ("name", Str("rule1")),
      #|          ("command", Str("exe")),
      #|          ("env", Obj([("k", Str("first")), ("k", Str("second"))])),
      #|        ],
      #|      ),
      #|    ),
      #|    ("rule", Obj([("name", Str("rule2")), ("command", Str("exe"))])),
      #|  ],
      #|)
    ),
  )
}

///|
test "moon_config parse nested options" {
  let source =
    #|options(
    #|  nested: { "a": ["x", "y", 123], "b": { "c": false } }
    #|)
    #|
  let (ast, reports) = parse_pkg(source)
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
      #|      "options",
      #|      Obj(
      #|        [
      #|          (
      #|            "nested",
      #|            Obj(
      #|              [
      #|                ("a", Arr([Str("x"), Str("y"), Float("123")])),
      #|                ("b", Obj([("c", Bool(false))])),
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
test "moon_config parse import for wbtest" {
  let source =
    #|import { "a/pkg", "b/pkg" } for "wbtest"
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("wbtest-import", Arr([Str("a/pkg"), Str("b/pkg")]))])
    ),
  )
}

///|
test "moon_config rejects legacy import kind syntax" {
  let source =
    #|import "test" { "path/to/pkg", "p" }
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 7 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 13 },
      #|    },
      #|    msg: "Old import syntax is no longer supported; use `import { ... }`, `import { ... } for \"test\"`, or `import { ... } for \"wbtest\"`.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("import", Arr([Str("path/to/pkg"), Str("p")]))])
    ),
  )
}

///|
test "moon_config rejects legacy import alias syntax" {
  let source =
    #|import { "a" as @alias, "b" @ok }
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 13 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 15 },
      #|    },
      #|    msg: "Old import alias syntax is no longer supported; use `\"...\" @alias` instead of `\"...\" as @alias`.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj(
      #|  [
      #|    (
      #|      "import",
      #|      Arr([Str("a"), Obj([("path", Str("b")), ("alias", Str("ok"))])]),
      #|    ),
      #|  ],
      #|)
    ),
  )
}

///|
test "moon_config avoids cascading after invalid import kind" {
  let source =
    #|import { "a" } for next = true
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 19 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 23 },
      #|    },
      #|    msg: "Unexpected token id (lowercase start), you may expect string literal \"test\" or \"wbtest\".",
      #|  },
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 19 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 23 },
      #|    },
      #|    msg: "Unexpected token id (lowercase start), you may expect `;` or EOF.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("import", Arr([Str("a")]))])
    ),
  )
}

///|
test "moon_config reports positional argument" {
  let source =
    #|apply("positional")
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 6 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 18 },
      #|    },
      #|    msg: "Unexpected token string, you may expect labeled argument.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("apply", Obj([("", Str("positional"))]))])
    ),
  )
}

///|
test "moon_config parse assignments" {
  let source =
    #|warnings = "+w1-w1"
    #|supported_targets = "+wasm"
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("warnings", Str("+w1-w1")), ("supported_targets", Str("+wasm"))])
    ),
  )
}

///|
test "moon_config requires statement separators" {
  let source =
    #|a = "x" b = "y"
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 8 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 9 },
      #|    },
      #|    msg: "Unexpected token id (lowercase start), you may expect `;` or EOF.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("a", Str("x"))])
    ),
  )
}

///|
test "moon_config keeps next statement after missing assignment value before semicolon" {
  let source =
    #|a = ;
    #|b = 1
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 4 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 5 },
      #|    },
      #|    msg: "Unexpected token `;`, you may expect expression.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("a", Null), ("b", Float("1"))])
    ),
  )
}

///|
test "moon_config keeps next statement after missing assignment value on next line" {
  let source =
    #|a =
    #|b = 1
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 2, bol: 4, cnum: 4 },
      #|      end: { fname: "moon.pkg", lnum: 2, bol: 4, cnum: 5 },
      #|    },
      #|    msg: "Unexpected token id (lowercase start), you may expect expression.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("a", Null), ("b", Float("1"))])
    ),
  )
}

///|
test "moon_config rejects unsupported numeric literals" {
  let source =
    #|unsigned = 1U
    #|floaty = 1.5
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 11 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 13 },
      #|    },
      #|    msg: "Unexpected token int, you may expect integer literals without U or L suffixes.",
      #|  },
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 2, bol: 14, cnum: 23 },
      #|      end: { fname: "moon.pkg", lnum: 2, bol: 14, cnum: 26 },
      #|    },
      #|    msg: "Unexpected token double, you may expect expression.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("unsigned", Null), ("floaty", Null)])
    ),
  )
}

///|
test "moon_config accepts bigint literal in pkg values" {
  let source =
    #|big = 1N
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("big", Float("1N"))])
    ),
  )
}

///|
test "moon_config preserves duplicate object and statement entries" {
  let source =
    #|options(dup: { "a": 1, "a": 2 })
    #|rule(name:"rule1", command:"exe")
    #|rule(name:"rule2", command:"exe")
  let (ast, reports) = parse_pkg(source)
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
      #|      "options",
      #|      Obj([("dup", Obj([("a", Float("1")), ("a", Float("2"))]))]),
      #|    ),
      #|    ("rule", Obj([("name", Str("rule1")), ("command", Str("exe"))])),
      #|    ("rule", Obj([("name", Str("rule2")), ("command", Str("exe"))])),
      #|  ],
      #|)
    ),
  )
}

///|
test "moon_config recovers map value after missing colon" {
  let source =
    #|options(nested: { "a" 1 })
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 22 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 23 },
      #|    },
      #|    msg: "Unexpected token int, you may expect \":\".",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("options", Obj([("nested", Obj([("a", Float("1"))]))]))])
    ),
  )
}

///|
test "moon_config keeps next statement after missing array right bracket" {
  let source =
    #|a = [1
    #|b = 2
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 6 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 6 },
      #|    },
      #|    msg: "Unexpected token `;`, you may expect `]`.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("a", Arr([Float("1")])), ("b", Float("2"))])
    ),
  )
}

///|
test "moon_config reports missing import right brace at eof" {
  let source =
    #|import {
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 2, bol: 9, cnum: 9 },
      #|      end: { fname: "moon.pkg", lnum: 2, bol: 9, cnum: 9 },
      #|    },
      #|    msg: "Unexpected end of file, missing `}` here.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("import", Arr([]))])
    ),
  )
}

///|
test "moon_config reports missing apply right paren at eof" {
  let source =
    #|options(
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 2, bol: 9, cnum: 9 },
      #|      end: { fname: "moon.pkg", lnum: 2, bol: 9, cnum: 9 },
      #|    },
      #|    msg: "Unexpected end of file, missing `)` here.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("options", Obj([]))])
    ),
  )
}

///|
test "moon_config reports missing array right bracket at eof" {
  let source =
    #|a = [
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 2, bol: 6, cnum: 6 },
      #|      end: { fname: "moon.pkg", lnum: 2, bol: 6, cnum: 6 },
      #|    },
      #|    msg: "Unexpected end of file, missing `]` here.",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("a", Arr([]))])
    ),
  )
}

///|
test "moon_config keeps next statement after missing apply left paren" {
  let source =
    #|options nested: true
    #|next = false
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 8 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 14 },
      #|    },
      #|    msg: "Unexpected token id (lowercase start), you may expect \"(\".",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("options", Obj([])), ("next", Bool(false))])
    ),
  )
}

///|
test "moon_config keeps next statement after missing import left brace" {
  let source =
    #|import [ "a/pkg" ]
    #|next = false
    #|
  let (ast, reports) = parse_pkg(source)
  debug_inspect(
    reports,
    content=(
      #|[
      #|  {
      #|    loc: {
      #|      start: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 7 },
      #|      end: { fname: "moon.pkg", lnum: 1, bol: 0, cnum: 8 },
      #|    },
      #|    msg: "Unexpected token `[`, you may expect \"{\".",
      #|  },
      #|]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([("import", Arr([])), ("next", Bool(false))])
    ),
  )
}

///|
test "moon_config root loc stays ordered for layout-only source" {
  let source =
    #|
    #|
  let (ast, reports) = parse_pkg(source)
  let loc = ast.loc()
  debug_inspect(
    reports,
    content=(
      #|[]
    ),
  )
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|Obj([])
    ),
  )
  debug_inspect(loc.start.compare(loc.end) <= 0, content="true")
}
