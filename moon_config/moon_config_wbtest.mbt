///|
fn parse_pkg(source : String) -> (Ast, Array[@basic.Report]) {
  let result = parse_string(source, name="moon.pkg", allow_reserved=true)
  (result.ast, result.diagnostics.errors)
}

///|
struct AstNoLoc(Ast)

///|
impl @debug.Debug for AstNoLoc with fn to_repr(self) {
  fn expr_repr(expr : Expr) -> @debug.Repr {
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
  fn field_repr(field : Field) -> @debug.Repr {
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
test "syntax preserves repeated top-level and nested entries" {
  let ast = parse_string(
      (
        #|rule(name: "rule1", command: "exe", env: { "k": "first", "k": "second" })
        #|rule(name: "rule2", command: "exe")
      ),
    ).ast
  debug_inspect(
    AstNoLoc(ast),
    content=(
      #|[
      #|  Apply(
      #|    "rule",
      #|    [
      #|      ("name", String("rule1")),
      #|      ("command", String("exe")),
      #|      ("env", Map([("k", String("first")), ("k", String("second"))])),
      #|    ],
      #|  ),
      #|  Apply("rule", [("name", String("rule2")), ("command", String("exe"))]),
      #|]
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
      #|[
      #|  Apply(
      #|    "options",
      #|    [
      #|      (
      #|        "nested",
      #|        Map(
      #|          [
      #|            ("a", Array([String("x"), String("y"), Int("123")])),
      #|            ("b", Map([("c", Bool(false))])),
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
      #|[Import(Wbtest, [("a/pkg", None), ("b/pkg", None)])]
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
      #|[Import(Normal, [("path/to/pkg", None), ("p", None)])]
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
      #|[Import(Normal, [("a", None), ("b", Some("ok"))])]
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
      #|[Import(Normal, [("a", None)])]
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
      #|[Apply("apply", [("", String("positional"))])]
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
      #|[
      #|  Assign(("warnings", String("+w1-w1"))),
      #|  Assign(("supported_targets", String("+wasm"))),
      #|]
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
      #|[Assign(("a", String("x")))]
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
      #|[Assign(("a", Hole)), Assign(("b", Int("1")))]
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
      #|[Assign(("a", Hole)), Assign(("b", Int("1")))]
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
      #|[Assign(("unsigned", Hole)), Assign(("floaty", Hole))]
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
      #|[Assign(("big", Int("1N")))]
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
      #|[
      #|  Apply("options", [("dup", Map([("a", Int("1")), ("a", Int("2"))]))]),
      #|  Apply("rule", [("name", String("rule1")), ("command", String("exe"))]),
      #|  Apply("rule", [("name", String("rule2")), ("command", String("exe"))]),
      #|]
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
      #|[Apply("options", [("nested", Map([("a", Int("1"))]))])]
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
      #|[Assign(("a", Array([Int("1")]))), Assign(("b", Int("2")))]
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
      #|[Import(Normal, [])]
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
      #|[Apply("options", [])]
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
      #|[Assign(("a", Array([])))]
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
      #|[Apply("options", []), Assign(("next", Bool(false)))]
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
      #|[Import(Normal, []), Assign(("next", Bool(false)))]
    ),
  )
}

///|
test "moon_config layout-only source has no statements" {
  let source =
    #|
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
      #|[]
    ),
  )
  assert_true(ast.0.is_empty())
}
