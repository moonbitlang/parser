///|
fn parse_pkg(source : String) -> (Ast, Array[@basic.Report]) {
  parse_string(source, name="moon.pkg")
}

///|
fn ast_summary(ast : Ast) -> Json {
  match ast {
    Null(_) => Json::null()
    False(_) => Json::boolean(false)
    True(_) => Json::boolean(true)
    Str(str~, ..) => Json::string(str)
    Float(float~, ..) =>
      Json::object(Map::from_array([("$number", Json::string(float))]))
    Arr(content~, ..) => Json::array(content.map(ast_summary))
    Obj(map~, ..) => {
      let object : Map[String, Json] = Map::new()
      for key, value in map {
        object[key] = ast_summary(value)
      }
      Json::object(object)
    }
  }
}

///|
fn report_contains(reports : Array[@basic.Report], needle : String) -> Bool {
  for report in reports {
    if report.msg.contains(needle) {
      return true
    }
  }
  false
}

///|
fn report_checks(
  reports : Array[@basic.Report],
  needles : Array[String],
) -> Json {
  Json::object(
    Map::from_array([
      ("count", Json::number(reports.length().to_double())),
      (
        "contains",
        Json::array(
          needles.map(needle => Json::boolean(report_contains(reports, needle))),
        ),
      ),
    ]),
  )
}

///|
test "pkg_parser parse nested options" {
  let source =
    #|options(
    #|  nested: { "a": ["x", "y", 123], "b": { "c": false } }
    #|)
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, []), content={
    "count": 0,
    "contains": [],
  })
  json_inspect(ast_summary(ast), content={
    "options": {
      "nested": { "a": ["x", "y", { "$number": "123" }], "b": { "c": false } },
    },
  })
}

///|
test "pkg_parser parse import for wbtest" {
  let source =
    #|import { "a/pkg", "b/pkg" } for "wbtest"
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, []), content={
    "count": 0,
    "contains": [],
  })
  json_inspect(ast_summary(ast), content={ "wbtest-import": ["a/pkg", "b/pkg"] })
}

///|
test "pkg_parser rejects legacy import kind syntax" {
  let source =
    #|import "test" { "path/to/pkg", "p" }
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(
    report_checks(reports, ["Old import syntax is no longer supported"]),
    content={ "count": 1, "contains": [true] },
  )
  json_inspect(ast_summary(ast), content={ "import": ["path/to/pkg", "p"] })
}

///|
test "pkg_parser rejects legacy import alias syntax" {
  let source =
    #|import { "a" as @alias, "b" @ok }
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(
    report_checks(reports, ["Old import alias syntax is no longer supported"]),
    content={ "count": 1, "contains": [true] },
  )
  json_inspect(ast_summary(ast), content={
    "import": ["a", { "path": "b", "alias": "ok" }],
  })
}

///|
test "pkg_parser avoids cascading after invalid import kind" {
  let source =
    #|import { "a" } for next = true
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(
    report_checks(reports, [
      "string literal \"test\" or \"wbtest\"", "`;` or EOF",
    ]),
    content={ "count": 2, "contains": [true, true] },
  )
  json_inspect(ast_summary(ast), content={ "import": ["a"] })
}

///|
test "pkg_parser reports positional argument" {
  let source =
    #|apply("positional")
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["labeled argument"]), content={
    "count": 1,
    "contains": [true],
  })
  json_inspect(ast_summary(ast), content={ "apply": { "": "positional" } })
}

///|
test "pkg_parser parse assignments" {
  let source =
    #|warnings = "+w1-w1"
    #|supported_targets = "+wasm"
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, []), content={
    "count": 0,
    "contains": [],
  })
  json_inspect(ast_summary(ast), content={
    "warnings": "+w1-w1",
    "supported_targets": "+wasm",
  })
}

///|
test "pkg_parser requires statement separators" {
  let source =
    #|a = "x" b = "y"
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["`;` or EOF"]), content={
    "count": 1,
    "contains": [true],
  })
  json_inspect(ast_summary(ast), content={ "a": "x" })
}

///|
test "pkg_parser keeps next statement after missing assignment value before semicolon" {
  let source =
    #|a = ;
    #|b = 1
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["expression", "`;` or EOF"]), content={
    "count": 1,
    "contains": [true, false],
  })
  json_inspect(ast_summary(ast), content={ "a": null, "b": { "$number": "1" } })
}

///|
test "pkg_parser keeps next statement after missing assignment value on next line" {
  let source =
    #|a =
    #|b = 1
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["expression", "`;` or EOF"]), content={
    "count": 1,
    "contains": [true, false],
  })
  json_inspect(ast_summary(ast), content={ "a": null, "b": { "$number": "1" } })
}

///|
test "pkg_parser rejects unsupported numeric literals" {
  let source =
    #|unsigned = 1U
    #|floaty = 1.5
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["suffixes", "expression"]), content={
    "count": 2,
    "contains": [true, true],
  })
  json_inspect(ast_summary(ast), content={ "unsigned": null, "floaty": null })
}

///|
test "pkg_parser accepts bigint literal in pkg values" {
  let source =
    #|big = 1N
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, []), content={
    "count": 0,
    "contains": [],
  })
  json_inspect(ast_summary(ast), content={ "big": { "$number": "1N" } })
}

///|
test "pkg_parser recovers map value after missing colon" {
  let source =
    #|options(nested: { "a" 1 })
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["\":\""]), content={
    "count": 1,
    "contains": [true],
  })
  json_inspect(ast_summary(ast), content={
    "options": { "nested": { "a": { "$number": "1" } } },
  })
}

///|
test "pkg_parser keeps next statement after missing array right bracket" {
  let source =
    #|a = [1
    #|b = 2
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["`]`"]), content={
    "count": 1,
    "contains": [true],
  })
  json_inspect(ast_summary(ast), content={
    "a": [{ "$number": "1" }],
    "b": { "$number": "2" },
  })
}

///|
test "pkg_parser reports missing import right brace at eof" {
  let source =
    #|import {
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["`}`"]), content={
    "count": 1,
    "contains": [true],
  })
  json_inspect(ast_summary(ast), content={ "import": [] })
}

///|
test "pkg_parser reports missing apply right paren at eof" {
  let source =
    #|options(
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["`)`"]), content={
    "count": 1,
    "contains": [true],
  })
  json_inspect(ast_summary(ast), content={ "options": {} })
}

///|
test "pkg_parser reports missing array right bracket at eof" {
  let source =
    #|a = [
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["`]`"]), content={
    "count": 1,
    "contains": [true],
  })
  json_inspect(ast_summary(ast), content={ "a": [] })
}

///|
test "pkg_parser keeps next statement after missing apply left paren" {
  let source =
    #|options nested: true
    #|next = false
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["\"(\""]), content={
    "count": 1,
    "contains": [true],
  })
  json_inspect(ast_summary(ast), content={ "options": {}, "next": false })
}

///|
test "pkg_parser keeps next statement after missing import left brace" {
  let source =
    #|import [ "a/pkg" ]
    #|next = false
    #|
  let (ast, reports) = parse_pkg(source)
  json_inspect(report_checks(reports, ["\"{\""]), content={
    "count": 1,
    "contains": [true],
  })
  json_inspect(ast_summary(ast), content={ "import": [], "next": false })
}

///|
test "pkg_parser root loc stays ordered for layout-only source" {
  let source =
    #|
    #|
  let (ast, reports) = parse_pkg(source)
  let loc = ast.loc()
  json_inspect(
    Json::object(
      Map::from_array([
        ("ast", ast_summary(ast)),
        ("reports", report_checks(reports, [])),
        ("ordered", Json::boolean(loc.start.compare(loc.end) <= 0)),
      ]),
    ),
    content={
      "ast": {},
      "reports": { "count": 0, "contains": [] },
      "ordered": true,
    },
  )
}
