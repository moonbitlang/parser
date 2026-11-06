///|
test "conditional and repeated imports retain structure and locations" {
  let source =
    #|import { "unconditional" }
    #|#cfg(target = "js")
    #|#cfg(not(false))
    #|import { "example/platform" @implementation * } for "test"
    #|#cfg(any(target = "wasm", target = "wasm-gc", target = "js", target = "llvm"))
    #|#cfg(all(true, not(false)))
    #|import { "test" * } for "test"
    #|#cfg(all(true, any(), all()))
    #|import { "whitebox" } for "wbtest"
    #|#cfg(false)
    #|import { "inactive" }
    #|import { "another" }
  let parsed = @moon_config.parse_moon_pkg(source)
  assert_eq(parsed.diagnostics.errors.length(), 0)
  assert_eq(parsed.diagnostics.warnings.length(), 0)
  assert_eq(parsed.ast.0.length(), 6)
  guard parsed.ast.0[1]
    is Import(conditions~, kind=Test, keyword_loc~, list_loc~, loc~, ..) else {
    fail("conditional test import missing")
  }
  assert_eq(conditions.length(), 2)
  assert_eq(conditions[0].parsed, Some(@moon_config.Condition::Target("js")))
  assert_eq(
    conditions[1].parsed,
    Some(@moon_config.Condition::Not(Bool(false))),
  )
  assert_eq(conditions[0].raw, "#cfg(target = \"js\")")
  assert_eq(conditions[0].loc.start.lnum, 2)
  assert_eq(conditions[0].loc.end.column(), 20)
  assert_eq(loc.start.lnum, 2)
  assert_eq(loc.end.lnum, 4)
  assert_eq(keyword_loc.start.lnum, 4)
  assert_eq(list_loc.start.column(), 8)
}

///|
test "conditional imports reject unsupported conditions and placements" {
  for
    payload in [
      "target = \"unknown\"", "target = native", "target: \"native\"", "os = \"linux\"",
      "not()", "not(true, false)", "", "true, false", "@pkg.all(true)", "[true]",
      "all(true, os = \"linux\")", "all(true,)",
    ] {
    let result = @moon_config.parse_moon_pkg(
      "#cfg(\{payload})\nimport { \"a/b\" }",
    )
    assert_true(!result.diagnostics.errors.is_empty(), msg=payload)
  }
  for
    source in [
      "#cfg(true)\noptions(\"is-main\": true)", "#cfg(true)\nwarnings = \"+unused_value\"",
      "#cfg(true)\npkgtype(kind: \"executable\")", "#cfg(true)", "import { #cfg(true) \"a/b\" }",
      "#other(true)\nimport { \"a/b\" }", "#cfg(true) import { \"a/b\" }", "#cfg(\n true\n)\nimport { \"a/b\" }",
    ] {
    assert_true(
      !@moon_config.parse_moon_pkg(source).diagnostics.errors.is_empty(),
      msg=source,
    )
  }
  let module_result = @moon_config.parse_moon_mod(
    "#cfg(false)\nimport { \"a/b@1.0.0\" }",
  )
  assert_eq(module_result.diagnostics.errors.length(), 1)
  assert_eq(module_result.diagnostics.errors[0].loc.start.lnum, 1)
  assert_true(module_result.diagnostics.errors[0].msg.contains("#cfg"))
}

///|
test "experimental accepts only boolean configuration values" {
  for value in ["true", "false"] {
    assert_true(
      @moon_config.parse_moon_pkg("experimental = \{value}").diagnostics.errors.is_empty(),
    )
  }
  for value in ["\"true\"", "1", "[]", "{}"] {
    assert_eq(
      @moon_config.parse_moon_pkg("experimental = \{value}").diagnostics.errors.length(),
      1,
    )
  }
  assert_eq(
    @moon_config.parse_moon_pkg("experimental(enabled: true)").diagnostics.errors.length(),
    1,
  )
}
