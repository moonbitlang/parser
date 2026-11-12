///|
test "moon.mod private is optional and preserves boolean values" {
  let { ast: absent, diagnostics: { errors: absent_reports, .. }, .. } = parse_moon_mod(
    "name = \"user/mod\"",
  )
  assert_true(absent_reports.is_empty())
  assert_true(
    absent.0[0]
    is Assign({ key: "name", value: String(str="user/mod", ..), .. }),
  )
  let { ast: public_mod, diagnostics: { errors: public_reports, .. }, .. } = parse_moon_mod(
    "private = false",
  )
  assert_true(public_reports.is_empty())
  assert_true(
    public_mod.0[0]
    is Assign({ key: "private", value: Bool(value=false, ..), .. }),
  )
  let { ast: private_mod, diagnostics: { errors: private_reports, .. }, .. } = parse_moon_mod(
    "private = true",
  )
  assert_true(private_reports.is_empty())
  assert_true(validate_moon_mod(private_mod).errors.is_empty())
  assert_true(
    private_mod.0[0]
    is Assign({ key: "private", value: Bool(value=true, ..), .. }),
  )
}

///|
test "moon.mod options preserves private" {
  let { ast, diagnostics: { errors: reports, .. }, .. } = parse_moon_mod(
    "options(private: true)",
  )
  assert_true(reports.is_empty())
  assert_true(
    ast.0[0]
    is Expr(
      Apply(
        name="options",
        args=[{ key: "private", value: Bool(value=true, ..), .. }],
        ..
      ),
    ),
  )
}

///|
test "moon.mod rejects non-boolean private" {
  let { ast: _, diagnostics: { errors: reports, .. }, .. } = parse_moon_mod(
    "private = \"true\"",
  )
  assert_eq(reports.map(report => report.msg), [
    "Invalid configuration `private` in moon.mod file.",
  ])
  let { ast: _, diagnostics: { errors: reports, .. }, .. } = parse_moon_mod(
    "options(private: 1)",
  )
  assert_eq(reports.map(report => report.msg), [
    "Invalid configuration `private` in moon.mod file.",
  ])
  let { ast, diagnostics: { errors: _, .. }, .. } = parse_moon_mod(
    "private = []",
  )
  assert_eq(validate_moon_mod(ast).errors.map(report => report.msg), [
    "Invalid configuration `private` in moon.mod file.",
  ])
}

///|
test "moon.mod rejects duplicate private across options" {
  let { ast: _, diagnostics: { errors: reports, .. }, .. } = parse_moon_mod(
    "private = true\noptions(private: false)",
  )
  assert_eq(reports.map(report => report.msg), [
    "Invalid configuration `private` in moon.mod file.",
  ])
}
