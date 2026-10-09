///|
test "moon.mod private is optional and preserves boolean values" {
  let (absent, absent_reports) = parse_moon_mod("name = \"user/mod\"")
  assert_true(absent_reports.is_empty())
  json_inspect(ToJson::to_json(absent), content={ "name": "user/mod" })
  let (public_mod, public_reports) = parse_moon_mod("private = false")
  assert_true(public_reports.is_empty())
  json_inspect(ToJson::to_json(public_mod), content={ "private": false })
  let (private_mod, private_reports) = parse_moon_mod("private = true")
  assert_true(private_reports.is_empty())
  assert_true(validate_moon_mod(private_mod).is_empty())
  json_inspect(ToJson::to_json(private_mod), content={ "private": true })
}

///|
test "moon.mod options preserves private" {
  let (ast, reports) = parse_moon_mod("options(private: true)")
  assert_true(reports.is_empty())
  json_inspect(ToJson::to_json(ast), content={ "private": true })
}

///|
test "moon.mod rejects non-boolean private" {
  let (_, reports) = parse_moon_mod("private = \"true\"")
  assert_eq(reports.map(report => report.msg), [
    "Invalid moon.mod config: `private` must be a boolean.",
  ])
  let (_, reports) = parse_moon_mod("options(private: 1)")
  assert_eq(reports.map(report => report.msg), [
    "Invalid moon.mod config: `private` must be a boolean.",
  ])
  let (ast, _) = parse_moon_mod("private = []")
  assert_eq(validate_moon_mod(ast).map(report => report.msg), [
    "Invalid moon.mod config: `private` must be a boolean.",
  ])
}

///|
test "moon.mod rejects duplicate private across options" {
  let (_, reports) = parse_moon_mod("private = true\noptions(private: false)")
  assert_eq(reports.map(report => report.msg), [
    "Duplicate key `private` found in moon.mod.",
  ])
}
