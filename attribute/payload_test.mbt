///|
test "package names uppercase calls and nested attribute arrays are structured" {
  let attr = @attribute.Attribute::new(
    loc=dummy_loc,
    (
      "derive",
      None,
      "(@debug.Debug(ignore=[T, @pkg.U]), Upper([]), @pkg.name, [[], [true, false]])",
    ),
  )
  guard attr.parsed is Some(Apply({ qual: None, name: "derive", }, props)) else {
    fail("derive payload was not parsed")
  }
  guard props.to_array()
    is [
      Expr(Apply({ qual: Some("debug"), name: "Debug", }, debug_props)),
      Expr(Apply({ qual: None, name: "Upper", }, upper_props)),
      Expr(Ident({ qual: Some("pkg"), name: "name", })),
      Expr(Array(nested)),
    ] else {
    fail("qualified identifiers or uppercase calls were lost")
  }
  guard debug_props.to_array() is [Labeled("ignore", Array(items))] else {
    fail("ignore array missing")
  }
  guard items.to_array()
    is [
      Ident({ qual: None, name: "T", }),
      Ident({ qual: Some("pkg"), name: "U", }),
    ] else {
    fail("array identifiers missing")
  }
  guard upper_props.to_array() is [Expr(Array(empty))] else {
    fail("empty array missing")
  }
  assert_true(empty.is_empty())
  guard nested.to_array() is [Array(empty), Array(bools)] else {
    fail("nested arrays missing")
  }
  assert_true(empty.is_empty())
  guard bools.to_array() is [Bool(true), Bool(false)] else {
    fail("nested values missing")
  }
}
