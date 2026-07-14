///|
test "series" {
  let integer = fn(x : Int) { text(x.to_string()) }
  let r1 = series(List([1, 2, 3, 4]), comma, integer)
  inspect(
    r1,
    content=(
      #|1,
      #|2,
      #|3,
      #|4,
    ),
  )
  let r2 = series(List([1, 2, 3, 4]), comma, integer, trailing=AlwaysHidden)
  inspect(
    r2,
    content=(
      #|1,
      #|2,
      #|3,
      #|4
    ),
  )
  let r3 = group(series(List([1, 2, 3, 4]), comma, integer))
  inspect(r3, content="1, 2, 3, 4")
}

///|
test "visibility token table records modifier and parentheses" {
  let source = "pub(all) struct T {}\n"
  let (impls, reports, tokens) = parse(source)
  if reports.length() != 0 {
    fail("expected pub(all) fixture to parse without diagnostics")
  }
  let vis_loc = match impls {
    More(TopTypeDef({ type_vis: Pub(attr=Some(_), loc~), .. }), ..) => loc
    _ => fail("expected a public type declaration")
  }
  let table = @comment.Mapper::new(tokens, impls).get_table(vis_loc)
  match
    (
      table.get_token("lparen"),
      table.get_token("lident"),
      table.get_token("rparen"),
    ) {
    (Some(lparen), Some(lident), Some(rparen)) => {
      if lparen.loc() == lident.loc() {
        fail("visibility lparen token should not be reused as modifier token")
      }
      if lident.loc() == rparen.loc() {
        fail("visibility modifier token should not be reused as rparen token")
      }
    }
    _ =>
      fail("expected visibility token table to include lparen, lident, rparen")
  }
}
