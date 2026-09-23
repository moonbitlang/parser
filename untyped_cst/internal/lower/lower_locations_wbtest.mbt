///|
fn detached_loc(start : Int, end : Int) -> Location {
  mk_loc(Position::{ fname: "detached", lnum: 1, bol: 0, cnum: start, }, Position::{
    fname: "detached",
    lnum: 1,
    bol: 0,
    cnum: end,
  })
}

///|
fn detached_token(kind : TokenKind, start : Int, end : Int) -> CstNode {
  // Model a preceding non-BMP character: UTF-16 offsets are not character offsets.
  CstNode::syntax_token(
    token_kind=kind,
    loc=detached_loc(start, end),
    source_span=(start + 1, end + 1),
    entries=[],
  )
}

///|
test "detached CST derives locations only from the owning syntax fields" {
  let nested = CstNode::expr_try_operator(
    loc=detached_loc(30, 45),
    source_span=(31, 46),
    kw=detached_token(TK_TRY_EXCLAMATION, 30, 34),
    body=CstNode::expr_function(
      loc=detached_loc(35, 45),
      source_span=(36, 46),
      kw=[detached_token(TK_ASYNC, 35, 40)],
      error=[],
    ),
  )
  let params = CstNode::list_parameter_list(
    loc=detached_loc(20, 22),
    source_span=(21, 23),
    entries=[],
  )
  for enabled in [false, true] {
    let node = CstNode::expr_function(
      loc=detached_loc(2, 45),
      source_span=(3, 46),
      kw=[detached_token(TK_ASYNC, 2, 7), detached_token(TK_FN, 8, 10)],
      error=[],
      params~,
      body=nested,
      async_=CstNode::flag_async(
        value=enabled,
        loc=detached_loc(45, 45),
        source_span=(46, 46),
      ),
    )
    guard lower_expr(node) is Function(func~, ..) else {
      fail("expected function")
    }
    @test.assert_eq(
      func.is_async,
      if enabled {
        Some(detached_loc(2, 7))
      } else {
        None
      },
    )
    @test.assert_eq(func.params_loc, params.loc)
  }
  let outer = CstNode::expr_function(
    loc=detached_loc(10, 45),
    source_span=(11, 46),
    kw=[detached_token(TK_FN, 10, 12)],
    error=[],
    body=nested,
  )
  guard lower_expr(outer) is Function(func~, ..) else {
    fail("expected function")
  }
  @test.assert_eq(func.is_async, None)
  let arg = CstNode::argument_labelled_option(
    loc=detached_loc(1, 45),
    source_span=(2, 46),
    name=CstNode::name_identifier(
      value="x",
      loc=detached_loc(1, 2),
      source_span=(2, 3),
    ),
    op=[detached_token(TK_EQUAL, 3, 4), detached_token(TK_QUESTION, 2, 3)],
    expr=nested,
  )
  guard lower_argument_kind(arg) is LabelledOption(question_loc~, ..) else {
    fail("expected optional argument")
  }
  @test.assert_eq(question_loc, detached_loc(2, 3))
}

///|
test "pipe discard keeps the parameter list span and the historical arrow override" {
  // The existing underscore-pipe body grouping differs from the handrolled
  // parser. This test isolates the location contract, without changing that AST.
  for
    (source, expected_param, expected_params) in [
      ("v |> _ => { 1 }", (7, 9), (5, 6)),
      ("v |> (_) => { 1 }", (6, 7), (5, 8)),
      ("v |> (_, _) => { 1 }", (6, 7), (5, 11)),
      ("_ => 1", (0, 1), (0, 1)),
    ] {
    let parsed = parse_expression(source)
    @test.assert_eq(parsed.diagnostics.length(), 0)
    let expr = lower_expr(parsed.root)
    let function = match expr {
      Pipe(rhs~, ..) => rhs
      other => other
    }
    guard function is Function(func~, ..) else { fail("expected function") }
    guard func.parameters is More(DiscardPositional(loc~, ..), tail=rest) else {
      fail("expected discard parameter")
    }
    @test.assert_eq((loc.start.cnum, loc.end.cnum), expected_param)
    @test.assert_eq(
      (func.params_loc.start.cnum, func.params_loc.end.cnum),
      expected_params,
    )
    if rest is More(DiscardPositional(loc=second, ..), ..) {
      @test.assert_eq((second.start.cnum, second.end.cnum), (9, 10))
    }
  }
}

///|
test "detached slice includes a missing close and multiline uses raw token bounds" {
  let missing = CstNode::missing(loc=detached_loc(12, 12), source_span=(13, 13))
  let slice = CstNode::expr_array_get_slice(
    loc=detached_loc(1, 12),
    source_span=(2, 13),
    self_=missing,
    open=detached_token(TK_LBRACKET, 8, 9),
    op=detached_token(TK_COLON, 10, 11),
    close=missing,
  )
  guard lower_expr(slice) is ArrayGetSlice(index_loc~, ..) else {
    fail("expected slice")
  }
  @test.assert_eq(index_loc, detached_loc(8, 12))
  let multiline = CstNode::expr_multiline_string(
    loc=detached_loc(0, 40),
    source_span=(0, 41),
    entries=[
      Token(detached_token(TK_MULTILINE_STRING, 2, 8)),
      Trivia(detached_token(TK_COMMENT, 10, 15)),
      Token(detached_token(TK_MULTILINE_INTERP, 18, 28)),
    ],
  )
  guard lower_expr(multiline) is MultilineString(loc~, ..) else {
    fail("expected multiline string")
  }
  @test.assert_eq(loc, detached_loc(2, 28))
}

///|
test "detached catch ranges retain prefix suffix and missing branch fallbacks" {
  let body = CstNode::expr_missing_expr(
    loc=detached_loc(5, 10),
    source_span=(6, 11),
    error=[],
  )
  let catch_kw = detached_token(TK_CATCH, 12, 17)
  let open = detached_token(TK_LBRACE, 20, 21)
  let cases = CstNode::list_match_case_list(
    loc=detached_loc(20, 30),
    source_span=(21, 31),
    entries=[Open(open)],
  )
  for prefix in [false, true] {
    let entries : Array[@node.CstRecoveryChild] = []
    if prefix {
      entries.push(Kw(detached_token(TK_TRY, 1, 4)))
    }
    entries.append([
      Body(body),
      Kw(catch_kw),
      Case(cases),
      HasTry(
        CstNode::flag_has_try(
          value=prefix,
          loc=detached_loc(30, 30),
          source_span=(31, 31),
        ),
      ),
    ])
    let node = CstNode::expr_try(
      loc=detached_loc(1, 30),
      source_span=(2, 31),
      entries~,
    )
    guard lower_expr(node) is Try(try_loc~, catch_loc~, else_loc~, ..) else {
      fail("expected try")
    }
    @test.assert_eq(
      try_loc,
      if prefix {
        detached_loc(1, 4)
      } else {
        catch_kw.loc
      },
    )
    @test.assert_eq(
      catch_loc,
      if prefix {
        detached_loc(12, 21)
      } else {
        catch_kw.loc
      },
    )
    @test.assert_eq(else_loc, node.loc)
  }
  let incomplete = CstNode::expr_try(
    loc=detached_loc(1, 10),
    source_span=(2, 11),
    entries=[
      Kw(detached_token(TK_TRY, 1, 4)),
      Body(body),
      HasTry(
        CstNode::flag_has_try(
          value=true,
          loc=detached_loc(10, 10),
          source_span=(11, 11),
        ),
      ),
    ],
  )
  guard lower_expr(incomplete) is Try(catch_loc~, else_loc~, ..) else {
    fail("expected try")
  }
  @test.assert_eq(catch_loc, incomplete.loc)
  @test.assert_eq(else_loc, incomplete.loc)
}
