///|
fn recovery_test_state(source : String) -> ParserState {
  let lex = @lexer.tokens_from_string(
    source,
    name="recovery_scan_test.mbt",
    comment=true,
  )
  new_state(lex.tokens, source, [], false, fn(_, _) {
    abort("unexpected interpolation in recovery test")
  })
}

///|
test "local recovery stop takes priority over nested scopes" {
  let state = recovery_test_state("bad)")
  let outer = state.push_recovery_scope([TK_RPAREN])
  let inner = state.push_recovery_scope([TK_RPAREN])
  let scan = state.recover_until([TK_RPAREN])
  @test.assert_eq(scan.owner is RecoveryLocal, true)
  @test.assert_eq(scan.hit is Some(RPAREN), true)
  @test.assert_eq(scan.skipped.length(), 1)
  @test.assert_eq(state.current_is(TK_RPAREN), true)
  state.pop_recovery_scope(inner)
  state.pop_recovery_scope(outer)
  @test.assert_eq(state.reports.length(), 0)
}

///|
test "innermost matching recovery scope owns the stop token" {
  let state = recovery_test_state("bad}")
  let outer = state.push_recovery_scope([TK_RBRACE])
  let inner = state.push_recovery_scope([TK_RBRACE])
  let scan = state.recover_until([TK_COMMA])
  match scan.owner {
    RecoveryScopeOwner(id) => @test.assert_eq(id, inner)
    _ => fail("expected the inner recovery scope")
  }
  @test.assert_eq(scan.hit is Some(RBRACE), true)
  @test.assert_eq(state.current_is(TK_RBRACE), true)
  state.pop_recovery_scope(inner)
  let second = state.recover_until([TK_COMMA])
  match second.owner {
    RecoveryScopeOwner(id) => @test.assert_eq(id, outer)
    _ => fail("expected the outer recovery scope")
  }
  @test.assert_eq(second.skipped.length(), 0)
  state.pop_recovery_scope(outer)
}

///|
test "declaration scope activates again after its block closes" {
  let state = recovery_test_state("{ fn } fn")
  let declaration = state.push_recovery_scope([TK_FN], depth_sensitive=true)
  let _ = state.consume_any()
  let inside = state.recover_until([TK_RBRACE])
  @test.assert_eq(inside.owner is RecoveryLocal, true)
  @test.assert_eq(inside.skipped.length() > 0, true)
  @test.assert_eq(state.current_is(TK_RBRACE), true)
  let _ = state.consume_any()
  let outside = state.recover_until([TK_COMMA])
  match outside.owner {
    RecoveryScopeOwner(id) => @test.assert_eq(id, declaration)
    _ => fail("expected declaration boundary after the block")
  }
  @test.assert_eq(state.current_is(TK_FN), true)
  state.pop_recovery_scope(declaration)
}

///|
test "balanced recovery skips inner separators and preserves raw trivia" {
  let source = "bad (a, b) // note\n more, tail"
  let state = recovery_test_state(source)
  let scan = state.recover_until([TK_COMMA], balance_groups=true)
  @test.assert_eq(scan.owner is RecoveryLocal, true)
  @test.assert_eq(state.current_is(TK_COMMA), true)
  let mut saw_inner_comma = false
  let mut saw_comment = false
  let mut saw_newline = false
  let mut previous_start = 0
  for skipped in scan.skipped {
    @test.assert_eq(skipped.source_span.0 >= previous_start, true)
    @test.assert_eq(skipped.source_span.1 >= skipped.source_span.0, true)
    previous_start = skipped.source_span.0
    if skipped.kind.token_kind() is Some(TK_COMMA) {
      saw_inner_comma = true
    }
    if source[skipped.source_span.0:skipped.source_span.1] == "// note"[:] {
      saw_comment = true
    }
    if skipped.kind.token_kind() is Some(TK_NEWLINE) {
      saw_newline = true
    }
  }
  @test.assert_eq(saw_inner_comma, true)
  @test.assert_eq(saw_comment, true)
  @test.assert_eq(saw_newline, true)
  @test.assert_eq(state.reports.length(), 0)
}

///|
test "recovery at a stop or EOF makes no progress and does not report" {
  let at_stop = recovery_test_state(",")
  let scan = at_stop.recover_until([TK_COMMA])
  @test.assert_eq(scan.owner is RecoveryLocal, true)
  @test.assert_eq(scan.skipped.length(), 0)
  @test.assert_eq(at_stop.pos, 0)
  let at_eof = recovery_test_state("")
  let eof_scan = at_eof.recover_until([TK_COMMA])
  @test.assert_eq(eof_scan.owner is RecoveryEof, true)
  @test.assert_eq(eof_scan.skipped.length(), 0)
  @test.assert_eq(at_eof.pos, 0)
  let unclosed = recovery_test_state("(bad")
  let unclosed_scan = unclosed.recover_until([TK_RPAREN], balance_groups=true)
  @test.assert_eq(unclosed_scan.owner is RecoveryEof, true)
  @test.assert_eq(unclosed_scan.skipped.length() > 0, true)
  @test.assert_eq(at_stop.reports.length(), 0)
  @test.assert_eq(at_eof.reports.length(), 0)
}
