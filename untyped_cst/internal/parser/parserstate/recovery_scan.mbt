///|
pub(all) struct RecoveryScope {
  id : Int
  stop_kinds : Array[TokenKind]
  depth : Int
  depth_sensitive : Bool
}

///|
pub(all) enum RecoveryOwner {
  RecoveryLocal
  RecoveryScopeOwner(Int)
  RecoveryEof
}

///|
pub(all) struct RecoveryScan {
  skipped : Array[CstNode]
  hit : Token?
  owner : RecoveryOwner
}

///|
pub fn ParserState::push_recovery_scope(
  self : ParserState,
  stop_kinds : Array[TokenKind],
  depth_sensitive? : Bool = false,
) -> Int {
  let id = self.next_recovery_scope_id
  self.next_recovery_scope_id += 1
  self.recovery_scopes.push({
    id,
    stop_kinds,
    depth: self.delimiter_depth,
    depth_sensitive,
  })
  id
}

///|
pub fn ParserState::pop_recovery_scope(self : ParserState, id : Int) -> Unit {
  match self.recovery_scopes.pop() {
    Some(scope) if scope.id == id => ()
    _ => abort("recovery scopes must be popped in nesting order")
  }
}

///|
pub fn ParserState::recovery_stop_at(
  self : ParserState,
  local_stops : Array[TokenKind],
) -> RecoveryOwner? {
  match self.peek() {
    None => Some(RecoveryEof)
    Some((tok, _, _)) => {
      let kind = tok.kind()
      if kind_in(kind, local_stops) {
        return Some(RecoveryLocal)
      }
      if tok is (COMMENT(_) | NEWLINE) {
        return None
      }
      let mut i = self.recovery_scopes.length() - 1
      while i >= 0 {
        let scope = self.recovery_scopes[i]
        if kind_in(kind, scope.stop_kinds) &&
          (!scope.depth_sensitive || self.delimiter_depth <= scope.depth) {
          return Some(RecoveryScopeOwner(scope.id))
        }
        i -= 1
      }
      if tok is EOF {
        Some(RecoveryEof)
      } else {
        None
      }
    }
  }
}

///|
pub fn ParserState::recover_until(
  self : ParserState,
  local_stops : Array[TokenKind],
  balance_groups? : Bool = false,
) -> RecoveryScan {
  let skipped : Array[CstNode] = []
  let mut depth = 0
  while true {
    match self.peek() {
      None => return { skipped, hit: None, owner: RecoveryEof, }
      Some((tok, _, _)) => {
        if tok is EOF && depth > 0 {
          return { skipped, hit: Some(tok), owner: RecoveryEof, }
        }
        if depth == 0 {
          match self.recovery_stop_at(local_stops) {
            Some(owner) => return { skipped, hit: Some(tok), owner, }
            None => ()
          }
        }
        match self.consume_any() {
          Some(node) => {
            skipped.push(node)
            if balance_groups {
              depth = next_depth(depth, tok)
            }
          }
          None => return { skipped, hit: None, owner: RecoveryEof, }
        }
      }
    }
  }
  { skipped, hit: None, owner: RecoveryEof, }
}

///|
pub fn next_depth(depth : Int, tok : Token) -> Int {
  let delta = match tok {
    LPAREN | LBRACKET | LBRACKET_BAR | LBRACE => 1
    RPAREN | RBRACKET | BAR_RBRACKET | RBRACE => -1
    _ => 0
  }
  let next = depth + delta
  if next < 0 {
    0
  } else {
    next
  }
}
