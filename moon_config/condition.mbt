///|
/// Conditions are preserved without selecting a compilation backend.
#derive(Debug, Eq, ToJson)
pub(all) enum Condition {
  Bool(Bool)
  Target(String)
  All(Array[Condition])
  Any(Array[Condition])
  Not(Condition)
}

///|
/// The original attribute is retained even if its payload is invalid.
#derive(Debug, Eq)
pub(all) struct ImportCondition {
  raw : String
  parsed : Condition?
  loc : Location
}

///|
fn Condition::is_valid(self : Self) -> Bool {
  match self {
    Bool(_) => true
    Target("wasm" | "wasm-gc" | "js" | "native" | "llvm") => true
    Target(_) => false
    All(args) | Any(args) => args.all(arg => arg.is_valid())
    Not(arg) => arg.is_valid()
  }
}

///|
fn parse_condition(state : State) -> Condition? {
  match state.consume().0 {
    TRUE => Some(Bool(true))
    FALSE => Some(Bool(false))
    LIDENT("target") => {
      guard state.consume_if(TK_EQUAL) else { return None }
      match state.peek_token() {
        STRING(value) => {
          state.skip()
          Some(Target(value))
        }
        _ => None
      }
    }
    LIDENT("all" | "any" | "not" as name) => {
      guard state.consume_if(TK_LPAREN) else { return None }
      let args : Array[Condition] = []
      if !state.consume_if(TK_RPAREN) {
        while true {
          guard state.peek_kind() != TK_EOF else { return None }
          guard parse_condition(state) is Some(arg) else { return None }
          args.push(arg)
          if state.consume_if(TK_RPAREN) {
            break
          }
          guard state.consume_if(TK_COMMA) else { return None }
        }
      }
      match name {
        "all" => Some(All(args))
        "any" => Some(Any(args))
        _ => if args is [arg] { Some(Not(arg)) } else { None }
      }
    }
    _ => None
  }
}

///|
fn parse_condition_payload(payload : String) -> Condition? {
  let lexed = @lexer.tokens_from_string(payload, comment=false)
  guard lexed.errors.is_empty() else { return None }
  let state : State = {
    tokens: lexed.tokens,
    diagnostics: [],
    next: 0,
    parsed_position: dummy_pos,
  }
  guard state.consume_if(TK_LPAREN) && state.peek_kind() != TK_EOF else {
    return None
  }
  let parsed = parse_condition(state)
  guard state.consume_if(TK_RPAREN) else { return None }
  guard state.peek_kind() == TK_EOF else { return None }
  parsed
}

///|
pub extend Condition with Debug::{to_repr}

///|
pub extend Condition with Eq::{equal, not_equal}

///|
pub extend Condition with ToJson::{to_json}

///|
pub extend ImportCondition with Debug::{to_repr}

///|
pub extend ImportCondition with Eq::{equal, not_equal}
