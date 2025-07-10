///|
pub struct LexResult {
  tokens : Array[(Token, Position, Position)]
  errors : Array[(Position, Position, LexicalError)]
  docstrings : Array[@list.T[(Location, Comment)]]
}

///|
pub fn tokens_from_bytes(
  name~ : String = "",
  start_pos~ : Position = Position::{ fname: "", lnum: 1, bol: 0, cnum: 0 },
  is_interpolation~ : Bool = false,
  comment~ : Bool,
  bytes : Bytes
) -> LexResult {
  let lexbuf = Lexbuf::from_bytes(bytes)
  let arr = Array::new(capacity=100)
  let (start_lnum, start_bol, start_cnum) = (
    start_pos.lnum,
    start_pos.bol,
    start_pos.cnum,
  )
  let env = LexEnv::{
    errors: [],
    docstrings: [],
    comment,
    file: name,
    tokens: arr,
    current_line: start_lnum,
    current_bol: start_bol,
    start_cnum,
    last_unhandled_comment: @ref.new(None),
    asi_context: ASIContext::new(),
    is_interpolation,
  }
  tokens(lexbuf, env~, preserve_comment=env.preserve_comment())
  let docstrings = env.docstrings
  if not(docstrings.is_empty()) {
    let last_idx = docstrings.length() - 1
    docstrings[last_idx] = docstrings[last_idx].rev()
  }
  docstrings.rev_inplace()
  LexResult::{ tokens: env.tokens, errors: env.errors, docstrings }
}
