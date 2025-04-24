///|
pub fn tokens_from_bytes(
  name~ : String = "",
  start_pos~ : Position = Position::{ fname: "", lnum: 1, bol: 0, cnum: 0 },
  is_interpolation~ : Bool = false,
  docstrings~ : Array[@immut/list.T[(Loc, Comment)]]? = None,
  comment~ : Bool,
  errors~ : Array[(Position, Position, String)] = [],
  bytes : Bytes
) -> Array[(Token, Position, Position)] {
  let lexbuf = Lexbuf::from_bytes(bytes)
  let arr = Array::new(capacity=100)
  let (start_lnum, start_bol, start_cnum) = (
    start_pos.lnum,
    start_pos.bol,
    start_pos.cnum,
  )
  let env = LexEnv::{
    errors,
    docstrings,
    comment,
    name,
    arr,
    current_line: start_lnum,
    current_bol: start_bol,
    start_cnum,
    last_unhandled_comment: @ref.new(None),
    asi_context: ASIContext::new(),
    is_interpolation,
  }
  tokens(lexbuf, env~, preserve_comment=env.preserve_comment())
}
