///|
priv struct LexEnv {
  errors : Array[(Position, Position, String)]
  docstrings : Array[@immut/list.T[(Loc, Comment)]]?
  comment : Bool
  name : String
  arr : Array[(Token, Position, Position)]
  mut current_line : Int
  mut current_bol : Int
  last_unhandled_comment : Ref[(Comment, Int)?]
  asi_context : ASIContext
  start_cnum : Int
  is_interpolation : Bool
}

///|
fn LexEnv::new() -> LexEnv {
  LexEnv::{
    errors: [],
    docstrings: None,
    comment: false,
    name: "",
    arr: [],
    current_line: 1,
    current_bol: 0,
    start_cnum: 0,
    last_unhandled_comment: @ref.new(None),
    asi_context: ASIContext::new(),
    is_interpolation: false,
  }
}

///|
fn add_lexing_error(
  self : LexEnv,
  start~ : Int,
  end~ : Int,
  err : LexicalError
) -> Unit {
  let loc_start = self.make_position(start)
  let loc_end = self.make_position(end)
  let message = match err {
    InvalidDotInt(s) => "invalid byte literal: \{s}"
    InvalidEscapeSequence(seq) => "invalid escape sequence: \{seq}"
    InterpMissingExpression => "missing expression in string interpolation"
    InterpInvalidAttribute => "invalid attribute in string interpolation"
    InterpInvalidComment => "invalid comment in string interpolation"
    InterpInvalidMultilineString =>
      "invalid multiline string in string interpolation"
    UnterminatedStringInVariableInterploation =>
      "(unterminated string in variable interpolation)"
    UnterminatedString => "(unterminated string)"
    IllegalCharacter(c) => "unrecognized character u32: \{c}"
    InvalidByteLiteral(s) => "invalid byte literal: \{s}"
    Reserved_keyword(s) => "reserved keyword: \{s}"
  }
  self.errors.push((loc_start, loc_end, message))
}

///|
fn make_position(self : LexEnv, cnum : Int) -> Position {
  Position::{
    fname: self.name,
    lnum: self.current_line,
    cnum: self.start_cnum + cnum,
    bol: self.current_bol,
  }
}

///|
fn LexEnv::add_token(
  self : LexEnv,
  tok : Token,
  start : Position,
  end : Position
) -> Unit {
  self.asi_context.add_token(
    tokens=self.arr,
    last_unhandled_comment=self.last_unhandled_comment,
    tok,
  )
  // handle comment: add comments to token array
  self.arr.push((tok, start, end))
  if self.comment {
    let tok_index = self.arr.length() - 1
    fn prehandle_comment(c : Comment) -> Unit {
      self.last_unhandled_comment.val = Some((c, tok_index))
    }

    fn handle_comment(ci : (Comment, Int)) -> Unit {
      let (c, i) = ci
      let mut at_file_start = false
      fn count_newlines(start_index : Int, direction : Int) -> Int {
        let mut count = 0
        let mut index = start_index
        while true {
          let target_index = index + direction
          match self.arr.get(target_index) {
            Some((COMMENT(_), _, _)) => return count
            Some((NEWLINE, _, _)) =>
              if count >= 2 {
                return count
              } else {
                count += 1
                index = target_index
              }
            None | Some((EOF, _, _)) => {
              if direction < 0 {
                at_file_start = true
              }
              return 0
            }
            Some(_) => return count
          }
        } else {
          panic() // impossible
        }
      }

      let leading_newlines = @math.minimum(2, count_newlines(i, -1))
      // Make sure add trailing newline if the comment is at end of file
      let trailing_newlines = @math.minimum(2, count_newlines(i, 1))
      let (_, start, end) = self.arr[i]
      let kind = if leading_newlines == 0 && not(at_file_start) {
        InlineTrailing
      } else {
        Ownline(
          leading_blank_line=leading_newlines == 2 && not(at_file_start),
          trailing_blank_line=trailing_newlines == 2,
        )
      }
      self.arr[i] = (COMMENT({ ..c, kind, }), start, end)
    }

    match (self.last_unhandled_comment.val, tok) {
      (_, NEWLINE) => ()
      (Some(ci), COMMENT(c)) => {
        handle_comment(ci)
        prehandle_comment(c)
      }
      (None, COMMENT(c)) => prehandle_comment(c)
      (Some(ci), _) => handle_comment(ci)
      (None, _) => ()
    }
  }
}

///|
fn add_token_with_loc(
  self : LexEnv,
  tok : Token,
  start~ : Int,
  end~ : Int,
  start_offset~ : Int = 0
) -> Unit {
  let start = self.make_position(start + start_offset)
  let end = self.make_position(end)
  self.add_token(tok, start, end)
}

///|
fn preserve_comment(self : LexEnv) -> (Comment, Int, Int) -> Unit {
  fn ignore3(cmt, start, end) {
    ignore(cmt)
    ignore(start)
    ignore(end)
  }

  match self.docstrings {
    Some(c) if self.comment =>
      fn(comment, start, end) {
        if comment.content is [_, _, '/', ..] {
          let loc = Loc::{
            start: self.make_position(start),
            end: self.make_position(end),
          }
          if c.is_empty() {
            c.push(@immut/list.singleton((loc, comment)))
          } else {
            guard c.last() is Some(Cons(head, _) as last)
            let last_idx = c.length() - 1
            if loc.start.lnum - head.0.start.lnum > 1 {
              c[last_idx] = last.rev()
              c.push(@immut/list.singleton((loc, comment)))
            } else {
              c[last_idx] = Cons((loc, comment), c[last_idx])
            }
          }
        }
      }
    _ => ignore3
  }
}
