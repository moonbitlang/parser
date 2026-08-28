///|
pub enum CheckpointMode {
  Attach
  Pretty
}

///|
pub struct CommentSlot {
  comments : Array[@tokens.Comment]
  ignore_blank : Bool
}

///|
pub struct CheckpointMapper {
  tokens : @tokens.Triples
  start : Int
  limit : Int
  slots : Array[CommentSlot]
  dangling : Array[@tokens.Comment]
  mut mode : CheckpointMode
  mut cursor : Int
  mut current_slot : Int?
  mut pretty_slot : Int
  mut failed : Bool
}

///|
pub fn CheckpointMapper::new(
  tokens : @tokens.Triples,
  start? : Int = 0,
  len? : Int = -1,
) -> Self {
  let start = @cmp.maximum(0, @cmp.minimum(start, tokens.length()))
  let limit = if len < 0 {
    tokens.length()
  } else {
    @cmp.minimum(tokens.length(), start + len)
  }
  {
    tokens,
    start,
    limit,
    slots: [],
    dangling: [],
    mode: Attach,
    cursor: start,
    current_slot: None,
    pretty_slot: 0,
    failed: false,
  }
}

///|
fn CheckpointMapper::collect_comment(
  self : Self,
  comment : @tokens.Comment,
) -> Unit {
  if !comment.consumed_by_docstring {
    self.dangling.push(comment)
  }
}

///|
fn CheckpointMapper::flush(self : Self) -> Unit {
  if self.dangling.is_empty() {
    return
  }
  match self.current_slot {
    Some(index) if self.slots.get(index) is Some(slot) => {
      slot.comments.push_iter(self.dangling.iter())
      self.dangling.clear()
    }
    _ => self.failed = true
  }
}

///|
fn CheckpointMapper::skip_one(self : Self) -> Unit {
  guard self.cursor < self.limit else { return }
  let triple = self.tokens[self.cursor]
  match triple.0 {
    COMMENT(comment) => self.collect_comment(comment)
    _ => ()
  }
  self.cursor += 1
}

///|
pub fn CheckpointMapper::checkpoint(self : Self, pos : Position) -> Unit {
  if self.mode is Pretty || pos.lnum < 0 {
    return
  }
  while self.cursor < self.limit {
    let (_, start, _) = self.tokens[self.cursor]
    if start < pos {
      self.skip_one()
    } else {
      break
    }
  }
  self.flush()
}

///|
fn is_ignorable_token(token : @tokens.Token) -> Bool {
  token is (COMMENT(_) | NEWLINE | SEMI(false))
}

///|
pub fn CheckpointMapper::token(self : Self, expected : TokenKind) -> Unit {
  if self.mode is Pretty {
    return
  }
  let mut index = self.cursor
  while index < self.limit && is_ignorable_token(self.tokens[index].0) {
    index += 1
  }
  guard index < self.limit && self.tokens[index].0.kind() == expected else {
    return
  }
  while self.cursor <= index {
    self.skip_one()
  }
  self.flush()
}

///|
fn comment_slot_doc(slot : CommentSlot) -> @pp.Document {
  let mut doc = @pp.empty
  for comment in slot.comments {
    match comment.kind {
      InlineTrailing => {
        doc += @pp.mandatory_space + @pp.text(comment.content)
        doc += @pp.mandatory_line
      }
      Ownline(leading_blank_line=_, trailing_blank_line=_) => {
        doc += @pp.mandatory_line
        doc += @pp.text(comment.content)
        doc += @pp.mandatory_line
      }
    }
  }
  doc
}

///|
pub fn CheckpointMapper::placeholder(
  self : Self,
  ignore_blank? : Bool = false,
) -> @pp.Document {
  match self.mode {
    Attach => {
      self.slots.push({ comments: [], ignore_blank, })
      self.current_slot = Some(self.slots.length() - 1)
      @pp.empty
    }
    Pretty => {
      let index = self.pretty_slot
      self.pretty_slot += 1
      match self.slots.get(index) {
        Some(slot) => comment_slot_doc(slot)
        None => {
          self.failed = true
          @pp.empty
        }
      }
    }
  }
}

///|
pub fn CheckpointMapper::placeholder_or_space(
  self : Self,
  ignore_blank? : Bool = false,
) -> @pp.Document {
  match self.mode {
    Attach => {
      self.slots.push({ comments: [], ignore_blank, })
      self.current_slot = Some(self.slots.length() - 1)
      @pp.space
    }
    Pretty => {
      let index = self.pretty_slot
      self.pretty_slot += 1
      match self.slots.get(index) {
        Some(slot) =>
          if slot.comments.is_empty() {
            @pp.space
          } else {
            comment_slot_doc(slot)
          }
        None => {
          self.failed = true
          @pp.space
        }
      }
    }
  }
}

///|
pub fn CheckpointMapper::eof(self : Self) -> Unit {
  if self.mode is Attach {
    while self.cursor < self.limit {
      self.skip_one()
    }
    self.flush()
  }
}

///|
pub fn CheckpointMapper::begin_pretty(self : Self) -> Unit {
  self.mode = Pretty
  self.cursor = self.start
  self.current_slot = None
  self.pretty_slot = 0
}

///|
pub fn CheckpointMapper::is_complete(self : Self) -> Bool {
  !self.failed && self.mode is Pretty && self.pretty_slot == self.slots.length()
}
