///|
pub(all) struct Comment {
  start : Position
  end : Position
  left : Triple?
  right : Triple?
  kind : CommentKind
} derive(@debug.Debug)

///|
fn pretty_triple(x : Triple?) -> @pp.Document {
  if x is Some(x) {
    text(x.0.to_expect_string()) +
    space +
    @pp.parens(text(Show::to_string(Location::{ start: x.1, end: x.2, })))
  } else {
    text("None")
  }
}

///|
// fn pretty_pos(x : Position) -> @pp.Document {
//   x.to_string() |> text
// }

///|
pub impl Pretty for Comment with fn pretty(self) {
  @pp.record({
    "kind": pretty(self.kind),
    "loc": text(
      Show::to_string(Location::{ start: self.start, end: self.end, }),
    ),
    "surround": pretty((pretty_triple(self.left), pretty_triple(self.right))),
  })
}

///|
pub(all) enum CommentKind {
  Blank
  Block(String)
  Line(String)
} derive(@debug.Debug)

///|
pub impl Pretty for CommentKind with fn pretty(x) {
  match x {
    Blank => text("BLANK")
    Block(s) => text(s.escape())
    Line(s) => text(s)
  }
}

///|
#deprecated
#doc(hidden)
pub extend Comment with @moonbit-community/prettyprinter.Pretty::{pretty}

///|
#deprecated
#doc(hidden)
pub extend Comment with Debug::{to_repr}

///|
#deprecated
#doc(hidden)
pub extend CommentKind with @moonbit-community/prettyprinter.Pretty::{pretty}

///|
#deprecated
#doc(hidden)
pub extend CommentKind with Debug::{to_repr}
