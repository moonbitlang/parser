// Companion to tail_labelled_block.mbt, reproducing the original report of the
// bug: the lexmatch lowering routes an arm action through an implicit labelled
// join block when the action is reachable from more than one DFA accept state
// and has captures — here `re"^ab?"` accepts after `a` (with an outgoing `b`
// transition) and after `ab`, so the two accept states cannot merge. The
// recursive call on `after` then sits inside that generated block, and only
// survives this input length as a tail call; with the block's tail flag lost
// it overflows the stack.

///|
fn lexing(s : StringView, acc : Int) -> Int {
  lexmatch s {
    (re"^ab?" as tok, after~) => lexing(after, acc + tok.length())
    _ => acc
  }
}

///|
fn main {
  let s = "a".repeat(1000000)
  println(lexing(s[:], 0))
}
