///|
struct Mapper {
  map : Map[Key, Array[Comment]]
  table : Map[Key, NodeTable]
  queue : @queue.Queue[Comment]
  stack : Array[Key]
  /// for debug purpose
  comments : Array[Comment]
  tokens : Array[Triple]
  mut cursor : Int
}

///|
pub fn Mapper::get_table(self : Self, node_loc : Location) -> NodeTable {
  self.table.get(Node(node_loc)).unwrap_or_else(fn() { NodeTable::empty() })
}

///|
pub fn Mapper::consume_comments(
  self : Self,
  key : Key?,
  filter : (Comment) -> Bool,
) -> Array[Comment] {
  match key {
    None => []
    Some(k) => {
      let arr = self.map.get(k).unwrap_or([])
      let keep_arr = []
      let move_arr = []
      for e in arr {
        if filter(e) {
          move_arr.push(e)
        } else {
          keep_arr.push(e)
        }
      }
      self.map.set(k, keep_arr)
      move_arr
    }
  }
}

///|
pub fn Mapper::has_comment(self : Self, key : Key?) -> Bool {
  match key {
    None => false
    Some(k) => self.map.contains(k)
  }
}

///|
pub fn Mapper::has_leading_comment(self : Self, key : Key?) -> Bool {
  // TODO: optimize this
  match key {
    None => false
    Some(k) =>
      self.map.get(k).unwrap_or([]).iter().any(x => x.end < k.loc().start)
  }
}

///|
pub fn Mapper::has_trailing_comment(self : Self, key : Key?) -> Bool {
  // TODO: optimize this
  match key {
    None => false
    Some(k) =>
      self.map.get(k).unwrap_or([]).iter().any(x => x.start > k.loc().start)
  }
}

///|
pub fn Mapper::new(tokens : Array[Triple], impls : @syntax.Impls) -> Self {
  let queue : @queue.Queue[Comment] = Queue([])
  let mut i = 0
  let mut previous = None
  while true {
    match tokens[i:] {
      [(COMMENT(c), spos, epos), ..] => {
        i += 2
        let next = tokens.get(i)
        queue.push({
          start: spos,
          end: epos,
          kind: Line(c.content),
          left: previous,
          right: next,
        })
      }
      [(NEWLINE, spos, epos), ..] => {
        let mut epos = epos
        let mut count = 0
        while tokens.get(i) is Some((NEWLINE, _, e)) {
          count += 1
          epos = e
          i += 1
        }
        if count > 1 {
          let next = tokens.get(i)
          queue.push({
            start: spos,
            end: epos,
            kind: Blank,
            left: previous,
            right: next,
          })
        }
      }
      [(SEMI(false), _, _), ..] => i += 1
      [triple, ..] => {
        previous = Some(triple)
        i += 1
      }
      _ => break
    }
  }
  // println(@pp.pretty(queue))
  let document_loc : Location = {
    let pos : Position = { fname: "", lnum: 0, bol: 0, cnum: 0 }
    if tokens.last() is Some(eof) {
      { start: pos, end: eof.2 }
    } else {
      { start: pos, end: pos }
    }
  }
  let self = {
    queue,
    comments: queue.iter().collect(),
    map: Map([]),
    stack: [],
    table: Map([]),
    tokens,
    cursor: 0,
  }
  self.visit_Impls(impls, document_loc)
  self
}

///|
pub fn Mapper::to_debug_info(self : Self) -> String {
  let dict : Map[Key, Array[Comment]] = self.map
  let doc = dict.to_array() |> @sorted_map.from_array |> @pp.render(width=80)
  let comments = @pp.render(width=80, self.comments)
  let token_table = @pp.render(@pp.pretty(self.table), width=80)
  comments +
  "\n\n" +
  doc +
  "\n\n" +
  @pp.render(self.table) +
  "\n\n" +
  token_table
}

///|
fn @basic.Location::contains_pos(self : Self, pos : Position) -> Bool {
  self.start <= pos && self.end > pos
}

///|
/// Associated related comment to AST node n, if:
///
///   1. c starts on the same line as n ends
///   2. c starts on the line following n, and there is
///     at least one empty line after c
///   3. c starts before n and is not associated to the node before n
///     via the previous rules 
/// 
/// This algorithm was inspired by gofmt.
fn Mapper::attach(self : Self, n : Key) -> Unit {
  // The stack tracks the path from root to the previous visited node. Pop the 
  // stack until the parent of n is at the top. The last popped node (pn) should 
  // be the previous sibling of n.
  //
  // Consider this case: The comment will be attach to previous sibling of n.
  // If previous sibling doesn't exist (stack top is `[[]]`, which is the parent of n), 
  // the comment should be attach to n instead.
  //
  // ```
  // [[
  //   //comment
  //   n
  // ]]
  // ```
  let mut pn = None
  while self.stack.last() is Some(top) && !top.loc().contains_pos(n.loc().start) {
    pn = self.stack.pop()
  }

  // handle comments before current node
  while self.queue.peek() is Some(c) && c.end < n.loc().start {
    self.queue.pop() |> ignore
    let assoc : Key = match pn {
      // 1) the `c` in the same line of the `pn`, or 
      // 2) immediately after the `pn` and there is an empty line before the `n`
      // attach `c` to `pn`. 
      //
      // NOTE:
      // There should be nothing between `pn` and `c` (except NEWLINE, COMMENT,
      // and inserted SEMI). Consider this case:
      // 
      // ```
      // //comment
      // match e {
      //   //comment
      //   pat => ...
      // }
      // ```
      // The comment should not be associated to `e`
      Some(pn) if pn.loc().contains_pos(c.left.unwrap().1) &&
        (
          pn.loc().end.lnum == c.start.lnum ||
          (
            pn.loc().end.lnum + 1 == c.start.lnum &&
            c.end.lnum + 1 < n.loc().start.lnum
          )
        ) => pn
      // The previous sibling may not exsit, attach `c` to `n` in this case.
      _ => n
    }
    match self.map.get(assoc) {
      None => self.map[assoc] = [c]
      Some(arr) => arr.push(c)
    }
    log("attached comment \{@pp.render(c.kind)} to \{@pp.render(assoc)}")
  }
  // Update the stack
  self.stack.push(n)
}

///|
fn Mapper::attach_node(self : Self, n : Location) -> Unit {
  self.attach(Node(n))
}

///|
fn Mapper::visit_Impls(
  self : Self,
  impls : @syntax.Impls,
  _loc : Location,
) -> Unit {
  impls.map(x => self.visit_Impl(x)) |> ignore
  // println("eof: \{eof_loc}, delayed: \{self.delayed}")
  // self.attach_after(eof_loc)
}
