struct Node {
  mut next : Option[Node]
  value : Int
}

fn Node::clear(self : Node) -> Unit {
  self.next = None
}

fn consume(saved : Option[Node], value : Int) -> Int {
  match saved {
    Some(node) => node.value + value
    None => value
  }
}

fn stale_field(seed : Node) -> (Int, Option[Node]) {
  let e : Node = { next: Some(seed), value: 1 }
  let saved = e.next
  e.clear()

  let mut copied = saved
  let value = e.value
  if value < 0 {
    copied = None
  }

  let result = consume(copied, value)
  (result, saved)
}

fn init {
  let seed : Node = { next: None, value: 0 }
  ignore(stale_field(seed))
}
