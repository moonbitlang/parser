enum Tree {
  Leaf
  Node(Int, Tree, Tree)
}

fn cps_traversal(tree : Tree, k : (Int) -> Int) -> Int {
  match tree {
    Leaf => k(0)
    Node(x, l, r) =>
      cps_traversal(l, fn(l) { cps_traversal(r, fn(r) { k(x + l + r) }) })
  }
}

fn cps_traversal2(tree : Tree, k : (Int) -> Int) -> Int {
  for curr_tree = tree, curr_k = k {
    match (curr_tree, curr_k) {
      (Leaf, curr_k) => break curr_k(0)
      (Node(x, l, r), curr_k) =>
        continue l, fn(l) { cps_traversal2(r, fn(r) { curr_k(x + l + r) }) }
    }
  }
}

fn init {
  let tree = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
  cps_traversal(tree, fn(x) { x }) |> println
  cps_traversal2(tree, fn(x) { x }) |> println
}
