fn pattern_foreach() {
  for (Some(x) as item, [head, ..tail], (n : Int)) in values {}
  for { x, y } in points {}
  for 0 | 1 in bits {}
  for i, (a, b) in indexed_values {}
  for _, { x, y } in indexed_points {}
}
fn mixed_pattern_sequence() {
  for Some(x), y in values {}
  for x, Some(y) in values {}
}
