fn main {
  // This may be a bug from upstream  
  @moonbitlang/core/builtin.Map::from_array([(1,"hel")]) |> debug
  { 1 :  "hel"} |> debug
}