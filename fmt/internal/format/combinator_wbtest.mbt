///|
test "catifflat renders compact, mixed, and expanded layouts" {
  let integer = fn(x : Int) { text(x.to_string()) }
  let numbers = @list.List([111111, 22222222, 33333333, 444444444])
  let left = group(
    nest(break_(0) + separate_map(numbers, comma + break_(1), integer) + comma),
  )
  let flat = group(
    space +
    text("fn(){") +
    nest(break_(1) + text("expressions;")) +
    break_(1) +
    text("}"),
  )
  let not_flat = nest(
      break_(1) +
      text("fn(){") +
      nest(break_(1) + text("expressions")) +
      break_(1),
    ) +
    text("}") +
    break_(0)
  let layout = text("call(") +
    catifflat(left~, right=text(")"), flat, not_flat) +
    comma
  inspect(
    @pp.render(layout, width=80),
    content="call(111111, 22222222, 33333333, 444444444, fn(){ expressions; }),",
  )
  inspect(
    @pp.render(layout, width=45),
    content=(
      #|call(111111, 22222222, 33333333, 444444444, fn(){
      #|  expressions;
      #|}),
    ),
  )
  inspect(
    @pp.render(layout, width=20),
    content=(
      #|call(
      #|  111111,
      #|  22222222,
      #|  33333333,
      #|  444444444,
      #|  fn(){
      #|    expressions
      #|  }
      #|),
    ),
  )
}
