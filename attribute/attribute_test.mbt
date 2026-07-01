///|
let dummy_loc : @basic.Location = {
  let pos = @basic.Position::{ fname: "", lnum: 0, bol: 0, cnum: 0 }
  { start: pos, end: pos }
}

///|
test "attribute payload accepts extend soft keyword" {
  let pos = dummy_loc.start
  let tokens : Array[(@tokens.Token, @basic.Position, @basic.Position)] = [
    (@tokens.Token::LPAREN, pos, pos),
    (@tokens.Token::EXTEND, pos, pos),
    (@tokens.Token::COMMA, pos, pos),
    (@tokens.Token::EXTEND, pos, pos),
    (@tokens.Token::EQUAL, pos, pos),
    (@tokens.Token::TRUE, pos, pos),
    (@tokens.Token::COMMA, pos, pos),
    (@tokens.Token::EXTEND, pos, pos),
    (@tokens.Token::LPAREN, pos, pos),
    (@tokens.Token::EXTEND, pos, pos),
    (@tokens.Token::RPAREN, pos, pos),
    (@tokens.Token::COMMA, pos, pos),
    (@tokens.Token::EXTEND, pos, pos),
    (@tokens.Token::DOT_LIDENT("field"), pos, pos),
    (@tokens.Token::RPAREN, pos, pos),
    (@tokens.Token::EOF, pos, pos),
  ]
  try payloads(tokens, initial_pos=pos) catch {
    _ => fail("expected extend soft keyword in attribute payload to parse")
  } noraise {
    props =>
      debug_inspect(
        props,
        content=(
          #|<List:
          #|  [
          #|    Expr(Ident(extend)),
          #|    Labeled("extend", Bool(true)),
          #|    Expr(Apply(extend, <List: [Expr(Ident(extend))]>)),
          #|    Expr(Ident(extend.field)),
          #|  ]>
        ),
      )
  }
}

///|
test "attribute" {
  let attr = Attribute::new(loc=dummy_loc, ("qual", Some("name"), "(a,b,c)"))
  debug_inspect(
    attr,
    content=(
      #|{
      #|  loc: {
      #|    start: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|    end: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|  },
      #|  raw: "#qual.name(a,b,c)",
      #|  parsed: Some(
      #|    Apply(
      #|      qual.name,
      #|      <List: [Expr(Ident(a)), Expr(Ident(b)), Expr(Ident(c))]>,
      #|    ),
      #|  ),
      #|}
    ),
  )
  let attr = Attribute::new(
    loc=dummy_loc,
    ("qual", Some("name"), "(label1=abc, label2=cde)"),
  )
  debug_inspect(
    attr,
    content=(
      #|{
      #|  loc: {
      #|    start: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|    end: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|  },
      #|  raw: "#qual.name(label1=abc, label2=cde)",
      #|  parsed: Some(
      #|    Apply(
      #|      qual.name,
      #|      <List: [Labeled("label1", Ident(abc)), Labeled("label2", Ident(cde))]>,
      #|    ),
      #|  ),
      #|}
    ),
  )
  let attr = Attribute::new(
    loc=dummy_loc,
    ("qual", Some("name"), "(f(false), g(true))"),
  )
  debug_inspect(
    attr,
    content=(
      #|{
      #|  loc: {
      #|    start: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|    end: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|  },
      #|  raw: "#qual.name(f(false), g(true))",
      #|  parsed: Some(
      #|    Apply(
      #|      qual.name,
      #|      <List:
      #|        [
      #|          Expr(Apply(f, <List: [Expr(Bool(false))]>)),
      #|          Expr(Apply(g, <List: [Expr(Bool(true))]>)),
      #|        ]>,
      #|    ),
      #|  ),
      #|}
    ),
  )
  let attr = Attribute::new(
    loc=dummy_loc,
    ("qual", Some("name"), "(\"abcdef\", g(true))"),
  )
  debug_inspect(
    attr,
    content=(
      #|{
      #|  loc: {
      #|    start: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|    end: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|  },
      #|  raw: "#qual.name(\"abcdef\", g(true))",
      #|  parsed: Some(
      #|    Apply(
      #|      qual.name,
      #|      <List:
      #|        [Expr(String("abcdef")), Expr(Apply(g, <List: [Expr(Bool(true))]>))]>,
      #|    ),
      #|  ),
      #|}
    ),
  )
  let attr = Attribute::new(loc=dummy_loc, ("qual", Some("name"), ""))
  debug_inspect(
    attr,
    content=(
      #|{
      #|  loc: {
      #|    start: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|    end: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|  },
      #|  raw: "#qual.name",
      #|  parsed: Some(Ident(qual.name)),
      #|}
    ),
  )
  let attr = Attribute::new(loc=dummy_loc, ("qual", Some("name"), "(Debug)"))
  debug_inspect(
    attr,
    content=(
      #|{
      #|  loc: {
      #|    start: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|    end: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|  },
      #|  raw: "#qual.name(Debug)",
      #|  parsed: Some(Apply(qual.name, <List: [Expr(Ident(Debug))]>)),
      #|}
    ),
  )
  let attr = Attribute::new(loc=dummy_loc, ("qual", Some("name"), "("))
  debug_inspect(
    attr,
    content=(
      #|{
      #|  loc: {
      #|    start: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|    end: { fname: "", lnum: 0, bol: 0, cnum: 0 },
      #|  },
      #|  raw: "#qual.name(",
      #|  parsed: None,
      #|}
    ),
  )
}
