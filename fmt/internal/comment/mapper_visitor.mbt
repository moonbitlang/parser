///|
extend Mapper with IterVisitor::{
  base,
  visit_Binder,
  visit_Expr,
  visit_Impl,
  visit_Type,
  visit_TypeName,
  visit_Visibility,
  visit_WhereClause,
}

///|
impl IterVisitor for Mapper with fn visit_Expr(self, expr) {
  self..attach(Node(expr.loc())).base().visit_Expr(expr)
}

///|
impl IterVisitor for Mapper with fn visit_Pattern(self, pat) {
  self..attach(Node(pat.loc())).base().visit_Pattern(pat)
}

///|
impl IterVisitor for Mapper with fn visit_Type(self, ty) {
  self..attach(Node(ty.loc())).base().visit_Type(ty)
}

///|
impl IterVisitor for Mapper with fn visit_Impl(self, top) {
  self..attach(Node(top.loc())).base().visit_Impl(top)
}

///|
impl IterVisitor for Mapper with fn visit_Expr_Array(
  self,
  exprs~,
  is_iter~,
  loc~,
) {
  self.attach(Node(loc))
  self.cursor_advance(loc.start)
  let (lbracket_kind, rbracket_kind) : (TokenKind, TokenKind) = if is_iter {
    (TK_LBRACKET_BAR, TK_BAR_RBRACKET)
  } else {
    (TK_LBRACKET, TK_RBRACKET)
  }
  let lbracket = self.cursor_find(lbracket_kind)
  let commas = []
  for elem in exprs {
    self.visit_Expr(elem)
    self.cursor_advance(elem.loc().end)
    let comma = self.cursor_find(TK_COMMA, before_pos=loc.end)
    commas.push(comma)
  }
  let rbracket = self.cursor_find(rbracket_kind)
  self.table[Node(loc)] = NodeTable::new({
    "lbracket": Token(lbracket),
    "comma": TokenArray(commas),
    "rbracket": Token(rbracket),
  })
}

///|
impl IterVisitor for Mapper with fn visit_Visibility(self, vis) {
  match vis {
    Default => ()
    Pub(attr~, loc~) => {
      self.cursor_advance(loc.start)
      let modifier0 = self.cursor_find(TK_PUB, before_pos=loc.end)
      let lparen = self.cursor_find(TK_LPAREN, before_pos=loc.end)
      let (lident, rparen) = if attr is Some(_) {
        let a = self.cursor_find(TK_LIDENT, before_token=TK_RPAREN)
        let b = self.cursor_find(TK_RPAREN, before_pos=loc.end)
        (a, b)
      } else {
        (None, None)
      }
      self.table[Node(loc)] = NodeTable::new({
        "pub": Token(modifier0),
        "lparen": Token(lparen),
        "lident": Token(lident),
        "rparen": Token(rparen),
      })
    }
    Priv(loc~) => {
      self.cursor_advance(loc.start)
      let modifier0 = self.cursor_find(TK_PRIV, before_pos=loc.end)
      self.table[Node(loc)] = NodeTable::new({ "priv": Token(modifier0) })
    }
  }
}

///|
fn Mapper::fill_parameter(self : Self, param : @syntax.Parameter) -> NodeTable {
  self.attach_node(param.loc())
  match param {
    DiscardPositional(ty~, loc~) => {
      self.cursor_advance(loc.start)
      let colon = self.cursor_find(TK_COLON)
      if ty is Some(ty) {
        self.visit_Type(ty)
      }
      NodeTable::new({ "colon": Token(colon) })
    }
    Labelled(ty~, binder~) | Positional(ty~, binder~) => {
      self.cursor_advance(binder.loc.start)
      let colon = self.cursor_find(TK_COLON)
      if ty is Some(ty) {
        self.visit_Type(ty)
      }
      NodeTable::new({ "colon": Token(colon) })
    }
    QuestionOptional(ty~, binder~) => {
      self.cursor_advance(binder.loc.start)
      let question = self.cursor_find(TK_QUESTION)
      let colon = self.cursor_find(TK_COLON)
      if ty is Some(ty) {
        self.visit_Type(ty)
      }
      NodeTable::new({ "question": Token(question), "colon": Token(colon) })
    }
    Optional(ty~, default~, binder~) => {
      self.cursor_advance(binder.loc.start)
      let colon = self.cursor_find(TK_COLON)
      if ty is Some(ty) {
        self.visit_Type(ty)
      }
      let equal = self.cursor_find(TK_EQUAL)
      self.visit_Expr(default)
      NodeTable::new({ "colon": Token(colon), "equal": Token(equal) })
    }
  }
}

///|
// TODO: patch the parser
fn @syntax.TypeVarBinder::loc(self : Self) -> Location {
  match self.constraints {
    More(x, ..) => self.name_loc.merge(x.loc)
    Empty => self.name_loc
  }
}

///|
fn Mapper::fill_type_var_binder(
  self : Self,
  binder : @syntax.TypeVarBinder,
) -> NodeTable {
  let loc = binder.loc()
  self.cursor_advance(loc.start)
  self.attach_node(loc)
  self.attach_node(binder.name_loc) //name_loc
  let mut colon = None
  let pluses = []
  for i, constraint in binder.constraints {
    if i == 0 {
      colon = self.cursor_find(TK_COLON, before_pos=loc.end)
    } else {
      pluses.push(self.cursor_find(TK_PLUS, before_pos=loc.end))
    }
    self.attach_node(constraint.loc)
  }
  NodeTable::new({ "colon": Token(colon), "pluses": TokenArray(pluses) })
}

///|
impl IterVisitor for Mapper with fn visit_Binder(self, binder) {
  self.attach_node(binder.loc)
}

///|
impl IterVisitor for Mapper with fn visit_Impl_TopFuncDef(
  s,
  fun_decl~,
  decl_body~,
  where_clause~,
  loc~,
) {
  s.cursor_advance(loc.start)
  s.visit_Visibility(fun_decl.vis)
  if fun_decl.type_name is Some(x) {
    s.visit_TypeName(x)
  }
  let async_ = s.cursor_find(TK_ASYNC, before_token=TK_FN)
  let fn_ = s.cursor_find(TK_FN, before_token=TK_LPAREN)
  let commas0 = []
  let type_var_binders = []
  let (lbracket, rbracket) = if !fun_decl.quantifiers.is_empty() {
    let lb = s.cursor_find(TK_LBRACKET, before_pos=loc.end)
    for tv in fun_decl.quantifiers {
      type_var_binders.push(s.fill_type_var_binder(tv))
      commas0.push(s.cursor_find(TK_COMMA, before_token=TK_RBRACKET))
    }
    let rb = s.cursor_find(TK_RBRACKET, before_pos=loc.end)
    (lb, rb)
  } else {
    (None, None)
  }
  s.visit_Binder(fun_decl.name)
  let parameters = []
  let commas1 = []
  let (lparen, rparen) = if fun_decl.decl_params is Some(ps) {
    let lparen = s.cursor_find(TK_LPAREN, before_pos=loc.end)
    for p in ps {
      parameters.push(s.fill_parameter(p))
      let comma = s.cursor_find(TK_COMMA, before_token=TK_RPAREN)
      commas1.push(comma)
    }
    let rparen = s.cursor_find(TK_RPAREN, before_pos=loc.end)
    (lparen, rparen)
  } else {
    (None, None)
  }
  let thin_arrow = s.cursor_find(TK_THIN_ARROW)
  if fun_decl.return_type is Some(ty) {
    s.visit_Type(ty)
  }
  let (raise_or_noraise, question) = match fun_decl.error_type {
    MaybeError(_) => {
      let a = s.cursor_find(TK_RAISE)
      let b = s.cursor_find(TK_QUESTION)
      (a, b)
    }
    ErrorType(_) | DefaultErrorType(_) => {
      let a = s.cursor_find(TK_RAISE)
      (a, None)
    }
    Noraise(_) => {
      let a = s.cursor_find(TK_NORAISE)
      (a, None)
    }
    NoErrorType => (None, None)
  }
  if where_clause is Some(where_clause) {
    ignore(s.cursor_find(TK_WHERE, before_pos=where_clause.loc.end))
    ignore(s.cursor_find(TK_LBRACE, before_pos=where_clause.loc.end))
    s.visit_WhereClause(where_clause)
    ignore(s.cursor_find(TK_RBRACE, before_pos=where_clause.loc.end))
  }
  let (lbrace, rbrace) = match decl_body {
    DeclBody(expr~) => {
      let lb = s.cursor_find(TK_LBRACE, before_pos=loc.end)
      s.visit_Expr(expr)
      let rb = s.cursor_find(TK_RBRACE, before_pos=loc.end)
      (lb, rb)
    }
    DeclStubs(_) | DeclNone => (None, None)
  }
  s.table[Node(loc)] = NodeTable::new({
    "async": Token(async_),
    "fn": Token(fn_),
    "lbracket": Token(lbracket),
    "commas0": TokenArray(commas0),
    "type_var_binders": TableArray(type_var_binders),
    "rbracket": Token(rbracket),
    "lparen": Token(lparen),
    "parameters": TableArray(parameters),
    "commas1": TokenArray(commas1),
    "rparen": Token(rparen),
    "thin_arrow": Token(thin_arrow),
    "raise_or_noraise": Token(raise_or_noraise),
    "question": Token(question),
    "lbrace": Token(lbrace),
    "rbrace": Token(rbrace),
  })
}
