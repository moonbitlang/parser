///|
priv enum ExprOrBinderFragment {
  Binder(@syntax.Expr, colon_loc~ : Location, ty~ : @syntax.Type)
  Expr(@syntax.Expr)
}

///|
priv struct BlockExpr {
  expr : @syntax.Expr
  lbrace_loc : Location
  rbrace_loc : Location
}

///|
fn State::peek_spos_with_parsed(
  self : Self,
  parsed~ : @syntax.Expr?,
) -> Position {
  match parsed {
    None => self.peek_spos()
    Some(e) => e.loc().start
  }
}

///|
fn State::parse_attributes(self : Self) -> List[Attribute] {
  let result = []
  while self.peek_token() is ATTRIBUTE(content) {
    let loc = self.peek_location()
    result.push(Attribute::new(loc~, content))
    self.skip()
  }
  @list.from_array(result)
}

///|
fn State::parse_toplevel(self : Self) -> Impls {
  guard self.mode is Normal else { @list.new() }
  let result = []
  self.push_sync(TK_EOF)
  while self.mode is Normal && !(self.peek_token() is EOF) {
    self.push_syncs(first_impl_)
    result.push(self.parse_top())
    if self.mode is Normal {
      match self.peek_token() {
        SEMI(_) => self.skip()
        EOF => ()
        other => {
          self.panic() |> ignore
          let loc = self.peek_location()
          let report : Report = if first_impl_.contains(other.kind()) {
            { loc, msg: "Missing newline here." }
          } else {
            { loc, msg: "Unexpected token here." }
          }
          self.report_error(report)
        }
      }
    }
    self.pop_syncs(first_impl_)
  }
  self.pop_sync(TK_EOF)
  @list.from_array(result)
}

///|
fn State::parse_qual_lident(
  self : Self,
  context~ : String,
) -> @syntax.LongIdent {
  if self.mode is Panic(_) {
    return @syntax.LongIdent::dummy()
  }
  match self.peek_token() {
    LIDENT(id) => {
      self.skip()
      Ident(name=id)
    }
    UIDENT(_) => {
      // error recovery
      self.skip()
      @syntax.LongIdent::dummy()
    }
    PACKAGE_NAME(pkg) => {
      let pkg_loc = self.peek_location()
      self.skip()
      let dot_id_loc = self.peek_location()
      let id = self.expect_dot_lident(context~)
      Dot(pkg~, id~, pkg_loc~, dot_id_loc~)
    }
    other => {
      let loc = self.peek_location()
      self.report_unexpected(other, first_qual_lident, loc)
      self.panic() |> ignore
      @syntax.LongIdent::dummy()
    }
  }
}

///|
fn State::parse_qual_luident(self : Self) -> @syntax.LongIdent {
  guard self.mode is Normal else { @syntax.LongIdent::dummy() }
  match self.peek_token() {
    LIDENT(id) | UIDENT(id) => {
      self.skip()
      Ident(name=id)
    }
    PACKAGE_NAME(pkg) => {
      let pkg_loc = self.peek_location()
      self.skip()
      match self.peek_token() {
        DOT_LIDENT(id) | DOT_UIDENT(id) => {
          let dot_id_loc = self.peek_location()
          self.skip()
          Dot(pkg~, id~, pkg_loc~, dot_id_loc~)
        }
        other => {
          let loc = self.peek_location()
          self.report_unexpected(other, [TK_DOT_LIDENT, TK_DOT_UIDENT], loc)
          self.panic() |> ignore
          @syntax.LongIdent::dummy()
        }
      }
    }
    other => {
      let loc = self.peek_location()
      self.report_unexpected(other, first_qual_luident, loc)
      self.panic() |> ignore
      @syntax.LongIdent::dummy()
    }
  }
}

///|
fn State::parse_qual_uident(self : Self) -> @syntax.LongIdent {
  guard self.mode is Normal else { @syntax.LongIdent::dummy() }
  match self.peek_token() {
    UIDENT(id) => {
      self.skip()
      Ident(name=id)
    }
    PACKAGE_NAME(pkg) => {
      let pkg_loc = self.peek_location()
      self.skip()
      let dot_id_loc = self.peek_location()
      let id = self.expect_dot_uident(context="qualified upper identifier")
      Dot(pkg~, id~, pkg_loc~, dot_id_loc~)
    }
    other => {
      let loc = self.peek_location()
      self.report_unexpected(other, [TK_UIDENT, TK_PACKAGE_NAME], loc)
      self.panic() |> ignore
      @syntax.LongIdent::dummy()
    }
  }
}

///|
fn State::parse_luident(self : Self) -> String {
  guard self.mode is Normal else { "" }
  match self.peek_token() {
    LIDENT(id) | UIDENT(id) => {
      self.skip()
      id
    }
    other => {
      let loc = self.peek_location()
      self.report_unexpected(other, first_luident, loc)
      self.panic() |> ignore
      ""
    }
  }
}

///|
fn State::parse_binder(self : Self, context~ : String) -> @syntax.Binder {
  if self.mode is Panic(loc~, ..) {
    return { name: "", loc }
  }
  let loc = self.peek_location()
  let name = self.expect_lident(context~)
  { name, loc }
}

///|
fn State::parse_type_name(self : Self) -> @syntax.TypeName {
  if self.mode is Panic(loc~, ..) {
    return {
      name: @syntax.LongIdent::dummy(),
      is_object: false,
      object_amp_loc: None,
      loc,
    }
  }
  let spos = self.peek_spos()
  let mut object_amp_loc = None
  let is_object = match self.peek_token() {
    AMPER => {
      object_amp_loc = Some(self.peek_location())
      self.skip()
      true
    }
    _ => false
  }
  let name = self.parse_qual_luident()
  let loc = self.loc_start_with(spos)
  { name, is_object, object_amp_loc, loc }
}

///|
fn State::parse_fun_binder(
  self : Self,
) -> (@syntax.TypeName?, @syntax.Binder, Location?) {
  if self.mode is Panic(loc~, ..) {
    return (None, { name: "", loc }, None)
  }
  fn parse_qual_with_coloncolon() {
    let type_name = self.parse_type_name()
    let coloncolon_loc = self.peek_location()
    self.expect_token(TK_COLONCOLON)
    let loc_func_name = self.peek_location()
    let func_name = self.expect_luident(context="name of `fn`")
    (
      Some(type_name),
      @syntax.Binder::{ name: func_name, loc: loc_func_name },
      Some(coloncolon_loc),
    )
  }

  match self.peek_token() {
    PACKAGE_NAME(_) | UIDENT(_) | AMPER => parse_qual_with_coloncolon()
    LIDENT(_) if self.peek_token(nth=1) is COLONCOLON =>
      parse_qual_with_coloncolon()
    LIDENT(_) => {
      let id = self.parse_binder(context="name of `fn`")
      (None, id, None)
    }
    other => {
      let loc = self.panic()
      self.report_unexpected(other, first_qual_lident, loc)
      (None, { name: "", loc }, None)
    }
  }
}

///|
fn State::parse_var(self : Self) -> @syntax.Var {
  if self.mode is Panic(loc~, ..) {
    return { name: @syntax.LongIdent::dummy(), loc }
  }
  let spos = self.peek_spos()
  let name = self.parse_qual_lident(context="")
  let loc = self.loc_start_with(spos)
  { name, loc }
}

///|
fn State::parse_tvar_binder(self : Self) -> @syntax.TypeVarBinder {
  if self.mode is Panic(loc~, ..) {
    return {
      name: "?tvar_name",
      name_loc: loc,
      constraints: @list.from_array([]),
      colon_loc: None,
      plus_locs: @list.empty(),
    }
  }
  let spos = self.peek_spos()
  let name = self.parse_luident()
  let loc = self.loc_start_with(spos)
  let constraint_payload : (
    @list.List[@syntax.TypeVarConstraint],
    @list.List[Location],
    Location,
  )? = self.optional([TK_COLON], fn(state) {
    let colon_loc = state.peek_location()
    state.expect_token(TK_COLON)
    let (constraints, plus_locs, _) = state.series_with_follow_locs(
      delim=TK_PLUS,
      follow_set=[TK_COMMA, TK_RBRACKET],
      fn(state) -> @syntax.TypeVarConstraint {
        let loc = state.peek_location()
        let trait_ = state.parse_qual_luident()
        { trait_, loc }
      },
    )
    (@list.from_array(constraints), @list.from_array(plus_locs), colon_loc)
  })
  let (constraints, plus_locs, colon_loc) = match constraint_payload {
    None => (@list.new(), @list.empty(), None)
    Some((constraints, plus_locs, colon_loc)) =>
      (constraints, plus_locs, Some(colon_loc))
  }
  { name, name_loc: loc, constraints, colon_loc, plus_locs }
}

///|
fn State::parse_type_decl_binder(self : Self) -> @syntax.TypeDeclBinder {
  if self.mode is Panic(loc~, ..) {
    return { name: None, loc }
  }
  let spos = self.peek_spos()
  let name = match self.peek_token() {
    UNDERSCORE => {
      self.skip()
      None
    }
    _ => Some(self.parse_luident())
  }
  let loc = self.loc_start_with(spos)
  { name, loc }
}

///|
fn State::parse_constr(self : Self) -> @syntax.Constructor {
  if self.mode is Panic(loc~, ..) {
    return { name: { name: "", loc }, extra_info: NoExtraInfo, loc }
  }
  let spos = self.peek_spos()
  match self.peek_token() {
    UIDENT(_) if !(self.peek_token(nth=1) is COLONCOLON) => {
      // Constr
      let loc = self.peek_location()
      let name = self.parse_luident()
      let loc_constr = self.loc_start_with(spos)
      { name: { name, loc }, extra_info: NoExtraInfo, loc: loc_constr }
    }
    PACKAGE_NAME(pkg) if self.peek_token(nth=1) is DOT_UIDENT(_) &&
      !(self.peek_token(nth=2) is COLONCOLON) => {
      // @pkg.Constr
      let pkg_loc = self.peek_location()
      self.skip()
      let name : @syntax.ConstrName = {
        let spos = self.peek_spos()
        let name = self.expect_dot_uident(context="")
        { name, loc: self.loc_start_with(spos) }
      }
      let loc = self.loc_start_with(spos)
      {
        name,
        extra_info: Package(pkg~, pkg_loc~, dot_constr_loc=name.loc),
        loc,
      }
    }
    LIDENT(_) | UIDENT(_) | PACKAGE_NAME(_) | AMPER(_) => {
      // @pkg.TypeName::Constr
      // TypeName::@pkg.Constr
      // TypeName::Constr
      // The "&" case is just for error recovery and consistency with yacc parser
      let type_name = self.parse_type_name()
      let coloncolon_loc = self.peek_location()
      self.expect_token(TK_COLONCOLON)
      let (extra_info, right_loc, right) = if self.peek_token()
        is PACKAGE_NAME(pkg) &&
        self.peek_token(nth=1) is DOT_UIDENT(_) {
        let pkg_loc = self.peek_location()
        self.skip()
        let right_loc = self.peek_location()
        let right = self.expect_dot_uident(context="constructor name")
        (
          @syntax.ConstructorExtraInfo::TypeNameWithConstrPackage(
            type_name~,
            pkg~,
            coloncolon_loc~,
            pkg_loc~,
            dot_constr_loc=right_loc,
          ),
          right_loc,
          right,
        )
      } else {
        let right_loc = self.peek_location()
        let right = self.expect_uident(context="constructor name")
        (
          @syntax.ConstructorExtraInfo::TypeName(type_name~, coloncolon_loc~),
          right_loc,
          right,
        )
      }
      let loc = self.loc_start_with(spos)
      { name: { name: right, loc: right_loc }, extra_info, loc }
    }
    other => {
      let loc = self.peek_location()
      self.report_unexpected(other, first_constr, loc)
      self.panic() |> ignore
      { name: { name: "", loc }, extra_info: NoExtraInfo, loc }
    }
  }
}

///|
fn State::parse_opt_type_params_no_constraints_with_locs(
  self : Self,
) -> (List[@syntax.TypeDeclBinder], Location?, Location?, List[Location]) {
  if self.mode is Panic(_) {
    return (@list.new(), None, None, @list.empty())
  }
  if self.peek_token() is LBRACKET {
    let lbracket_loc = self.peek_location()
    self.expect_token(TK_LBRACKET)
    let (params, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RBRACKET],
      State::parse_type_decl_binder,
    )
    let rbracket_loc = self.peek_location()
    self.expect_token(TK_RBRACKET)
    (
      @list.from_array(params),
      Some(lbracket_loc),
      Some(rbracket_loc),
      @list.from_array(comma_locs),
    )
  } else {
    (@list.new(), None, None, @list.empty())
  }
}

///|
fn State::parse_fun_decl(
  self : Self,
  attrs~ : @list.List[@attribute.Attribute],
  declare_loc? : Location? = None,
  extern_loc? : Location? = None,
) -> @syntax.FunDecl {
  if self.mode is Panic(loc~, ..) {
    return {
      name: @syntax.Binder::{ name: "", loc },
      is_async: None,
      declare_loc,
      extern_loc,
      fn_loc: loc,
      coloncolon_loc: None,
      type_name: None,
      decl_params: Some(@list.new()),
      params_loc: loc,
      type_params_lbracket_loc: None,
      type_params_rbracket_loc: None,
      type_param_comma_locs: @list.empty(),
      params_lparen_loc: None,
      params_rparen_loc: None,
      param_comma_locs: @list.empty(),
      quantifiers: @list.new(),
      return_type: None,
      return_arrow_loc: None,
      error_type: NoErrorType,
      vis: Default,
      doc: @syntax.DocString::empty(),
      attrs,
      loc,
    }
  }
  let spos = self.peek_spos()
  let is_async = if self.peek_token() is ASYNC {
    let loc = self.peek_location()
    self.skip()
    Some(loc)
  } else {
    None
  }
  let fn_loc = self.peek_location()
  self.expect_token(TK_FN)
  let (
    quantifiers,
    type_params_lbracket_loc,
    type_params_rbracket_loc,
    type_param_comma_locs,
  ) = self.parse_opt_typ_params_with_locs()
  let (type_name, name, coloncolon_loc) = self.parse_fun_binder()
  let (
    decl_params,
    params_loc,
    params_lparen_loc,
    params_rparen_loc,
    param_comma_locs,
  ) = if self.peek_token() is LPAREN {
    let lparen_loc = self.peek_location()
    self.expect_token(TK_LPAREN)
    self.push_sync(TK_RPAREN)
    let (params, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RPAREN],
      State::parse_parameter,
    )
    self.pop_sync(TK_RPAREN)
    let rparen_loc = self.peek_location()
    self.expect_token(TK_RPAREN)
    (
      Some(@list.from_array(params)),
      lparen_loc.merge(rparen_loc),
      Some(lparen_loc),
      Some(rparen_loc),
      @list.from_array(comma_locs),
    )
  } else {
    (None, self.empty_loc(), None, None, @list.empty())
  }
  let (return_type, error_type, return_arrow_loc) = self.parse_func_return_type_with_loc()
  let loc = self.loc_start_with(spos)
  {
    name,
    is_async,
    declare_loc,
    extern_loc,
    fn_loc,
    coloncolon_loc,
    type_name,
    quantifiers,
    params_loc,
    type_params_lbracket_loc,
    type_params_rbracket_loc,
    type_param_comma_locs,
    params_lparen_loc,
    params_rparen_loc,
    param_comma_locs,
    decl_params,
    return_type,
    return_arrow_loc,
    error_type,
    vis: Default,
    doc: @syntax.DocString::empty(),
    attrs,
    loc,
  }
}

///|
fn State::parse_parameters_with_locs(
  self : Self,
) -> (@syntax.Parameters, Location, Location, @list.List[Location]) {
  let lparen_loc = self.peek_location()
  self.expect_token(TK_LPAREN)
  self.push_sync(TK_RPAREN)
  let (params, comma_locs, _) = self.series_with_follow_locs(
    delim=TK_COMMA,
    follow_set=[TK_RPAREN],
    State::parse_parameter,
  )
  self.pop_sync(TK_RPAREN)
  let rparen_loc = self.peek_location()
  self.expect_token(TK_RPAREN)
  (
    @list.from_array(params),
    lparen_loc,
    rparen_loc,
    @list.from_array(comma_locs),
  )
}

///|
fn State::parse_trait_method_decl(self : Self) -> @syntax.TraitMethodDecl {
  if self.mode is Panic(loc~, ..) {
    return {
      name: { name: "", loc },
      is_async: None,
      fn_loc: None,
      quantifiers: @list.new(),
      type_params_lbracket_loc: None,
      type_params_rbracket_loc: None,
      type_param_comma_locs: @list.empty(),
      params: @list.new(),
      params_lparen_loc: loc,
      params_rparen_loc: loc,
      param_comma_locs: @list.empty(),
      return_type: None,
      return_arrow_loc: None,
      error_type: NoErrorType,
      has_default: None,
      default_equal_loc: None,
      attrs: @list.empty(),
      loc,
    }
  }
  let spos = self.peek_spos()
  let attrs = self.parse_attributes()
  self.push_sync(TK_LPAREN)
  let is_async = if self.peek_token() is ASYNC {
    let loc = self.peek_location()
    self.skip()
    Some(loc)
  } else {
    None
  }
  let (
    quantifiers,
    fn_loc,
    type_params_lbracket_loc,
    type_params_rbracket_loc,
    type_param_comma_locs,
  ) = if self.peek_token() is FN {
    let fn_loc = self.peek_location()
    self.skip()
    let (
      quantifiers,
      type_params_lbracket_loc,
      type_params_rbracket_loc,
      type_param_comma_locs,
    ) = self.parse_opt_typ_params_with_locs()
    (
      quantifiers,
      Some(fn_loc),
      type_params_lbracket_loc,
      type_params_rbracket_loc,
      type_param_comma_locs,
    )
  } else {
    (@list.empty(), None, None, None, @list.empty())
  }
  let func_name = self.parse_binder(context="trait method name")
  self.pop_sync(TK_LPAREN)
  let (params, params_lparen_loc, params_rparen_loc, param_comma_locs) = self.parse_trait_method_params_with_locs()
  let (return_type, error_type, return_arrow_loc) = self.parse_func_return_type_with_loc()
  let (has_default, default_equal_loc) = if self.peek_token() is EQUAL {
    let equal_loc = self.peek_location()
    self.skip()
    let loc = self.peek_location()
    self.expect_token(TK_UNDERSCORE)
    (Some(loc), Some(equal_loc))
  } else {
    (None, None)
  }
  let loc = self.loc_start_with(spos)
  {
    name: func_name,
    is_async,
    fn_loc,
    quantifiers,
    type_params_lbracket_loc,
    type_params_rbracket_loc,
    type_param_comma_locs,
    params,
    params_lparen_loc,
    params_rparen_loc,
    param_comma_locs,
    return_type,
    return_arrow_loc,
    error_type,
    has_default,
    default_equal_loc,
    attrs,
    loc,
  }
}

///|
fn State::parse_trait_decl(
  self : Self,
  vis~ : @syntax.Visibility,
  attrs~ : List[@attribute.Attribute],
  is_declare~ : Bool,
  declare_loc? : Location? = None,
) -> @syntax.TraitDecl {
  if self.mode is Panic(loc~, ..) {
    return {
      name: { name: "", loc },
      supers: @list.new(),
      methods: @list.new(),
      vis: Default,
      declare_loc,
      trait_loc: loc,
      colon_loc: None,
      super_plus_locs: @list.empty(),
      lbrace_loc: loc,
      rbrace_loc: loc,
      semi_locs: @list.empty(),
      loc,
      attrs,
      doc: @syntax.DocString::empty(),
      is_declare,
    }
  }
  let spos = self.peek_spos()
  let trait_loc = self.peek_location()
  self.expect_token(TK_TRAIT)
  let name_loc = self.peek_location()
  let name = self.parse_luident()
  let trait_name : @syntax.Binder = { name, loc: name_loc }
  let (supers, colon_loc, super_plus_locs) = if self.peek_token() is COLON {
    let colon_loc = self.peek_location()
    self.skip()
    let (supers, plus_locs, _) = self.with_follow(follow_set=[TK_LBRACE], fn(
      state,
    ) {
      state.series_with_follow_locs(delim=TK_PLUS, follow_set=[TK_LBRACE], fn(
        state,
      ) {
        let loc = state.peek_location()
        let trait_ = state.parse_qual_luident()
        @syntax.TypeVarConstraint::{ trait_, loc }
      })
    })
    (@list.from_array(supers), Some(colon_loc), @list.from_array(plus_locs))
  } else {
    (@list.new(), None, @list.empty())
  }
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  let (methods, semi_locs, _) = self.series_with_follow_locs(
    delim=TK_SEMI,
    follow_set=[TK_RBRACE],
    State::parse_trait_method_decl,
  )
  let methods = @list.from_array(methods)
  let rbrace_loc = self.peek_location()
  self.expect_token(TK_RBRACE)
  let loc = self.loc_start_with(spos)
  {
    name: trait_name,
    supers,
    methods,
    vis,
    declare_loc,
    trait_loc,
    colon_loc,
    super_plus_locs,
    lbrace_loc,
    rbrace_loc,
    semi_locs: @list.from_array(semi_locs),
    loc,
    attrs,
    doc: @syntax.DocString::empty(),
    is_declare,
  }
}

///|
fn State::parse_visibility(self : Self) -> @syntax.Visibility {
  if self.mode is Panic(..) {
    return Default
  }
  match self.peek_token() {
    PRIV => {
      let loc = self.peek_location()
      self.expect_token(TK_PRIV)
      Priv(loc~)
    }
    PUB => {
      let spos = self.peek_spos()
      self.expect_token(TK_PUB)
      let pub_attr = self.optional([TK_LPAREN], fn(state) {
        state.surround(left=TK_LPAREN, right=TK_RPAREN, fn(state) {
          match state.peek_token() {
            READONLY => {
              state.skip()
              "readonly"
            }
            LIDENT(attr) => {
              state.skip()
              attr
            }
            tok => {
              let loc = state.peek_location()
              state.panic() |> ignore
              state.report_unexpected(tok, [TK_READONLY, TK_LIDENT], loc)
              ""
            }
          }
        })
      })
      let loc = self.loc_start_with(spos)
      Pub(attr=pub_attr, loc~)
    }
    _ => Default
  }
}

///|
fn State::parse_test_block(
  self : Self,
  attrs~ : List[Attribute],
  spos~ : Position,
  is_async~ : Location?,
) -> @syntax.Impl {
  let test_loc = self.peek_location()
  self.expect_token(TK_TEST)
  let name : @syntax.TestName = if self.peek_token() is STRING(_) {
    let name_loc = self.peek_location()
    let v = self.expect_string()
    Some((v, name_loc))
  } else {
    None
  }
  let (params, params_lparen_loc, params_rparen_loc, param_comma_locs) = if self.peek_token()
    is LPAREN {
    let lparen_loc = self.peek_location()
    self.expect_token(TK_LPAREN)
    self.push_sync(TK_RPAREN)
    let (params, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RPAREN],
      State::parse_parameter,
    )
    self.pop_sync(TK_RPAREN)
    let rparen_loc = self.peek_location()
    self.expect_token(TK_RPAREN)
    (
      Some(@list.from_array(params)),
      Some(lparen_loc),
      Some(rparen_loc),
      @list.from_array(comma_locs),
    )
  } else {
    (None, None, None, @list.empty())
  }
  let body = self.parse_block_expr_with_locs()
  let loc = self.loc_start_with(spos)
  TopTest(
    name~,
    params~,
    expr=body.expr,
    is_async~,
    test_loc~,
    params_lparen_loc~,
    params_rparen_loc~,
    param_comma_locs~,
    body_lbrace_loc=body.lbrace_loc,
    body_rbrace_loc=body.rbrace_loc,
    loc~,
    doc=DocString::empty(),
    attrs~,
  )
}

///|
fn State::parse_top_func(
  self : Self,
  attrs~ : List[Attribute],
  vis~ : @syntax.Visibility,
  spos~ : Position,
  is_declare~ : Bool,
  declare_loc? : Location? = None,
) -> @syntax.Impl {
  let fun_decl = self.parse_fun_decl(attrs~, declare_loc~)
  let fun_decl = { ..fun_decl, vis, }
  let mut where_clause : @syntax.WhereClause? = None
  let mut top_equal_loc : Location? = None
  let decl_body : @syntax.DeclBody = if is_declare {
    DeclNone
  } else if self.peek_token() is EQUAL {
    let equal_loc = self.peek_location()
    top_equal_loc = Some(equal_loc)
    self.skip()
    if self.peek_token() is MULTILINE_STRING(_) {
      let xs = []
      let string_locs = []
      let code_start_loc = self.peek_location()
      let mut code_end_loc = code_start_loc
      while self.peek_token() is MULTILINE_STRING(x) {
        let string_loc = self.peek_location()
        string_locs.push(string_loc)
        code_end_loc = string_loc
        xs.push(x)
        self.skip()
      }
      DeclStubs(
        Embedded(
          language=None,
          code=CodeMultilineString(
            @list.from_array(xs),
            string_locs=@list.from_array(string_locs),
          ),
          language_loc=None,
          code_loc=code_start_loc.merge(code_end_loc),
        ),
        equal_loc~,
      )
    } else {
      let name1_loc = self.peek_location()
      let name1 = self.expect_string()
      if self.peek_token() is STRING(name2) {
        let name2_loc = self.peek_location()
        self.skip()
        DeclStubs(
          Import(
            module_name=name1,
            func_name=name2,
            language=None,
            module_name_loc=name1_loc,
            func_name_loc=name2_loc,
            language_loc=None,
          ),
          equal_loc~,
        )
      } else {
        DeclStubs(
          Embedded(
            language=None,
            code=CodeString(name1, loc=name1_loc),
            language_loc=None,
            code_loc=name1_loc,
          ),
          equal_loc~,
        )
      }
    }
  } else {
    where_clause = self.parse_where_clause()
    let body = self.parse_block_expr_with_locs()
    DeclBody(
      expr=body.expr,
      lbrace_loc=Some(body.lbrace_loc),
      rbrace_loc=Some(body.rbrace_loc),
    )
  }
  let loc = self.loc_start_with(spos)
  TopFuncDef(
    fun_decl~,
    decl_body~,
    where_clause~,
    equal_loc=top_equal_loc,
    loc~,
  )
}

///|
fn State::parse_top(self : Self) -> @syntax.Impl {
  if self.mode is Panic(loc~, ..) {
    return @syntax.TopExpr(
      expr=@syntax.Hole(loc~, kind=Synthesized),
      is_main=false,
      is_async=None,
      loc~,
    )
  }
  let spos = self.peek_spos()
  let attrs = self.parse_attributes()
  let (is_declare, declare_loc) = if self.peek_token() is DECLARE {
    let loc = self.peek_location()
    self.skip()
    (true, Some(loc))
  } else {
    (false, None)
  }
  let vis = self.parse_visibility()
  match self.peek_token() {
    SUBERROR => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_SUBERROR)
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let components : @syntax.TypeDesc = match self.peek_token() {
        LBRACE => {
          let lbrace_loc = self.peek_location()
          self.expect_token(TK_LBRACE)
          self.push_sync(TK_RBRACE)
          let (cs, semi_locs, _) = self.series_with_follow_locs(
            invalid_delims=[TK_COMMA],
            delim=TK_SEMI,
            follow_set=[TK_RBRACE],
            State::parse_constr_decl,
          )
          self.pop_sync(TK_RBRACE)
          let rbrace_loc = self.peek_location()
          self.expect_token(TK_RBRACE)
          Error(
            EnumPayload(
              constrs=@list.from_array(cs),
              lbrace_loc~,
              rbrace_loc~,
              semi_locs=@list.from_array(semi_locs),
            ),
          )
        }
        _ => {
          let exception_decl = @syntax.ExceptionDecl::NoPayload
          Error(exception_decl)
        }
      }
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_with_locs()
      let loc = self.loc_start_with(spos)
      TopTypeDef({
        tycon,
        tycon_loc,
        params: @list.new(),
        components,
        type_vis: vis,
        doc: DocString::empty(),
        deriving,
        declare_loc,
        keyword_loc,
        params_lbracket_loc: None,
        params_rbracket_loc: None,
        param_comma_locs: @list.empty(),
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
        loc,
        attrs,
        is_declare,
      })
    }
    TYPE => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_TYPE)
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let (params, params_lbracket_loc, params_rbracket_loc, param_comma_locs) = self.parse_opt_type_params_no_constraints_with_locs()
      let components : @syntax.TypeDesc = match self.peek_token() {
        SEMI(_) | EOF | DERIVE => Abstract
        EQUAL => {
          let equal_loc = self.peek_location()
          self.expect_token(TK_EQUAL)
          let typ = self.parse_type()
          Alias(type_=typ, equal_loc~)
        }
        other => {
          self.report_unexpected(
            other,
            [TK_SEMI, TK_EOF, TK_DERIVE, TK_EQUAL],
            self.peek_location(),
          )
          let loc = self.panic()
          return TopTypeDef({
            tycon,
            tycon_loc,
            params,
            components: Abstract,
            type_vis: vis,
            doc: DocString::empty(),
            deriving: @list.empty(),
            declare_loc,
            keyword_loc,
            params_lbracket_loc,
            params_rbracket_loc,
            param_comma_locs,
            derive_loc: None,
            derive_lparen_loc: None,
            derive_rparen_loc: None,
            derive_comma_locs: @list.empty(),
            loc,
            attrs,
            is_declare,
          })
        }
      }
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_with_locs()
      let loc = self.loc_start_with(spos)
      TopTypeDef({
        tycon,
        tycon_loc,
        params,
        components,
        type_vis: vis,
        doc: DocString::empty(),
        deriving,
        declare_loc,
        keyword_loc,
        params_lbracket_loc,
        params_rbracket_loc,
        param_comma_locs,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
        loc,
        attrs,
        is_declare,
      })
    }
    STRUCT => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_STRUCT)
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let (params, params_lbracket_loc, params_rbracket_loc, param_comma_locs) = self.parse_opt_type_params_no_constraints_with_locs()
      let components : @syntax.TypeDesc = match self.peek_token() {
        LPAREN => {
          // tuple struct
          let lparen_loc = self.peek_location()
          self.skip() // consume '('
          let (ts, comma_locs, _) = self.series1_with_follow_locs(
            delim=TK_COMMA,
            follow_set=[TK_RPAREN],
            State::parse_type,
          )
          let rparen_loc = self.peek_location()
          self.expect_token(TK_RPAREN)
          TupleStruct(
            types=@list.from_array(ts),
            lparen_loc~,
            rparen_loc~,
            comma_locs=@list.from_array(comma_locs),
          )
        }
        _ => self.parse_struct_body(allow_constr=true)
      }
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_with_locs()
      let loc = self.loc_start_with(spos)
      TopTypeDef({
        tycon,
        tycon_loc,
        params,
        components,
        type_vis: vis,
        doc: DocString::empty(),
        deriving,
        declare_loc: None,
        keyword_loc,
        params_lbracket_loc,
        params_rbracket_loc,
        param_comma_locs,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
        loc,
        attrs,
        is_declare,
      })
    }
    ENUM => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_ENUM)
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let (params, params_lbracket_loc, params_rbracket_loc, param_comma_locs) = self.parse_opt_type_params_no_constraints_with_locs()
      let lbrace_loc = self.peek_location()
      self.expect_token(TK_LBRACE)
      let (constrs_array, semi_locs_array, _) = self.series_with_follow_locs(
        invalid_delims=[TK_COMMA],
        delim=TK_SEMI,
        follow_set=[TK_RBRACE],
        State::parse_constr_decl,
      )
      let rbrace_loc = self.peek_location()
      self.expect_token(TK_RBRACE)
      let constrs = @list.from_array(constrs_array)
      let semi_locs = @list.from_array(semi_locs_array)
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_with_locs()
      let loc = self.loc_start_with(spos)
      TopTypeDef({
        tycon,
        tycon_loc,
        params,
        components: Variant(constrs~, lbrace_loc~, rbrace_loc~, semi_locs~),
        type_vis: vis,
        doc: DocString::empty(),
        deriving,
        declare_loc: None,
        keyword_loc,
        params_lbracket_loc,
        params_rbracket_loc,
        param_comma_locs,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
        loc,
        attrs,
        is_declare,
      })
    }
    EXTENUM => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_EXTENUM)
      let target = self.parse_type_name()
      let (tycon, tycon_loc) = match target.name {
        Ident(name~) => (name, target.loc)
        Dot(id~, ..) => (id, target.loc)
      }
      let (params, params_lbracket_loc, params_rbracket_loc, param_comma_locs) = self.parse_opt_type_params_no_constraints_with_locs()
      let components : @syntax.TypeDesc = if self.peek_token() is PLUS_EQUAL {
        let plus_equal_loc = self.peek_location()
        self.expect_token(TK_PLUS_EQUAL)
        let lbrace_loc = self.peek_location()
        self.expect_token(TK_LBRACE)
        let (constructors_array, semi_locs_array, _) = self.series_with_follow_locs(
          invalid_delims=[TK_COMMA],
          delim=TK_SEMI,
          follow_set=[TK_RBRACE],
          State::parse_constr_decl,
        )
        let rbrace_loc = self.peek_location()
        self.expect_token(TK_RBRACE)
        ExtendEnum(
          target~,
          constructors=@list.from_array(constructors_array),
          plus_equal_loc~,
          lbrace_loc~,
          rbrace_loc~,
          semi_locs=@list.from_array(semi_locs_array),
        )
      } else {
        let lbrace_loc = self.peek_location()
        self.expect_token(TK_LBRACE)
        let (constrs_array, semi_locs_array, _) = self.series_with_follow_locs(
          invalid_delims=[TK_COMMA],
          delim=TK_SEMI,
          follow_set=[TK_RBRACE],
          State::parse_constr_decl,
        )
        let rbrace_loc = self.peek_location()
        self.expect_token(TK_RBRACE)
        ExtensibleEnum(
          constrs=@list.from_array(constrs_array),
          lbrace_loc~,
          rbrace_loc~,
          semi_locs=@list.from_array(semi_locs_array),
        )
      }
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_with_locs()
      let loc = self.loc_start_with(spos)
      TopTypeDef({
        tycon,
        tycon_loc,
        params,
        components,
        type_vis: vis,
        doc: DocString::empty(),
        deriving,
        declare_loc,
        keyword_loc,
        params_lbracket_loc,
        params_rbracket_loc,
        param_comma_locs,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
        loc,
        attrs,
        is_declare,
      })
    }
    LET | CONST => {
      let let_or_const_loc = self.peek_location()
      let (is_constant, binder) = match self.peek_token() {
        LET => {
          self.skip()
          (false, self.parse_binder(context="name of `let`"))
        }
        CONST => {
          self.skip()
          let loc = self.peek_location()
          let name = self.expect_uident(context="name of `const`")
          (true, { name, loc })
        }
        _ => panic()
      }
      let (ty, annot_colon_loc) = self.parse_opt_annot_with_loc()
      let equal_loc = self.peek_location()
      self.expect_token(TK_EQUAL)
      let expr = self.parse_expr()
      let loc = self.loc_start_with(spos)
      TopLetDef(
        binder~,
        ty~,
        expr~,
        is_constant~,
        vis~,
        declare_loc~,
        let_or_const_loc~,
        annot_colon_loc~,
        equal_loc~,
        loc~,
        doc=DocString::empty(),
        attrs~,
        is_declare~,
      )
    }
    EXTERN => {
      let extern_loc = self.peek_location()
      self.expect_token(TK_EXTERN)
      let language_loc = self.peek_location()
      let language = self.expect_string()
      let fun_decl = self.parse_fun_decl(attrs~, extern_loc=Some(extern_loc))
      let fun_decl = { ..fun_decl, vis, loc: self.loc_start_with(spos) }
      let equal_loc = self.peek_location()
      self.expect_token(TK_EQUAL)
      if self.peek_token() is MULTILINE_STRING(_) {
        let xs = []
        let string_locs = []
        let code_start_loc = self.peek_location()
        let mut code_end_loc = code_start_loc
        while self.peek_token() is MULTILINE_STRING(x) {
          let string_loc = self.peek_location()
          string_locs.push(string_loc)
          code_end_loc = string_loc
          self.skip()
          xs.push(x)
        }
        let loc = self.loc_start_with(spos)
        TopFuncDef(
          fun_decl~,
          decl_body=DeclStubs(
            Embedded(
              language=Some(language),
              code=CodeMultilineString(
                @list.from_array(xs),
                string_locs=@list.from_array(string_locs),
              ),
              language_loc=Some(language_loc),
              code_loc=code_start_loc.merge(code_end_loc),
            ),
            equal_loc~,
          ),
          where_clause=None,
          equal_loc=Some(equal_loc),
          loc~,
        )
      } else {
        let body_loc = self.peek_location()
        let body = self.expect_string()
        if self.peek_token() is STRING(_) {
          let fname_loc = self.peek_location()
          let fname = self.expect_string()
          let loc = self.loc_start_with(spos)
          TopFuncDef(
            fun_decl~,
            decl_body=DeclStubs(
              Import(
                module_name=body,
                func_name=fname,
                language=Some(language),
                module_name_loc=body_loc,
                func_name_loc=fname_loc,
                language_loc=Some(language_loc),
              ),
              equal_loc~,
            ),
            where_clause=None,
            equal_loc=Some(equal_loc),
            loc~,
          )
        } else {
          let loc = self.loc_start_with(spos)
          TopFuncDef(
            fun_decl~,
            decl_body=DeclStubs(
              Embedded(
                language=Some(language),
                code=CodeString(body, loc=body_loc),
                language_loc=Some(language_loc),
                code_loc=body_loc,
              ),
              equal_loc~,
            ),
            where_clause=None,
            equal_loc=Some(equal_loc),
            loc~,
          )
        }
      }
    }
    ASYNC =>
      match self.peek_token(nth=1) {
        TEST => {
          let is_async = Some(self.peek_location())
          self.skip()
          self.parse_test_block(attrs~, spos~, is_async~)
        }
        _ => self.parse_top_func(attrs~, vis~, spos~, is_declare~, declare_loc~)
      }
    FN => self.parse_top_func(attrs~, vis~, spos~, is_declare~, declare_loc~)
    TRAIT => {
      let trait_decl = self.parse_trait_decl(
        attrs~,
        vis~,
        is_declare~,
        declare_loc~,
      )
      let trait_decl = { ..trait_decl, vis, }
      @syntax.TopTrait(trait_decl)
    }
    TEST => self.parse_test_block(attrs~, spos~, is_async=None)
    IMPL => {
      let impl_loc = self.peek_location()
      self.skip()
      self.push_sync(TK_WITH)
      self.push_sync(TK_FOR)
      let (
        quantifiers,
        type_params_lbracket_loc,
        type_params_rbracket_loc,
        type_param_comma_locs,
      ) = self.parse_opt_typ_params_with_locs()
      let trait_ = self.parse_type_name()
      self.pop_sync(TK_FOR)
      if self.mode is Panic(loc~, ..) {
        self.pop_sync(TK_WITH)
        return TopExpr(
          expr=Hole(loc~, kind=Synthesized),
          is_main=false,
          is_async=None,
          loc~,
        )
      }
      match self.peek_token() {
        FOR => {
          let for_loc = self.peek_location()
          self.skip()
          let self_ty = self.parse_type()
          self.pop_sync(TK_WITH)
          if self.peek_token() is WITH {
            let with_loc = self.peek_location()
            self.skip()
            self.push_sync(TK_LBRACE)
            let fn_loc = if self.peek_token() is FN {
              let loc = self.peek_location()
              self.skip()
              Some(loc)
            } else {
              None
            }
            let method_name = self.parse_binder(context="name of method")
            let (params, params_lparen_loc, params_rparen_loc, param_comma_locs) = self.parse_parameters_with_locs()
            let (ret_ty, err_ty, return_arrow_loc) = self.parse_func_return_type_with_loc()
            self.pop_sync(TK_LBRACE)
            let body : @syntax.DeclBody = if self.peek_token() is EQUAL {
              let equal_loc = self.peek_location()
              self.skip()
              let code_loc = self.peek_location()
              let code = self.expect_string()
              DeclStubs(
                Embedded(
                  language=None,
                  code=CodeString(code, loc=code_loc),
                  language_loc=None,
                  code_loc~,
                ),
                equal_loc~,
              )
            } else {
              let body = self.parse_block_expr_with_locs()
              DeclBody(
                expr=body.expr,
                lbrace_loc=Some(body.lbrace_loc),
                rbrace_loc=Some(body.rbrace_loc),
              )
            }
            TopImpl(
              self_ty=Some(self_ty),
              trait_~,
              method_name~,
              quantifiers~,
              params~,
              ret_ty~,
              err_ty~,
              vis~,
              impl_loc~,
              type_params_lbracket_loc~,
              type_params_rbracket_loc~,
              type_param_comma_locs~,
              for_loc=Some(for_loc),
              with_loc~,
              fn_loc~,
              params_lparen_loc~,
              params_rparen_loc~,
              param_comma_locs~,
              return_arrow_loc~,
              body~,
              loc=self.loc_start_with(spos),
              doc=DocString::empty(),
              attrs~,
            )
          } else {
            TopImplRelation(
              self_ty~,
              trait_~,
              vis~,
              quantifiers~,
              declare_loc~,
              impl_loc~,
              type_params_lbracket_loc~,
              type_params_rbracket_loc~,
              type_param_comma_locs~,
              for_loc~,
              loc=self.loc_start_with(spos),
              attrs~,
              doc=DocString::empty(),
              is_declare~,
            )
          }
        }
        WITH => {
          self.pop_sync(TK_WITH)
          let with_loc = self.peek_location()
          self.skip()
          self.push_sync(TK_LBRACE)
          let fn_loc = if self.peek_token() is FN {
            let loc = self.peek_location()
            self.skip()
            Some(loc)
          } else {
            None
          }
          let method_name = self.parse_binder(context="name of method")
          let (params, params_lparen_loc, params_rparen_loc, param_comma_locs) = self.parse_parameters_with_locs()
          let (ret_ty, err_ty, return_arrow_loc) = self.parse_func_return_type_with_loc()
          self.pop_sync(TK_LBRACE)
          let body : @syntax.DeclBody = if self.peek_token() is EQUAL {
            let equal_loc = self.peek_location()
            self.skip()
            let code_loc = self.peek_location()
            let code = self.expect_string()
            DeclStubs(
              Embedded(
                language=None,
                code=CodeString(code, loc=code_loc),
                language_loc=None,
                code_loc~,
              ),
              equal_loc~,
            )
          } else {
            let body = self.parse_block_expr_with_locs()
            DeclBody(
              expr=body.expr,
              lbrace_loc=Some(body.lbrace_loc),
              rbrace_loc=Some(body.rbrace_loc),
            )
          }
          TopImpl(
            self_ty=None,
            trait_~,
            method_name~,
            quantifiers~,
            params~,
            ret_ty~,
            err_ty~,
            vis~,
            body~,
            impl_loc~,
            type_params_lbracket_loc~,
            type_params_rbracket_loc~,
            type_param_comma_locs~,
            for_loc=None,
            with_loc~,
            fn_loc~,
            params_lparen_loc~,
            params_rparen_loc~,
            param_comma_locs~,
            return_arrow_loc~,
            loc=self.loc_start_with(spos),
            doc=DocString::empty(),
            attrs~,
          )
        }
        other => {
          self.pop_sync(TK_WITH)
          let other_loc = self.peek_location()
          self.report_unexpected(other, [TK_FOR, TK_WITH], other_loc)
          self.panic() |> ignore
          let loc = self.loc_start_with(spos)
          TopImpl(
            self_ty=None,
            trait_~,
            method_name={ name: "", loc: @syntax.no_location },
            quantifiers=@list.new(),
            params=@list.new(),
            ret_ty=None,
            err_ty=NoErrorType,
            vis~,
            body=DeclBody(
              expr=Hole(loc=@syntax.no_location, kind=Synthesized),
              lbrace_loc=None,
              rbrace_loc=None,
            ),
            impl_loc~,
            type_params_lbracket_loc~,
            type_params_rbracket_loc~,
            type_param_comma_locs~,
            for_loc=None,
            with_loc=@syntax.no_location,
            fn_loc=None,
            params_lparen_loc=@syntax.no_location,
            params_rparen_loc=@syntax.no_location,
            param_comma_locs=@list.empty(),
            return_arrow_loc=None,
            loc~,
            doc=DocString::empty(),
            attrs~,
          )
        }
      }
    }
    ENUMVIEW => {
      let enumview_loc = self.peek_location()
      self.expect_token(TK_ENUMVIEW)
      let (
        quantifiers,
        type_params_lbracket_loc,
        type_params_rbracket_loc,
        type_param_comma_locs,
      ) = self.parse_opt_typ_params_with_locs()
      let view_type_loc = self.peek_location()
      let view_type_name = self.expect_uident(context="view type name")
      let lbrace_loc = self.peek_location()
      self.expect_token(TK_LBRACE)
      let (view_constrs_array, constructor_semi_locs_array, _) = self.series_with_follow_locs(
        invalid_delims=[TK_COMMA],
        delim=TK_SEMI,
        follow_set=[TK_RBRACE],
        State::parse_constr_decl,
      )
      let rbrace_loc = self.peek_location()
      self.expect_token(TK_RBRACE)
      let view_constrs = @list.from_array(view_constrs_array)
      let constructor_semi_locs = @list.from_array(constructor_semi_locs_array)
      let for_loc = self.peek_location()
      self.expect_token(TK_FOR)
      let source_ty = self.parse_type()
      let with_loc = self.peek_location()
      self.expect_token(TK_WITH)
      let view_func_name = self.parse_binder(context="name of view function")
      let params_spos = self.peek_spos()
      let (parameters, params_lparen_loc, params_rparen_loc, param_comma_locs) = self.parse_parameters_with_locs()
      let params_loc = self.loc_start_with(params_spos)
      let body = self.parse_block_expr_with_locs()
      let loc = self.loc_start_with(spos)
      TopView(
        quantifiers~,
        source_ty~,
        view_type_name~,
        view_type_loc~,
        view_func_name~,
        parameters~,
        params_loc~,
        view_constrs~,
        body=body.expr,
        vis~,
        enumview_loc~,
        type_params_lbracket_loc~,
        type_params_rbracket_loc~,
        type_param_comma_locs~,
        lbrace_loc~,
        rbrace_loc~,
        constructor_semi_locs~,
        for_loc~,
        with_loc~,
        params_lparen_loc~,
        params_rparen_loc~,
        param_comma_locs~,
        body_lbrace_loc=body.lbrace_loc,
        body_rbrace_loc=body.rbrace_loc,
        loc~,
        attrs~,
        doc=DocString::empty(),
      )
    }
    USING if !is_declare => {
      let using_loc = self.peek_location()
      self.skip()
      let pkg : @syntax.Label = match self.peek_token() {
        PACKAGE_NAME(name) => {
          let loc = self.peek_location()
          self.skip()
          { name, loc }
        }
        otherwise => {
          let loc = self.peek_location()
          self.report_unexpected(otherwise, [TK_PACKAGE_NAME], loc)
          self.panic() |> ignore
          { name: "", loc }
        }
      }
      fn parse_alias_target(
        state : State,
        lower : Bool,
        context : String,
      ) -> @syntax.AliasTarget {
        let target_loc = state.peek_location()
        let target = if lower {
          state.expect_lident(context~)
        } else {
          state.expect_uident(context~)
        }
        match self.peek_token() {
          AS => {
            let as_loc = state.peek_location()
            state.skip()
            let binder_loc = state.peek_location()
            let binder_name = if lower {
              state.expect_lident(context~)
            } else {
              state.expect_uident(context~)
            }
            @syntax.AliasTarget::{
              binder: { name: binder_name, loc: binder_loc },
              target: Some({ name: target, loc: target_loc }),
              as_loc: Some(as_loc),
            }
          }
          _ =>
            @syntax.AliasTarget::{
              binder: { name: target, loc: target_loc },
              target: None,
              as_loc: None,
            }
        }
      }

      let lbrace_loc = self.peek_location()
      self.expect_token(TK_LBRACE)
      self.push_sync(TK_RBRACE)
      let (names_array, comma_locs_array, _) = self.series_with_follow_locs(
        delim=TK_COMMA,
        follow_set=[TK_RBRACE],
        fn(state) {
          match state.peek_token() {
            LIDENT(_) => {
              let target = parse_alias_target(state, true, "value name")
              (target, @syntax.UsingKind::Value)
            }
            UIDENT(_) => {
              let target = parse_alias_target(state, false, "constant name")
              (target, @syntax.UsingKind::Value)
            }
            TYPE => {
              let type_loc = state.peek_location()
              state.skip()
              let target = parse_alias_target(state, false, "type name")
              (target, @syntax.UsingKind::Type(loc=type_loc))
            }
            TRAIT => {
              let trait_loc = state.peek_location()
              state.skip()
              let target = parse_alias_target(state, false, "trait name")
              (target, @syntax.UsingKind::Trait(loc=trait_loc))
            }
            other => {
              let other_loc = state.peek_location()
              state.report_unexpected(
                other,
                [TK_LIDENT, TK_UIDENT, TK_TYPE, TK_TRAIT],
                other_loc,
              )
              let loc = state.panic()
              (
                { binder: { name: "", loc }, target: None, as_loc: None },
                @syntax.UsingKind::Value,
              )
            }
          }
        },
      )
      self.pop_sync(TK_RBRACE)
      let rbrace_loc = self.peek_location()
      self.expect_token(TK_RBRACE)
      let names = @list.from_array(names_array)
      let comma_locs = @list.from_array(comma_locs_array)
      let loc = self.loc_start_with(spos)
      @syntax.Impl::TopUsing(
        pkg~,
        names~,
        vis~,
        attrs~,
        using_loc~,
        lbrace_loc~,
        rbrace_loc~,
        comma_locs~,
        loc~,
        doc=DocString::empty(),
      )
    }
    other => {
      let other_loc = self.peek_location()
      self.report_unexpected(other, first_impl_, other_loc)
      let loc = self.panic()
      TopExpr(
        expr=Hole(loc~, kind=Synthesized),
        is_main=false,
        is_async=None,
        loc~,
      )
    }
  }
}

///|
fn State::parse_struct_body(
  self : State,
  allow_constr~ : Bool,
) -> @syntax.TypeDesc {
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  let (foc_array, semi_locs_array, _) = self.series_with_follow_locs(
    invalid_delims=[TK_COMMA],
    delim=TK_SEMI,
    follow_set=[TK_RBRACE],
    State::parse_field_decl_or_constr_decl,
  )
  let rbrace_loc = self.peek_location()
  self.expect_token(TK_RBRACE)
  let mut fields = @list.empty()
  fn check_fields(field_or_constr_list : ArrayView[FieldOrConstr]) -> Unit {
    match field_or_constr_list {
      [] => ()
      [.. rest, Constr(constr_decl)] => {
        self.report_error({
          loc: constr_decl.loc,
          msg: "Constructor function declaration can only occur at the end of `struct` declaration.",
        })
        check_fields(rest)
      }
      [.. rest, Field(field)] => {
        fields = fields.add(field)
        check_fields(rest)
      }
    }
  }
  let constr_decl = if foc_array is [.. fields, Constr(constr_decl)] {
    check_fields(fields)
    if allow_constr {
      Some(constr_decl)
    } else {
      self.report_error({
        loc: constr_decl.loc,
        msg: "Constructor function declaration is not allowed in local type declaration.",
      })
      None
    }
  } else {
    check_fields(foc_array)
    None
  }
  Record(
    fields~,
    constr_decl~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs=@list.from_array(semi_locs_array),
  )
}

///|
fn State::parse_deriving_directive_list_with_locs(
  self : Self,
) -> (
  List[@syntax.DerivingDirective],
  Location?,
  Location?,
  Location?,
  List[Location],
) {
  if self.mode is Panic(_) {
    return (@list.new(), None, None, None, @list.empty())
  }
  if self.peek_token() is DERIVE {
    let derive_loc = self.peek_location()
    self.skip()
    let lparen_loc = self.peek_location()
    self.expect_token(TK_LPAREN)
    let (directives, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RPAREN],
      State::parse_deriving_directive,
    )
    let rparen_loc = self.peek_location()
    self.expect_token(TK_RPAREN)
    (
      @list.from_array(directives),
      Some(derive_loc),
      Some(lparen_loc),
      Some(rparen_loc),
      @list.from_array(comma_locs),
    )
  } else {
    (@list.new(), None, None, None, @list.empty())
  }
}

///|
fn State::parse_deriving_directive(self : Self) -> @syntax.DerivingDirective {
  if self.mode is Panic(loc~, ..) {
    return {
      type_name: {
        name: @syntax.Ident(name=""),
        is_object: false,
        object_amp_loc: None,
        loc,
      },
      args: @list.new(),
      args_lparen_loc: None,
      args_rparen_loc: None,
      arg_comma_locs: @list.empty(),
      loc,
    }
  }
  let spos = self.peek_spos()
  let type_name = self.parse_type_name()
  let (args, args_lparen_loc, args_rparen_loc, arg_comma_locs) = if self.peek_token()
    is LPAREN {
    let lparen_loc = self.peek_location()
    self.expect_token(TK_LPAREN)
    let (args, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RPAREN],
      State::parse_argument,
    )
    let rparen_loc = self.peek_location()
    self.expect_token(TK_RPAREN)
    (
      @list.from_array(args),
      Some(lparen_loc),
      Some(rparen_loc),
      @list.from_array(comma_locs),
    )
  } else {
    (@list.new(), None, None, @list.empty())
  }
  let loc = self.loc_start_with(spos)
  { type_name, args, args_lparen_loc, args_rparen_loc, arg_comma_locs, loc }
}

///|
priv enum FieldOrConstr {
  Field(@syntax.FieldDecl)
  Constr(@syntax.FunDecl)
}

///|
fn State::parse_trait_method_params_with_locs(
  self : State,
) -> (@syntax.Parameters, Location, Location, @list.List[Location]) {
  let lparen_loc = self.peek_location()
  self.expect_token(TK_LPAREN)
  self.push_sync(TK_RPAREN)
  let (params, comma_locs, _) = self.series_with_follow_locs(
    delim=TK_COMMA,
    follow_set=[TK_RPAREN],
    fn(state) -> @syntax.Parameter {
      match state.peek_token() {
        // `label~ : Type`
        POST_LABEL(binder_name) => {
          let post_label_loc = state.peek_location()
          let binder_loc = Location::trim_last_char(post_label_loc)
          state.skip()
          let binder : @syntax.Binder = { name: binder_name, loc: binder_loc }
          let colon_loc = state.peek_location()
          state.expect_token(TK_COLON)
          let typ = state.parse_type()
          Labelled(
            binder~,
            ty=Some(typ),
            post_label_loc=Some(post_label_loc),
            colon_loc=Some(colon_loc),
          )
        }
        LIDENT(_) => {
          let binder_loc = state.peek_location()
          let binder_name = state.expect_lident(context="parameter name")
          let binder : @syntax.Binder = { name: binder_name, loc: binder_loc }
          let has_question = state.peek_token() is QUESTION
          let question_loc = if has_question {
            Some(state.peek_location())
          } else {
            None
          }
          if has_question {
            state.skip()
          }
          let colon_loc = state.peek_location()
          state.expect_token(TK_COLON)
          let typ = state.parse_type()
          match question_loc {
            Some(question_loc) =>
              QuestionOptional(
                binder~,
                ty=Some(typ),
                question_loc~,
                colon_loc=Some(colon_loc),
              )
            None => Positional(binder~, ty=Some(typ), colon_loc=Some(colon_loc))
          }
        }
        tok if first_type_.contains(tok.kind()) => {
          let typ = state.parse_type()
          let loc = typ.loc()
          DiscardPositional(ty=Some(typ), colon_loc=None, loc~)
        }
        other => {
          let other_loc = state.peek_location()
          state.report_unexpected(
            other,
            [TK_POST_LABEL, ..first_type_],
            other_loc,
          )
          let loc = state.panic()
          DiscardPositional(ty=None, colon_loc=None, loc~)
        }
      }
    },
  )
  self.pop_sync(TK_RPAREN)
  let rparen_loc = self.peek_location()
  self.expect_token(TK_RPAREN)
  (
    params |> @list.from_array,
    lparen_loc,
    rparen_loc,
    comma_locs |> @list.from_array,
  )
}

///|
fn State::parse_field_decl_or_constr_decl(self : Self) -> FieldOrConstr {
  if self.mode is Panic(loc~, ..) {
    return Field({
      name: { label: "?field_name", loc },
      ty: Any(loc~),
      mut_: false,
      mut_loc: None,
      colon_loc: loc,
      vis: Default,
      doc: DocString::empty(),
      attrs: @list.empty(),
      loc,
    })
  }
  let spos = self.peek_spos()
  let attrs = self.parse_attributes()
  match self.peek_token() {
    ASYNC | FN => {
      let is_async = if self.peek_token() is ASYNC {
        let loc = self.peek_location()
        self.skip()
        Some(loc)
      } else {
        None
      }
      let fn_loc = self.peek_location()
      self.expect_token(TK_FN)
      let (
        quantifiers,
        type_params_lbracket_loc,
        type_params_rbracket_loc,
        type_param_comma_locs,
      ) = self.parse_opt_typ_params_with_locs()
      let name = self.parse_binder(context="name of `fn`")
      let (params, params_lparen_loc, params_rparen_loc, param_comma_locs) = self.parse_trait_method_params_with_locs()
      let params_loc = params_lparen_loc.merge(params_rparen_loc)
      let (return_type, error_type, return_arrow_loc) = self.parse_func_return_type_with_loc()
      Constr({
        name,
        is_async,
        declare_loc: None,
        extern_loc: None,
        fn_loc,
        coloncolon_loc: None,
        type_name: None,
        quantifiers,
        params_loc,
        type_params_lbracket_loc,
        type_params_rbracket_loc,
        type_param_comma_locs,
        params_lparen_loc: Some(params_lparen_loc),
        params_rparen_loc: Some(params_rparen_loc),
        param_comma_locs,
        decl_params: Some(params),
        return_type,
        return_arrow_loc,
        error_type,
        vis: Default,
        doc: DocString::empty(),
        attrs,
        loc: self.loc_start_with(spos),
      })
    }
    _ => {
      let vis = self.parse_visibility()
      let (mut_, mut_loc) = if self.peek_token() is MUTABLE {
        let mut_loc = self.peek_location()
        self.expect_token(TK_MUTABLE)
        (true, Some(mut_loc))
      } else {
        (false, None)
      }
      let name_loc = self.peek_location()
      let name = self.expect_lident(context="struct field name")
      let colon_loc = self.peek_location()
      self.expect_token(TK_COLON)
      let ty = self.parse_type()
      let loc = self.loc_start_with(spos)
      Field({
        name: { label: name, loc: name_loc },
        ty,
        mut_,
        mut_loc,
        colon_loc,
        vis,
        loc,
        attrs,
        doc: DocString::empty(),
      })
    }
  }
}

///|
fn State::parse_constr_decl(self : Self) -> @syntax.ConstrDecl {
  if self.mode is Panic(loc~, ..) {
    return {
      name: { name: "?constr-name", loc },
      args: None,
      args_lparen_loc: None,
      args_rparen_loc: None,
      arg_comma_locs: @list.empty(),
      tag: None,
      tag_equal_loc: None,
      loc,
      attrs: @list.empty(),
      doc: @syntax.DocString::empty(),
    }
  }
  let spos = self.peek_spos()
  let attrs = self.parse_attributes()
  if self.peek_token() is (PRIV | PUB | READONLY) {
    let loc = self.peek_location()
    self.skip()
    self.report_error({
      loc,
      msg: "No individual visibility for enum constructor.",
    })
  }
  let constr_name_loc = self.peek_location()
  let name = self.expect_uident(context="constructor name")
  let constr_name : @syntax.ConstrName = { name, loc: constr_name_loc }
  let (args, args_lparen_loc, args_rparen_loc, arg_comma_locs) = if self.peek_token()
    is LPAREN {
    let args_lparen_loc = self.peek_location()
    self.expect_token(TK_LPAREN)
    self.push_sync(TK_RPAREN)
    let (xs, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RPAREN],
      fn(state) -> @syntax.ConstrParam {
        let (mut_, mut_loc) = if state.peek_token() is MUTABLE {
          let mut_loc = state.peek_location()
          state.skip()
          (true, Some(mut_loc))
        } else {
          (false, None)
        }
        // `label~ : Type`
        if state.peek_token() is POST_LABEL(name) {
          let post_label_loc = state.peek_location()
          let label_loc = Location::trim_last_char(post_label_loc)
          let label : @syntax.Label = { name, loc: label_loc }
          state.skip()
          let colon_loc = state.peek_location()
          state.expect_token(TK_COLON)
          let ty = state.parse_type()
          {
            ty,
            mut_,
            label: Some(label),
            mut_loc,
            post_label_loc: Some(post_label_loc),
            colon_loc: Some(colon_loc),
          }
        } else {
          let ty = state.parse_type()
          {
            ty,
            mut_,
            label: None,
            mut_loc,
            post_label_loc: None,
            colon_loc: None,
          }
        }
      },
    )
    self.pop_sync(TK_RPAREN)
    let args_rparen_loc = self.peek_location()
    self.expect_token(TK_RPAREN)
    (
      Some(@list.from_array(xs)),
      Some(args_lparen_loc),
      Some(args_rparen_loc),
      @list.from_array(comma_locs),
    )
  } else {
    (None, None, None, @list.empty())
  }
  let (tag, tag_equal_loc) = if self.peek_token() is EQUAL {
    let tag_equal_loc = self.peek_location()
    self.skip()
    let tag_loc = self.peek_location()
    let tag = self.expect_int()
    (Some((tag, tag_loc)), Some(tag_equal_loc))
  } else {
    (None, None)
  }
  let loc = self.loc_start_with(spos)
  {
    name: constr_name,
    args,
    args_lparen_loc,
    args_rparen_loc,
    arg_comma_locs,
    tag,
    tag_equal_loc,
    loc,
    attrs,
    doc: DocString::empty(),
  }
}

///|
fn State::parse_pattern(self : Self) -> @syntax.Pattern {
  if self.mode is Panic(loc~, ..) {
    return Any(loc~)
  }
  let spos = self.peek_spos()
  let mut pat = self.parse_or_pattern()
  while self.peek_token() is AS {
    let as_loc = self.peek_location()
    self.skip()
    let alias_ = self.parse_binder(context="variable name")
    let loc = self.loc_start_with(spos)
    pat = Alias(pat~, alias_~, as_loc~, loc~)
  } nobreak {
    pat
  }
}

///|
fn State::parse_or_pattern(self : Self) -> @syntax.Pattern {
  if self.mode is Panic(loc~, ..) {
    return Any(loc~)
  }
  let spos = self.peek_spos()
  let pat = self.parse_range_pattern()
  // TODO: use stack-safe loop
  if self.peek_token() is BAR {
    let bar_loc = self.peek_location()
    self.skip()
    let pat2 = self.parse_or_pattern()
    let loc = self.loc_start_with(spos)
    Or(pat1=pat, pat2~, bar_loc~, loc~)
  } else {
    pat
  }
}

///|
fn State::parse_range_pattern(self : Self) -> @syntax.Pattern {
  if self.mode is Panic(loc~, ..) {
    return Any(loc~)
  }
  let spos = self.peek_spos()
  let lhs = self.parse_simple_pattern()
  match self.peek_token() {
    RANGE_INCLUSIVE => {
      let range_op_loc = self.peek_location()
      self.skip()
      let rhs = self.parse_simple_pattern()
      let loc = self.loc_start_with(spos)
      Range(lhs~, rhs~, kind=Inclusive, range_op_loc~, loc~)
    }
    RANGE_EXCLUSIVE => {
      let range_op_loc = self.peek_location()
      self.skip()
      let rhs = self.parse_simple_pattern()
      let loc = self.loc_start_with(spos)
      Range(lhs~, rhs~, kind=Exclusive, range_op_loc~, loc~)
    }
    _ => lhs
  }
}

///|
fn State::parse_constr_pattern(self : Self) -> @syntax.Pattern {
  let spos = self.peek_spos()
  fn parse_constr_args() {
    let lparen_loc = self.peek_location()
    self.expect_token(TK_LPAREN)
    self.push_sync(TK_RPAREN)
    let (args, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RPAREN],
      fn(_) {
        if self.peek_token() is DOTDOT {
          let loc = self.peek_location()
          self.skip()
          ConstrArgRelatedness::Dotdot(loc)
        } else {
          let args = self.parse_constr_pat_argument()
          ConstrArgRelatedness::Arg(args)
        }
      },
    )
    self.pop_sync(TK_RPAREN)
    let rparen_loc = self.peek_location()
    self.expect_token(TK_RPAREN)
    (
      @list.from_array(args),
      lparen_loc,
      rparen_loc,
      @list.from_array(comma_locs),
    )
  }

  fn parse_constr_args_no_open() {
    let lparen_loc = self.peek_location()
    self.expect_token(TK_LPAREN)
    self.push_sync(TK_RPAREN)
    let (args, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RPAREN],
      _ => self.parse_constr_pat_argument(),
    )
    self.pop_sync(TK_RPAREN)
    let rparen_loc = self.peek_location()
    self.expect_token(TK_RPAREN)
    (
      @list.from_array(args),
      lparen_loc,
      rparen_loc,
      @list.from_array(comma_locs),
    )
  }

  match self.peek_token() {
    // bitstring constr pattern `i32(args)`
    // To disambiguate with pattern variable, the args list must be present.
    LIDENT(name) if self.peek_token(nth=1) is LPAREN => {
      let name_loc = self.peek_location()
      self.skip() // skip lident
      let binder : @syntax.Binder = { name, loc: name_loc }
      let (args, lparen_loc, rparen_loc, comma_locs) = parse_constr_args_no_open()
      let loc = self.loc_start_with(spos)
      @syntax.Pattern::SpecialConstr(
        binder~,
        args~,
        lparen_loc~,
        rparen_loc~,
        comma_locs~,
        loc~,
      )
    }
    PACKAGE_NAME(_) | UIDENT(_) | LIDENT(_) => {
      let constr = self.parse_constr()
      let maybe_args = self.optional([TK_LPAREN], _ => parse_constr_args())
      fn check_args(
        args : List[ConstrArgRelatedness],
      ) -> (List[@syntax.ConstrPatArg], Bool, Location?) {
        match args {
          Empty => (@list.empty(), false, None)
          More(Dotdot(loc), tail=Empty) => (@list.empty(), true, Some(loc))
          More(Dotdot(loc), tail~) => {
            self.report_error({
              loc,
              msg: "Unexpected `..` here, add `, ..` behind the last argument to ignore the rest arguments.",
            })
            let (args, _, _) = check_args(tail)
            (args, true, Some(loc))
          }
          More(Arg(arg), tail~) => {
            let (args, is_open, open_dotdot_loc) = check_args(tail)
            (args.add(arg), is_open, open_dotdot_loc)
          }
        }
      }

      let (
        args,
        is_open,
        args_lparen_loc,
        args_rparen_loc,
        arg_comma_locs,
        open_dotdot_loc,
      ) = match maybe_args {
        None => (None, false, None, None, @list.empty(), None)
        Some((raw_args, lparen_loc, rparen_loc, arg_comma_locs)) => {
          let (args, is_open, open_dotdot_loc) = check_args(raw_args)
          (
            Some(args),
            is_open,
            Some(lparen_loc),
            Some(rparen_loc),
            arg_comma_locs,
            open_dotdot_loc,
          )
        }
      }
      let loc = self.loc_start_with(spos)
      Constr(
        constr~,
        args~,
        is_open~,
        args_lparen_loc~,
        args_rparen_loc~,
        arg_comma_locs~,
        open_dotdot_loc~,
        loc~,
      )
    }
    _ => panic()
  }
}

///|
fn State::parse_simple_pattern(self : Self) -> @syntax.Pattern {
  if self.mode is Panic(loc~, ..) {
    return Any(loc~)
  }
  match self.peek_token() {
    TRUE => {
      let loc = self.peek_location()
      self.expect_token(TK_TRUE)
      Constant(c=Bool(true), loc~)
    }
    FALSE => {
      let loc = self.peek_location()
      self.expect_token(TK_FALSE)
      Constant(c=Bool(false), loc~)
    }
    CHAR(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_CHAR)
      Constant(c=Char(x), loc~)
    }
    INT(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_INT)
      Constant(c=@syntax.make_int(x), loc~)
    }
    BYTE(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_BYTE)
      Constant(c=Byte(x), loc~)
    }
    FLOAT(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_FLOAT)
      Constant(c=@syntax.make_float(x), loc~)
    }
    DOUBLE(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_DOUBLE)
      Constant(c=@syntax.make_double(x), loc~)
    }
    MINUS => {
      self.expect_token(TK_MINUS)
      match self.peek_token() {
        INT(x) => {
          let loc = self.peek_location()
          self.expect_token(TK_INT)
          Constant(c=@syntax.make_int("-" + x), loc~)
        }
        FLOAT(x) => {
          let loc = self.peek_location()
          self.expect_token(TK_FLOAT)
          Constant(c=@syntax.make_float("-" + x), loc~)
        }
        DOUBLE(x) => {
          let loc = self.peek_location()
          self.expect_token(TK_DOUBLE)
          Constant(c=@syntax.make_double("-" + x), loc~)
        }
        other => {
          let other_loc = self.peek_location()
          self.report_unexpected(
            other,
            [TK_INT, TK_FLOAT, TK_DOUBLE],
            other_loc,
          )
          let loc = self.panic()
          Any(loc~)
        }
      }
    }
    BYTES(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_BYTES)
      Constant(c=Bytes(x), loc~)
    }
    STRING(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_STRING)
      Constant(c=String(x), loc~)
    }
    UNDERSCORE => {
      let loc = self.peek_location()
      self.expect_token(TK_UNDERSCORE)
      Any(loc~)
    }
    LIDENT(_) if !(self.peek_token(nth=1) is (COLONCOLON | LPAREN)) => {
      let binder = self.parse_binder(context="")
      Var(binder)
    }
    PACKAGE_NAME(_) | UIDENT(_) | LIDENT(_) => self.parse_constr_pattern()
    LPAREN => {
      let lparen_loc = self.peek_location()
      let result = self.surround(left=TK_LPAREN, right=TK_RPAREN, fn(state) {
        let first = state.parse_pattern()
        match state.peek_token() {
          COMMA => {
            let first_comma_loc = state.peek_location()
            state.skip()
            let (pats, rest_comma_locs, _) = state.series_with_follow_locs(
              delim=TK_COMMA,
              follow_set=[TK_RPAREN],
              State::parse_pattern,
            )
            let rparen_loc = state.peek_location()
            if pats is [] {
              first
            } else {
              let comma_locs = @list.from_array(
                [first_comma_loc, ..rest_comma_locs],
              )
              Tuple(
                pats=@list.from_array([first, ..pats]),
                lparen_loc~,
                rparen_loc~,
                comma_locs~,
                loc=lparen_loc.merge(rparen_loc),
              )
            }
          }
          COLON => {
            let colon_loc = state.peek_location()
            state.skip()
            let ty = state.parse_type()
            let rparen_loc = state.peek_location()
            @syntax.Pattern::Constraint(
              pat=first,
              ty~,
              lparen_loc=Some(lparen_loc),
              colon_loc~,
              rparen_loc=Some(rparen_loc),
              loc=lparen_loc.merge(rparen_loc),
            )
          }
          RPAREN => first
          other => {
            let other_loc = state.peek_location()
            state.report_unexpected(
              other,
              [TK_COMMA, TK_COLON, TK_RPAREN],
              other_loc,
            )
            state.panic() |> ignore
            first
          }
        }
      })
      result
    }
    LBRACKET => {
      let spos = self.peek_spos()
      let lbracket_loc = self.peek_location()
      self.expect_token(TK_LBRACKET)
      self.push_sync(TK_RBRACKET)
      let pats1 = []
      let pats2 = []
      let mut dotdot = NoDotDot
      fn push_pat(pat : @syntax.ArrayPattern) {
        if dotdot is NoDotDot {
          pats1.push(pat)
        } else {
          pats2.push(pat)
        }
      }

      let (_, comma_locs_array, _) = self.series_with_follow_locs(
        delim=TK_COMMA,
        follow_set=[TK_RBRACKET],
        fn(self) {
          if self.peek_token() is DOTDOT {
            let pat_spos = self.peek_spos()
            let dotdot_loc = self.peek_location()
            self.skip()
            match self.peek_token() {
              STRING(str) => {
                let loc = self.peek_location()
                self.skip()
                push_pat(StringSpread(str~, dotdot_loc~, loc~))
              }
              BYTES(bytes) => {
                let loc = self.peek_location()
                self.skip()
                push_pat(BytesSpread(bytes~, dotdot_loc~, loc~))
              }
              UIDENT(name) => {
                let id_loc = self.peek_location()
                self.skip()
                let binder : @syntax.Binder = { name, loc: id_loc }
                let loc = self.loc_start_with(pat_spos)
                push_pat(
                  ConstSpread(
                    binder~,
                    pkg=None,
                    dotdot_loc~,
                    pkg_loc=None,
                    dot_uident_loc=None,
                    loc~,
                  ),
                )
              }
              PACKAGE_NAME(pkg) => {
                let pkg_loc = self.peek_location()
                self.skip()
                let dot_uident_loc = self.peek_location()
                let name = self.expect_dot_uident(context="name of constant")
                let binder : @syntax.Binder = { name, loc: dot_uident_loc }
                let loc = self.loc_start_with(pat_spos)
                push_pat(
                  ConstSpread(
                    binder~,
                    pkg=Some(pkg),
                    dotdot_loc~,
                    pkg_loc=Some(pkg_loc),
                    dot_uident_loc=Some(dot_uident_loc),
                    loc~,
                  ),
                )
              }
              _ =>
                match dotdot {
                  NoBinder(_)
                  | WithBinder(_, _)
                  | Underscore(_, _)
                  | WithAsBinder(_, _, _) =>
                    self.report_error({
                      loc: dotdot_loc,
                      msg: "At most one `..` is allowed in array pattern.",
                    })
                  NoDotDot =>
                    dotdot = match self.peek_token() {
                      LIDENT(_) => {
                        let binder = self.parse_binder(context="")
                        WithBinder(binder, dotdot_loc)
                      }
                      AS(_) => {
                        let as_loc = self.peek_location()
                        self.skip()
                        let binder = self.parse_binder(context="variable name")
                        WithAsBinder(binder, dotdot_loc, as_loc)
                      }
                      UNDERSCORE => {
                        let underscore_loc = self.peek_location()
                        self.skip()
                        Underscore(dotdot_loc, underscore_loc)
                      }
                      _ => NoBinder(dotdot_loc)
                    }
                }
            }
          } else {
            push_pat(Pattern(self.parse_pattern()))
          }
        },
      )
      self.pop_sync(TK_RBRACKET)
      let rbracket_loc = self.peek_location()
      self.expect_token(TK_RBRACKET)
      let pats1 = @list.from_array(pats1)
      let pats2 = @list.from_array(pats2)
      let left_comma_locs_array = []
      let mut dotdot_comma_loc = None
      let right_comma_locs_array = []
      let mut comma_index = 0
      comma_locs_array.each(fn(loc) {
        if comma_index < pats1.length() {
          left_comma_locs_array.push(loc)
        } else if comma_index == pats1.length() {
          dotdot_comma_loc = Some(loc)
        } else {
          right_comma_locs_array.push(loc)
        }
        comma_index = comma_index + 1
      })
      let left_comma_locs = @list.from_array(left_comma_locs_array)
      let right_comma_locs = @list.from_array(right_comma_locs_array)
      let pats : @syntax.ArrayPatterns = match dotdot {
        NoDotDot => Closed(pats1, comma_locs=@list.from_array(comma_locs_array))
        NoBinder(dotdot_loc) =>
          Open(
            pats1,
            pats2,
            @syntax.DotDotBinder::NoBinder(dotdot_loc~),
            left_comma_locs~,
            dotdot_comma_loc~,
            right_comma_locs~,
          )
        WithBinder(binder, dotdot_loc) =>
          Open(
            pats1,
            pats2,
            @syntax.DotDotBinder::Binder(binder~, dotdot_loc~),
            left_comma_locs~,
            dotdot_comma_loc~,
            right_comma_locs~,
          )
        Underscore(dotdot_loc, underscore_loc) =>
          Open(
            pats1,
            pats2,
            @syntax.DotDotBinder::Underscore(dotdot_loc~, underscore_loc~),
            left_comma_locs~,
            dotdot_comma_loc~,
            right_comma_locs~,
          )
        WithAsBinder(binder, dotdot_loc, as_loc) =>
          Open(
            pats1,
            pats2,
            @syntax.DotDotBinder::BinderAs(binder~, dotdot_loc~, as_loc~),
            left_comma_locs~,
            dotdot_comma_loc~,
            right_comma_locs~,
          )
      }
      let loc = self.loc_start_with(spos)
      Array(pats~, lbracket_loc~, rbracket_loc~, loc~)
    }
    LBRACE => {
      let spos = self.peek_spos()
      let mut kind : BraceLikeSyntaxKind = Unknown
      let lbrace_loc = self.peek_location()
      self.expect_token(TK_LBRACE)
      self.push_sync(TK_RBRACE)
      let (field_pat_or_dotdots, comma_locs_array, _) = self.series_with_follow_locs(
        delim=TK_COMMA,
        follow_set=[TK_RBRACE],
        fn(self) {
          let spos = self.peek_spos()
          fn parse_map_elem_pat(key) {
            if kind is Unknown {
              kind = Map
            }
            self.skip() // skip the token for constant
            let key_loc = self.loc_start_with(spos)
            let question_loc = if self.peek_token() is QUESTION {
              let question_loc = self.peek_location()
              self.skip()
              Some(question_loc)
            } else {
              None
            }
            let match_absent = question_loc is Some(_)
            let colon_loc = self.peek_location()
            self.expect_token(TK_COLON)
            let pat = self.parse_pattern()
            let loc = self.loc_start_with(spos)
            MapPatternFragment::MapElem({
              key,
              pat,
              match_absent,
              key_loc,
              question_loc,
              colon_loc,
              loc,
            })
          }

          match self.peek_token() {
            DOTDOT => {
              let loc = self.peek_location()
              self.skip()
              MapPatternFragment::Dotdot(loc)
            }
            TRUE => parse_map_elem_pat(Bool(true))
            FALSE => parse_map_elem_pat(Bool(false))
            CHAR(x) => parse_map_elem_pat(Char(x))
            INT(x) => parse_map_elem_pat(@syntax.make_int(x))
            BYTE(x) => parse_map_elem_pat(Byte(x))
            FLOAT(x) => parse_map_elem_pat(@syntax.make_float(x))
            DOUBLE(x) => parse_map_elem_pat(@syntax.make_double(x))
            MINUS => {
              self.expect_token(TK_MINUS)
              match self.peek_token() {
                INT(x) => parse_map_elem_pat(@syntax.make_int("-" + x))
                FLOAT(x) => parse_map_elem_pat(@syntax.make_float("-" + x))
                DOUBLE(x) => parse_map_elem_pat(Double("-" + x))
                other => {
                  let other_loc = self.peek_location()
                  self.report_failed_to_parse(
                    other, "`-Int` or `-Float` in key of map pattern", other_loc,
                  )
                  self.panic() |> ignore
                  MapPatternFragment::Error
                }
              }
            }
            BYTES(x) => parse_map_elem_pat(Bytes(x))
            STRING(x) => parse_map_elem_pat(String(x))
            LIDENT(_) => {
              if kind is Unknown {
                kind = Record
              }
              MapPatternFragment::Field(self.parse_field_pat())
            }
            other => {
              let expected : Array[TokenKind] = match kind {
                Unknown => [TK_LIDENT, ..first_map_pattern_key]
                Map => first_map_pattern_key
                Record => [TK_LIDENT]
              }
              self.report_unexpected(other, expected, self.peek_location())
              self.panic() |> ignore
              MapPatternFragment::Error
            }
          }
        },
      )
      self.pop_sync(TK_RBRACE)
      let rbrace_loc = self.peek_location()
      self.expect_token(TK_RBRACE)
      let loc = self.loc_start_with(spos)
      let comma_locs = @list.from_array(comma_locs_array)
      match kind {
        Unknown | Record => {
          // record pattern
          fn collect_fields(
            dotdot_loc,
            acc : List[@syntax.FieldPat],
            field_or_dotdots : ArrayView[MapPatternFragment],
          ) -> @syntax.Pattern {
            match field_or_dotdots {
              [] =>
                Record(
                  fields=acc.rev(),
                  is_closed=dotdot_loc is None,
                  lbrace_loc~,
                  rbrace_loc~,
                  comma_locs~,
                  dotdot_loc~,
                  loc~,
                )
              [Dotdot(loc), .. rest] => {
                if !(rest is []) {
                  self.report_error({
                    loc,
                    msg: "Unexpected `..` here, add `, ..` behind the last field to ignore the rest of record.",
                  })
                }
                collect_fields(Some(loc), acc, rest)
              }
              [Field(field_pat), .. rest] =>
                collect_fields(dotdot_loc, acc.add(field_pat), rest)
              [MapElem({ loc, .. }), .. rest] => {
                self.report_error({
                  loc,
                  msg: "Record pattern and map pattern cannot be mixed",
                })
                collect_fields(dotdot_loc, acc, rest)
              }
              [Error, .. rest] => collect_fields(dotdot_loc, acc, rest)
            }
          }

          collect_fields(None, @list.new(), field_pat_or_dotdots)
        }
        Map => {
          // Error only appears when we fail to parse a contant pattern,
          // so it's fine to assume it is a map pattern in this case
          fn collect_elems(
            dotdot_loc,
            acc : List[_],
            field_pat_or_dotdots : ArrayView[MapPatternFragment],
          ) -> @syntax.Pattern {
            match field_pat_or_dotdots {
              [] =>
                Map(
                  elems=acc.rev(),
                  is_closed=dotdot_loc is None,
                  lbrace_loc~,
                  rbrace_loc~,
                  comma_locs~,
                  dotdot_loc~,
                  loc~,
                )
              [Dotdot(loc), .. rest] => {
                if !(rest is []) {
                  self.report_error({ loc, msg: "Unexpected `..` here" })
                }
                collect_elems(Some(loc), acc, rest)
              }
              [Field({ loc, .. }), .. rest] => {
                self.report_error({ loc, msg: "Unexpected `..` here" })
                collect_elems(dotdot_loc, acc, rest)
              }
              [Error, .. rest] => collect_elems(dotdot_loc, acc, rest)
              [MapElem(elem), .. rest] =>
                collect_elems(dotdot_loc, acc.add(elem), rest)
            }
          }

          collect_elems(None, @list.new(), field_pat_or_dotdots)
        }
      }
    }
    other => {
      let other_loc = self.peek_location()
      self.report_unexpected(other, first_simple_pattern, other_loc)
      let loc = self.panic()
      Any(loc~)
    }
  }
}

///|
priv enum BraceLikeSyntaxKind {
  Map
  Record
  Unknown
}

///|
fn State::parse_label(self : Self, context~ : String) -> @syntax.Label {
  if self.mode is Panic(loc~, ..) {
    return { name: "?lident", loc }
  }
  let loc = self.peek_location()
  let name = self.expect_lident(context~)
  { name, loc }
}

///|
fn State::parse_field_pat(self : Self) -> @syntax.FieldPat {
  if self.mode is Panic(loc~, ..) {
    return {
      label: { name: "?ident", loc },
      pattern: Any(loc~),
      is_pun: false,
      colon_loc: None,
      loc,
    }
  }
  let spos = self.peek_spos()
  let label = self.parse_label(context="struct field")
  if self.peek_token() is COLON {
    let colon_loc = self.peek_location()
    self.skip()
    let pattern = self.parse_pattern()
    let loc = self.loc_start_with(spos)
    { label, pattern, is_pun: false, colon_loc: Some(colon_loc), loc }
  } else {
    let loc = self.loc_start_with(spos)
    let pattern : @syntax.Pattern = Var({ name: label.name, loc: label.loc })
    { label, pattern, is_pun: true, colon_loc: None, loc }
  }
}

///|
fn State::parse_constr_pat_argument(self : Self) -> @syntax.ConstrPatArg {
  if self.mode is Panic(loc~, ..) {
    return { pat: Any(loc~), kind: Positional, equal_loc: None }
  }
  match self.peek_token() {
    // label~
    POST_LABEL(label_name) => {
      let label : @syntax.Label = {
        name: label_name,
        loc: self.peek_location(),
      }
      self.skip()
      let (pat, kind, equal_loc) : (_, @syntax.ArgumentKind, Location?) = match
        self.peek_token() {
        EQUAL => {
          let equal_loc = self.peek_location()
          self.skip()
          let pat = self.parse_pattern()
          self.report_error({
            loc: label.loc,
            msg: "unexpected `~` in argument. Did you mean `label=pattern` or `label~`?",
          })
          (pat, Labelled(label~, equal_loc~), Some(equal_loc))
        }
        COMMA | RPAREN => {
          let loc = Location::trim_last_char(label.loc)
          let pat = @syntax.label_to_pat(loc~, label)
          (pat, LabelledPun(label), None)
        }
        _ => {
          let loc = self.peek_location()
          (Any(loc~), Labelled(label~, equal_loc=@syntax.no_location), None)
        }
      }
      { pat, kind, equal_loc }
    }
    LIDENT(name) | UIDENT(name) if self.peek_token(nth=1) is EQUAL => {
      let label : @syntax.Label = { name, loc: self.peek_location() }
      self.skip() // luident
      let equal_loc = self.peek_location()
      self.expect_token(TK_EQUAL)
      let pat = self.parse_pattern()
      { pat, kind: Labelled(label~, equal_loc~), equal_loc: Some(equal_loc) }
    }
    _ => {
      let pat = self.parse_pattern()
      { pat, kind: Positional, equal_loc: None }
    }
  }
}

///|
priv enum Assocativity {
  Left
  Right
}

///|
/// suppress the `Right is never used` warning
test {
  let r = Right
  match r {
    Left => ()
    Right => ()
  }
}

///|
priv enum ConstrArgRelatedness {
  Dotdot(Location)
  Arg(@syntax.ConstrPatArg)
}

///|
priv enum MapPatternFragment {
  MapElem(@syntax.MapPatElem)
  Field(@syntax.FieldPat)
  Dotdot(Location)
  Error
}

///|
priv enum ArrayPatternDotDot {
  NoBinder(Location)
  WithBinder(@syntax.Binder, Location)
  Underscore(Location, Location)
  WithAsBinder(@syntax.Binder, Location, Location)
  NoDotDot
}

///|
fn is_binop(tok : Token) -> Bool {
  match tok {
    IMPLIES
    | BARBAR
    | AMPERAMPER
    | BAR
    | CARET
    | AMPER
    | INFIX1(_)
    | INFIX2(_)
    | PLUS
    | MINUS
    | INFIX3(_)
    | INFIX4(_) => true
    _ => false
  }
}

///|
fn get_binop_prior_assoc(tok : Token) -> (Int, Assocativity) {
  match tok {
    IMPLIES => (0, Right)
    BARBAR => (1, Right)
    AMPERAMPER => (2, Right)
    BAR => (4, Left)
    CARET => (5, Left)
    AMPER => (6, Left)
    INFIX1(_) => (7, Left)
    INFIX2(_) => (8, Left)
    PLUS | MINUS => (9, Left)
    INFIX3(_) => (10, Left)
    INFIX4(_) => (11, Left)
    _ => abort("not a binop!")
  }
}

///|
fn State::parse_binop(self : Self) -> @syntax.Var {
  if self.mode is Panic(loc~, ..) {
    return { name: Ident(name="?binop"), loc }
  }
  match self.peek_token() {
    IMPLIES => {
      let loc = self.peek_location()
      self.expect_token(TK_IMPLIES)
      { name: Ident(name="→"), loc }
    }
    BARBAR => {
      let loc = self.peek_location()
      self.expect_token(TK_BARBAR)
      { name: Ident(name="||"), loc }
    }
    AMPERAMPER => {
      let loc = self.peek_location()
      self.expect_token(TK_AMPERAMPER)
      { name: Ident(name="&&"), loc }
    }
    BAR => {
      let loc = self.peek_location()
      self.expect_token(TK_BAR)
      { name: Ident(name="|"), loc }
    }
    CARET => {
      let loc = self.peek_location()
      self.expect_token(TK_CARET)
      { name: Ident(name="^"), loc }
    }
    AMPER => {
      let loc = self.peek_location()
      self.expect_token(TK_AMPER)
      { name: Ident(name="&"), loc }
    }
    INFIX1(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_INFIX1)
      { name: Ident(name=x), loc }
    }
    INFIX2(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_INFIX2)
      { name: Ident(name=x), loc }
    }
    PLUS => {
      let loc = self.peek_location()
      self.expect_token(TK_PLUS)
      { name: Ident(name="+"), loc }
    }
    MINUS => {
      let loc = self.peek_location()
      self.expect_token(TK_MINUS)
      { name: Ident(name="-"), loc }
    }
    INFIX3(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_INFIX3)
      { name: Ident(name=x), loc }
    }
    INFIX4(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_INFIX4)
      { name: Ident(name=x), loc }
    }
    other => {
      let other_loc = self.peek_location()
      self.report_failed_to_parse(other, "binary operator", other_loc)
      let loc = self.panic()
      { name: Ident(name="?binop"), loc }
    }
  }
}

///|
fn State::parse_loop_binder_with_value_and_equal_loc(
  self : State,
) -> (@syntax.Binder, Location, @syntax.Expr) {
  let binder = self.parse_binder(context="loop variable name of `for`")
  let equal_loc = self.peek_location()
  self.expect_token(TK_EQUAL)
  let value = self.parse_expr()
  (binder, equal_loc, value)
}

///|
fn State::parse_loop_binder_with_infix_value_and_equal_loc(
  self : State,
) -> (@syntax.Binder, Location, @syntax.Expr) {
  let binder = self.parse_binder(context="loop variable name of `for`")
  let equal_loc = self.peek_location()
  self.expect_token(TK_EQUAL)
  let value = self.parse_infix_expr(parsed=None)
  (binder, equal_loc, value)
}

///|
fn split_loop_bindings_with_equal_locs(
  bindings_with_locs : Array[(@syntax.Binder, Location, @syntax.Expr)],
) -> (@list.List[(@syntax.Binder, @syntax.Expr)], @list.List[Location]) {
  let bindings = []
  let equal_locs = []
  bindings_with_locs.each(binding => {
    let (binder, equal_loc, expr) = binding
    bindings.push((binder, expr))
    equal_locs.push(equal_loc)
  })
  (@list.from_array(bindings), @list.from_array(equal_locs))
}

///|
fn State::parse_foreach_binder(self : State) -> @syntax.Binder? {
  match self.peek_token() {
    UNDERSCORE => {
      self.skip()
      None
    }
    LIDENT(name) => {
      let loc = self.peek_location()
      self.skip()
      Some({ name, loc })
    }
    other => {
      let loc = self.peek_location()
      self.report_unexpected(other, [TK_LIDENT, TK_UNDERSCORE], loc)
      self.panic() |> ignore
      None
    }
  }
}

///|
fn State::parse_where_clause(self : State) -> @syntax.WhereClause? {
  if self.peek_token() is WHERE {
    let spos = self.peek_spos()
    let where_loc = self.peek_location()
    self.skip()
    let lbrace_loc = self.peek_location()
    self.expect_token(TK_LBRACE)
    let (fields, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RBRACE],
      State::parse_field_def,
    )
    let fields = @list.from_array(fields)
    let comma_locs = @list.from_array(comma_locs)
    let rbrace_loc = self.peek_location()
    self.expect_token(TK_RBRACE)
    let loc = self.loc_start_with(spos)
    Some({ fields, where_loc, lbrace_loc, rbrace_loc, comma_locs, loc })
  } else {
    None
  }
}

///|
fn State::parse_for_expr(
  self : Self,
  label~ : @syntax.Label?,
  label_colon_loc? : Location? = None,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let for_spos = self.peek_spos()
  let spos = match label_colon_loc {
    Some(loc) => loc.start
    None => for_spos
  }
  let for_loc = self.peek_location()
  self.skip() // skip FOR token
  fn handle_error(expected : Array[TokenKind]) -> @syntax.Expr {
    let loc = self.peek_location()
    let token = self.peek_token()
    self.report_unexpected(token, expected, loc)
    // If the loop header is incorrect,
    // but there is a loop body there,
    // currently our error recovery (which does not balance parenthesis)
    // may give bad result.
    // So try to recover a loop body here. 
    //
    // This pattern is equivalent to skip_until(LBRACE). There is
    // always an invariant that "if the next token is not expected, it's in panic mode."
    // Why do we still need to keep track of the panic and sync states if there is a simpler approach?
    self.push_sync(TK_LBRACE)
    self.panic() |> ignore
    self.pop_sync(TK_LBRACE)
    let block = self.parse_block_expr_with_locs()
    let loc = self.loc_start_with(spos)
    For(
      binders=@list.new(),
      condition=None,
      continue_block=@list.new(),
      body=block.expr,
      for_else=None,
      label~,
      label_colon_loc~,
      for_loc~,
      binder_comma_locs=@list.empty(),
      binder_equal_locs=@list.empty(),
      semi_locs=@list.empty(),
      continue_comma_locs=@list.empty(),
      continue_equal_locs=@list.empty(),
      body_lbrace_loc=block.lbrace_loc,
      body_rbrace_loc=block.rbrace_loc,
      nobreak_loc=None,
      nobreak_lbrace_loc=None,
      nobreak_rbrace_loc=None,
      loc~,
      where_clause=None,
    )
  }

  match self.peek_token() {
    SEMI(_) | LBRACE =>
      self.parse_for_loop(spos~, label~, label_colon_loc~, for_loc~)
    UNDERSCORE | IN =>
      // The case of `in` is actually syntax-incorrect,
      // but it is very likely a `for .. in` loop,
      // so we treat it as `for .. in` for better error message & recovery.
      self.parse_foreach_loop(spos~, label~, label_colon_loc~, for_loc~)
    LIDENT(_) =>
      match self.peek_token(nth=1) {
        EQUAL => self.parse_for_loop(spos~, label~, label_colon_loc~, for_loc~)
        COMMA | IN =>
          self.parse_foreach_loop(spos~, label~, label_colon_loc~, for_loc~)
        _ => {
          self.skip()
          handle_error([TK_EQUAL, TK_COMMA, TK_IN])
        }
      }
    _ => handle_error([TK_LIDENT, TK_SEMI, TK_LBRACE])
  }
}

///|
fn State::parse_foreach_loop(
  self : Self,
  spos~ : Position,
  label~ : @syntax.Label?,
  label_colon_loc~ : Location?,
  for_loc~ : Location,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let (binders_array, binder_comma_locs_array) = self.with_follow(
    follow_set=[TK_IN],
    fn(state) {
      match state.mode {
        Panic(_) => ([], [])
        Normal => {
          state.push_sync(TK_COMMA)
          let binders = [state.parse_foreach_binder()]
          let binder_comma_locs = []
          while state.peek_kind() == TK_COMMA {
            binder_comma_locs.push(state.peek_location())
            state
            ..pop_sync(TK_COMMA)
            ..expect_token(TK_COMMA)
            .push_sync(TK_COMMA)
            binders.push(state.parse_foreach_binder())
          }
          state.pop_sync(TK_COMMA)
          (binders, binder_comma_locs)
        }
      }
    },
  )
  let binders = @list.from_array(binders_array)
  let binder_comma_locs = @list.from_array(binder_comma_locs_array)
  let in_loc = self.peek_location()
  self.expect_token(TK_IN)
  let expr = self.parse_expr()
  let (
    init,
    continue_block,
    semi_locs,
    init_comma_locs,
    init_equal_locs,
    continue_comma_locs,
    continue_equal_locs,
  ) = if self.peek_token() is SEMI(_) {
    let first_semi_loc = self.peek_location()
    self.skip()
    let (init_with_locs, init_comma_locs_array, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_SEMI, TK_LBRACE],
      State::parse_loop_binder_with_value_and_equal_loc,
    )
    let (init, init_equal_locs) = split_loop_bindings_with_equal_locs(
      init_with_locs,
    )
    let init_comma_locs = @list.from_array(init_comma_locs_array)
    if self.peek_token() is SEMI(_) {
      let second_semi_loc = self.peek_location()
      self.skip()
      let (continue_with_locs, continue_comma_locs_array, _) = self.series_with_follow_locs(
        delim=TK_COMMA,
        follow_set=[TK_LBRACE],
        State::parse_loop_binder_with_value_and_equal_loc,
      )
      let (continue_block, continue_equal_locs) = split_loop_bindings_with_equal_locs(
        continue_with_locs,
      )
      (
        init,
        continue_block,
        @list.from_array([first_semi_loc, second_semi_loc]),
        init_comma_locs,
        init_equal_locs,
        @list.from_array(continue_comma_locs_array),
        continue_equal_locs,
      )
    } else {
      (
        init,
        @list.empty(),
        @list.singleton(first_semi_loc),
        init_comma_locs,
        init_equal_locs,
        @list.empty(),
        @list.empty(),
      )
    }
  } else {
    (
      @list.empty(),
      @list.empty(),
      @list.empty(),
      @list.empty(),
      @list.empty(),
      @list.empty(),
      @list.empty(),
    )
  }
  let block = self.parse_block_expr_with_locs()
  let body = block.expr
  let (else_block, nobreak_loc, nobreak_lbrace_loc, nobreak_rbrace_loc) = if self.peek_token()
    is NOBREAK {
    let nobreak_loc = self.peek_location()
    self.skip()
    let block = self.parse_block_expr_with_locs()
    (
      Some(block.expr),
      Some(nobreak_loc),
      Some(block.lbrace_loc),
      Some(block.rbrace_loc),
    )
  } else {
    (None, None, None, None)
  }
  let where_clause = self.parse_where_clause()
  let loc = self.loc_start_with(spos)
  ForEach(
    binders~,
    expr~,
    init~,
    continue_block~,
    body~,
    else_block~,
    where_clause~,
    loc~,
    label~,
    label_colon_loc~,
    for_loc~,
    binder_comma_locs~,
    in_loc~,
    semi_locs~,
    init_comma_locs~,
    init_equal_locs~,
    continue_comma_locs~,
    continue_equal_locs~,
    body_lbrace_loc=block.lbrace_loc,
    body_rbrace_loc=block.rbrace_loc,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
  )
}

///|
fn State::parse_for_loop(
  self : Self,
  spos~ : Position,
  label~ : @syntax.Label?,
  label_colon_loc~ : Location?,
  for_loc~ : Location,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let (bindings_with_locs, binder_comma_locs_array, _) = self.series_with_follow_locs(
    delim=TK_COMMA,
    follow_set=[TK_SEMI, TK_LBRACE],
    State::parse_loop_binder_with_value_and_equal_loc,
  )
  let (binders, binder_equal_locs) = split_loop_bindings_with_equal_locs(
    bindings_with_locs,
  )
  let binder_comma_locs = @list.from_array(binder_comma_locs_array)
  let (
    condition,
    continue_block,
    semi_locs,
    continue_comma_locs,
    continue_equal_locs,
  ) = match self.peek_token() {
    LBRACE => (None, @list.empty(), @list.empty(), @list.empty(), @list.empty())
    SEMI(_) => {
      self.push_sync(TK_LBRACE)
      let first_semi_loc = self.peek_location()
      self.expect_token(TK_SEMI)
      let has_condition = first_expr.contains(self.peek_token().kind())
      let condition = self.optional(first_expr, State::try_parse_condition_expr)
      let result = if has_condition && self.peek_token() is LBRACE {
        (
          condition,
          @list.empty(),
          @list.singleton(first_semi_loc),
          @list.empty(),
          @list.empty(),
        )
      } else {
        let second_semi_loc = match self.peek_token() {
          SEMI(_) => Some(self.peek_location())
          _ => None
        }
        self.expect_token(TK_SEMI)
        let (continue_with_locs, continue_comma_locs_array, _) = self.series_with_follow_locs(
          delim=TK_COMMA,
          follow_set=[TK_LBRACE],
          State::parse_loop_binder_with_value_and_equal_loc,
        )
        let (continue_block, continue_equal_locs) = split_loop_bindings_with_equal_locs(
          continue_with_locs,
        )
        let semi_locs = match second_semi_loc {
          Some(loc) => @list.from_array([first_semi_loc, loc])
          None => @list.singleton(first_semi_loc)
        }
        (
          condition,
          continue_block,
          semi_locs,
          @list.from_array(continue_comma_locs_array),
          continue_equal_locs,
        )
      }
      self.pop_sync(TK_LBRACE)
      result
    }
    other => {
      let other_loc = self.peek_location()
      self.report_unexpected(other, [TK_SEMI, TK_LBRACE], other_loc)
      (None, @list.empty(), @list.empty(), @list.empty(), @list.empty())
    }
  }
  let block = self.parse_block_expr_with_locs()
  let body = block.expr
  let (for_else, nobreak_loc, nobreak_lbrace_loc, nobreak_rbrace_loc) = if self.peek_token()
    is NOBREAK {
    let nobreak_loc = self.peek_location()
    self.with_follow(follow_set=[TK_LBRACE], fn(state) {
      state.expect_token(TK_NOBREAK)
    })
    let block = self.parse_block_expr_with_locs()
    (
      Some(block.expr),
      Some(nobreak_loc),
      Some(block.lbrace_loc),
      Some(block.rbrace_loc),
    )
  } else {
    (None, None, None, None)
  }
  let where_clause = self.parse_where_clause()
  let loc = self.loc_start_with(spos)
  For(
    binders~,
    condition~,
    continue_block~,
    body~,
    for_else~,
    label~,
    label_colon_loc~,
    for_loc~,
    binder_comma_locs~,
    binder_equal_locs~,
    semi_locs~,
    continue_comma_locs~,
    continue_equal_locs~,
    body_lbrace_loc=block.lbrace_loc,
    body_rbrace_loc=block.rbrace_loc,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
    loc~,
    where_clause~,
  )
}

///|
fn State::parse_while_loop(
  self : Self,
  label~ : @syntax.Label?,
  label_colon_loc? : Location? = None,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let while_spos = self.peek_spos()
  let spos = match label_colon_loc {
    Some(loc) => loc.start
    None => while_spos
  }
  let while_loc = self.peek_location()
  self.expect_token(TK_WHILE)
  let loop_cond = self.with_follow(
    follow_set=[TK_LBRACE],
    State::try_parse_condition_expr,
  )
  let block = self.parse_block_expr_with_locs()
  let loop_body = block.expr
  let mut nobreak_loc = None
  let mut nobreak_lbrace_loc = None
  let mut nobreak_rbrace_loc = None
  let while_else = if self.peek_token() is NOBREAK {
    nobreak_loc = Some(self.peek_location())
    self.with_follow(follow_set=[TK_LBRACE], fn(state) {
      state.expect_token(TK_NOBREAK)
    })
    let block = self.parse_block_expr_with_locs()
    nobreak_lbrace_loc = Some(block.lbrace_loc)
    nobreak_rbrace_loc = Some(block.rbrace_loc)
    Some(block.expr)
  } else {
    None
  }
  let loc = self.loc_start_with(spos)
  While(
    loop_cond~,
    loop_body~,
    while_else~,
    label~,
    label_colon_loc~,
    while_loc~,
    body_lbrace_loc=block.lbrace_loc,
    body_rbrace_loc=block.rbrace_loc,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
    loc~,
  )
}

///|
fn State::try_parse_pipe_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  match self.peek_token() {
    // an error production to handle `try invalid-pipe-expr`
    IF | MATCH | WHILE | FOR | TRY | LET | GUARD | BREAK | RETURN | CONTINUE =>
      self.handle_unexpected_expr_or_statement(context="the try body")
    _ => self.parse_pipe_expr(parsed=None)
  }
}

///|
fn State::parse_try_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let try_loc = self.peek_location()
  self.expect_token(TK_TRY)
  let body = self.try_parse_pipe_expr()
  let catch_start = self.peek_spos()
  self.expect_token(TK_CATCH)
  let lbrace_epos = self.peek_epos()
  // the catch_loc is the location of `catch {`
  let catch_loc : Location = { start: catch_start, end: lbrace_epos }
  let (catch_cases, catch_lbrace_loc, catch_rbrace_loc, catch_semi_locs) = self.parse_try_case_block_with_locs(invalid_delims=[
      TK_COMMA,
    ],
  )
  let (
    try_else,
    else_loc,
    noraise_lbrace_loc,
    noraise_rbrace_loc,
    noraise_semi_locs,
  ) = if self.peek_token() is NORAISE {
    let else_spos = self.peek_spos()
    self.skip()
    let lbrace_epos = self.peek_epos()
    // the else_loc is the location of `noraise {`
    let else_loc : Location = { start: else_spos, end: lbrace_epos }
    let (
      try_else_cases,
      noraise_lbrace_loc,
      noraise_rbrace_loc,
      noraise_semi_locs,
    ) = self.parse_try_case_block_with_locs(invalid_delims=[TK_COMMA])
    (
      Some(try_else_cases),
      else_loc,
      Some(noraise_lbrace_loc),
      Some(noraise_rbrace_loc),
      noraise_semi_locs,
    )
  } else {
    (None, @syntax.no_location, None, None, @list.empty())
  }
  let loc = self.loc_start_with(spos)
  Try(
    body~,
    catch_=catch_cases,
    try_else~,
    has_try=true,
    try_loc~,
    catch_loc~,
    else_loc~,
    catch_lbrace_loc~,
    catch_rbrace_loc~,
    catch_semi_locs~,
    noraise_lbrace_loc~,
    noraise_rbrace_loc~,
    noraise_semi_locs~,
    loc~,
  )
}

///|
fn State::parse_try_case_block_with_locs(
  self : Self,
  invalid_delims? : Array[TokenKind] = [],
) -> (@list.List[@syntax.Case], Location, Location, @list.List[Location]) {
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  self.push_sync(TK_RBRACE)
  let (case_array, semi_locs_array, _) = self.series_with_follow_locs(
    invalid_delims~,
    delim=TK_SEMI,
    follow_set=[TK_RBRACE],
    State::parse_case,
  )
  self.pop_sync(TK_RBRACE)
  let rbrace_loc = self.peek_location()
  self.expect_token(TK_RBRACE)
  (
    @list.from_array(case_array),
    lbrace_loc,
    rbrace_loc,
    @list.from_array(semi_locs_array),
  )
}

///|
fn State::parse_try_question(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let try_loc = self.peek_location()
  self.skip()
  let body = self.parse_pipe_expr(parsed=None)
  let loc = self.loc_start_with(spos)
  TryOperator(body~, kind=Question, try_loc~, loc~)
}

///|
fn State::parse_try_exclamation(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let try_loc = self.peek_location()
  self.skip()
  let body = self.parse_pipe_expr(parsed=None)
  let loc = self.loc_start_with(spos)
  TryOperator(body~, kind=Exclamation, try_loc~, loc~)
}

///|
fn State::parse_if_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let if_loc = self.peek_location()
  self.expect_token(TK_IF)
  let cond = self.with_follow(
    follow_set=[TK_LBRACE],
    State::try_parse_condition_expr,
  )
  let ifso = self.parse_block_expr_with_locs()
  let mut else_loc = None
  let mut else_lbrace_loc = None
  let mut else_rbrace_loc = None
  let ifnot = if self.peek_token() is ELSE {
    else_loc = Some(self.peek_location())
    self.skip()
    Some(
      if self.peek_token() is IF {
        self.parse_expr() // TODO: call parse_if_expr directly
      } else {
        let block = self.parse_block_expr_with_locs()
        else_lbrace_loc = Some(block.lbrace_loc)
        else_rbrace_loc = Some(block.rbrace_loc)
        block.expr
      },
    )
  } else {
    None
  }
  let loc = self.loc_start_with(spos)
  If(
    cond~,
    ifso=ifso.expr,
    ifnot~,
    if_loc~,
    then_lbrace_loc=ifso.lbrace_loc,
    then_rbrace_loc=ifso.rbrace_loc,
    else_loc~,
    else_lbrace_loc~,
    else_rbrace_loc~,
    loc~,
  )
}

///|
fn State::parse_match_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  self.expect_token(TK_MATCH)
  let expr = self.with_follow(follow_set=[TK_LBRACE], fn(_) {
    self.try_parse_condition_expr()
  })
  let lbrace_epos = self.peek_epos()
  // The main_loc is the location of `match expr {`
  let match_loc : Location = { start: spos, end: lbrace_epos }
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  self.push_sync(TK_RBRACE)
  let (case_array, semi_locs_array, _) = self.series_with_follow_locs(
    invalid_delims=[TK_COMMA],
    delim=TK_SEMI,
    follow_set=[TK_RBRACE],
    State::parse_case,
  )
  self.pop_sync(TK_RBRACE)
  let rbrace_loc = self.peek_location()
  self.expect_token(TK_RBRACE)
  let cases = @list.from_array(case_array)
  let semi_locs = @list.from_array(semi_locs_array)
  let loc = self.loc_start_with(spos)
  Match(expr~, cases~, match_loc~, lbrace_loc~, rbrace_loc~, semi_locs~, loc~)
}

///|
fn State::parse_case(self : Self) -> @syntax.Case {
  if self.mode is Panic(loc~, ..) {
    return {
      pattern: Any(loc~),
      guard_: None,
      body: Hole(loc~, kind=Synthesized),
      if_loc: None,
      fat_arrow_loc: None,
      ellipsis_loc: None,
    }
  }
  if self.peek_token() is ELLIPSIS {
    let loc = self.peek_location()
    self.skip()
    {
      pattern: Any(loc~),
      guard_: None,
      body: Hole(loc~, kind=Todo),
      if_loc: None,
      fat_arrow_loc: None,
      ellipsis_loc: Some(loc),
    }
  } else {
    let pat = self.with_syncs([TK_FAT_ARROW, TK_IF], State::parse_pattern)
    let mut if_loc = None
    let guard_ = if self.peek_token() is IF {
      if_loc = Some(self.peek_location())
      self.skip()
      let expr = match self.peek_token() {
        // an error production to handle `pattern if invalid-infix-expr => action`
        IF
        | MATCH
        | WHILE
        | FOR
        | TRY
        | LET
        | GUARD
        | BREAK
        | RETURN
        | CONTINUE =>
          self.handle_unexpected_expr_or_statement(context="pattern guard")
        _ => self.parse_infix_expr(parsed=None)
      }
      Some(expr)
    } else {
      None
    }
    let (fat_arrow_loc, body) = self.parse_fat_arrow_action_with_loc()
    {
      pattern: pat,
      guard_,
      body,
      if_loc,
      fat_arrow_loc: Some(fat_arrow_loc),
      ellipsis_loc: None,
    }
  }
}

///|
fn State::parse_fat_arrow_action_with_loc(
  self : State,
) -> (Location, @syntax.Expr) {
  let fat_arrow_loc = self.peek_location()
  self.expect_token(TK_FAT_ARROW)
  let expr = match self.peek_token() {
    // an error production to handle `pattern => invalid-expr-statement`
    LET | LETREC | GUARD | DEFER =>
      self.handle_unexpected_expr_or_statement(
        context="the action part of a matching case",
      )
    _ => self.parse_expr_stmt()
  }
  (fat_arrow_loc, expr)
}

///|
fn State::parse_opt_with_strategy(self : State) -> (@syntax.Label?, Location?) {
  match self.peek_token() {
    WITH => {
      let with_loc = self.peek_location()
      self.skip()
      let label = self.parse_label(context="strategy of `lexmatch`")
      (Some(label), Some(with_loc))
    }
    _ => (None, None)
  }
}

///|
fn State::parse_lexmatch_expr(self : State) -> @syntax.Expr {
  let spos = self.peek_spos()
  self.expect_token(TK_LEXMATCH)
  let (expr, strategy, with_loc) = self.with_follow(follow_set=[TK_LBRACE], fn(
    state,
  ) {
    let expr = state.try_parse_condition_expr()
    let (strategy, with_loc) = state.parse_opt_with_strategy()
    (expr, strategy, with_loc)
  })
  let match_loc = @basic.Location::{ start: spos, end: self.peek_epos() }
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  self.push_sync(TK_RBRACE)
  let (case_array, semi_locs_array, _) = self.series_with_follow_locs(
    invalid_delims=[TK_COMMA],
    delim=TK_SEMI,
    follow_set=[TK_RBRACE],
    State::parse_lexmatch_case,
  )
  self.pop_sync(TK_RBRACE)
  let rbrace_loc = self.peek_location()
  self.expect_token(TK_RBRACE)
  let cases = @list.from_array(case_array)
  let semi_locs = @list.from_array(semi_locs_array)
  let loc = self.loc_start_with(spos)
  @syntax.Expr::LexMatch(
    strategy~,
    expr~,
    match_loc~,
    with_loc~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
    cases~,
    loc~,
  )
}

///|
fn State::parse_lexmatch_case(self : State) -> @syntax.LexCase {
  match self.peek_token() {
    ELLIPSIS => {
      let loc = self.peek_location()
      self.skip()
      @syntax.LexCase::{
        pat: @list.singleton(Wildcard(loc~)),
        pat_loc: loc,
        guard_: None,
        body: Hole(loc~, kind=Todo),
        if_loc: None,
        fat_arrow_loc: None,
        ellipsis_loc: Some(loc),
      }
    }
    _ => {
      let (pat, pat_loc) = self.with_syncs(
        [TK_FAT_ARROW, TK_COMMA],
        State::parse_lex_pattern,
      )
      let (guard_, if_loc) = self.parse_lex_case_guard()
      let (fat_arrow_loc, act) = self.parse_fat_arrow_action_with_loc()
      {
        pat,
        pat_loc,
        guard_,
        body: act,
        if_loc,
        fat_arrow_loc: Some(fat_arrow_loc),
        ellipsis_loc: None,
      }
    }
  }
}

///|
fn State::parse_lex_case_guard(self : State) -> (@syntax.Expr?, Location?) {
  match self.peek_token() {
    IF => {
      let if_loc = self.peek_location()
      self.skip()
      // an error production to handle `lex pattern if invalid-infix-expr => action`
      let expr = match self.peek_token() {
        IF
        | MATCH
        | WHILE
        | FOR
        | TRY
        | TRY_QUESTION
        | TRY_EXCLAMATION
        | LET
        | LETREC
        | GUARD
        | BREAK
        | RETURN
        | CONTINUE =>
          self.handle_unexpected_expr_or_statement(context="lex case guard")
        _ => self.parse_infix_expr(parsed=None)
      }
      (Some(expr), Some(if_loc))
    }
    _ => (None, None)
  }
}

///|
fn State::parse_lex_pattern(
  self : State,
) -> (@list.List[@syntax.LexTopPattern], @basic.Location) {
  match self.peek_token() {
    LPAREN => {
      let spos = self.peek_spos()
      self.skip()
      let pats = self.with_follow(follow_set=[TK_RPAREN], state => {
          state.sepby1(delim=TK_COMMA, State::parse_lex_top_pattern)
        })
        |> @list.from_array
      self.expect_token(TK_RPAREN)
      let loc = self.loc_start_with(spos)
      (pats, loc)
    }
    LIDENT(_) => {
      let loc = self.peek_location()
      let name = self.expect_lident(context="lex rest binder")
      (@list.singleton(Binder({ name, loc })), loc)
    }
    UNDERSCORE => {
      let loc = self.peek_location()
      self.skip()
      (@list.singleton(Wildcard(loc~)), loc)
    }
    REGEX_LITERAL(_)
    | REGEX_INTERP(_)
    | STRING(_)
    | INTERP(_)
    | UIDENT(_)
    | PACKAGE_NAME(_) => {
      let spos = self.peek_spos()
      let pat = self.parse_lex_simple_atom_pattern()
      (@list.singleton(Pattern(pat)), self.loc_start_with(spos))
    }
    other => {
      let other_loc = self.peek_location()
      self.report_failed_to_parse(other, "lex pattern", other_loc)
      let loc = self.panic()
      (@list.singleton(Wildcard(loc~)), loc)
    }
  }
}

///|
fn State::parse_lex_top_pattern(self : State) -> @syntax.LexTopPattern {
  match self.peek_token() {
    LIDENT(_) => {
      let loc = self.peek_location()
      let name = self.expect_lident(context="lex rest binder")
      Binder({ name, loc })
    }
    UNDERSCORE(_) => {
      let loc = self.peek_location()
      self.skip()
      Wildcard(loc~)
    }
    _ => {
      let pat = self.parse_lex_as_pattern()
      Pattern(pat)
    }
  }
}

///|
fn State::parse_lex_as_pattern(self : State) -> @syntax.LexPattern {
  let spos = self.peek_spos()
  let (pats, semi_locs) = self.parse_lex_pattern_sequence()
  match pats {
    More(pat, tail=Empty) if self.peek_token() is AS => {
      let as_loc = self.peek_location()
      self.skip()
      let binder = self.parse_binder(context="binder in `as` pattern")
      let loc = self.loc_start_with(spos)
      Alias(pat~, binder~, as_loc~, loc~)
    }
    More(pat, tail=Empty) => return pat
    _ => {
      let loc = self.loc_start_with(spos)
      Sequence(pats~, semi_locs~, loc~)
    }
  }
}

///|
fn State::parse_lex_pattern_sequence(
  self : State,
) -> (@list.List[@syntax.LexPattern], @list.List[Location]) {
  let first = self.parse_lex_atom_pattern()
  let pats = [first]
  let semi_locs = []
  for tok = self.peek_token() {
    match tok {
      SEMI(explicit) => {
        if explicit {
          semi_locs.push(self.peek_location())
        }
        self.skip()
        let pat = self.parse_lex_atom_pattern()
        pats.push(pat)
        continue self.peek_token()
      }
      REGEX_LITERAL(_)
      | REGEX_INTERP(_)
      | STRING(_)
      | INTERP(_)
      | UIDENT(_)
      | PACKAGE_NAME(_)
      | LPAREN => {
        let pat = self.parse_lex_atom_pattern()
        pats.push(pat)
        continue self.peek_token()
      }
      _ => break ()
    }
  }
  (@list.from_array(pats), @list.from_array(semi_locs))
}

///|
fn State::parse_lex_atom_pattern(self : State) -> @syntax.LexPattern {
  match self.peek_token() {
    REGEX_LITERAL(_)
    | REGEX_INTERP(_)
    | STRING(_)
    | INTERP(_)
    | UIDENT(_)
    | PACKAGE_NAME(_) => self.parse_lex_simple_atom_pattern()
    LPAREN => {
      self.skip()
      let alias_spos = self.peek_spos()
      let (pats, semi_locs) = self.with_syncs(
        [TK_RPAREN],
        State::parse_lex_pattern_sequence,
      )
      match pats {
        More(pat, tail=Empty) =>
          match self.peek_token() {
            AS => {
              let as_loc = self.peek_location()
              self.skip()
              let binder = self.parse_binder(context="binder in `as` pattern")
              let loc = self.loc_start_with(alias_spos)
              self.expect_token(TK_RPAREN)
              Alias(pat~, binder~, as_loc~, loc~)
            }
            _ => {
              self.expect_token(TK_RPAREN)
              pat
            }
          }
        _ => {
          self.expect_token(TK_RPAREN)
          Sequence(pats~, semi_locs~, loc=self.peek_location())
        }
      }
    }
    other => {
      let other_loc = self.peek_location()
      self.report_failed_to_parse(other, "lex atom pattern", other_loc)
      let loc = self.panic()
      Sequence(pats=@list.empty(), semi_locs=@list.empty(), loc~)
    }
  }
}

///|
fn State::parse_lex_simple_atom_pattern(self : State) -> @syntax.LexPattern {
  match self.peek_token() {
    REGEX_LITERAL(lit) => {
      let loc = self.peek_location()
      self.skip()
      Regex(lit~, offset=3, loc~)
    }
    REGEX_INTERP(elems) => {
      let loc = self.peek_location()
      self.skip()
      let elems = @syntax.make_interps(elems)
      RegexInterp(elems~, loc~)
    }
    STRING(lit) => {
      let loc = self.peek_location()
      self.skip()
      Regex(lit~, offset=1, loc~)
    }
    INTERP(elems) => {
      let loc = self.peek_location()
      self.skip()
      let elems = @syntax.make_interps(elems)
      RegexInterp(elems~, loc~)
    }
    UIDENT(_) | PACKAGE_NAME(_) => {
      let spos = self.peek_spos()
      let lid = self.parse_qual_uident()
      let loc = self.loc_start_with(spos)
      ConstantRef(lid~, loc~)
    }
    _ => panic()
  }
}

///|
fn State::parse_tuple_or_constraint(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let lparen_loc = self.peek_location()
  self.expect_token(TK_LPAREN)
  self.push_sync(TK_RPAREN)
  let state = self
  let first = state.optional(first_expr, State::parse_expr)
  match first {
    None => {
      let rparen_loc = self.peek_location()
      self.expect_token(TK_RPAREN)
      self.pop_sync(TK_RPAREN)
      let loc = state.loc_start_with(spos)
      @syntax.Expr::Unit(loc~, faked=false, lparen_loc~, rparen_loc~)
    }
    Some(first) =>
      match state.peek_token() {
        COLON => {
          let colon_loc = state.peek_location()
          state.expect_token(TK_COLON)
          let ty = state.parse_type()
          let rparen_loc = self.peek_location()
          self.expect_token(TK_RPAREN)
          self.pop_sync(TK_RPAREN)
          let loc = state.loc_start_with(spos)
          Constraint(
            expr=first,
            ty~,
            lparen_loc=Some(lparen_loc),
            colon_loc~,
            rparen_loc=Some(rparen_loc),
            loc~,
          )
        }
        COMMA => {
          let first_comma_loc = state.peek_location()
          state.expect_token(TK_COMMA)
          let (rest, rest_comma_locs, _) = state.series_with_follow_locs(
            delim=TK_COMMA,
            follow_set=[TK_RPAREN],
            State::parse_expr,
          )
          let rparen_loc = self.peek_location()
          self.expect_token(TK_RPAREN)
          self.pop_sync(TK_RPAREN)
          let loc = state.loc_start_with(spos)
          if rest.length() == 0 {
            Group(
              expr=first,
              group=Paren,
              left_delim_loc=lparen_loc,
              right_delim_loc=rparen_loc,
              loc~,
            )
          } else {
            let rest = @list.from_array(rest)
            let comma_locs = @list.from_array(
              [first_comma_loc, ..rest_comma_locs],
            )
            Tuple(
              exprs=rest.add(first),
              lparen_loc~,
              rparen_loc~,
              comma_locs~,
              loc~,
            )
          }
        }
        RPAREN => {
          let rparen_loc = self.peek_location()
          self.expect_token(TK_RPAREN)
          self.pop_sync(TK_RPAREN)
          let loc = state.loc_start_with(spos)
          Group(
            expr=first,
            group=Paren,
            left_delim_loc=lparen_loc,
            right_delim_loc=rparen_loc,
            loc~,
          )
        }
        _ => first //TODO: error recovery and add group node
      }
  }
}

///|
fn State::handle_lparen(self : Self) -> @syntax.Expr {
  let spos = self.peek_spos()
  let lparen_loc = self.peek_location()
  self.expect_token(TK_LPAREN)
  let (expr_or_binders, comma_locs_array, has_trailing) = self.series_with_follow_locs(
    delim=TK_COMMA,
    follow_set=[TK_RPAREN],
    State::parse_expr_or_binder,
  )
  let rparen_loc = self.peek_location()
  self.expect_token(TK_RPAREN)
  let comma_locs = @list.from_array(comma_locs_array)
  let handle_expr = fn(expr) { self.parse_simple_try_expr(parsed=Some(expr)) }
  match (self.peek_token(), expr_or_binders) {
    (FAT_ARROW, _) => {
      let params_loc = self.loc_start_with(spos)
      self.skip()
      fn as_binder(expr : @syntax.Expr) -> @syntax.ArrowFnParam {
        match expr {
          Ident(id={ name: Ident(name~), .. }, loc~) => Named({ name, loc })
          Hole(kind=Incomplete, loc~) => Unnamed(loc)
          // error recovery
          Hole(kind=Synthesized, loc~) => Unnamed(loc)
          expr => {
            let loc = expr.loc()
            self.report_error({
              loc,
              msg: "Unexpected expression. You may expect parameter here.",
            })
            Unnamed(loc)
          }
        }
      }

      let parameters = expr_or_binders.map(x => {
          match x {
            Binder(expr, colon_loc=_, ty~) => (as_binder(expr), Some(ty))
            Expr(expr) => (as_binder(expr), None)
          }
        })
        |> @list.from_array
      let body = self.parse_expr_stmt_no_break_continue_return()
      let loc = self.loc_start_with(spos)
      Function(
        loc~,
        func=@syntax.make_arrow_fn(
          params_loc~,
          lparen_loc=Some(lparen_loc),
          rparen_loc=Some(rparen_loc),
          comma_locs~,
          loc~,
          parameters,
          body,
        ),
      )
    }
    (_, []) => {
      let loc = self.loc_start_with(spos)
      handle_expr(Unit(loc~, faked=false, lparen_loc~, rparen_loc~))
    }
    (_, [Binder(expr, ty~, colon_loc~)]) if !has_trailing => {
      let loc = self.loc_start_with(spos)
      handle_expr(
        Constraint(
          expr~,
          ty~,
          lparen_loc=Some(lparen_loc),
          colon_loc~,
          rparen_loc=Some(rparen_loc),
          loc~,
        ),
      )
    }
    (_, [Expr(expr)]) => {
      let loc = self.loc_start_with(spos)
      handle_expr(
        Group(
          expr~,
          group=Paren,
          left_delim_loc=lparen_loc,
          right_delim_loc=rparen_loc,
          loc~,
        ),
      )
    }
    (_, _) => {
      let exprs = expr_or_binders.map(x => {
          match x {
            Binder(expr, colon_loc~, ty=_) => {
              self.report_unexpected(COLON, [TK_COMMA, TK_RPAREN], colon_loc)
              @syntax.Hole(kind=Synthesized, loc=expr.loc())
            }
            Expr(expr) => expr
          }
        })
        |> @list.from_array
      let loc = self.loc_start_with(spos)
      handle_expr(Tuple(exprs~, lparen_loc~, rparen_loc~, comma_locs~, loc~))
    }
  }
}

///|
fn State::parse_expr_or_binder(self : Self) -> ExprOrBinderFragment {
  if self.mode is Panic(loc~, ..) {
    return Expr(Hole(kind=Synthesized, loc~))
  }
  let expr = self.parse_expr()
  if self.peek_token() is COLON {
    let colon_loc = self.peek_location()
    self.skip()
    let ty = self.parse_type()
    Binder(expr, colon_loc~, ty~)
  } else {
    Expr(expr)
  }
}

///|
fn State::parse_quantifier_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let (kind, quantifier_loc) : (@syntax.PredicateQuantifierKind, Location) = match
    self.peek_token() {
    FORALL => {
      let quantifier_loc = self.peek_location()
      self.skip()
      (Forall, quantifier_loc)
    }
    EXISTS => {
      let quantifier_loc = self.peek_location()
      self.skip()
      (Exists, quantifier_loc)
    }
    _ => panic()
  }
  let binder = self.parse_binder(context="quantifier binder")
  let colon_loc = self.peek_location()
  self.expect_token(TK_COLON)
  let binder_ty = self.parse_type()
  let comma_loc = self.peek_location()
  self.expect_token(TK_COMMA)
  let body = match self.peek_token() {
    FORALL | EXISTS => self.parse_quantifier_expr()
    _ => self.parse_infix_expr(parsed=None)
  }
  let loc = self.loc_start_with(spos)
  Quantifier(
    kind~,
    binder~,
    binder_ty~,
    body~,
    quantifier_loc~,
    colon_loc~,
    comma_loc~,
    loc~,
  )
}

///|
fn State::parse_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  match self.peek_token() {
    POST_LABEL(label_name) => {
      let label_spos = self.peek_spos()
      self.skip()
      self.expect_token(TK_COLON)
      let label_colon_loc = self.loc_start_with(label_spos)
      let label : @syntax.Label = { name: label_name, loc: label_colon_loc }
      match self.peek_token() {
        FOR =>
          self.parse_for_expr(
            label=Some(label),
            label_colon_loc=Some(label_colon_loc),
          )
        WHILE =>
          self.parse_while_loop(
            label=Some(label),
            label_colon_loc=Some(label_colon_loc),
          )
        other => {
          let loc = self.peek_location()
          self.report_unexpected(other, [TK_FOR, TK_WHILE], loc)
          self.panic() |> ignore
          Hole(loc~, kind=Synthesized)
        }
      }
    }
    LEXMATCH => self.parse_lexmatch_expr()
    FOR => self.parse_for_expr(label=None)
    WHILE => self.parse_while_loop(label=None)
    TRY => self.parse_try_expr()
    TRY_QUESTION => self.parse_try_question()
    TRY_EXCLAMATION => self.parse_try_exclamation()
    IF => self.parse_if_expr()
    MATCH => self.parse_match_expr()
    FORALL | EXISTS => self.parse_quantifier_expr()
    LPAREN => self.handle_lparen()
    LIDENT(_) | UNDERSCORE if self.peek_token(nth=1) is FAT_ARROW => {
      let spos = self.peek_spos()
      let p : @syntax.ArrowFnParam = match self.peek_token() {
        LIDENT(_) => {
          let binder = self.parse_binder(context="")
          Named(binder)
        }
        UNDERSCORE => {
          let loc = self.peek_location()
          self.expect_token(TK_UNDERSCORE)
          Unnamed(loc)
        }
        _ => panic()
      }
      let params_loc = self.loc_start_with(spos)
      self.expect_token(TK_FAT_ARROW)
      let body = self.parse_expr_stmt_no_break_continue_return()
      let loc = self.loc_start_with(spos)
      let func = @syntax.make_arrow_fn(
        @list.from_array([(p, None)]),
        body,
        params_loc~,
        loc~,
      )
      Function(loc~, func~)
    }
    _ =>
      match self.try_parse_arrow_fn_expr() {
        Err(parsed) => self.parse_simple_try_expr(parsed~)
        Ok(expr) => expr
      }
  }
}

///|
fn State::parse_simple_try_expr(
  self : Self,
  parsed~ : @syntax.Expr?,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let e = self.parse_pipe_expr(parsed~)
  match self.peek_token() {
    CATCH => {
      let catch_start = self.peek_spos()
      self.skip()
      let catch_loc = self.loc_start_with(catch_start)
      let (catch_cases, catch_lbrace_loc, catch_rbrace_loc, catch_semi_locs) = self.parse_try_case_block_with_locs()
      let (
        try_else,
        else_loc,
        noraise_lbrace_loc,
        noraise_rbrace_loc,
        noraise_semi_locs,
      ) = if self.peek_token() is NORAISE {
        let else_spos = self.peek_spos()
        self.skip()
        let lbrace_epos = self.peek_epos()
        // the else_loc is the location of `else {` or `noraise {`
        let else_loc : Location = { start: else_spos, end: lbrace_epos }
        let (
          try_else_cases,
          noraise_lbrace_loc,
          noraise_rbrace_loc,
          noraise_semi_locs,
        ) = self.parse_try_case_block_with_locs(invalid_delims=[TK_COMMA])
        (
          Some(try_else_cases),
          else_loc,
          Some(noraise_lbrace_loc),
          Some(noraise_rbrace_loc),
          noraise_semi_locs,
        )
      } else {
        (None, @syntax.no_location, None, None, @list.empty())
      }
      let loc = self.loc_start_with(spos)
      Try(
        body=e,
        catch_=catch_cases,
        try_else~,
        has_try=false,
        try_loc=catch_loc,
        catch_loc~,
        else_loc~,
        catch_lbrace_loc~,
        catch_rbrace_loc~,
        catch_semi_locs~,
        noraise_lbrace_loc~,
        noraise_rbrace_loc~,
        noraise_semi_locs~,
        loc~,
      )
    }
    _ => e
  }
}

///|
fn State::parse_arrow_fn_body(
  self : Self,
  explicit_brace_body~ : Bool,
) -> @syntax.Expr {
  if explicit_brace_body && self.peek_token() is LBRACE {
    self.parse_block_expr()
  } else {
    self.parse_expr_stmt_no_break_continue_return()
  }
}

///|
fn State::try_parse_arrow_fn_expr(
  self : State,
  explicit_brace_body? : Bool = false,
) -> Result[@syntax.Expr, @syntax.Expr?] {
  match self.peek_token() {
    LPAREN => {
      let spos = self.peek_spos()
      let lparen_loc = self.peek_location()
      self.skip()
      let (expr_or_binders, comma_locs_array, has_trailing) = self.series_with_follow_locs(
        delim=TK_COMMA,
        follow_set=[TK_RPAREN],
        State::parse_expr_or_binder,
      )
      let rparen_loc = self.peek_location()
      self.expect_token(TK_RPAREN)
      let comma_locs = @list.from_array(comma_locs_array)
      let params_loc = self.loc_start_with(spos)
      match (self.peek_token(), expr_or_binders) {
        (FAT_ARROW, _) => {
          let fat_arrow_loc = self.peek_location()
          self.skip()
          let parameters = expr_or_binders.map(fn(expr_or_binder) {
            let as_binder = fn(expr : @syntax.Expr) -> @syntax.ArrowFnParam {
              match expr {
                Ident(id={ name: Ident(name~), .. }, loc~) =>
                  Named({ name, loc })
                Hole(kind=Incomplete, loc~) => Unnamed(loc)
                Hole(kind=Synthesized, loc~) => Unnamed(loc)
                expr => {
                  let loc = expr.loc()
                  self.report_error({
                    loc,
                    msg: "Parse error, unexpected expression, you may expect parameter",
                  })
                  Unnamed(loc)
                }
              }
            }
            match expr_or_binder {
              Binder(expr, ty~, colon_loc=_) => (as_binder(expr), Some(ty))
              Expr(expr) => (as_binder(expr), None)
            }
          })
          let body = self.parse_arrow_fn_body(explicit_brace_body~)
          let loc = self.loc_start_with(spos)
          Ok(
            @syntax.Expr::Function(
              func=@syntax.make_arrow_fn(
                params_loc~,
                lparen_loc=Some(lparen_loc),
                rparen_loc=Some(rparen_loc),
                comma_locs~,
                fat_arrow_loc=Some(fat_arrow_loc),
                loc~,
                @list.from_array(parameters),
                body,
              ),
              loc~,
            ),
          )
        }
        (_, []) => {
          let loc = self.loc_start_with(spos)
          Err(
            Some(
              @syntax.Expr::Unit(loc~, faked=false, lparen_loc~, rparen_loc~),
            ),
          )
        }
        (_, [Binder(expr, ty~, colon_loc~)]) if !has_trailing => {
          let loc = self.loc_start_with(spos)
          Err(
            Some(
              @syntax.Expr::Constraint(
                expr~,
                ty~,
                lparen_loc=Some(lparen_loc),
                colon_loc~,
                rparen_loc=Some(rparen_loc),
                loc~,
              ),
            ),
          )
        }
        (_, [Expr(expr)]) => {
          let loc = self.loc_start_with(spos)
          Err(
            Some(
              @syntax.Group(
                expr~,
                group=Paren,
                left_delim_loc=lparen_loc,
                right_delim_loc=rparen_loc,
                loc~,
              ),
            ),
          )
        }
        (_, _) => {
          let exprs = expr_or_binders.map(fn(expr_or_binder) {
            match expr_or_binder {
              Binder(expr, colon_loc~, ty=_) => {
                self.report_unexpected(COLON, [TK_COMMA, TK_RPAREN], colon_loc)
                @syntax.Expr::Hole(kind=Synthesized, loc=expr.loc())
              }
              Expr(expr) => expr
            }
          })
          let loc = self.loc_start_with(spos)
          Err(
            Some(
              @syntax.Expr::Tuple(
                exprs=@list.from_array(exprs),
                lparen_loc~,
                rparen_loc~,
                comma_locs~,
                loc~,
              ),
            ),
          )
        }
      }
    }
    LIDENT(_) | UNDERSCORE if self.peek_token(nth=1) is FAT_ARROW => {
      let spos = self.peek_spos()
      let p : @syntax.ArrowFnParam = match self.peek_token() {
        LIDENT(_) => Named(self.parse_binder(context=""))
        UNDERSCORE => {
          self.expect_token(TK_UNDERSCORE)
          Unnamed(self.peek_location())
        }
        _ => panic()
      }
      let params_loc = self.loc_start_with(spos)
      self.expect_token(TK_FAT_ARROW)
      let body = self.parse_arrow_fn_body(explicit_brace_body~)
      let loc = self.loc_start_with(spos)
      Ok(
        @syntax.Expr::Function(
          loc~,
          func=@syntax.make_arrow_fn(
            params_loc~,
            loc~,
            @list.singleton((p, None)),
            body,
          ),
        ),
      )
    }
    _ => Err(None)
  }
}

///|
fn State::parse_pipe_expr(self : Self, parsed~ : @syntax.Expr?) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos_with_parsed(parsed~)
  let mut lhs = self.parse_infix_expr(parsed~)
  while self.peek_token() is (PIPE | PIPE_LEFT) {
    match self.peek_token() {
      PIPE => {
        let pipe_loc = self.peek_location()
        self.skip()
        let rhs = if self.peek_token() is LIDENT(binder) &&
          self.peek_token(nth=1) is FAT_ARROW {
          let binder_loc = self.peek_location()
          let spos = self.peek_spos()
          self.skip()
          self.skip()
          let body = if self.peek_token() is LBRACE {
            self.parse_block_expr()
          } else {
            let loc = self.peek_location()
            self.report_failed_to_parse(self.peek_token(), "`{ ... }`", loc)
            self.parse_infix_expr(parsed=None)
          }
          let loc = self.loc_start_with(spos)
          let binder = @syntax.Binder::{ name: binder, loc: binder_loc }
          let func = @syntax.make_arrow_fn(
            params_loc=binder_loc,
            loc~,
            @list.singleton((Named(binder), None)),
            body,
          )
          @syntax.Expr::Function(func~, loc~)
        } else {
          match self.try_parse_arrow_fn_expr() {
            Ok(fn_expr) => {
              // error recover: `(x) => e`, `e1 |> (x,y) => e2` or `e1 |> (x : Int) => e2`
              let loc = fn_expr.loc()
              ignore(loc)
              // self.report_unexpected_structure(found="form of anonymous function", expected="`x => { ... }`", loc~)
              fn_expr
            }
            Err(parsed) =>
              // allow `lhs |> infix_expr`, the typer will check infix_expr in a later stage.
              self.parse_infix_expr(parsed~)
          }
        }
        let loc = self.loc_start_with(spos)
        lhs = Pipe(lhs~, rhs~, pipe_loc~, loc~)
      }
      PIPE_LEFT => {
        let rev_pipe_loc = self.peek_location()
        self.skip()
        let rhs = match self.try_parse_arrow_fn_expr(explicit_brace_body=true) {
          Ok(fn_expr) => fn_expr
          Err(parsed) => self.parse_infix_expr(parsed~)
        }
        let loc = self.loc_start_with(spos)
        lhs = RevPipe(lhs~, rhs~, rev_pipe_loc~, loc~)
      }
      _ => panic()
    }
  }
  lhs
}

///|
fn State::parse_infix_expr(
  self : Self,
  parsed~ : @syntax.Expr?,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  fn make_infix(
    lpos : Position,
    lhs : @syntax.Expr,
    op : @syntax.Var,
    rhs : @syntax.Expr,
  ) -> (Position, @syntax.Expr) {
    let loc = self.loc_start_with(lpos)
    let expr : @syntax.Expr = match op.name {
      Ident(name="→") =>
        @syntax.Expr::Implies(lhs~, rhs~, implies_loc=op.loc, loc~)
      _ => @syntax.Expr::Infix(lhs~, op~, rhs~, loc~)
    }
    (lpos, expr)
  }

  fn push_infix(
    expr_stack : List[(Position, @syntax.Expr)],
    op_stack : List[(Token, @syntax.Var)],
    next_op : Token,
  ) -> (List[(Position, @syntax.Expr)], List[(Token, @syntax.Var)]) {
    match (expr_stack, op_stack) {
      (_, Empty) => (expr_stack, op_stack)
      (More(rhs, tail=More(lhs, tail=expr_stack2)), More(op, tail=op_stack2)) => {
        let (priority1, assoc1) = get_binop_prior_assoc(op.0)
        let (priority2, _) = get_binop_prior_assoc(next_op)
        if priority1 > priority2 || (priority1 == priority2 && assoc1 is Left) {
          let new_expr = make_infix(lhs.0, lhs.1, op.1, rhs.1)
          push_infix(expr_stack2.add(new_expr), op_stack2, next_op)
        } else {
          (expr_stack, op_stack)
        }
      }
      _ => panic()
    }
  }

  fn reduce_current_stack(
    expr_stack : List[(Position, @syntax.Expr)],
    op_stack : List[(Token, @syntax.Var)],
  ) -> (Position, @syntax.Expr) {
    match (expr_stack, op_stack) {
      (More(expr, ..), Empty) => expr
      (More(rhs, tail=More(lhs, tail=expr_stack2)), More(op, tail=op_stack2)) => {
        let new_expr = make_infix(lhs.0, lhs.1, op.1, rhs.1)
        reduce_current_stack(expr_stack2.add(new_expr), op_stack2)
      }
      _ => panic()
    }
  }

  fn go(
    expr_stack : List[(Position, @syntax.Expr)],
    op_stack : List[(Token, @syntax.Var)],
  ) -> (Position, @syntax.Expr) {
    if self.mode is Panic(_) {
      reduce_current_stack(expr_stack, op_stack)
    } else {
      let tok = self.peek_token()
      if !is_binop(tok) {
        reduce_current_stack(expr_stack, op_stack)
      } else {
        let (expr_stack, op_stack) = push_infix(expr_stack, op_stack, tok)
        let var_ = self.parse_binop()
        let rhs_spos = self.peek_spos()
        let new_rhs = self.parse_postfix_expr(parsed=None)
        go(expr_stack.add((rhs_spos, new_rhs)), op_stack.add((tok, var_)))
      }
    }
  }

  let first_spos = self.peek_spos_with_parsed(parsed~)
  let first_expr = self.parse_postfix_expr(parsed~)
  let (_, expr) = go(@list.from_array([(first_spos, first_expr)]), @list.new())
  expr
}

///|
fn State::parse_regex_match_binding(
  self : Self,
) -> (@syntax.Label, Location?, @syntax.Binder?) {
  match self.peek_token() {
    LIDENT(name) if self.peek_token(nth=1) is EQUAL => {
      let label : @syntax.Label = { name, loc: self.peek_location() }
      self.skip()
      let equal_loc = self.peek_location()
      self.expect_token(TK_EQUAL)
      let binder = self.parse_binder(context="regex match binding")
      (label, Some(equal_loc), Some(binder))
    }
    POST_LABEL(name) => {
      let label : @syntax.Label = { name, loc: self.peek_location() }
      self.skip()
      (label, None, None)
    }
    other => {
      let loc = self.peek_location()
      self.report_unexpected(other, [TK_LIDENT, TK_POST_LABEL], loc)
      self.panic() |> ignore
      ({ name: "", loc }, None, None)
    }
  }
}

///|
fn State::parse_regex_atom_pattern(self : Self) -> @syntax.RegexPattern {
  let spos = self.peek_spos()
  match self.peek_token() {
    REGEX_LITERAL(lit) => {
      let loc = self.peek_location()
      self.skip()
      @syntax.RegexPattern::Literal(lit~, loc~)
    }
    UIDENT(_) | PACKAGE_NAME(_) => {
      let lid = self.parse_qual_uident()
      let loc = self.loc_start_with(spos)
      @syntax.RegexPattern::Reference(lid~, loc~)
    }
    LPAREN => {
      self.skip()
      let pat = self.parse_regex_as_pattern()
      self.expect_token(TK_RPAREN)
      pat
    }
    other => {
      let loc = self.peek_location()
      self.report_failed_to_parse(other, "regex pattern", loc)
      self.panic() |> ignore
      @syntax.RegexPattern::Literal(lit="", loc~)
    }
  }
}

///|
fn State::parse_regex_sequence_pattern(self : Self) -> @syntax.RegexPattern {
  let spos = self.peek_spos()
  let mut pat = self.parse_regex_atom_pattern()
  while self.peek_token() is PLUS {
    let plus_loc = self.peek_location()
    self.skip()
    let rhs = self.parse_regex_atom_pattern()
    let loc = self.loc_start_with(spos)
    pat = @syntax.RegexPattern::Sequence(pat1=pat, pat2=rhs, plus_loc~, loc~)
  }
  pat
}

///|
fn State::parse_regex_or_pattern(self : Self) -> @syntax.RegexPattern {
  let spos = self.peek_spos()
  let mut pat = self.parse_regex_sequence_pattern()
  while self.peek_token() is BAR {
    let bar_loc = self.peek_location()
    self.skip()
    let rhs = self.parse_regex_sequence_pattern()
    let loc = self.loc_start_with(spos)
    pat = @syntax.RegexPattern::Alternation(pat1=pat, pat2=rhs, bar_loc~, loc~)
  }
  pat
}

///|
fn State::parse_regex_as_pattern(self : Self) -> @syntax.RegexPattern {
  let spos = self.peek_spos()
  let pat = self.parse_regex_or_pattern()
  if self.peek_token() is AS {
    let as_loc = self.peek_location()
    self.skip()
    let binder = self.parse_binder(context="regex alias")
    let loc = self.loc_start_with(spos)
    @syntax.RegexPattern::Alias(pat~, binder~, as_loc~, loc~)
  } else {
    pat
  }
}

///|
fn State::parse_regex_match_rhs(
  self : Self,
) -> (
  @syntax.RegexPattern,
  @list.List[(@syntax.Label, @syntax.Binder?)],
  Location?,
  Location?,
  @list.List[Location],
  @list.List[Location],
) {
  match self.peek_token() {
    LPAREN => {
      let rhs_lparen_loc = self.peek_location()
      self.skip()
      let pat = self.parse_regex_as_pattern()
      let (bindings, binding_comma_locs, binding_equal_locs) = if self.peek_token()
        is COMMA {
        let first_comma_loc = self.peek_location()
        self.skip()
        let (bindings_with_locs, comma_locs, _) = self.series_with_follow_locs(
          delim=TK_COMMA,
          follow_set=[TK_RPAREN],
          State::parse_regex_match_binding,
        )
        let bindings = []
        let equal_locs = []
        bindings_with_locs.each(binding => {
          let (label, equal_loc, binder) = binding
          bindings.push((label, binder))
          match equal_loc {
            Some(loc) => equal_locs.push(loc)
            None => ()
          }
        })
        (
          @list.from_array(bindings),
          @list.from_array([first_comma_loc, ..comma_locs]),
          @list.from_array(equal_locs),
        )
      } else {
        (@list.empty(), @list.empty(), @list.empty())
      }
      let rhs_rparen_loc = self.peek_location()
      self.expect_token(TK_RPAREN)
      (
        pat,
        bindings,
        Some(rhs_lparen_loc),
        Some(rhs_rparen_loc),
        binding_comma_locs,
        binding_equal_locs,
      )
    }
    _ =>
      (
        self.parse_regex_atom_pattern(),
        @list.empty(),
        None,
        None,
        @list.empty(),
        @list.empty(),
      )
  }
}

///|
fn State::parse_postfix_expr(
  self : Self,
  parsed~ : @syntax.Expr?,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos_with_parsed(parsed~)
  let expr = self.parse_range_expr(parsed~)
  match self.peek_token() {
    AS => {
      let as_loc = self.peek_location()
      self.skip()
      let trait_ = self.parse_type_name()
      let loc = self.loc_start_with(spos)
      As(expr~, trait_~, as_loc~, loc~)
    }
    IS => {
      let is_loc = self.peek_location()
      self.skip()
      let pat = self.parse_range_pattern()
      let loc = self.loc_start_with(spos)
      Is(expr~, pat~, is_loc~, loc~)
    }
    LEXMATCH_QUESTION => {
      let lexmatch_question_loc = self.peek_location()
      self.skip()
      let (pat, pat_loc) = self.parse_lex_pattern()
      let (strategy, with_loc) = self.parse_opt_with_strategy()
      IsLexMatch(
        expr~,
        pat~,
        pat_loc~,
        strategy~,
        lexmatch_question_loc~,
        with_loc~,
        loc=self.loc_start_with(spos),
      )
    }
    EQ_TILDE => {
      let eq_tilde_loc = self.peek_location()
      self.skip()
      let (
        pat,
        bindings,
        rhs_lparen_loc,
        rhs_rparen_loc,
        binding_comma_locs,
        binding_equal_locs,
      ) = self.parse_regex_match_rhs()
      let loc = self.loc_start_with(spos)
      RegexMatch(
        expr~,
        pat~,
        bindings~,
        eq_tilde_loc~,
        rhs_lparen_loc~,
        rhs_rparen_loc~,
        binding_comma_locs~,
        binding_equal_locs~,
        loc~,
      )
    }
    LT_PLUS => {
      let lt_plus_loc = self.peek_location()
      self.expect_token(TK_LT_PLUS)
      let template = self.parse_template_literal_expr()
      let loc = self.loc_start_with(spos)
      TemplateWriting(expr~, template~, lt_plus_loc~, loc~)
    }
    _ => expr
  }
}

///|
fn State::parse_range_expr(
  self : Self,
  parsed~ : @syntax.Expr?,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos_with_parsed(parsed~)
  let lhs = self.parse_prefix_expr(parsed~)
  let op_start = self.peek_spos()
  match self.peek_token() {
    RANGE_INCLUSIVE => {
      self.skip()
      let op_loc = self.loc_start_with(op_start)
      let rhs = self.parse_prefix_expr(parsed=None)
      let loc = self.loc_start_with(spos)
      let op : @syntax.Var = { name: Ident(name="..="), loc: op_loc }
      Infix(op~, lhs~, rhs~, loc~)
    }
    RANGE_LT_INCLUSIVE => {
      self.skip()
      let op_loc = self.loc_start_with(op_start)
      let rhs = self.parse_prefix_expr(parsed=None)
      let loc = self.loc_start_with(spos)
      let op : @syntax.Var = { name: Ident(name="..<="), loc: op_loc }
      @syntax.Expr::Infix(op~, lhs~, rhs~, loc~)
    }
    RANGE_EXCLUSIVE => {
      self.skip()
      let op_loc = self.loc_start_with(op_start)
      let rhs = self.parse_prefix_expr(parsed=None)
      let loc = self.loc_start_with(spos)
      let op : @syntax.Var = { name: Ident(name="..<"), loc: op_loc }
      Infix(op~, lhs~, rhs~, loc~)
    }
    RANGE_INCLUSIVE_REV => {
      self.skip()
      let op_loc = self.loc_start_with(op_start)
      let rhs = self.parse_prefix_expr(parsed=None)
      let loc = self.loc_start_with(spos)
      let op : @syntax.Var = { name: Ident(name=">=.."), loc: op_loc }
      @syntax.Expr::Infix(op~, lhs~, rhs~, loc~)
    }
    RANGE_EXCLUSIVE_REV => {
      self.skip()
      let op_loc = self.loc_start_with(op_start)
      let rhs = self.parse_prefix_expr(parsed=None)
      let loc = self.loc_start_with(spos)
      let op : @syntax.Var = { name: Ident(name=">.."), loc: op_loc }
      @syntax.Expr::Infix(op~, lhs~, rhs~, loc~)
    }
    _ => lhs
  }
}

///|
fn State::parse_prefix_expr(
  self : Self,
  parsed~ : @syntax.Expr?,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  match self.peek_token() {
    PLUS if parsed is None => {
      self.expect_token(TK_PLUS)
      let expr = self.parse_prefix_expr(parsed=None)
      let loc = self.loc_start_with(spos)
      @syntax.make_uplus(loc~, "+", expr)
    }
    MINUS if parsed is None => {
      self.expect_token(TK_MINUS)
      let expr = self.parse_prefix_expr(parsed=None)
      let loc = self.loc_start_with(spos)
      @syntax.make_uminus(loc~, "-", expr)
    }
    EXCLAMATION => {
      self.expect_token(TK_EXCLAMATION)
      let expr = self.parse_prefix_expr(parsed=None)
      let loc = self.loc_start_with(spos)
      @syntax.make_unot(loc~, expr)
    }
    _ => {
      fn go(left : @syntax.Expr) -> @syntax.Expr {
        match self.peek_token() {
          LPAREN => {
            let lparen_loc = self.peek_location()
            self.expect_token(TK_LPAREN)
            self.push_sync(TK_RPAREN)
            let (args, comma_locs, _) = self.series_with_follow_locs(
              delim=TK_COMMA,
              follow_set=[TK_RPAREN],
              State::parse_argument,
            )
            self.pop_sync(TK_RPAREN)
            let rparen_loc = self.peek_location()
            self.expect_token(TK_RPAREN)
            let loc = self.loc_start_with(spos)
            go(
              Apply(
                func=left,
                args=@list.from_array(args),
                lparen_loc~,
                rparen_loc~,
                comma_locs=@list.from_array(comma_locs),
                loc~,
              ),
            )
          }
          LBRACKET => {
            fn parse_index() {
              match self.peek_token() {
                COLON => {
                  let colon_loc = self.peek_location()
                  self.skip()
                  let end_index = self.optional(first_expr, State::parse_expr)
                  SliceIndex(None, end_index, colon_loc)
                }
                _ => {
                  let start_index = self.parse_expr()
                  match self.peek_token() {
                    COLON => {
                      let colon_loc = self.peek_location()
                      self.skip()
                      let end_index = self.optional(
                        first_expr,
                        State::parse_expr,
                      )
                      SliceIndex(Some(start_index), end_index, colon_loc)
                    }
                    _ => ArrayIndex(start_index)
                  }
                }
              }
            }

            let index_spos = self.peek_spos()
            let lbracket_loc = self.peek_location()
            self.expect_token(TK_LBRACKET)
            self.push_sync(TK_RBRACKET)
            let index = parse_index()
            self.pop_sync(TK_RBRACKET)
            let rbracket_loc = self.peek_location()
            self.expect_token(TK_RBRACKET)
            let loc = self.loc_start_with(spos)
            let index_loc = self.loc_start_with(index_spos)
            let array_get_expr : @syntax.Expr = match index {
              ArrayIndex(index) =>
                ArrayGet(loc~, array=left, index~, lbracket_loc~, rbracket_loc~)
              SliceIndex(start_index, end_index, colon_loc) =>
                ArrayGetSlice(
                  loc~,
                  index_loc~,
                  array=left,
                  start_index~,
                  end_index~,
                  lbracket_loc~,
                  colon_loc~,
                  rbracket_loc~,
                )
            }
            go(array_get_expr)
          }
          DOT_LIDENT(name) => {
            let label_loc = self.peek_location()
            let label : @syntax.Label = { name, loc: label_loc }
            self.skip()
            match self.peek_token() {
              LPAREN => {
                let lparen_loc = self.peek_location()
                self.expect_token(TK_LPAREN)
                self.push_sync(TK_RPAREN)
                let (args, comma_locs, _) = self.series_with_follow_locs(
                  delim=TK_COMMA,
                  follow_set=[TK_RPAREN],
                  State::parse_argument,
                )
                self.pop_sync(TK_RPAREN)
                let rparen_loc = self.peek_location()
                self.expect_token(TK_RPAREN)
                let loc = self.loc_start_with(spos)
                let expr : @syntax.Expr = DotApply(
                  self=left,
                  method_name=label,
                  args=@list.from_array(args),
                  return_self=false,
                  dot_loc=label_loc,
                  lparen_loc~,
                  rparen_loc~,
                  comma_locs=@list.from_array(comma_locs),
                  loc~,
                )
                go(expr)
              }
              _ => {
                let loc = self.loc_start_with(spos)
                let accessor : @syntax.Accessor = if label.name == "_" {
                  Newtype(loc~)
                } else {
                  Label(label)
                }
                go(Field(record=left, accessor~, loc~))
              }
            }
          }
          DOTDOT => {
            let dot_loc = self.peek_location()
            self.skip()
            let label = self.parse_label(context="method name")
            let lparen_loc = self.peek_location()
            self.expect_token(TK_LPAREN)
            self.push_sync(TK_RPAREN)
            let (args, comma_locs, _) = self.series_with_follow_locs(
              delim=TK_COMMA,
              follow_set=[TK_RPAREN],
              State::parse_argument,
            )
            self.pop_sync(TK_RPAREN)
            let rparen_loc = self.peek_location()
            self.expect_token(TK_RPAREN)
            let loc = self.loc_start_with(spos)
            let expr : @syntax.Expr = DotApply(
              self=left,
              method_name=label,
              args=@list.from_array(args),
              return_self=true,
              dot_loc~,
              lparen_loc~,
              rparen_loc~,
              comma_locs=@list.from_array(comma_locs),
              loc~,
            )
            go(expr)
          }
          DOT_INT(_) => {
            let accessor_loc = self.peek_location()
            let tuple_index = self.expect_dot_int()
            let accessor : @syntax.Accessor = Index(
              tuple_index~,
              loc=accessor_loc,
            )
            let loc = self.loc_start_with(spos)
            go(Field(record=left, accessor~, loc~))
          }
          _ => left
        }
      }

      go(self.parse_simple_expr(parsed~))
    }
  }
}

///|
fn State::parse_constr_expr(
  self : Self,
  spos~ : Position,
  extra_info~ : @syntax.ConstructorExtraInfo,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let name_loc_start = self.peek_spos()
  let constr_name = match extra_info {
    Package(..) | TypeNameWithConstrPackage(..) =>
      self.expect_dot_uident(context="constructor name")
    NoExtraInfo | TypeName(..) => self.expect_uident(context="constructor name")
  }
  let name_loc = self.loc_start_with(name_loc_start)
  let loc = self.loc_start_with(spos)
  let constr : @syntax.Constructor = {
    extra_info,
    name: { name: constr_name, loc: name_loc },
    loc,
  }
  // constructor with arguments is handled by [Apply],
  // and desugared during ast linting
  Constr(constr~, loc~)
}

///|
priv enum IndexingSyntaxFragment {
  ArrayIndex(@syntax.Expr)
  SliceIndex(@syntax.Expr?, @syntax.Expr?, Location)
}

///|
fn State::parse_argument(self : Self) -> @syntax.Argument {
  if self.mode is Panic(loc~, ..) {
    return { value: Hole(loc~, kind=Synthesized), kind: @syntax.Positional }
  }
  match self.peek_token() {
    // `label=expr` or `label?=expr`
    LIDENT(label_name) | UIDENT(label_name) if self.peek_token(nth=1) is EQUAL ||
      (self.peek_token(nth=1) is QUESTION && self.peek_token(nth=2) is EQUAL) => {
      let label : @syntax.Label = {
        name: label_name,
        loc: self.peek_location(),
      }
      self.skip() // luident
      let kind : @syntax.ArgumentKind = if self.peek_token() is QUESTION {
        let question_loc = self.peek_location()
        self.skip()
        let equal_loc = self.peek_location()
        LabelledOption(label~, question_loc~, equal_loc~)
      } else {
        let equal_loc = self.peek_location()
        Labelled(label~, equal_loc~)
      }
      self.expect_token(TK_EQUAL) // `=`
      let value = self.parse_expr()
      { value, kind }
    }
    // label~
    POST_LABEL(label_name) => {
      let spos = self.peek_spos()
      self.skip()
      let label : @syntax.Label = {
        name: label_name,
        loc: self.peek_location(),
      }
      // error recovery for `label~=value`
      if self.peek_token() is EQUAL {
        let equal_loc = self.peek_location()
        self.skip()
        let value = self.parse_expr()
        let loc = self.loc_start_with(spos)
        self.report_error({
          loc,
          msg: "Invalid syntax `label~=value`. Do you mean `label=value`?",
        })
        { value, kind: Labelled(label~, equal_loc~) }
      } else {
        let value = @syntax.label_to_expr(
          loc=Location::trim_last_char(label.loc),
          label,
        )
        { value, kind: LabelledPun(label) }
      }
    }
    // label?
    LIDENT(label_name) if self.peek_token(nth=1) is QUESTION &&
      (self.peek_token(nth=2) is COMMA || self.peek_token(nth=2) is RPAREN) => {
      let label : @syntax.Label = {
        name: label_name,
        loc: self.peek_location(),
      }
      self.expect_token(TK_LIDENT) // lident
      let question_loc = self.peek_location()
      self.expect_token(TK_QUESTION) // `?`
      let kind = @syntax.LabelledOptionPun(label~, question_loc~)
      let value = @syntax.label_to_expr(loc=label.loc, label)
      { value, kind }
    }
    _ => {
      let value = self.parse_expr()
      { value, kind: Positional }
    }
  }
}

///|
fn State::parse_func(
  self : Self,
  is_async~ : Location?,
  fn_loc~ : Location?,
) -> @syntax.Func {
  if self.mode is Panic(loc~, ..) {
    return @syntax.Func::{
      parameters: @list.new(),
      params_loc: loc,
      body: @syntax.Hole(loc~, kind=Synthesized),
      return_type: None,
      error_type: @syntax.NoErrorType,
      kind: Lambda,
      is_async,
      fn_loc,
      lparen_loc: None,
      rparen_loc: None,
      comma_locs: @list.empty(),
      return_arrow_loc: None,
      fat_arrow_loc: None,
      body_lbrace_loc: None,
      body_rbrace_loc: None,
      loc,
    }
  }
  match self.peek_token() {
    LPAREN => {
      let param_loc_start = self.peek_spos()
      let lparen_loc = self.peek_location()
      self.expect_token(TK_LPAREN)
      self.push_sync(TK_LBRACE)
      let (params, comma_locs, _) = self.series_with_follow_locs(
        delim=TK_COMMA,
        follow_set=[TK_RPAREN],
        fn(s) -> @syntax.Parameter { s.parse_parameter() },
      )
      let rparen_loc = self.peek_location()
      self.expect_token(TK_RPAREN)
      let params_loc = self.loc_start_with(param_loc_start)
      let (return_type, error_type, return_arrow_loc) = self.parse_func_return_type_with_loc()
      self.pop_sync(TK_LBRACE)
      let block = self.parse_block_expr_with_locs()
      let func_loc = match (is_async, fn_loc) {
        (Some(async_loc), _) => async_loc.merge(block.rbrace_loc)
        (_, Some(fn_loc)) => fn_loc.merge(block.rbrace_loc)
        _ => params_loc.merge(block.rbrace_loc)
      }
      @syntax.Func::{ // Will be set by caller
        parameters: @list.from_array(params),
        params_loc,
        body: block.expr,
        return_type,
        error_type,
        kind: Lambda,
        is_async,
        fn_loc,
        lparen_loc: Some(lparen_loc),
        rparen_loc: Some(rparen_loc),
        comma_locs: @list.from_array(comma_locs),
        return_arrow_loc,
        fat_arrow_loc: None,
        body_lbrace_loc: Some(block.lbrace_loc),
        body_rbrace_loc: Some(block.rbrace_loc),
        loc: func_loc,
      }
    }
    other => {
      self.report_error({
        loc: self.peek_location(),
        msg: "Failed to parse func: expected '(' or '{', got \{@debug.render(other.to_repr())}",
      })
      let loc = self.panic()
      @syntax.Func::{
        parameters: @list.new(),
        params_loc: loc,
        body: @syntax.Hole(loc~, kind=Synthesized),
        return_type: None,
        error_type: @syntax.NoErrorType,
        kind: Lambda,
        is_async,
        fn_loc,
        lparen_loc: None,
        rparen_loc: None,
        comma_locs: @list.empty(),
        return_arrow_loc: None,
        fat_arrow_loc: None,
        body_lbrace_loc: None,
        body_rbrace_loc: None,
        loc,
      }
    }
  }
}

///|
fn State::parse_parameter(self : Self) -> @syntax.Parameter {
  if self.mode is Panic(loc~, ..) {
    return @syntax.Positional(
      binder={ name: "?binder", loc },
      ty=None,
      colon_loc=None,
    )
  }
  match self.peek_token() {
    UNDERSCORE => {
      let loc = self.peek_location()
      self.skip()
      let (param_annot, colon_loc) = self.parse_opt_annot_with_loc()
      @syntax.DiscardPositional(ty=param_annot, colon_loc~, loc~)
    }
    // `label~ : Type` 
    // or `label~ : Type = expr` (same as `label? : Type = expr`, deprecated syntax)
    POST_LABEL(label_name) => {
      let post_label_loc = self.peek_location()
      let param_binder : @syntax.Binder = {
        name: label_name,
        loc: Location::trim_last_char(post_label_loc),
      }
      self.skip() // skip POST_LABEL
      let (param_annot, colon_loc) = self.parse_opt_annot_with_loc()
      match self.peek_token() {
        EQUAL if self.mode is Normal => {
          let equal_loc = self.peek_location()
          self.skip()
          let default = self.parse_expr()
          Optional(
            default~,
            binder=param_binder,
            ty=param_annot,
            post_label_loc=Some(post_label_loc),
            question_loc=None,
            colon_loc~,
            equal_loc~,
          )
        }
        _ =>
          Labelled(
            binder=param_binder,
            ty=param_annot,
            post_label_loc=Some(post_label_loc),
            colon_loc~,
          )
      }
    }
    // `label? : Type` or `label? : Type = default`
    LIDENT(label_name) if self.peek_token(nth=1) is QUESTION => {
      let param_binder : @syntax.Binder = {
        name: label_name,
        loc: self.peek_location(),
      }
      self.skip() // skip LIDENT
      let question_loc = self.peek_location()
      self.skip() // skip QUESTION
      let (param_annot, colon_loc) = self.parse_opt_annot_with_loc()
      match self.peek_token() {
        // default value
        EQUAL if self.mode is Normal => {
          let equal_loc = self.peek_location()
          self.skip()
          let default = self.parse_expr()
          Optional(
            default~,
            binder=param_binder,
            ty=param_annot,
            post_label_loc=None,
            question_loc=Some(question_loc),
            colon_loc~,
            equal_loc~,
          )
        }
        _ =>
          QuestionOptional(
            binder=param_binder,
            ty=param_annot,
            question_loc~,
            colon_loc~,
          )
      }
    }
    // `param : Type`
    LIDENT(param_name) => {
      let param_binder : @syntax.Binder = {
        name: param_name,
        loc: self.peek_location(),
      }
      self.skip()
      let (param_annot, colon_loc) = self.parse_opt_annot_with_loc()
      if self.peek_token() is EQUAL && self.mode is Normal {
        // error recovery for `param : Type = expr`
        let eq_loc = self.peek_location()
        self.skip()
        let default = self.parse_expr()
        self.report_error({
          loc: eq_loc,
          msg: "Only labeled argument can have default value",
        })
        Optional(
          default~,
          binder=param_binder,
          ty=param_annot,
          post_label_loc=None,
          question_loc=None,
          colon_loc~,
          equal_loc=eq_loc,
        )
      } else {
        Positional(binder=param_binder, ty=param_annot, colon_loc~)
      }
    }
    other => {
      let other_loc = self.peek_location()
      self.report_failed_to_parse(other, "parameter name or label", other_loc)
      self.panic() |> ignore
      Positional(
        binder={ name: "?binder", loc: @syntax.no_location },
        ty=None,
        colon_loc=None,
      )
    }
  }
}

///|
fn State::parse_simple_expr(
  self : Self,
  parsed~ : @syntax.Expr?,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  if parsed is Some(e) {
    return e
  }
  match self.peek_token() {
    TRUE => {
      let loc = self.peek_location()
      self.expect_token(TK_TRUE)
      Constant(c=Bool(true), loc~)
    }
    FALSE => {
      let loc = self.peek_location()
      self.expect_token(TK_FALSE)
      Constant(c=Bool(false), loc~)
    }
    BYTE(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_BYTE)
      Constant(c=Byte(x), loc~)
    }
    CHAR(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_CHAR)
      Constant(c=Char(x), loc~)
    }
    INT(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_INT)
      Constant(c=@syntax.make_int(x), loc~)
    }
    FLOAT(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_FLOAT)
      Constant(c=@syntax.make_float(x), loc~)
    }
    DOUBLE(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_DOUBLE)
      Constant(c=@syntax.make_double(x), loc~)
    }
    STRING(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_STRING)
      Constant(c=String(x), loc~)
    }
    REGEX_LITERAL(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_REGEX_LITERAL)
      Constant(c=Regex(x), loc~)
    }
    BYTES(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_BYTES)
      Constant(c=Bytes(x), loc~)
    }
    MULTILINE_STRING(_) | MULTILINE_INTERP(_) => {
      let spos = self.peek_spos()
      let elems : Array[@syntax.MultilineStringElem] = []
      while true {
        match self.peek_token() {
          MULTILINE_STRING(x) => {
            self.expect_token(TK_MULTILINE_STRING)
            elems.push(String(x))
          }
          MULTILINE_INTERP(xs) => {
            self.expect_token(TK_MULTILINE_INTERP)
            let converted : Array[@syntax.InterpElem] = xs.map(fn(elem) {
              match elem {
                InterpLit(repr~, loc~) =>
                  @syntax.InterpElem::Literal(repr~, loc~)
                InterpSource(source) => @syntax.InterpElem::Source(source)
              }
            })
            elems.push(Interp(@list.from_array(converted)))
          }
          _ => break
        }
      }
      let loc = self.loc_start_with(spos)
      MultilineString(elems=@list.from_array(elems), loc~)
    }
    INTERP(xs) => {
      let loc = self.peek_location()
      self.skip()
      @syntax.make_interp_expr(loc~, xs)
    }
    PACKAGE_NAME(pkg) =>
      match self.peek_token(nth=1) {
        // @pkg.Type::
        DOT_LIDENT(_) | DOT_UIDENT(_) if self.peek_token(nth=2) is COLONCOLON =>
          self.parse_coloncolon_expr()
        // @pkg.Constr
        DOT_UIDENT(_) => {
          let spos = self.peek_spos()
          let pkg_loc = self.peek_location()
          self.skip()
          let dot_constr_loc = self.peek_location()
          self.parse_constr_expr(
            spos~,
            extra_info=Package(pkg~, pkg_loc~, dot_constr_loc~),
          )
        }
        // @pkg.f
        _ => {
          let spos = self.peek_spos()
          let name = self.parse_qual_lident(context="variable name")
          let loc = self.loc_start_with(spos)
          Ident(id={ name, loc }, loc~)
        }
      }
    LIDENT(_) | UIDENT(_) if self.peek_token(nth=1) is COLONCOLON =>
      self.parse_coloncolon_expr()
    AMPER => self.parse_coloncolon_expr()
    LIDENT(_) => {
      let spos = self.peek_spos()
      let var_ = self.parse_var()
      let loc = self.loc_start_with(spos)
      Ident(id=var_, loc~)
    }
    // Cons(x,y)
    UIDENT(_) => {
      let spos = self.peek_spos()
      self.parse_constr_expr(spos~, extra_info=NoExtraInfo)
    }
    ASYNC => {
      let is_async = Some(self.peek_location())
      let spos = self.peek_spos()
      self.expect_token(TK_ASYNC) // async
      // TODO: improve error recovery for case like `async \n trait`
      let fn_loc = self.peek_location()
      self.with_syncs([TK_LPAREN, TK_LBRACE], self => self.expect_token(TK_FN)) // fn
      let func = self.parse_func(is_async~, fn_loc=Some(fn_loc))
      let loc = self.loc_start_with(spos)
      Function(func~, loc~)
    }
    FN => {
      let spos = self.peek_spos()
      let fn_loc = self.peek_location()
      self.expect_token(TK_FN)
      let func = self.parse_func(is_async=None, fn_loc=Some(fn_loc))
      let loc = self.loc_start_with(spos)
      Function(func~, loc~)
    }
    LPAREN => self.parse_tuple_or_constraint()
    LBRACKET => {
      if self.peek_token(nth=1) is FOR {
        return self.parse_list_comprehension()
      }
      let spos = self.peek_spos()
      let mut to_spread = false
      let lbracket_loc = self.peek_location()
      self.expect_token(TK_LBRACKET)
      self.push_sync(TK_RBRACKET)
      let (elems, comma_locs, _) = self.series_with_follow_locs(
        delim=TK_COMMA,
        follow_set=[TK_RBRACKET],
        fn(state) -> @syntax.SpreadableElem {
          match state.peek_token() {
            DOTDOT => {
              to_spread = true
              let spread_spos = state.peek_spos()
              let dotdot_loc = state.peek_location()
              state.expect_token(TK_DOTDOT)
              let expr = state.parse_expr()
              let loc = state.loc_start_with(spread_spos)
              Spread(expr~, dotdot_loc~, loc~)
            }
            _ => Regular(state.parse_expr())
          }
        },
      )
      self.pop_sync(TK_RBRACKET)
      let rbracket_loc = self.peek_location()
      self.expect_token(TK_RBRACKET)
      let elems = @list.from_array(elems)
      let comma_locs = @list.from_array(comma_locs)
      let loc = self.loc_start_with(spos)
      if to_spread {
        ArraySpread(elems~, lbracket_loc~, rbracket_loc~, comma_locs~, loc~)
      } else {
        let exprs = elems.map(elem => {
          match elem {
            Regular(e) => e
            Spread(..) => panic()
          }
        })
        Array(exprs~, lbracket_loc~, rbracket_loc~, comma_locs~, loc~)
      }
    }
    LBRACE => {
      // to parse a block expr or a record expr ?
      // things got a little bit tricky here ...
      let spos = self.peek_spos()
      match self.peek_token(nth=1) {
        DOTDOT => self.parse_record_expr(type_name=None, spos~)
        LIDENT(_) =>
          match self.peek_token(nth=2) {
            // In cases `{ x: ... }` `{ x, }` `{ x; }` `{ x }` the result is a record expression.
            SEMI(_) if self.peek_token(nth=3) is RBRACE =>
              self.parse_semi_trailing_record()
            COLON | COMMA | RBRACE =>
              self.parse_record_expr(type_name=None, spos~)
            _ => {
              let block = self.parse_block_expr_with_locs()
              let loc = self.loc_start_with(spos)
              Group(
                expr=block.expr,
                group=Brace,
                left_delim_loc=block.lbrace_loc,
                right_delim_loc=block.rbrace_loc,
                loc~,
              )
            }
          }
        RBRACE => self.parse_map_expr()
        TRUE
        | FALSE
        | CHAR(_)
        | INT(_)
        | BYTE(_)
        | DOUBLE(_)
        | FLOAT(_)
        | MINUS
        | BYTES(_)
        | STRING(_)
        | REGEX_LITERAL(_) if self.peek_token(nth=2) is COLON =>
          self.parse_map_expr()
        _ => {
          let block = self.parse_block_expr_with_locs()
          let loc = self.loc_start_with(spos)
          Group(
            expr=block.expr,
            group=Brace,
            left_delim_loc=block.lbrace_loc,
            right_delim_loc=block.rbrace_loc,
            loc~,
          )
        }
      }
    }
    UNDERSCORE => {
      let loc = self.peek_location()
      self.expect_token(TK_UNDERSCORE)
      Hole(loc~, kind=Incomplete)
    }
    other => {
      let other_loc = self.peek_location()
      self.report_failed_to_parse(other, "simple expression", other_loc)
      let loc = self.panic()
      Hole(loc~, kind=Synthesized)
    }
  }
}

///|
fn State::parse_coloncolon_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let type_name = self.parse_type_name()
  let coloncolon_loc = self.peek_location()
  self.expect_token(TK_COLONCOLON)
  match self.peek_token() {
    LIDENT(_) => {
      // TypeName::method
      let method_name = self.parse_label(context="")
      let loc = self.loc_start_with(spos)
      Method(type_name~, method_name~, coloncolon_loc~, loc~)
    }
    UIDENT(_) =>
      self.parse_constr_expr(
        spos~,
        extra_info=TypeName(type_name~, coloncolon_loc~),
      )
    PACKAGE_NAME(pkg) if self.peek_token(nth=1) is DOT_UIDENT(_) => {
      let pkg_loc = self.peek_location()
      self.skip()
      let dot_constr_loc = self.peek_location()
      self.parse_constr_expr(
        spos~,
        extra_info=TypeNameWithConstrPackage(
          type_name~,
          pkg~,
          coloncolon_loc~,
          pkg_loc~,
          dot_constr_loc~,
        ),
      )
    }
    LBRACE =>
      self.parse_record_expr(
        type_name=Some(type_name),
        coloncolon_loc=Some(coloncolon_loc),
        spos~,
      )
    other => {
      self.report_failed_to_parse(
        other,
        "method name, constructor name, \"[\" or \"{\"",
        self.peek_location(),
      )
      let skipped_loc = self.panic()
      let loc : Location = { start: spos, end: skipped_loc.end }
      @syntax.Constr(
        constr={
          extra_info: TypeName(type_name~, coloncolon_loc=@syntax.no_location),
          name: { name: "?constr", loc: @syntax.no_location },
          loc,
        },
        loc~,
      )
    }
  }
}

///|
fn State::parse_template_literal_expr(self : Self) -> @syntax.Expr {
  match self.peek_token() {
    STRING(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_STRING)
      Constant(c=String(x), loc~)
    }
    INTERP(xs) => {
      let loc = self.peek_location()
      self.skip()
      @syntax.make_interp_expr(loc~, xs)
    }
    MULTILINE_STRING(_) | MULTILINE_INTERP(_) => {
      let spos = self.peek_spos()
      let elems : Array[@syntax.MultilineStringElem] = []
      while true {
        match self.peek_token() {
          MULTILINE_STRING(x) => {
            self.expect_token(TK_MULTILINE_STRING)
            elems.push(String(x))
          }
          MULTILINE_INTERP(xs) => {
            self.expect_token(TK_MULTILINE_INTERP)
            let converted : Array[@syntax.InterpElem] = xs.map(fn(elem) {
              match elem {
                InterpLit(repr~, loc~) =>
                  @syntax.InterpElem::Literal(repr~, loc~)
                InterpSource(source) => @syntax.InterpElem::Source(source)
              }
            })
            elems.push(Interp(@list.from_array(converted)))
          }
          _ => break
        }
      }
      let loc = self.loc_start_with(spos)
      MultilineString(elems=@list.from_array(elems), loc~)
    }
    other => {
      let loc = self.peek_location()
      self.report_failed_to_parse(other, "template string", loc)
      self.panic() |> ignore
      Hole(loc~, kind=Synthesized)
    }
  }
}

///|
fn State::next_is_foreach_comprehension(self : Self) -> Bool {
  let mut nth = 0
  match self.peek_token(nth~) {
    LIDENT(_) | UNDERSCORE => ()
    _ => return false
  }
  while true {
    nth = nth + 1
    match self.peek_token(nth~) {
      IN => return true
      COMMA => {
        nth = nth + 1
        match self.peek_token(nth~) {
          LIDENT(_) | UNDERSCORE => continue
          _ => return false
        }
      }
      _ => return false
    }
  }
  false
}

///|
fn State::parse_list_comprehension(self : Self) -> @syntax.Expr {
  let spos = self.peek_spos()
  let lbracket_loc = self.peek_location()
  self.expect_token(TK_LBRACKET)
  let for_loc = self.peek_location()
  self.expect_token(TK_FOR)
  let kind : @syntax.ListComprehensionKind = if self.next_is_foreach_comprehension() {
    let (binder_array, binder_comma_locs_array, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_IN],
      State::parse_foreach_binder,
    )
    let binders = @list.from_array(binder_array)
    let binder_comma_locs = @list.from_array(binder_comma_locs_array)
    let in_loc = self.peek_location()
    self.expect_token(TK_IN)
    let expr = self.parse_infix_expr(parsed=None)
    let semi_locs_array = []
    let init_comma_locs_array = []
    let init_equal_locs_array = []
    let continue_comma_locs_array = []
    let continue_equal_locs_array = []
    let (init, continue_block) = if self.peek_token() is SEMI(_) {
      semi_locs_array.push(self.peek_location())
      self.skip()
      let (init_with_locs, init_comma_locs, _) = self.series_with_follow_locs(
        delim=TK_COMMA,
        follow_set=[TK_SEMI, TK_IF, TK_FAT_ARROW],
        State::parse_loop_binder_with_infix_value_and_equal_loc,
      )
      let (init, init_equal_locs) = split_loop_bindings_with_equal_locs(
        init_with_locs,
      )
      init_comma_locs.each(loc => init_comma_locs_array.push(loc))
      init_equal_locs.each(loc => init_equal_locs_array.push(loc))
      let continue_block = if self.peek_token() is SEMI(_) {
        semi_locs_array.push(self.peek_location())
        self.skip()
        let (continue_with_locs, continue_comma_locs, _) = self.series_with_follow_locs(
          delim=TK_COMMA,
          follow_set=[TK_IF, TK_FAT_ARROW],
          State::parse_loop_binder_with_infix_value_and_equal_loc,
        )
        let (continue_block, continue_equal_locs) = split_loop_bindings_with_equal_locs(
          continue_with_locs,
        )
        continue_comma_locs.each(loc => continue_comma_locs_array.push(loc))
        continue_equal_locs.each(loc => continue_equal_locs_array.push(loc))
        continue_block
      } else {
        @list.empty()
      }
      (init, continue_block)
    } else {
      (@list.empty(), @list.empty())
    }
    Foreach(
      binders~,
      expr~,
      init~,
      continue_block~,
      for_loc~,
      in_loc~,
      binder_comma_locs~,
      semi_locs=@list.from_array(semi_locs_array),
      init_comma_locs=@list.from_array(init_comma_locs_array),
      init_equal_locs=@list.from_array(init_equal_locs_array),
      continue_comma_locs=@list.from_array(continue_comma_locs_array),
      continue_equal_locs=@list.from_array(continue_equal_locs_array),
    )
  } else {
    let (bindings_with_locs, binder_comma_locs_array, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_SEMI, TK_IF, TK_FAT_ARROW],
      State::parse_loop_binder_with_infix_value_and_equal_loc,
    )
    let (binders, binder_equal_locs) = split_loop_bindings_with_equal_locs(
      bindings_with_locs,
    )
    let semi_locs_array = []
    let continue_comma_locs_array = []
    let continue_equal_locs_array = []
    let (condition, continue_block) = if self.peek_token() is SEMI(_) {
      semi_locs_array.push(self.peek_location())
      self.skip()
      let condition = match self.peek_token() {
        SEMI(_) | IF | FAT_ARROW => None
        _ => Some(self.parse_infix_expr(parsed=None))
      }
      let continue_block = if self.peek_token() is SEMI(_) {
        semi_locs_array.push(self.peek_location())
        self.skip()
        let (continue_with_locs, continue_comma_locs, _) = self.series_with_follow_locs(
          delim=TK_COMMA,
          follow_set=[TK_IF, TK_FAT_ARROW],
          State::parse_loop_binder_with_infix_value_and_equal_loc,
        )
        let (continue_block, continue_equal_locs) = split_loop_bindings_with_equal_locs(
          continue_with_locs,
        )
        continue_comma_locs.each(loc => continue_comma_locs_array.push(loc))
        continue_equal_locs.each(loc => continue_equal_locs_array.push(loc))
        continue_block
      } else {
        @list.empty()
      }
      (condition, continue_block)
    } else {
      (None, @list.empty())
    }
    For(
      binders~,
      condition~,
      continue_block~,
      for_loc~,
      binder_comma_locs=@list.from_array(binder_comma_locs_array),
      semi_locs=@list.from_array(semi_locs_array),
      binder_equal_locs~,
      continue_comma_locs=@list.from_array(continue_comma_locs_array),
      continue_equal_locs=@list.from_array(continue_equal_locs_array),
    )
  }
  let mut if_loc = None
  let guard_ = if self.peek_token() is IF {
    if_loc = Some(self.peek_location())
    self.skip()
    Some(self.parse_infix_expr(parsed=None))
  } else {
    None
  }
  let fat_arrow_loc = self.peek_location()
  self.expect_token(TK_FAT_ARROW)
  let body = self.parse_expr()
  let rbracket_loc = self.peek_location()
  self.expect_token(TK_RBRACKET)
  let loc = self.loc_start_with(spos)
  ListComprehension(
    kind~,
    guard_~,
    body~,
    lbracket_loc~,
    rbracket_loc~,
    if_loc~,
    fat_arrow_loc~,
    loc~,
  )
}

///|
fn State::parse_field_def(self : Self) -> @syntax.FieldDef {
  if self.mode is Panic(loc~, ..) {
    return {
      label: { name: "?", loc },
      expr: Hole(loc~, kind=Synthesized),
      is_pun: false,
      colon_loc: None,
      loc,
    }
  }
  let spos = self.peek_spos()
  let label = self.parse_label(context="struct field")
  let expr_opt = match self.peek_token() {
    EQUAL => {
      // Error production for `field = expr` in struct expression
      let loc = self.peek_location()
      self.report_error({
        loc,
        msg: "Invalid equal sign in struct expression, use `:` instead",
      })
      self.skip() // skip =
      Some((self.parse_expr(), None))
    }
    COLON => {
      let colon_loc = self.peek_location()
      self.skip() // skip :
      Some((self.parse_expr(), Some(colon_loc)))
    }
    _ => None
  }
  let loc = self.loc_start_with(spos)
  match expr_opt {
    Some((expr, colon_loc)) => { label, expr, is_pun: false, colon_loc, loc }
    None => {
      let expr = @syntax.label_to_expr(loc=label.loc, label)
      { label, expr, is_pun: true, colon_loc: None, loc }
    }
  }
}

///|
fn State::parse_semi_trailing_record(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  let field = self.parse_field_def()
  let semi_loc = self.peek_location()
  self.expect_token(TK_SEMI)
  let rbrace_loc = self.peek_location()
  self.expect_token(TK_RBRACE)
  let loc = self.loc_start_with(spos)
  Record(
    type_name=None,
    fields=@list.from_array([field]),
    trailing=Semi,
    coloncolon_loc=None,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs=@list.empty(),
    semi_locs=@list.singleton(semi_loc),
    loc~,
  )
}

///|
fn State::parse_record_expr(
  self : Self,
  spos~ : Position,
  type_name~ : @syntax.TypeName?,
  coloncolon_loc? : Location? = None,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  let (spread_record, dotdot_loc, fields, trailing_comma, comma_locs) = if self.peek_token()
    is DOTDOT {
    let dotdot_loc = self.peek_location()
    self.skip()
    let spread_record = self.parse_expr()
    let (fields, trailing, comma_locs) = if self.peek_token() is COMMA {
      let first_comma_loc = self.peek_location()
      self.expect_token(TK_COMMA)
      let (fields, comma_locs, trailing) = self.series_with_follow_locs(
        delim=TK_COMMA,
        follow_set=[TK_RBRACE],
        State::parse_field_def,
      )
      (
        @list.from_array(fields),
        trailing,
        @list.from_array([first_comma_loc, ..comma_locs]),
      )
    } else {
      (@list.new(), false, @list.empty())
    }
    (Some(spread_record), Some(dotdot_loc), fields, trailing, comma_locs)
  } else {
    let (fields, comma_locs, trailing_semi) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RBRACE],
      State::parse_field_def,
    )
    (
      None,
      None,
      @list.from_array(fields),
      trailing_semi,
      @list.from_array(comma_locs),
    )
  }
  let rbrace_loc = self.peek_location()
  self.expect_token(TK_RBRACE)
  let loc = self.loc_start_with(spos)
  match spread_record {
    Some(record) =>
      RecordUpdate(
        type_name~,
        record~,
        fields~,
        coloncolon_loc~,
        lbrace_loc~,
        dotdot_loc=dotdot_loc.unwrap(),
        rbrace_loc~,
        comma_locs~,
        loc~,
      )
    None => {
      let trailing : @syntax.TrailingMark = if trailing_comma {
        Comma
      } else {
        None
      }
      Record(
        type_name~,
        fields~,
        trailing~,
        coloncolon_loc~,
        lbrace_loc~,
        rbrace_loc~,
        comma_locs~,
        semi_locs=@list.empty(),
        loc~,
      )
    }
  }
}

///|
fn State::parse_map_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  fn parse_elem(state : State) -> @syntax.MapExprElem? {
    let spos = state.peek_spos()
    fn parse_map_expr_elem(key : @syntax.Constant) -> @syntax.MapExprElem? {
      state.skip()
      let key_loc = state.loc_start_with(spos)
      let colon_loc = state.peek_location()
      state.expect_token(TK_COLON)
      let expr = state.parse_expr()
      let loc = state.loc_start_with(spos)
      Some({ key, expr, colon_loc, key_loc, loc })
    }

    match state.peek_token() {
      TRUE => parse_map_expr_elem(Bool(true))
      FALSE => parse_map_expr_elem(Bool(false))
      CHAR(x) => parse_map_expr_elem(Char(x))
      INT(x) => parse_map_expr_elem(@syntax.make_int(x))
      BYTE(x) => parse_map_expr_elem(Byte(x))
      FLOAT(x) => parse_map_expr_elem(@syntax.make_float(x))
      DOUBLE(x) => parse_map_expr_elem(@syntax.make_double(x))
      MINUS => {
        state.skip()
        match state.peek_token() {
          INT(x) => parse_map_expr_elem(@syntax.make_int("-" + x))
          FLOAT(x) => parse_map_expr_elem(@syntax.make_float("-" + x))
          DOUBLE(x) => parse_map_expr_elem(@syntax.make_double("-" + x))
          other => {
            let other_loc = state.peek_location()
            state.report_failed_to_parse(
              other, "`-Int` or `-Float` in key of map pattern", other_loc,
            )
            state.panic() |> ignore
            None
          }
        }
      }
      BYTES(x) => parse_map_expr_elem(Bytes(x))
      STRING(x) => parse_map_expr_elem(String(x))
      REGEX_LITERAL(x) => parse_map_expr_elem(Regex(x))
      other => {
        let other_loc = state.peek_location()
        state.report_unexpected(other, first_map_pattern_key, other_loc)
        state.panic() |> ignore
        None
      }
    }
  }

  let spos = self.peek_spos()
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  self.push_sync(TK_RBRACE)
  let (elems, comma_locs, _) = self.series_with_follow_locs(
    delim=TK_COMMA,
    follow_set=[TK_RBRACE],
    parse_elem,
  )
  self.pop_sync(TK_RBRACE)
  let rbrace_loc = self.peek_location()
  self.expect_token(TK_RBRACE)
  let elems = elems.filter_map(elem => elem) // TODO: add catMaybes to core?
  let loc = self.loc_start_with(spos)
  Map(
    elems=@list.from_array(elems),
    lbrace_loc~,
    rbrace_loc~,
    comma_locs=@list.from_array(comma_locs),
    loc~,
  )
}

///|
fn State::parse_opt_annot(self : Self) -> @syntax.Type? {
  self.parse_opt_annot_with_loc().0
}

///|
fn State::parse_opt_annot_with_loc(self : Self) -> (@syntax.Type?, Location?) {
  if self.mode is Panic(..) {
    return (None, None)
  }
  if self.peek_token() is COLON {
    let colon_loc = self.peek_location()
    self.skip()
    (Some(self.parse_type()), Some(colon_loc))
  } else {
    (None, None)
  }
}

///|
fn State::parse_block_expr(self : Self) -> @syntax.Expr {
  self.parse_block_expr_with_locs().expr
}

///|
fn State::parse_block_expr_with_locs(self : Self) -> BlockExpr {
  if self.mode is Panic(loc~, ..) {
    return {
      expr: @syntax.Hole(loc~, kind=Synthesized),
      lbrace_loc: loc,
      rbrace_loc: loc,
    }
  }
  let loc_start = self.peek_spos()
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  let stmts = []
  self.push_sync(TK_RBRACE)
  while self.mode is Normal && self.peek_kind() != TK_RBRACE {
    self.push_sync(TK_SEMI)
    let stmt = self.parse_stmt()
    self.pop_sync(TK_SEMI)
    let stmt = if self.peek_token() is SEMI(is_real) {
      let semi_loc = if is_real { Some(self.peek_location()) } else { None }
      self.expect_token(TK_SEMI)
      stmt.with_semi_loc(semi_loc)
    } else {
      stmt
    }
    stmts.push(stmt)
  }
  self.pop_sync(TK_RBRACE)
  let rbrace_loc = self.peek_location()
  self.expect_token(TK_RBRACE)
  let loc = self.loc_start_with(loc_start)
  let expr = Statement::compact_rev(@list.from_array(stmts).rev(), loc~)
  { expr, lbrace_loc, rbrace_loc }
}

///|
fn State::parse_letand_func(self : Self) -> @syntax.Func {
  if self.mode is Panic(loc~, ..) {
    return @syntax.Func::{
      parameters: @list.new(),
      params_loc: loc,
      body: Hole(loc~, kind=Synthesized),
      return_type: None,
      error_type: NoErrorType,
      kind: Lambda,
      is_async: None,
      fn_loc: None,
      lparen_loc: None,
      rparen_loc: None,
      comma_locs: @list.empty(),
      return_arrow_loc: None,
      fat_arrow_loc: None,
      body_lbrace_loc: None,
      body_rbrace_loc: None,
      loc,
    }
  }
  fn parse_arrow_fn_param() -> @syntax.ArrowFnParam {
    if self.peek_token() is UNDERSCORE {
      let loc = self.peek_location()
      self.expect_token(TK_UNDERSCORE)
      Unnamed(loc)
    } else {
      let binder = self.parse_binder(context="function parameter")
      Named(binder)
    }
  }

  let spos = self.peek_spos()
  match self.peek_token() {
    LPAREN => {
      let lparen_loc = self.peek_location()
      self.expect_token(TK_LPAREN)
      let (parameters, comma_locs, _) = self.series_with_follow_locs(
        delim=TK_COMMA,
        follow_set=[TK_RPAREN],
        fn(self) -> (@syntax.ArrowFnParam, @syntax.Type?) {
          let param = parse_arrow_fn_param()
          let ty_opt = self.parse_opt_annot()
          (param, ty_opt)
        },
      )
      let rparen_loc = self.peek_location()
      self.expect_token(TK_RPAREN)
      let params_loc = self.loc_start_with(spos)
      let fat_arrow_loc = self.peek_location()
      self.expect_token(TK_FAT_ARROW)
      let body = self.parse_expr_stmt_no_break_continue_return()
      let loc = self.loc_start_with(spos)
      @syntax.make_arrow_fn(
        @list.from_array(parameters),
        body,
        params_loc~,
        lparen_loc=Some(lparen_loc),
        rparen_loc=Some(rparen_loc),
        comma_locs=@list.from_array(comma_locs),
        fat_arrow_loc=Some(fat_arrow_loc),
        loc~,
      )
    }
    LIDENT(_) | UNDERSCORE => {
      let param = parse_arrow_fn_param()
      let params_loc = self.loc_start_with(spos)
      let fat_arrow_loc = self.peek_location()
      self.expect_token(TK_FAT_ARROW)
      let body = self.parse_expr_stmt_no_break_continue_return()
      let loc = self.loc_start_with(spos)
      @syntax.make_arrow_fn(
        @list.from_array([(param, None)]),
        body,
        params_loc~,
        fat_arrow_loc=Some(fat_arrow_loc),
        loc~,
      )
    }
    _ => {
      let is_async = if self.peek_token() is ASYNC {
        let loc = self.peek_location()
        self.expect_token(TK_ASYNC)
        Some(loc)
      } else {
        None
      }
      let fn_loc = self.peek_location()
      self.with_syncs([TK_LPAREN, TK_LBRACE], self => self.expect_token(TK_FN)) // fn
      self.parse_func(is_async~, fn_loc=Some(fn_loc))
    }
  }
}

///|
fn State::peek_explicit_semi_loc(self : Self) -> Location? {
  match self.peek_token() {
    SEMI(true) => Some(self.peek_location())
    _ => None
  }
}

///|
fn State::parse_stmt(self : Self) -> Statement {
  if self.mode is Panic(loc~, ..) {
    return Statement::StmtExpr(expr=Hole(loc~, kind=Synthesized), semi_loc=None)
  }
  match self.peek_token() {
    LET => {
      let spos = self.peek_spos()
      let let_loc = self.peek_location()
      self.expect_token(TK_LET)
      if self.peek_token() is MUTABLE {
        let mut_loc = self.peek_location()
        self.expect_token(TK_MUTABLE)
        let binder = self.parse_binder(context="variable name")
        let (ty_opt, annot_colon_loc) = self.parse_opt_annot_with_loc()
        let equal_loc = self.peek_location()
        self.expect_token(TK_EQUAL)
        let expr = self.parse_expr()
        let loc = self.loc_start_with(spos)
        StmtLetmut(
          binder~,
          ty_opt~,
          expr~,
          let_loc~,
          mut_loc~,
          annot_colon_loc~,
          equal_loc~,
          semi_loc=self.peek_explicit_semi_loc(),
          loc~,
        )
      } else {
        let pat = self.parse_pattern()
        let (ty_opt, annot_colon_loc) = self.parse_opt_annot_with_loc()
        let pat : @syntax.Pattern = match ty_opt {
          Some(ty) => {
            let colon_loc = match annot_colon_loc {
              Some(loc) => loc
              None => @syntax.no_location
            }
            Constraint(
              pat~,
              ty~,
              lparen_loc=None,
              colon_loc~,
              rparen_loc=None,
              loc=pat.loc().merge(ty.loc()),
            )
          }
          None => pat
        }
        let equal_loc = self.peek_location()
        self.expect_token(TK_EQUAL)
        let expr = self.parse_expr()
        let loc = self.loc_start_with(spos)
        StmtLet(
          pat~,
          expr~,
          let_loc~,
          annot_colon_loc~,
          equal_loc~,
          semi_loc=self.peek_explicit_semi_loc(),
          loc~,
        )
      }
    }
    LETREC => {
      let spos = self.peek_spos()
      let letrec_loc = self.peek_location()
      self.expect_token(TK_LETREC)
      let bindings = []
      let and_locs = []
      let equal_locs = []
      let annot_colon_locs = []
      fn parse_binding(self : State) {
        let binder = self.parse_binder(context="variable name")
        let (ty_opt, annot_colon_loc) = self.parse_opt_annot_with_loc()
        match annot_colon_loc {
          Some(loc) => annot_colon_locs.push(loc)
          None => ()
        }
        equal_locs.push(self.peek_location())
        self.expect_token(TK_EQUAL)
        let func = self.parse_letand_func()
        bindings.push((binder, ty_opt, func))
      }
      parse_binding(self)
      while self.peek_token() is AND {
        and_locs.push(self.peek_location())
        self.expect_token(TK_AND)
        parse_binding(self)
      }
      let loc = self.loc_start_with(spos)
      StmtLetand(
        bindings=@list.from_array(bindings),
        letrec_loc~,
        and_locs=@list.from_array(and_locs),
        equal_locs=@list.from_array(equal_locs),
        annot_colon_locs=@list.from_array(annot_colon_locs),
        semi_loc=self.peek_explicit_semi_loc(),
        loc~,
      )
    }
    ASYNC if self.peek_token(nth=1) is FN && self.peek_token(nth=2) is LIDENT(_) => {
      let is_async = Some(self.peek_location())
      let spos = self.peek_spos()
      self.skip() // async
      let fn_loc = self.peek_location()
      self.expect_token(TK_FN)
      let binder = self.parse_binder(context="")
      let func = self.parse_func(is_async~, fn_loc=Some(fn_loc))
      let loc = self.loc_start_with(spos)
      StmtFunc(binder~, func~, semi_loc=self.peek_explicit_semi_loc(), loc~)
    }
    FN if self.peek_token(nth=1) is LIDENT(_) => {
      let spos = self.peek_spos()
      let fn_loc = self.peek_location()
      self.expect_token(TK_FN)
      let binder = self.parse_binder(context="")
      let func = self.parse_func(is_async=None, fn_loc=Some(fn_loc))
      let loc = self.loc_start_with(spos)
      StmtFunc(binder~, func~, semi_loc=self.peek_explicit_semi_loc(), loc~)
    }
    GUARD => {
      let spos = self.peek_spos()
      let guard_loc = self.peek_location()
      self.skip()
      let cond = self.with_syncs([TK_ELSE], State::try_parse_condition_expr)
      let mut else_loc = None
      let mut else_lbrace_loc = None
      let mut else_rbrace_loc = None
      let otherwise = if self.peek_token() is ELSE {
        else_loc = Some(self.peek_location())
        self.skip()
        let block = self.parse_block_expr_with_locs()
        else_lbrace_loc = Some(block.lbrace_loc)
        else_rbrace_loc = Some(block.rbrace_loc)
        Some(block.expr)
      } else {
        None
      }
      let loc = self.loc_start_with(spos)
      StmtGuard(
        cond~,
        otherwise~,
        guard_loc~,
        else_loc~,
        else_lbrace_loc~,
        else_rbrace_loc~,
        semi_loc=self.peek_explicit_semi_loc(),
        loc~,
      )
    }
    DEFER => {
      let spos = self.peek_spos()
      let defer_loc = self.peek_location()
      self.skip()
      let expr = self.try_parse_pipe_expr()
      let loc = self.loc_start_with(spos)
      StmtDefer(expr~, defer_loc~, semi_loc=self.peek_explicit_semi_loc(), loc~)
    }
    PROOF_ASSERT => {
      let spos = self.peek_spos()
      let proof_assert_loc = self.peek_location()
      self.skip()
      let expr = self.parse_expr()
      let loc = self.loc_start_with(spos)
      StmtExpr(
        expr=ProofAssert(expr~, proof_assert_loc~, loc~),
        semi_loc=self.peek_explicit_semi_loc(),
      )
    }
    PROOF_LET => {
      let spos = self.peek_spos()
      let proof_let_loc = self.peek_location()
      self.skip()
      let binder = self.parse_binder(context="proof binding")
      let equal_loc = self.peek_location()
      self.expect_token(TK_EQUAL)
      let expr = self.parse_expr()
      let loc = self.loc_start_with(spos)
      StmtExpr(
        expr=ProofLet(binder~, expr~, proof_let_loc~, equal_loc~, loc~),
        semi_loc=self.peek_explicit_semi_loc(),
      )
    }
    _ =>
      StmtExpr(
        expr=self.parse_expr_stmt(),
        semi_loc=self.peek_explicit_semi_loc(),
      )
  }
}

///|
fn State::handle_unexpected_expr_or_statement(
  self : Self,
  context~ : String,
) -> @syntax.Expr {
  let token = self.peek_token()
  fn handle_stmt(self : State) -> @syntax.Expr {
    let stmt = self.parse_stmt()
    // TODO: add stmt.loc()
    let loc = match stmt {
      StmtExpr(expr~, ..) => expr.loc()
      StmtLet(loc~, ..)
      | StmtLetmut(loc~, ..)
      | StmtFunc(loc~, ..)
      | StmtLetand(loc~, ..)
      | StmtDefer(loc~, ..)
      | StmtGuard(loc~, ..) => loc
    }
    Statement::compact_rev(@list.from_array([stmt]), loc~)
  }

  let (actual, expr) = match token {
    IF => ("if expression", self.parse_if_expr())
    MATCH => ("match expression", self.parse_match_expr())
    WHILE => ("while expression", self.parse_while_loop(label=None))
    FOR => ("for expression", self.parse_for_expr(label=None))
    TRY => ("try expression", self.parse_try_expr())
    TRY_EXCLAMATION => ("`try!` expression", self.parse_try_exclamation())
    TRY_QUESTION => ("`try?` expression", self.parse_try_question())
    LET | LETREC => ("let statement", handle_stmt(self))
    GUARD => ("guard statement", handle_stmt(self))
    DEFER => ("defer statement", handle_stmt(self))
    BREAK => ("break statement", handle_stmt(self))
    CONTINUE => ("break statement", handle_stmt(self))
    RETURN => ("return statement", handle_stmt(self))
    _ => panic()
  }
  let loc = expr.loc()
  self.report_error({ loc, msg: "Unexpected \{actual} in \{context}" })
  expr
}

///|
fn State::try_parse_condition_expr(self : Self) -> @syntax.Expr {
  // an error production to handle unexpected expression in condition. 
  // For example: match if true {0} else {1} { ... => ... }
  match self.peek_token() {
    IF
    | MATCH
    | WHILE
    | FOR
    | TRY
    | TRY_QUESTION
    | TRY_EXCLAMATION
    | LET
    | LETREC
    | GUARD
    | BREAK
    | RETURN
    | CONTINUE => self.handle_unexpected_expr_or_statement(context="condition")
    FORALL | EXISTS => self.parse_quantifier_expr()
    _ => self.parse_infix_expr(parsed=None)
  }
}

///|
fn State::parse_assign_or_simple_expr(self : Self) -> @syntax.Expr {
  let spos = self.peek_spos()
  let lhs = self.parse_expr()
  match self.peek_token() {
    EQUAL => {
      let equal_loc = self.peek_location()
      self.expect_token(TK_EQUAL)
      let rhs = self.parse_expr()
      let loc = self.loc_start_with(spos)
      match @syntax.make_assign_opt(loc~, lhs, rhs, equal_loc~) {
        Some(res) => res
        None => {
          self.report_error({
            loc: lhs.loc(),
            msg: "Invalid left value in assignment",
          })
          lhs
        }
      }
    }
    AUGMENTED_ASSIGNMENT(op) => {
      let op_loc = self.peek_location()
      let op : @syntax.Var = { name: Ident(name=op), loc: op_loc }
      self.expect_token(TK_AUGMENTED_ASSIGNMENT)
      let rhs = self.parse_expr()
      let loc = self.loc_start_with(spos)
      match
        @syntax.make_augmented_assign_opt(
          loc~,
          op,
          lhs,
          rhs,
          augmented_assign_loc=op_loc,
        ) {
        Some(res) => res
        None => {
          self.report_error({
            loc: lhs.loc(),
            msg: "Invalid left value in assignment",
          })
          lhs
        }
      }
    }
    PLUS_EQUAL => {
      let op_loc = self.peek_location()
      let op : @syntax.Var = { name: Ident(name="+"), loc: op_loc }
      self.expect_token(TK_PLUS_EQUAL)
      let rhs = self.parse_expr()
      let loc = self.loc_start_with(spos)
      match
        @syntax.make_augmented_assign_opt(
          loc~,
          op,
          lhs,
          rhs,
          augmented_assign_loc=op_loc,
        ) {
        Some(res) => res
        None => {
          self.report_error({
            loc: lhs.loc(),
            msg: "Invalid left value in assignment",
          })
          lhs
        }
      }
    }
    _ => lhs
  }
}

///|
fn State::parse_expr_stmt_no_break_continue_return(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  match self.peek_token() {
    RAISE => {
      let loc_start = self.peek_spos()
      let raise_loc = self.peek_location()
      self.expect_token(TK_RAISE)
      let expr = self.parse_expr()
      let loc_ = self.loc_start_with(loc_start)
      @syntax.Raise(err_value=expr, raise_loc~, loc=loc_)
    }
    ELLIPSIS => {
      let loc_ = self.peek_location()
      self.skip()
      @syntax.Hole(kind=Todo, loc=loc_)
    }
    _ => self.parse_assign_or_simple_expr()
  }
}

///|
fn State::parse_expr_stmt(self : Self) -> @syntax.Expr {
  match self.peek_token() {
    BREAK => {
      let spos = self.peek_spos()
      let break_loc = self.peek_location()
      self.expect_token(TK_BREAK)
      let (label, post_label_loc) = if self.peek_token()
        is POST_LABEL(label_name) {
        let post_label_loc = self.peek_location()
        self.skip()
        (
          Some(@syntax.Label::{ name: label_name, loc: post_label_loc }),
          Some(post_label_loc),
        )
      } else {
        (None, None)
      }
      let arg = self.optional(first_expr, State::parse_expr)
      Break(
        arg~,
        label~,
        break_loc~,
        post_label_loc~,
        loc=self.loc_start_with(spos),
      )
    }
    CONTINUE => {
      let spos = self.peek_spos()
      let continue_loc = self.peek_location()
      self.expect_token(TK_CONTINUE)
      let (label, post_label_loc) : (@syntax.Label?, @basic.Location?) = match
        self.peek_token() {
        //  Consider the case:
        //  ```
        //  continue l~: for i = 0 { ... }
        // ```
        //  Here the label l~ is for the following for expression instead of the continue statement.
        POST_LABEL(label_name) if !(self.peek_token(nth=1) is COLON) => {
          let post_label_loc = self.peek_location()
          self.skip()
          (
            Some({ name: label_name, loc: post_label_loc }),
            Some(post_label_loc),
          )
        }
        _ => (None, None)
      }
      let (args, comma_locs) = self.sepby_locs(
        delim=TK_COMMA,
        first_set=first_expr,
        State::parse_expr,
      )
      Continue(
        args=@list.from_array(args),
        label~,
        continue_loc~,
        post_label_loc~,
        comma_locs=@list.from_array(comma_locs),
        loc=self.loc_start_with(spos),
      )
    }
    RETURN => {
      let spos = self.peek_spos()
      let return_loc = self.peek_location()
      self.expect_token(TK_RETURN)
      let expr = self.optional(first_expr, State::parse_expr)
      let loc_ = self.loc_start_with(spos)
      Return(return_value=expr, return_loc~, loc=loc_)
    }
    _ => self.parse_expr_stmt_no_break_continue_return()
  }
}

///|
fn State::parse_error_type(self : Self) -> @syntax.Type {
  if self.mode is Panic(loc~, ..) {
    return Any(loc~)
  }
  let spos = self.peek_spos()
  match self.peek_token() {
    UNDERSCORE => {
      self.skip()
      let loc = self.loc_start_with(spos)
      Any(loc~)
    }
    UIDENT(id) if self.peek_token(nth=1) is QUESTION => {
      self.skip() // UIDENT
      self.skip() // QUESTION  
      let name = @syntax.LongIdent::Ident(name=id + "?")
      let loc = self.loc_start_with(spos)
      let constr_id : @syntax.ConstrId = { id: name, loc }
      Name(
        constr_id~,
        tys=@list.new(),
        type_args_lbracket_loc=None,
        type_args_rbracket_loc=None,
        type_arg_comma_locs=@list.empty(),
        loc~,
      )
    }
    _ => {
      let name = self.parse_qual_luident()
      let loc = self.loc_start_with(spos)
      let constr_id : @syntax.ConstrId = { id: name, loc }
      Name(
        constr_id~,
        tys=@list.new(),
        type_args_lbracket_loc=None,
        type_args_rbracket_loc=None,
        type_arg_comma_locs=@list.empty(),
        loc~,
      )
    }
  }
}

///|
fn State::parse_error_annotation(self : Self) -> @syntax.ErrorType {
  if self.mode is Panic(..) {
    return NoErrorType
  }
  match self.peek_token() {
    RAISE => {
      let raise_loc = self.peek_location()
      self.skip()
      match self.peek_token() {
        QUESTION => {
          let question_loc = self.peek_location()
          self.skip()
          let loc = raise_loc.merge(question_loc)
          let fake_error = @syntax.Type::Name(
            constr_id={ id: Ident(name="Error"), loc },
            tys=@list.new(),
            type_args_lbracket_loc=None,
            type_args_rbracket_loc=None,
            type_arg_comma_locs=@list.empty(),
            loc~,
          )
          MaybeError(ty=fake_error, raise_loc~, question_loc~, loc~)
        }
        PACKAGE_NAME(_) | UIDENT(_) | LIDENT(_) | UNDERSCORE => {
          let ty = self.parse_error_type()
          let loc = raise_loc.merge(ty.loc())
          ErrorType(ty~, raise_loc~, loc~)
        }
        _ => DefaultErrorType(loc=raise_loc)
      }
    }
    NORAISE => {
      let loc = self.peek_location()
      self.skip()
      Noraise(loc~)
    }
    _ => NoErrorType
  }
}

///|
fn State::parse_return_type(self : Self) -> (@syntax.Type, @syntax.ErrorType) {
  if self.mode is Panic(loc~, ..) {
    return (Any(loc~), NoErrorType)
  }
  fn report_missing_return_type(token) -> (@syntax.Type, @syntax.ErrorType) {
    let loc = self.peek_location()
    State::report_failed_to_parse(self, token, "return type annotation", loc)
    (Any(loc~), NoErrorType)
  }

  match self.peek_token() {
    // Error production rules for missing return type
    LBRACE | EQUAL | EOF => report_missing_return_type(self.peek_token())
    // Error production for first_impl tokens (approximate from pattern observation)
    token if first_impl_.contains(token.kind()) && !(token is ASYNC) =>
      report_missing_return_type(token)
    _ => {
      let (ty_res_is_simple, ty_res) = self.parse_type_aux()
      if !ty_res_is_simple {
        (ty_res, NoErrorType)
      } else {
        let err = self.parse_error_annotation()
        (ty_res, err)
      }
    }
  }
}

///|
fn State::parse_func_return_type_with_loc(
  self : Self,
) -> (@syntax.Type?, @syntax.ErrorType, Location?) {
  if self.mode is Panic(..) {
    return (None, NoErrorType, None)
  }
  if self.peek_token() is THIN_ARROW {
    let arrow_loc = self.peek_location()
    self.skip()
    let (ret_ty, err_ty) = self.parse_return_type()
    (Some(ret_ty), err_ty, Some(arrow_loc))
  } else {
    let err = self.parse_error_annotation()
    (None, err, None)
  }
}

///|
fn State::parse_type(self : Self) -> @syntax.Type {
  let (_, ty) = self.parse_type_aux()
  ty
}

///|
/// The bool indicates whether the type is a simple type.
fn State::parse_type_aux(self : Self) -> (Bool, @syntax.Type) {
  if self.mode is Panic(loc~, ..) {
    return (false, @syntax.Type::Any(loc~))
  }
  fn parse_question_mark(spos, ty) -> (Bool, @syntax.Type) {
    if self.peek_token() is QUESTION &&
      !first_type_.contains(self.peek_token(nth=1).kind()) {
      let constr_loc = self.peek_location()
      self.expect_token(TK_QUESTION)
      let loc = self.loc_start_with(spos)
      let ty = @syntax.make_option_type(loc~, constr_loc~, ty)
      parse_question_mark(spos, ty)
    } else {
      (true, ty)
    }
  }
  fn parse_paren_type_list_with_locs() -> (
    @list.List[@syntax.Type],
    Location,
    Location,
    @list.List[Location],
  ) {
    let lparen_loc = self.peek_location()
    self.expect_token(TK_LPAREN)
    let (tys, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RPAREN],
      State::parse_type,
    )
    let rparen_loc = self.peek_location()
    self.expect_token(TK_RPAREN)
    (
      @list.from_array(tys),
      lparen_loc,
      rparen_loc,
      @list.from_array(comma_locs),
    )
  }

  let spos = self.peek_spos()
  match self.peek_token() {
    PACKAGE_NAME(_) | LIDENT(_) | UIDENT(_) => {
      let name_loc = self.peek_location()
      let name = self.parse_qual_luident()
      let (
        args,
        type_args_lbracket_loc,
        type_args_rbracket_loc,
        type_arg_comma_locs,
      ) = if self.peek_token() is LBRACKET {
        let lbracket_loc = self.peek_location()
        self.expect_token(TK_LBRACKET)
        let (args, comma_locs, _) = self.series_with_follow_locs(
          delim=TK_COMMA,
          follow_set=[TK_RBRACKET],
          State::parse_type,
        )
        let rbracket_loc = self.peek_location()
        self.expect_token(TK_RBRACKET)
        (
          @list.from_array(args),
          Some(lbracket_loc),
          Some(rbracket_loc),
          @list.from_array(comma_locs),
        )
      } else {
        (@list.new(), None, None, @list.empty())
      }
      let loc = self.loc_start_with(spos)
      let ty = @syntax.Type::Name(
        constr_id={ id: name, loc: name_loc },
        tys=args,
        type_args_lbracket_loc~,
        type_args_rbracket_loc~,
        type_arg_comma_locs~,
        loc~,
      )
      parse_question_mark(spos, ty)
    }
    ASYNC => {
      let is_async = Some(self.peek_location())
      self.skip()
      let (tys, lparen_loc, rparen_loc, comma_locs) = parse_paren_type_list_with_locs()
      let arrow_loc = self.peek_location()
      self.expect_token(TK_THIN_ARROW)
      let (ty_res, ty_err) = self.parse_return_type()
      let loc = self.loc_start_with(spos)
      (
        false,
        Arrow(
          args=tys,
          res=ty_res,
          err=ty_err,
          is_async~,
          lparen_loc~,
          rparen_loc~,
          arrow_loc~,
          comma_locs~,
          loc~,
        ),
      )
    }
    LPAREN => {
      let (tys, lparen_loc, rparen_loc, comma_locs) = parse_paren_type_list_with_locs()
      if tys is Empty {
        let arrow_loc = self.peek_location()
        self.expect_token(TK_THIN_ARROW)
        let (ty_res, ty_err) = self.parse_return_type()
        let loc = self.loc_start_with(spos)
        (
          false,
          Arrow(
            args=@list.new(),
            res=ty_res,
            err=ty_err,
            is_async=None,
            lparen_loc~,
            rparen_loc~,
            arrow_loc~,
            comma_locs~,
            loc~,
          ),
        )
      } else {
        match self.peek_token() {
          THIN_ARROW => {
            let arrow_loc = self.peek_location()
            self.expect_token(TK_THIN_ARROW)
            let (ty_res, ty_err) = self.parse_return_type()
            let loc = self.loc_start_with(spos)
            (
              false,
              Arrow(
                args=tys,
                res=ty_res,
                err=ty_err,
                is_async=None,
                lparen_loc~,
                rparen_loc~,
                arrow_loc~,
                comma_locs~,
                loc~,
              ),
            )
          }
          _ => {
            let tys_array = tys.to_array()
            match tys_array {
              [] => panic()
              [ty] => parse_question_mark(spos, ty)
              _ => {
                let loc = self.loc_start_with(spos)
                let ty : @syntax.Type = Tuple(
                  tys~,
                  lparen_loc~,
                  rparen_loc~,
                  comma_locs~,
                  loc~,
                )
                parse_question_mark(spos, ty)
              }
            }
          }
        }
      }
    }
    UNDERSCORE => {
      let loc = self.peek_location()
      self.expect_token(TK_UNDERSCORE)
      let ty : @syntax.Type = Any(loc~)
      parse_question_mark(spos, ty)
    }
    AMPER => {
      let amp_loc = self.peek_location()
      self.skip()
      let name_loc = self.peek_location()
      let lid = self.parse_qual_luident()
      let loc = self.loc_start_with(spos)
      let ty = @syntax.Type::Object(
        constr_id={ id: lid, loc: name_loc },
        amp_loc~,
        loc~,
      )
      parse_question_mark(spos, ty)
    }
    other => {
      let other_loc = self.peek_location()
      self.report_failed_to_parse(other, "type", other_loc)
      let loc = self.panic()
      (false, @syntax.Type::Any(loc~))
    }
  }
}

///|
fn State::parse_opt_typ_params_with_locs(
  self : Self,
) -> (
  @list.List[@syntax.TypeVarBinder],
  Location?,
  Location?,
  @list.List[Location],
) {
  if self.mode is Panic(..) {
    return (@list.new(), None, None, @list.empty())
  }
  if self.peek_token() is LBRACKET {
    let lbracket_loc = self.peek_location()
    self.expect_token(TK_LBRACKET)
    self.push_sync(TK_RBRACKET)
    let (params, comma_locs, _) = self.series_with_follow_locs(
      delim=TK_COMMA,
      follow_set=[TK_RBRACKET],
      State::parse_tvar_binder,
    )
    self.pop_sync(TK_RBRACKET)
    let rbracket_loc = self.peek_location()
    self.expect_token(TK_RBRACKET)
    (
      @list.from_array(params),
      Some(lbracket_loc),
      Some(rbracket_loc),
      @list.from_array(comma_locs),
    )
  } else {
    (@list.new(), None, None, @list.empty())
  }
}

///|
pub fn parse(tokens : Array[Triple]) -> (Impls, Array[Report]) {
  let self : State = {
    tokens,
    diagnostics: [],
    next: 0,
    syncs: [],
    mode: Normal,
    parsed_position: dummy_pos,
  }
  let impls = self.parse_toplevel()
  (impls, self.diagnostics)
}

///|
pub fn parse_expr(tokens : Array[Triple]) -> (@syntax.Expr, Array[Report]) {
  let self : State = {
    tokens,
    diagnostics: [],
    next: 0,
    syncs: [],
    mode: Normal,
    parsed_position: dummy_pos,
  }
  let loc_start = self.peek_spos()
  let (stmts, _) = self.series_with_follow(delim=TK_SEMI, follow_set=[TK_EOF], fn(
    s,
  ) -> Statement {
    s.parse_stmt()
  })
  let loc = self.loc_start_with(loc_start)
  let expr = Statement::compact_rev(@list.from_array(stmts).rev(), loc~)
  (expr, self.diagnostics)
}
