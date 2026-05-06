///|
priv enum ExprOrBinderFragment {
  Binder(@syntax.Expr, colon_loc~ : Location, ty~ : @syntax.Type)
  Expr(@syntax.Expr)
}

///|
priv struct BlockExprInfo {
  expr : @syntax.Expr
  lbrace_loc : Location?
  rbrace_loc : Location?
  semi_locs : @list.List[Location]
  trailing_semi_loc : Location?
}

///|
priv struct BlockExprWithLocalTypesInfo {
  local_types : @list.List[@syntax.LocalTypeDecl]
  expr : @syntax.Expr
  lbrace_loc : Location?
  rbrace_loc : Location?
  semi_locs : @list.List[Location]
  trailing_semi_loc : Location?
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
fn State::parse_optional_bang(self : Self) -> Location? {
  match self.peek_token() {
    EXCLAMATION => {
      let loc = self.peek_location()
      self.skip()
      Some(loc)
    }
    _ => None
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
    let mut top = self.parse_top()
    if self.mode is Normal {
      match self.peek_token() {
        SEMI(_) => {
          top = @syntax.set_impl_trailing_semi(top, Some(self.peek_location()))
          self.skip()
        }
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
    result.push(top)
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
      self.skip()
      let id = self.expect_dot_lident(context~)
      Dot(pkg~, id~)
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
      self.skip()
      match self.peek_token() {
        DOT_LIDENT(id) | DOT_UIDENT(id) => {
          self.skip()
          Dot(pkg~, id~)
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
      self.skip()
      let id = self.expect_dot_uident(context="qualified upper identifier")
      Dot(pkg~, id~)
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
    return { name: @syntax.LongIdent::dummy(), is_object: false, loc }
  }
  let spos = self.peek_spos()
  let is_object = match self.peek_token() {
    AMPER => {
      self.skip()
      true
    }
    _ => false
  }
  let name = self.parse_qual_luident()
  let loc = self.loc_start_with(spos)
  { name, is_object, loc }
}

///|
fn State::parse_fun_binder(self : Self) -> (@syntax.TypeName?, @syntax.Binder) {
  if self.mode is Panic(loc~, ..) {
    return (None, { name: "", loc })
  }
  fn parse_qual_with_coloncolon() {
    let type_name = self.parse_type_name()
    self.expect_token(TK_COLONCOLON)
    let loc_func_name = self.peek_location()
    let func_name = self.expect_luident(context="name of `fn`")
    (Some(type_name), @syntax.Binder::{ name: func_name, loc: loc_func_name })
  }

  match self.peek_token() {
    PACKAGE_NAME(_) | UIDENT(_) | AMPER => parse_qual_with_coloncolon()
    LIDENT(_) if self.peek_token(nth=1) is COLONCOLON =>
      parse_qual_with_coloncolon()
    LIDENT(_) => {
      let id = self.parse_binder(context="name of `fn`")
      (None, id)
    }
    other => {
      let loc = self.panic()
      self.report_unexpected(other, first_qual_lident, loc)
      (None, { name: "", loc })
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
  let (constraints, colon_loc, plus_locs) = match self.peek_token() {
    COLON => {
      let colon_loc = self.peek_location()
      self.skip()
      let constraints = []
      let plus_locs = []
      for ;; {
        let loc = self.peek_location()
        let trait_ = self.parse_qual_luident()
        constraints.push(@syntax.TypeVarConstraint::{ trait_, loc })
        if self.peek_token() is PLUS {
          plus_locs.push(self.peek_location())
          self.skip()
        } else {
          break
        }
      }
      (
        @list.from_array(constraints),
        Some(colon_loc),
        @list.from_array(plus_locs),
      )
    }
    _ => (@list.new(), None, @list.empty())
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
      self.skip()
      let name : @syntax.ConstrName = {
        let spos = self.peek_spos()
        let name = self.expect_dot_uident(context="")
        { name, loc: self.loc_start_with(spos) }
      }
      let loc = self.loc_start_with(spos)
      { name, extra_info: Package(pkg), loc }
    }
    LIDENT(_) | UIDENT(_) | PACKAGE_NAME(_) | AMPER(_) => {
      // @pkg.TypeName::Constr
      // TypeName::@pkg.Constr
      // TypeName::Constr
      // The "&" case is just for error recovery and consistency with yacc parser
      let type_name = self.parse_type_name()
      self.expect_token(TK_COLONCOLON)
      let (extra_info, right_loc, right) = if self.peek_token()
        is PACKAGE_NAME(pkg) &&
        self.peek_token(nth=1) is DOT_UIDENT(_) {
        self.skip()
        let right_loc = self.peek_location()
        let right = self.expect_dot_uident(context="constructor name")
        (
          @syntax.ConstructorExtraInfo::TypeNameWithConstrPackage(
            type_name~,
            pkg~,
          ),
          right_loc,
          right,
        )
      } else {
        let right_loc = self.peek_location()
        let right = self.expect_uident(context="constructor name")
        (@syntax.ConstructorExtraInfo::TypeName(type_name), right_loc, right)
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
fn State::parse_opt_type_params_no_constraints_info(
  self : Self,
) -> (
  List[@syntax.TypeDeclBinder],
  Location?,
  Location?,
  @list.List[Location],
  Location?,
) {
  if self.mode is Panic(_) {
    return (@list.new(), None, None, @list.empty(), None)
  }
  self
  .optional([TK_LBRACKET], fn(state) {
    let lbracket_loc = state.peek_location()
    let (params, comma_locs, trailing_comma_loc) = state.surround_series_info(
      left=TK_LBRACKET,
      right=TK_RBRACKET,
      delim=TK_COMMA,
      State::parse_type_decl_binder,
    )
    let (_, rbracket_start, rbracket_end) = state.previous_triple()
    (
      params |> @list.from_array,
      Some(lbracket_loc),
      Some(Location::{ start: rbracket_start, end: rbracket_end }),
      comma_locs |> @list.from_array,
      trailing_comma_loc,
    )
  })
  .unwrap_or((@list.new(), None, None, @list.empty(), None))
}

///|
fn State::parse_local_type_decl(self : Self) -> @syntax.LocalTypeDecl {
  if self.mode is Panic(loc~, ..) {
    return {
      tycon: "?local_tycon",
      tycon_loc: loc,
      components: Variant(@list.new()),
      deriving: @list.new(),
      keyword_loc: None,
      body_lbrace_loc: None,
      body_rbrace_loc: None,
      body_lparen_loc: None,
      body_rparen_loc: None,
      body_comma_locs: @list.empty(),
      body_trailing_comma_loc: None,
      derive_loc: None,
      derive_lparen_loc: None,
      derive_rparen_loc: None,
      derive_comma_locs: @list.empty(),
    }
  }
  match self.peek_token() {
    STRUCT => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_STRUCT)
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let (
        components,
        body_lbrace_loc,
        body_rbrace_loc,
        body_lparen_loc,
        body_rparen_loc,
        body_comma_locs,
        body_trailing_comma_loc,
      ) : (
        @syntax.TypeDesc,
        Location?,
        Location?,
        Location?,
        Location?,
        @list.List[Location],
        Location?,
      ) = match self.peek_token() {
        LPAREN => {
          let lparen_loc = self.peek_location()
          let (ts, comma_locs, trailing_comma_loc) = self.surround_series_info(
            left=TK_LPAREN,
            right=TK_RPAREN,
            delim=TK_COMMA,
            State::parse_type,
          )
          let (_, rparen_start, rparen_end) = self.previous_triple()
          (
            TupleStruct(@list.from_array(ts)),
            None,
            None,
            Some(lparen_loc),
            Some(Location::{ start: rparen_start, end: rparen_end }),
            @list.from_array(comma_locs),
            trailing_comma_loc,
          )
        }
        _ => {
          let (components, lbrace_loc, rbrace_loc) = self.parse_struct_body_info(
            allow_constr=false,
          )
          (
            components,
            Some(lbrace_loc),
            Some(rbrace_loc),
            None,
            None,
            @list.empty(),
            None,
          )
        }
      }
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_info()
      {
        tycon,
        tycon_loc,
        components,
        deriving,
        keyword_loc: Some(keyword_loc),
        body_lbrace_loc,
        body_rbrace_loc,
        body_lparen_loc,
        body_rparen_loc,
        body_comma_locs,
        body_trailing_comma_loc,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      }
    }
    ENUM => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_ENUM)
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let (constrs, body_lbrace_loc, body_rbrace_loc) = self.parse_enum_constrs_info()
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_info()
      {
        tycon,
        tycon_loc,
        components: Variant(constrs),
        deriving,
        keyword_loc: Some(keyword_loc),
        body_lbrace_loc: Some(body_lbrace_loc),
        body_rbrace_loc: Some(body_rbrace_loc),
        body_lparen_loc: None,
        body_rparen_loc: None,
        body_comma_locs: @list.empty(),
        body_trailing_comma_loc: None,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      }
    }
    other => {
      let other_loc = self.peek_location()
      self.report_unexpected(other, [TK_STRUCT, TK_ENUM, TK_TYPE], other_loc)
      let loc = self.panic()
      {
        tycon: "?local_tycon",
        tycon_loc: loc,
        components: Variant(@list.new()),
        deriving: @list.new(),
        keyword_loc: None,
        body_lbrace_loc: None,
        body_rbrace_loc: None,
        body_lparen_loc: None,
        body_rparen_loc: None,
        body_comma_locs: @list.empty(),
        body_trailing_comma_loc: None,
        derive_loc: None,
        derive_lparen_loc: None,
        derive_rparen_loc: None,
        derive_comma_locs: @list.empty(),
      }
    }
  }
}

///|
fn State::parse_fun_decl(
  self : Self,
  attrs~ : @list.List[@attribute.Attribute],
) -> @syntax.FunDecl {
  if self.mode is Panic(loc~, ..) {
    return {
      name: @syntax.Binder::{ name: "", loc },
      has_error: None,
      is_async: None,
      type_name: None,
      decl_params: Some(@list.new()),
      params_loc: loc,
      quantifiers: @list.new(),
      return_type: None,
      error_type: NoErrorType,
      vis: Default,
      doc: @syntax.DocString::empty(),
      attrs,
      loc,
      declare_loc: None,
      extern_loc: None,
      fn_loc: None,
      type_params_lbracket_loc: None,
      type_params_rbracket_loc: None,
      type_param_comma_locs: @list.empty(),
      type_param_trailing_comma_loc: None,
      params_lparen_loc: None,
      params_rparen_loc: None,
      param_comma_locs: @list.empty(),
      trailing_param_comma_loc: None,
      thin_arrow_loc: None,
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
    quantifiers0,
    type_params_lbracket_loc0,
    type_params_rbracket_loc0,
    type_param_comma_locs0,
    type_param_trailing_comma_loc0,
  ) = self.parse_opt_typ_params_info()
  let mut type_params_lbracket_loc = type_params_lbracket_loc0
  let mut type_params_rbracket_loc = type_params_rbracket_loc0
  let mut type_param_comma_locs = type_param_comma_locs0
  let mut type_param_trailing_comma_loc = type_param_trailing_comma_loc0
  let (type_name, name) = self.parse_fun_binder()
  let has_error = self.parse_optional_bang()
  let quantifiers = match quantifiers0 {
    Empty => {
      let (quants, lbracket_loc, rbracket_loc, comma_locs, trailing_comma_loc) = self.parse_opt_typ_params_info()
      type_params_lbracket_loc = lbracket_loc
      type_params_rbracket_loc = rbracket_loc
      type_param_comma_locs = comma_locs
      type_param_trailing_comma_loc = trailing_comma_loc
      quants
    }
    More(_, ..) => quantifiers0
  }
  let (
    decl_params,
    params_lparen_loc,
    params_rparen_loc,
    param_comma_locs,
    trailing_param_comma_loc,
  ) = match self.peek_token() {
    LPAREN => {
      let (params, lparen_loc, rparen_loc, comma_locs, trailing_comma_loc) = self.parse_parameters_info()
      (
        Some(params),
        Some(lparen_loc),
        Some(rparen_loc),
        comma_locs,
        trailing_comma_loc,
      )
    }
    _ => (None, None, None, @list.empty(), None)
  }
  let params_loc = match decl_params {
    Some(_) =>
      params_lparen_loc
      .unwrap_or(self.empty_loc())
      .merge(params_rparen_loc.unwrap_or(self.empty_loc()))
    None => self.empty_loc()
  }
  let (return_type, error_type, thin_arrow_loc) = self.parse_func_return_type_info()
  let loc = self.loc_start_with(spos)
  {
    name,
    has_error,
    is_async,
    type_name,
    quantifiers,
    params_loc,
    decl_params,
    return_type,
    error_type,
    vis: Default,
    doc: @syntax.DocString::empty(),
    attrs,
    loc,
    declare_loc: None,
    extern_loc: None,
    fn_loc: Some(fn_loc),
    type_params_lbracket_loc,
    type_params_rbracket_loc,
    type_param_comma_locs,
    type_param_trailing_comma_loc,
    params_lparen_loc,
    params_rparen_loc,
    param_comma_locs,
    trailing_param_comma_loc,
    thin_arrow_loc,
  }
}

///|
fn State::parse_trait_method_decl(self : Self) -> @syntax.TraitMethodDecl {
  if self.mode is Panic(loc~, ..) {
    return {
      name: { name: "", loc },
      has_error: None,
      is_async: None,
      quantifiers: @list.new(),
      params: @list.new(),
      return_type: None,
      error_type: NoErrorType,
      has_default: None,
      attrs: @list.empty(),
      loc,
      type_params_lbracket_loc: None,
      type_params_rbracket_loc: None,
      type_param_comma_locs: @list.empty(),
      type_param_trailing_comma_loc: None,
      params_lparen_loc: None,
      params_rparen_loc: None,
      param_comma_locs: @list.empty(),
      trailing_param_comma_loc: None,
      thin_arrow_loc: None,
      equal_loc: None,
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
  let func_name = self.parse_binder(context="trait method name")
  let has_error = self.parse_optional_bang()
  let (
    quantifiers,
    type_params_lbracket_loc,
    type_params_rbracket_loc,
    type_param_comma_locs,
    type_param_trailing_comma_loc,
  ) = self.parse_opt_typ_params_info()
  self.pop_sync(TK_LPAREN)
  let (
    params,
    params_lparen_loc,
    params_rparen_loc,
    param_comma_locs,
    trailing_param_comma_loc,
  ) = self.parse_trait_method_params_info()
  let (return_type, error_type, thin_arrow_loc) = self.parse_func_return_type_info()
  let (has_default, equal_loc) = if self.peek_token() is EQUAL {
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
    has_error,
    is_async,
    quantifiers,
    params,
    return_type,
    error_type,
    has_default,
    attrs,
    loc,
    type_params_lbracket_loc,
    type_params_rbracket_loc,
    type_param_comma_locs,
    type_param_trailing_comma_loc,
    params_lparen_loc: Some(params_lparen_loc),
    params_rparen_loc: Some(params_rparen_loc),
    param_comma_locs,
    trailing_param_comma_loc,
    thin_arrow_loc,
    equal_loc,
  }
}

///|
fn State::parse_trait_supers_info(
  self : Self,
) -> (@list.List[@syntax.TypeVarConstraint], Location?, @list.List[Location]) {
  if self.mode is Panic(..) {
    return (@list.empty(), None, @list.empty())
  }
  if !(self.peek_token() is COLON) {
    return (@list.empty(), None, @list.empty())
  }
  let colon_loc = self.peek_location()
  self.skip()
  let plus_locs = []
  let supers = self.with_follow(follow_set=[TK_LBRACE], fn(state) {
    let constraints = []
    for ;; {
      let loc = state.peek_location()
      let trait_ = state.parse_qual_luident()
      constraints.push(@syntax.TypeVarConstraint::{ trait_, loc })
      if state.peek_token() is PLUS {
        plus_locs.push(state.peek_location())
        state.skip()
      } else {
        break
      }
    }
    constraints
  })
  (@list.from_array(supers), Some(colon_loc), @list.from_array(plus_locs))
}

///|
fn State::parse_trait_decl(
  self : Self,
  vis~ : @syntax.Visibility,
  attrs~ : List[@attribute.Attribute],
  is_declare~ : Bool,
  declare_loc~ : Location?,
) -> @syntax.TraitDecl {
  if self.mode is Panic(loc~, ..) {
    return {
      name: { name: "", loc },
      supers: @list.new(),
      methods: @list.new(),
      vis: Default,
      loc,
      attrs,
      doc: @syntax.DocString::empty(),
      is_declare,
      declare_loc,
      trait_loc: None,
      colon_loc: None,
      plus_locs: @list.empty(),
      lbrace_loc: None,
      rbrace_loc: None,
    }
  }
  let spos = self.peek_spos()
  let trait_loc = self.peek_location()
  self.expect_token(TK_TRAIT)
  let name_loc = self.peek_location()
  let name = self.parse_luident()
  let trait_name : @syntax.Binder = { name, loc: name_loc }
  let (supers, colon_loc, plus_locs) = self.parse_trait_supers_info()
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  let (methods, _) = self.series_with_follow(
    delim=TK_SEMI,
    follow_set=[TK_RBRACE],
    State::parse_trait_method_decl,
  )
  let methods = @list.from_array(methods)
  self.expect_token(TK_RBRACE)
  let (_, rbrace_start, rbrace_end) = self.previous_triple()
  let loc = self.loc_start_with(spos)
  {
    name: trait_name,
    supers,
    methods,
    vis,
    loc,
    attrs,
    doc: @syntax.DocString::empty(),
    is_declare,
    declare_loc,
    trait_loc: Some(trait_loc),
    colon_loc,
    plus_locs,
    lbrace_loc: Some(lbrace_loc),
    rbrace_loc: Some(Location::{ start: rbrace_start, end: rbrace_end }),
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
  self.expect_token(TK_TEST)
  let name : @syntax.TestName = if self.peek_token() is STRING(_) {
    let name_loc = self.peek_location()
    let v = self.expect_string()
    Some((v, name_loc))
  } else {
    None
  }
  let params = if self.peek_token() is LPAREN {
    let params = self.surround_series(
      left=TK_LPAREN,
      right=TK_RPAREN,
      delim=TK_COMMA,
      State::parse_parameter,
    )
    Some(@list.from_array(params))
  } else {
    None
  }
  let body_info = self.parse_block_expr_with_local_types()
  let loc = self.loc_start_with(spos)
  TopTest(
    name~,
    params~,
    expr=body_info.expr,
    is_async~,
    local_types=body_info.local_types,
    loc~,
    doc=DocString::empty(),
    attrs~,
    trailing_semi_loc=None,
  )
}

///|
fn State::parse_top_func(
  self : Self,
  attrs~ : List[Attribute],
  vis~ : @syntax.Visibility,
  spos~ : Position,
  is_declare~ : Bool,
  declare_loc~ : Location?,
) -> @syntax.Impl {
  let fun_decl = self.parse_fun_decl(attrs~)
  let mut where_clause : @syntax.WhereClause? = None
  let fun_decl = match declare_loc {
    Some(declare_loc) => {
      let loc = Location::{ start: declare_loc.start, end: fun_decl.loc.end }
      { ..fun_decl, vis, declare_loc: Some(declare_loc), loc }
    }
    None => { ..fun_decl, vis, declare_loc: None }
  }
  let decl_body : @syntax.DeclBody = if is_declare {
    DeclNone
  } else if self.peek_token() is EQUAL {
    self.skip()
    if self.peek_token() is MULTILINE_STRING(_) {
      let xs = []
      while self.peek_token() is MULTILINE_STRING(x) {
        xs.push(x)
        self.skip()
      }
      DeclStubs(
        Embedded(language=None, code=CodeMultilineString(@list.from_array(xs))),
      )
    } else {
      let name1 = self.expect_string()
      if self.peek_token() is STRING(name2) {
        self.skip()
        DeclStubs(Import(module_name=name1, func_name=name2, language=None))
      } else {
        DeclStubs(Embedded(language=None, code=CodeString(name1)))
      }
    }
  } else {
    where_clause = self.parse_where_clause()
    let body_info = self.parse_block_expr_with_local_types()
    DeclBody(
      expr=body_info.expr,
      local_types=body_info.local_types,
      lbrace_loc=body_info.lbrace_loc,
      rbrace_loc=body_info.rbrace_loc,
      semi_locs=body_info.semi_locs,
      trailing_semi_loc=body_info.trailing_semi_loc,
    )
  }
  let loc = self.loc_start_with(spos)
  TopFuncDef(fun_decl~, decl_body~, where_clause~, loc~, trailing_semi_loc=None)
}

///|
fn State::parse_top(self : Self) -> @syntax.Impl {
  if self.mode is Panic(loc~, ..) {
    return @syntax.TopExpr(
      expr=@syntax.Hole(loc~, kind=Synthesized),
      is_main=false,
      local_types=@list.new(),
      is_async=None,
      loc~,
      trailing_semi_loc=None,
    )
  }
  let spos = self.peek_spos()
  let attrs = self.parse_attributes()
  let declare_loc = if self.peek_token() is DECLARE {
    let loc = self.peek_location()
    self.skip()
    Some(loc)
  } else {
    None
  }
  let is_declare = declare_loc is Some(_)
  let vis = self.parse_visibility()
  match self.peek_token() {
    SUBERROR => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_SUBERROR)
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let (components, body_lbrace_loc, body_rbrace_loc) : (
        @syntax.TypeDesc,
        Location?,
        Location?,
      ) = match self.peek_token() {
        LBRACE => {
          let (cs, lbrace_loc, rbrace_loc) = self.parse_enum_constrs_info()
          (Error(EnumPayload(cs)), Some(lbrace_loc), Some(rbrace_loc))
        }
        _ => {
          let exception_decl = @syntax.ExceptionDecl::NoPayload
          (Error(exception_decl), None, None)
        }
      }
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_info()
      let loc = self.loc_start_with(spos)
      TopTypeDef(
        {
          tycon,
          tycon_loc,
          params: @list.new(),
          components,
          type_vis: vis,
          doc: DocString::empty(),
          deriving,
          loc,
          attrs,
          is_declare,
          declare_loc,
          extern_loc: None,
          keyword_loc: Some(keyword_loc),
          type_params_lbracket_loc: None,
          type_params_rbracket_loc: None,
          type_param_comma_locs: @list.empty(),
          type_param_trailing_comma_loc: None,
          equal_loc: None,
          body_lbrace_loc,
          body_rbrace_loc,
          body_lparen_loc: None,
          body_rparen_loc: None,
          body_comma_locs: @list.empty(),
          body_trailing_comma_loc: None,
          derive_loc,
          derive_lparen_loc,
          derive_rparen_loc,
          derive_comma_locs,
        },
        trailing_semi_loc=None,
      )
    }
    TYPE => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_TYPE)
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let (
        params,
        type_params_lbracket_loc,
        type_params_rbracket_loc,
        type_param_comma_locs,
        type_param_trailing_comma_loc,
      ) = self.parse_opt_type_params_no_constraints_info()
      let (components, equal_loc) : (@syntax.TypeDesc, Location?) = match
        self.peek_token() {
        SEMI(_) | EOF | DERIVE => (Abstract, None)
        EQUAL => {
          let equal_loc = self.peek_location()
          self.expect_token(TK_EQUAL)
          let typ = self.parse_type()
          (Alias(typ), Some(equal_loc))
        }
        other => {
          self.report_unexpected(
            other,
            [TK_SEMI, TK_EOF, TK_DERIVE, TK_EQUAL],
            self.peek_location(),
          )
          let loc = self.panic()
          return TopTypeDef(
            {
              tycon,
              tycon_loc,
              params,
              components: Abstract,
              type_vis: vis,
              doc: DocString::empty(),
              deriving: @list.empty(),
              loc,
              attrs,
              is_declare,
              declare_loc,
              extern_loc: None,
              keyword_loc: Some(keyword_loc),
              type_params_lbracket_loc,
              type_params_rbracket_loc,
              type_param_comma_locs,
              type_param_trailing_comma_loc,
              equal_loc: None,
              body_lbrace_loc: None,
              body_rbrace_loc: None,
              body_lparen_loc: None,
              body_rparen_loc: None,
              body_comma_locs: @list.empty(),
              body_trailing_comma_loc: None,
              derive_loc: None,
              derive_lparen_loc: None,
              derive_rparen_loc: None,
              derive_comma_locs: @list.empty(),
            },
            trailing_semi_loc=None,
          )
        }
      }
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_info()
      let loc = self.loc_start_with(spos)
      TopTypeDef(
        {
          tycon,
          tycon_loc,
          params,
          components,
          type_vis: vis,
          doc: DocString::empty(),
          deriving,
          loc,
          attrs,
          is_declare,
          declare_loc,
          extern_loc: None,
          keyword_loc: Some(keyword_loc),
          type_params_lbracket_loc,
          type_params_rbracket_loc,
          type_param_comma_locs,
          type_param_trailing_comma_loc,
          equal_loc,
          body_lbrace_loc: None,
          body_rbrace_loc: None,
          body_lparen_loc: None,
          body_rparen_loc: None,
          body_comma_locs: @list.empty(),
          body_trailing_comma_loc: None,
          derive_loc,
          derive_lparen_loc,
          derive_rparen_loc,
          derive_comma_locs,
        },
        trailing_semi_loc=None,
      )
    }
    STRUCT => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_STRUCT)
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let (
        params,
        type_params_lbracket_loc,
        type_params_rbracket_loc,
        type_param_comma_locs,
        type_param_trailing_comma_loc,
      ) = self.parse_opt_type_params_no_constraints_info()
      let (
        components,
        body_lbrace_loc,
        body_rbrace_loc,
        body_lparen_loc,
        body_rparen_loc,
        body_comma_locs,
        body_trailing_comma_loc,
      ) : (
        @syntax.TypeDesc,
        Location?,
        Location?,
        Location?,
        Location?,
        @list.List[Location],
        Location?,
      ) = match self.peek_token() {
        LPAREN => {
          let lparen_loc = self.peek_location()
          let (ts, comma_locs, trailing_comma_loc) = self.surround_series_info(
            left=TK_LPAREN,
            right=TK_RPAREN,
            delim=TK_COMMA,
            State::parse_type,
          )
          let (_, rparen_start, rparen_end) = self.previous_triple()
          (
            TupleStruct(@list.from_array(ts)),
            None,
            None,
            Some(lparen_loc),
            Some(Location::{ start: rparen_start, end: rparen_end }),
            @list.from_array(comma_locs),
            trailing_comma_loc,
          )
        }
        _ => {
          let (components, lbrace_loc, rbrace_loc) = self.parse_struct_body_info(
            allow_constr=true,
          )
          (
            components,
            Some(lbrace_loc),
            Some(rbrace_loc),
            None,
            None,
            @list.empty(),
            None,
          )
        }
      }
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_info()
      let loc = self.loc_start_with(spos)
      TopTypeDef(
        {
          tycon,
          tycon_loc,
          params,
          components,
          type_vis: vis,
          doc: DocString::empty(),
          deriving,
          loc,
          attrs,
          is_declare,
          declare_loc,
          extern_loc: None,
          keyword_loc: Some(keyword_loc),
          type_params_lbracket_loc,
          type_params_rbracket_loc,
          type_param_comma_locs,
          type_param_trailing_comma_loc,
          equal_loc: None,
          body_lbrace_loc,
          body_rbrace_loc,
          body_lparen_loc,
          body_rparen_loc,
          body_comma_locs,
          body_trailing_comma_loc,
          derive_loc,
          derive_lparen_loc,
          derive_rparen_loc,
          derive_comma_locs,
        },
        trailing_semi_loc=None,
      )
    }
    ENUM => {
      let keyword_loc = self.peek_location()
      self.expect_token(TK_ENUM)
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let (
        params,
        type_params_lbracket_loc,
        type_params_rbracket_loc,
        type_param_comma_locs,
        type_param_trailing_comma_loc,
      ) = self.parse_opt_type_params_no_constraints_info()
      let (constrs, body_lbrace_loc, body_rbrace_loc) = self.parse_enum_constrs_info()
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_info()
      let loc = self.loc_start_with(spos)
      TopTypeDef(
        {
          tycon,
          tycon_loc,
          params,
          components: Variant(constrs),
          type_vis: vis,
          doc: DocString::empty(),
          deriving,
          loc,
          attrs,
          is_declare,
          declare_loc,
          extern_loc: None,
          keyword_loc: Some(keyword_loc),
          type_params_lbracket_loc,
          type_params_rbracket_loc,
          type_param_comma_locs,
          type_param_trailing_comma_loc,
          equal_loc: None,
          body_lbrace_loc: Some(body_lbrace_loc),
          body_rbrace_loc: Some(body_rbrace_loc),
          body_lparen_loc: None,
          body_rparen_loc: None,
          body_comma_locs: @list.empty(),
          body_trailing_comma_loc: None,
          derive_loc,
          derive_lparen_loc,
          derive_rparen_loc,
          derive_comma_locs,
        },
        trailing_semi_loc=None,
      )
    }
    EXTENUM => {
      self.expect_token(TK_EXTENUM)
      let target = self.parse_type_name()
      let (tycon, tycon_loc) = match target.name {
        Ident(name~) => (name, target.loc)
        Dot(id~, ..) => (id, target.loc)
      }
      let params = self.parse_opt_type_params_no_constraints()
      let components : @syntax.TypeDesc = if self.peek_token() is PLUS_EQUAL {
        self.expect_token(TK_PLUS_EQUAL)
        let constructors = self.surround_series(
            invalid_delims=[TK_COMMA],
            left=TK_LBRACE,
            right=TK_RBRACE,
            delim=TK_SEMI,
            State::parse_constr_decl,
          )
          |> @list.from_array
        ExtendEnum(target~, constructors~)
      } else {
        let constrs = self.surround_series(
            invalid_delims=[TK_COMMA],
            left=TK_LBRACE,
            right=TK_RBRACE,
            delim=TK_SEMI,
            State::parse_constr_decl,
          )
          |> @list.from_array
        ExtensibleEnum(constrs)
      }
      let deriving = self.parse_deriving_directive_list()
      let loc = self.loc_start_with(spos)
      TopTypeDef({
        tycon,
        tycon_loc,
        params,
        components,
        type_vis: vis,
        doc: DocString::empty(),
        deriving,
        loc,
        attrs,
        is_declare,
      })
    }
    LET | CONST => {
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
      let ty = self.parse_opt_annot()
      self.expect_token(TK_EQUAL)
      let expr = self.parse_expr()
      let loc = self.loc_start_with(spos)
      TopLetDef(
        binder~,
        ty~,
        expr~,
        is_constant~,
        vis~,
        loc~,
        doc=DocString::empty(),
        attrs~,
        is_declare~,
        trailing_semi_loc=None,
      )
    }
    EXTERN if self.peek_token(nth=1) is TYPE => {
      let extern_loc = self.peek_location()
      self.skip()
      let keyword_loc = self.peek_location()
      self.skip()
      let tycon_loc = self.peek_location()
      let tycon = self.parse_luident()
      let (
        params,
        type_params_lbracket_loc,
        type_params_rbracket_loc,
        type_param_comma_locs,
        type_param_trailing_comma_loc,
      ) = self.parse_opt_type_params_no_constraints_info()
      let (
        deriving,
        derive_loc,
        derive_lparen_loc,
        derive_rparen_loc,
        derive_comma_locs,
      ) = self.parse_deriving_directive_list_info()
      let loc = self.loc_start_with(spos)
      TopTypeDef(
        {
          tycon,
          tycon_loc,
          params,
          components: Extern,
          type_vis: vis,
          doc: DocString::empty(),
          deriving,
          loc,
          attrs,
          is_declare,
          declare_loc,
          extern_loc: Some(extern_loc),
          keyword_loc: Some(keyword_loc),
          type_params_lbracket_loc,
          type_params_rbracket_loc,
          type_param_comma_locs,
          type_param_trailing_comma_loc,
          equal_loc: None,
          body_lbrace_loc: None,
          body_rbrace_loc: None,
          body_lparen_loc: None,
          body_rparen_loc: None,
          body_comma_locs: @list.empty(),
          body_trailing_comma_loc: None,
          derive_loc,
          derive_lparen_loc,
          derive_rparen_loc,
          derive_comma_locs,
        },
        trailing_semi_loc=None,
      )
    }
    EXTERN => {
      let extern_loc = self.peek_location()
      self.expect_token(TK_EXTERN)
      let language = self.expect_string()
      let fun_decl = self.parse_fun_decl(attrs~)
      let loc = Location::{ start: extern_loc.start, end: fun_decl.loc.end }
      let fun_decl = {
        ..fun_decl,
        vis,
        declare_loc,
        extern_loc: Some(extern_loc),
        loc,
      }
      self.expect_token(TK_EQUAL)
      if self.peek_token() is MULTILINE_STRING(_) {
        let xs = []
        while self.peek_token() is MULTILINE_STRING(x) {
          self.skip()
          xs.push(x)
        }
        let loc = self.loc_start_with(spos)
        TopFuncDef(
          fun_decl~,
          decl_body=DeclStubs(
            Embedded(
              language=Some(language),
              code=CodeMultilineString(@list.from_array(xs)),
            ),
          ),
          where_clause=None,
          loc~,
          trailing_semi_loc=None,
        )
      } else {
        let body = self.expect_string()
        if self.peek_token() is STRING(_) {
          let fname = self.expect_string()
          let loc = self.loc_start_with(spos)
          TopFuncDef(
            fun_decl~,
            decl_body=DeclStubs(
              Import(module_name=body, func_name=fname, language=Some(language)),
            ),
            where_clause=None,
            loc~,
            trailing_semi_loc=None,
          )
        } else {
          let loc = self.loc_start_with(spos)
          TopFuncDef(
            fun_decl~,
            decl_body=DeclStubs(
              Embedded(language=Some(language), code=CodeString(body)),
            ),
            where_clause=None,
            loc~,
            trailing_semi_loc=None,
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
      @syntax.TopTrait(trait_decl, trailing_semi_loc=None)
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
        type_param_trailing_comma_loc,
      ) = self.parse_opt_typ_params_info()
      let trait_ = self.parse_type_name()
      self.pop_sync(TK_FOR)
      if self.mode is Panic(loc~, ..) {
        self.pop_sync(TK_WITH)
        return TopExpr(
          expr=Hole(loc~, kind=Synthesized),
          is_main=false,
          local_types=@list.new(),
          is_async=None,
          loc~,
          trailing_semi_loc=None,
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
            let method_name = self.parse_binder(context="name of method")
            let has_error = self.parse_optional_bang()
            let (
              params,
              params_lparen_loc,
              params_rparen_loc,
              param_comma_locs,
              trailing_param_comma_loc,
            ) = self.parse_parameters_info()
            let (ret_ty, err_ty, thin_arrow_loc) = self.parse_func_return_type_info()
            self.pop_sync(TK_LBRACE)
            let body : @syntax.DeclBody = if self.peek_token() is EQUAL {
              self.skip()
              let code = self.expect_string()
              DeclStubs(Embedded(language=None, code=CodeString(code)))
            } else {
              let body_info = self.parse_block_expr_with_local_types()
              DeclBody(
                expr=body_info.expr,
                local_types=body_info.local_types,
                lbrace_loc=body_info.lbrace_loc,
                rbrace_loc=body_info.rbrace_loc,
                semi_locs=body_info.semi_locs,
                trailing_semi_loc=body_info.trailing_semi_loc,
              )
            }
            TopImpl(
              self_ty=Some(self_ty),
              trait_~,
              method_name~,
              has_error~,
              quantifiers~,
              params~,
              ret_ty~,
              err_ty~,
              vis~,
              body~,
              loc=self.loc_start_with(spos),
              doc=DocString::empty(),
              attrs~,
              trailing_semi_loc=None,
              impl_loc=Some(impl_loc),
              type_params_lbracket_loc~,
              type_params_rbracket_loc~,
              type_param_comma_locs~,
              type_param_trailing_comma_loc~,
              for_loc=Some(for_loc),
              with_loc=Some(with_loc),
              params_lparen_loc=Some(params_lparen_loc),
              params_rparen_loc=Some(params_rparen_loc),
              param_comma_locs~,
              trailing_param_comma_loc~,
              thin_arrow_loc~,
            )
          } else {
            TopImplRelation(
              self_ty~,
              trait_~,
              vis~,
              quantifiers~,
              loc=self.loc_start_with(spos),
              attrs~,
              doc=DocString::empty(),
              is_declare~,
              trailing_semi_loc=None,
              declare_loc~,
              impl_loc=Some(impl_loc),
              type_params_lbracket_loc~,
              type_params_rbracket_loc~,
              type_param_comma_locs~,
              type_param_trailing_comma_loc~,
              for_loc=Some(for_loc),
            )
          }
        }
        WITH => {
          self.pop_sync(TK_WITH)
          let with_loc = self.peek_location()
          self.skip()
          self.push_sync(TK_LBRACE)
          let method_name = self.parse_binder(context="name of method")
          let has_error = self.parse_optional_bang()
          let (
            params,
            params_lparen_loc,
            params_rparen_loc,
            param_comma_locs,
            trailing_param_comma_loc,
          ) = self.parse_parameters_info()
          let (ret_ty, err_ty, thin_arrow_loc) = self.parse_func_return_type_info()
          self.pop_sync(TK_LBRACE)
          let body : @syntax.DeclBody = if self.peek_token() is EQUAL {
            self.skip()
            let code = self.expect_string()
            DeclStubs(Embedded(language=None, code=CodeString(code)))
          } else {
            let body_info = self.parse_block_expr_with_local_types()
            DeclBody(
              expr=body_info.expr,
              local_types=body_info.local_types,
              lbrace_loc=body_info.lbrace_loc,
              rbrace_loc=body_info.rbrace_loc,
              semi_locs=body_info.semi_locs,
              trailing_semi_loc=body_info.trailing_semi_loc,
            )
          }
          TopImpl(
            self_ty=None,
            trait_~,
            method_name~,
            has_error~,
            quantifiers~,
            params~,
            ret_ty~,
            err_ty~,
            vis~,
            body~,
            loc=self.loc_start_with(spos),
            doc=DocString::empty(),
            attrs~,
            trailing_semi_loc=None,
            impl_loc=Some(impl_loc),
            type_params_lbracket_loc~,
            type_params_rbracket_loc~,
            type_param_comma_locs~,
            type_param_trailing_comma_loc~,
            for_loc=None,
            with_loc=Some(with_loc),
            params_lparen_loc=Some(params_lparen_loc),
            params_rparen_loc=Some(params_rparen_loc),
            param_comma_locs~,
            trailing_param_comma_loc~,
            thin_arrow_loc~,
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
            has_error=None,
            quantifiers=@list.new(),
            params=@list.new(),
            ret_ty=None,
            err_ty=NoErrorType,
            vis~,
            body=DeclBody(
              expr=Hole(loc=@syntax.no_location, kind=Synthesized),
              local_types=@list.new(),
              lbrace_loc=None,
              rbrace_loc=None,
              semi_locs=@list.empty(),
              trailing_semi_loc=None,
            ),
            loc~,
            doc=DocString::empty(),
            attrs~,
            trailing_semi_loc=None,
            impl_loc=Some(impl_loc),
            type_params_lbracket_loc~,
            type_params_rbracket_loc~,
            type_param_comma_locs~,
            type_param_trailing_comma_loc~,
            for_loc=None,
            with_loc=None,
            params_lparen_loc=None,
            params_rparen_loc=None,
            param_comma_locs=@list.empty(),
            trailing_param_comma_loc=None,
            thin_arrow_loc=None,
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
        type_param_trailing_comma_loc,
      ) = self.parse_opt_typ_params_info()
      let view_type_loc = self.peek_location()
      let view_type_name = self.expect_uident(context="view type name")
      let (view_constrs, constr_lbrace_loc, constr_rbrace_loc) = self.parse_enum_constrs_info()
      let for_loc = self.peek_location()
      self.expect_token(TK_FOR)
      let source_ty = self.parse_type()
      let with_loc = self.peek_location()
      self.expect_token(TK_WITH)
      let view_func_name = self.parse_binder(context="name of view function")
      let (
        parameters,
        params_lparen_loc,
        params_rparen_loc,
        param_comma_locs,
        trailing_param_comma_loc,
      ) = self.parse_parameters_info()
      let params_loc = Location::{
        start: params_lparen_loc.start,
        end: params_rparen_loc.end,
      }
      let body = self.parse_block_expr()
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
        body~,
        vis~,
        loc~,
        attrs~,
        doc=DocString::empty(),
        trailing_semi_loc=None,
        enumview_loc=Some(enumview_loc),
        type_params_lbracket_loc~,
        type_params_rbracket_loc~,
        type_param_comma_locs~,
        type_param_trailing_comma_loc~,
        constr_lbrace_loc=Some(constr_lbrace_loc),
        constr_rbrace_loc=Some(constr_rbrace_loc),
        for_loc=Some(for_loc),
        with_loc=Some(with_loc),
        params_lparen_loc=Some(params_lparen_loc),
        params_rparen_loc=Some(params_rparen_loc),
        param_comma_locs~,
        trailing_param_comma_loc~,
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
            }
          }
          _ =>
            @syntax.AliasTarget::{
              binder: { name: target, loc: target_loc },
              target: None,
            }
        }
      }

      let lbrace_loc = self.peek_location()
      let (names, comma_locs, trailing_comma_loc) = self.surround_series_info(
        left=TK_LBRACE,
        right=TK_RBRACE,
        delim=TK_COMMA,
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
              state.skip()
              let target = parse_alias_target(state, false, "type name")
              (target, @syntax.UsingKind::Type)
            }
            TRAIT => {
              state.skip()
              let target = parse_alias_target(state, false, "trait name")
              (target, @syntax.UsingKind::Trait)
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
                { binder: { name: "", loc }, target: None },
                @syntax.UsingKind::Value,
              )
            }
          }
        },
      )
      let (_, rbrace_start, rbrace_end) = self.previous_triple()
      let loc = self.loc_start_with(spos)
      @syntax.Impl::TopUsing(
        pkg~,
        names=@list.from_array(names),
        vis~,
        attrs~,
        loc~,
        doc=DocString::empty(),
        trailing_semi_loc=None,
        using_loc=Some(using_loc),
        lbrace_loc=Some(lbrace_loc),
        rbrace_loc=Some(Location::{ start: rbrace_start, end: rbrace_end }),
        comma_locs=@list.from_array(comma_locs),
        trailing_comma_loc~,
      )
    }
    other => {
      let other_loc = self.peek_location()
      self.report_unexpected(other, first_impl_, other_loc)
      let loc = self.panic()
      TopExpr(
        expr=Hole(loc~, kind=Synthesized),
        is_main=false,
        local_types=@list.new(),
        is_async=None,
        loc~,
        trailing_semi_loc=None,
      )
    }
  }
}

///|
fn State::parse_struct_body_info(
  self : State,
  allow_constr~ : Bool,
) -> (@syntax.TypeDesc, Location, Location) {
  let lbrace_loc = self.peek_location()
  let foc_list = self.surround_series(
    invalid_delims=[TK_COMMA],
    left=TK_LBRACE,
    right=TK_RBRACE,
    delim=TK_SEMI,
    State::parse_field_decl_or_constr_decl,
  )
  let (_, rbrace_start, rbrace_end) = self.previous_triple()
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
  let constr_decl = if foc_list is [.. fields, Constr(constr_decl)] {
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
    check_fields(foc_list)
    None
  }
  (
    Record(fields~, constr_decl~),
    lbrace_loc,
    Location::{ start: rbrace_start, end: rbrace_end },
  )
}

///|
fn State::parse_enum_constrs_info(
  self : State,
) -> (@list.List[@syntax.ConstrDecl], Location, Location) {
  let lbrace_loc = self.peek_location()
  let constrs = self.surround_series(
      invalid_delims=[TK_COMMA],
      left=TK_LBRACE,
      right=TK_RBRACE,
      delim=TK_SEMI,
      State::parse_constr_decl,
    )
    |> @list.from_array
  let (_, rbrace_start, rbrace_end) = self.previous_triple()
  (constrs, lbrace_loc, Location::{ start: rbrace_start, end: rbrace_end })
}

///|
fn State::parse_deriving_directive_list_info(
  self : Self,
) -> (
  List[@syntax.DerivingDirective],
  Location?,
  Location?,
  Location?,
  @list.List[Location],
) {
  if self.mode is Panic(_) {
    return (@list.new(), None, None, None, @list.empty())
  }
  if self.peek_token() is DERIVE {
    let derive_loc = self.peek_location()
    self.skip()
    let lparen_loc = if self.peek_token() is LPAREN {
      Some(self.peek_location())
    } else {
      None
    }
    let (directives, comma_locs, _) = self.surround_series_info(
      left=TK_LPAREN,
      right=TK_RPAREN,
      delim=TK_COMMA,
      State::parse_deriving_directive,
    )
    let (_, rparen_start, rparen_end) = self.previous_triple()
    (
      directives |> @list.from_array,
      Some(derive_loc),
      lparen_loc,
      Some(Location::{ start: rparen_start, end: rparen_end }),
      comma_locs |> @list.from_array,
    )
  } else {
    (@list.new(), None, None, None, @list.empty())
  }
}

///|
fn State::parse_deriving_directive(self : Self) -> @syntax.DerivingDirective {
  if self.mode is Panic(loc~, ..) {
    return {
      type_name: { name: @syntax.Ident(name=""), is_object: false, loc },
      args: @list.new(),
      loc,
    }
  }
  let spos = self.peek_spos()
  let type_name = self.parse_type_name()
  let args = if self.peek_token() is LPAREN {
    self.surround_series(
      left=TK_LPAREN,
      right=TK_RPAREN,
      delim=TK_COMMA,
      State::parse_argument,
    )
    |> @list.from_array
  } else {
    @list.new()
  }
  let loc = self.loc_start_with(spos)
  { type_name, args, loc }
}

///|
priv enum FieldOrConstr {
  Field(@syntax.FieldDecl)
  Constr(@syntax.FunDecl)
}

///|
fn State::parse_trait_method_params_info(
  self : State,
) -> (@syntax.Parameters, Location, Location, @list.List[Location], Location?) {
  let lparen_loc = self.peek_location()
  let (params, comma_locs, trailing_comma_loc) = self.surround_series_info(
    delim=TK_COMMA,
    left=TK_LPAREN,
    right=TK_RPAREN,
    fn(state) -> @syntax.Parameter {
      match state.peek_token() {
        POST_LABEL(binder_name) => {
          let binder_loc = Location::trim_last_char(state.peek_location())
          state.skip()
          let binder : @syntax.Binder = { name: binder_name, loc: binder_loc }
          let colon_loc = state.peek_location()
          state.expect_token(TK_COLON)
          let typ = state.parse_type()
          Labelled(binder~, ty=Some(typ), colon_loc=Some(colon_loc))
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
          if has_question {
            QuestionOptional(
              binder~,
              ty=Some(typ),
              question_loc~,
              colon_loc=Some(colon_loc),
            )
          } else {
            Positional(binder~, ty=Some(typ), colon_loc=Some(colon_loc))
          }
        }
        tok if first_type_.contains(tok.kind()) => {
          let typ = state.parse_type()
          let loc = typ.loc()
          DiscardPositional(ty=Some(typ), loc~, colon_loc=None)
        }
        other => {
          let other_loc = state.peek_location()
          state.report_unexpected(
            other,
            [TK_POST_LABEL, ..first_type_],
            other_loc,
          )
          let loc = state.panic()
          DiscardPositional(ty=None, loc~, colon_loc=None)
        }
      }
    },
  )
  let (_, rparen_start, rparen_end) = self.previous_triple()
  (
    params |> @list.from_array,
    lparen_loc,
    Location::{ start: rparen_start, end: rparen_end },
    @list.from_array(comma_locs),
    trailing_comma_loc,
  )
}

///|
fn State::parse_field_decl_or_constr_decl(self : Self) -> FieldOrConstr {
  if self.mode is Panic(loc~, ..) {
    return Field({
      name: { label: "?field_name", loc },
      ty: Any(loc~),
      mut_: false,
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
        type_param_trailing_comma_loc,
      ) = self.parse_opt_typ_params_info()
      let name = self.parse_binder(context="name of `fn`")
      let (
        params,
        params_lparen_loc,
        params_rparen_loc,
        param_comma_locs,
        trailing_param_comma_loc,
      ) = self.parse_trait_method_params_info()
      let params_loc = params_lparen_loc.merge(params_rparen_loc)
      let (return_type, error_type, thin_arrow_loc) = self.parse_func_return_type_info()
      Constr({
        name,
        has_error: None,
        is_async,
        type_name: None,
        quantifiers,
        params_loc,
        decl_params: Some(params),
        return_type,
        error_type,
        vis: Default,
        doc: DocString::empty(),
        attrs,
        loc: self.loc_start_with(spos),
        declare_loc: None,
        extern_loc: None,
        fn_loc: Some(fn_loc),
        type_params_lbracket_loc,
        type_params_rbracket_loc,
        type_param_comma_locs,
        type_param_trailing_comma_loc,
        params_lparen_loc: Some(params_lparen_loc),
        params_rparen_loc: Some(params_rparen_loc),
        param_comma_locs,
        trailing_param_comma_loc,
        thin_arrow_loc,
      })
    }
    _ => {
      let vis = self.parse_visibility()
      let mut_ = if self.peek_token() is MUTABLE {
        self.expect_token(TK_MUTABLE)
        true
      } else {
        false
      }
      let name_loc = self.peek_location()
      let name = self.expect_lident(context="struct field name")
      self.expect_token(TK_COLON)
      let ty = self.parse_type()
      let loc = self.loc_start_with(spos)
      Field({
        name: { label: name, loc: name_loc },
        ty,
        mut_,
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
      tag: None,
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
  let args = if self.peek_token() is LPAREN {
    let xs = self.surround_series(
      left=TK_LPAREN,
      right=TK_RPAREN,
      delim=TK_COMMA,
      fn(state) -> @syntax.ConstrParam {
        let param_spos = state.peek_spos()
        let mut_ = if state.peek_token() is MUTABLE {
          let loc = state.peek_location()
          state.skip()
          (true, Some(loc))
        } else {
          (false, None)
        }
        let (mut_, mut_loc) = mut_
        // `label~ : Type`
        if state.peek_token() is POST_LABEL(name) {
          let label_loc = Location::trim_last_char(state.peek_location())
          let label : @syntax.Label = { name, loc: label_loc }
          state.skip()
          let colon_loc = state.peek_location()
          state.expect_token(TK_COLON)
          let ty = state.parse_type()
          let loc = state.loc_start_with(param_spos)
          {
            loc,
            ty,
            mut_,
            label: Some(label),
            mut_loc,
            colon_loc: Some(colon_loc),
          }
        } else {
          let ty = state.parse_type()
          let loc = state.loc_start_with(param_spos)
          { loc, ty, mut_, label: None, mut_loc, colon_loc: None }
        }
      },
    )
    Some(@list.from_array(xs))
  } else {
    None
  }
  let tag = if self.peek_token() is EQUAL {
    self.skip()
    let tag_loc = self.peek_location()
    let tag = self.expect_int()
    Some((tag, tag_loc))
  } else {
    None
  }
  let loc = self.loc_start_with(spos)
  { name: constr_name, args, tag, loc, attrs, doc: DocString::empty() }
}

///|
fn State::parse_pattern(self : Self) -> @syntax.Pattern {
  if self.mode is Panic(loc~, ..) {
    return Any(loc~)
  }
  let spos = self.peek_spos()
  let mut pat = self.parse_or_pattern()
  while self.peek_token() is AS {
    self.skip()
    let alias_ = self.parse_binder(context="variable name")
    let loc = self.loc_start_with(spos)
    pat = Alias(pat~, alias_~, loc~)
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
    self.skip()
    let pat2 = self.parse_or_pattern()
    let loc = self.loc_start_with(spos)
    Or(pat1=pat, pat2~, loc~)
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
      let op_loc = self.peek_location()
      self.skip()
      let rhs = self.parse_simple_pattern()
      let loc = self.loc_start_with(spos)
      Range(lhs~, rhs~, kind=Inclusive, loc~, op_loc=Some(op_loc))
    }
    RANGE_EXCLUSIVE => {
      let op_loc = self.peek_location()
      self.skip()
      let rhs = self.parse_simple_pattern()
      let loc = self.loc_start_with(spos)
      Range(lhs~, rhs~, kind=Exclusive, loc~, op_loc=Some(op_loc))
    }
    _ => lhs
  }
}

///|
fn State::parse_constr_pattern(self : Self) -> @syntax.Pattern {
  let spos = self.peek_spos()
  fn parse_constr_args() -> (
    List[ConstrArgRelatedness],
    Location,
    Location,
    List[Location],
    Location?,
  ) {
    let lparen_loc = self.peek_location()
    let (args, comma_locs, trailing_comma_loc) = self.surround_series_info(
      left=TK_LPAREN,
      right=TK_RPAREN,
      delim=TK_COMMA,
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
    let (_, rparen_start, rparen_end) = self.previous_triple()
    let rparen_loc : Location = { start: rparen_start, end: rparen_end }
    (
      @list.from_array(args),
      lparen_loc,
      rparen_loc,
      @list.from_array(comma_locs),
      trailing_comma_loc,
    )
  }

  fn parse_constr_args_no_open() {
    self.surround_series(left=TK_LPAREN, right=TK_RPAREN, delim=TK_COMMA, _ => {
      self.parse_constr_pat_argument()
    })
    |> @list.from_array
  }

  match self.peek_token() {
    // bitstring constr pattern `i32(args)`
    // To disambiguate with pattern variable, the args list must be present.
    LIDENT(name) if self.peek_token(nth=1) is LPAREN => {
      self.skip() // skip lident
      let name_loc = self.peek_location()
      let binder : @syntax.Binder = { name, loc: name_loc }
      let args = parse_constr_args_no_open()
      let loc = self.loc_start_with(spos)
      @syntax.Pattern::SpecialConstr(binder~, args~, loc~)
    }
    PACKAGE_NAME(_) | UIDENT(_) | LIDENT(_) => {
      let constr = self.parse_constr()
      let maybe_args = self.optional([TK_LPAREN], _ => parse_constr_args())
      let mut is_open = false
      let mut dotdot_loc = None
      fn check_args(args : List[ConstrArgRelatedness]) {
        match args {
          Empty => @list.empty()
          More(Dotdot(loc), tail=Empty) => {
            is_open = true
            dotdot_loc = Some(loc)
            @list.empty()
          }
          More(Dotdot(loc), tail~) => {
            is_open = true
            dotdot_loc = Some(loc)
            self.report_error({
              loc,
              msg: "Unexpected `..` here, add `, ..` behind the last argument to ignore the rest arguments.",
            })
            check_args(tail)
          }
          More(Arg(arg), tail~) => check_args(tail).add(arg)
        }
      }

      let (args, lparen_loc, rparen_loc, comma_locs, trailing_comma_loc) = match
        maybe_args {
        Some((args, lparen_loc, rparen_loc, comma_locs, trailing_comma_loc)) =>
          (
            Some(check_args(args)),
            Some(lparen_loc),
            Some(rparen_loc),
            comma_locs,
            trailing_comma_loc,
          )
        None => (None, None, None, @list.empty(), None)
      }
      let loc = self.loc_start_with(spos)
      Constr(
        constr~,
        args~,
        is_open~,
        loc~,
        lparen_loc~,
        rparen_loc~,
        comma_locs~,
        trailing_comma_loc~,
        dotdot_loc~,
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
      Constant(c=Bool(true), loc~, uminus_loc=None)
    }
    FALSE => {
      let loc = self.peek_location()
      self.expect_token(TK_FALSE)
      Constant(c=Bool(false), loc~, uminus_loc=None)
    }
    CHAR(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_CHAR)
      Constant(c=Char(x), loc~, uminus_loc=None)
    }
    INT(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_INT)
      Constant(c=@syntax.make_int(x), loc~, uminus_loc=None)
    }
    BYTE(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_BYTE)
      Constant(c=Byte(x), loc~, uminus_loc=None)
    }
    FLOAT(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_FLOAT)
      Constant(c=@syntax.make_float(x), loc~, uminus_loc=None)
    }
    DOUBLE(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_DOUBLE)
      Constant(c=@syntax.make_double(x), loc~, uminus_loc=None)
    }
    MINUS => {
      let minus_loc = self.peek_location()
      self.expect_token(TK_MINUS)
      match self.peek_token() {
        INT(x) => {
          let loc = self.peek_location()
          self.expect_token(TK_INT)
          Constant(
            c=@syntax.make_int("-" + x),
            loc~,
            uminus_loc=Some(minus_loc),
          )
        }
        FLOAT(x) => {
          let loc = self.peek_location()
          self.expect_token(TK_FLOAT)
          Constant(
            c=@syntax.make_float("-" + x),
            loc~,
            uminus_loc=Some(minus_loc),
          )
        }
        DOUBLE(x) => {
          let loc = self.peek_location()
          self.expect_token(TK_DOUBLE)
          Constant(
            c=@syntax.make_double("-" + x),
            loc~,
            uminus_loc=Some(minus_loc),
          )
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
      Constant(c=Bytes(x), loc~, uminus_loc=None)
    }
    STRING(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_STRING)
      Constant(c=String(x), loc~, uminus_loc=None)
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
      let spos = self.peek_spos()
      let lparen_loc = self.peek_location()
      let result = self.surround(left=TK_LPAREN, right=TK_RPAREN, fn(state) {
        let first = state.parse_pattern()
        match state.peek_token() {
          COMMA => {
            let first_comma_loc = state.peek_location()
            state.skip()
            let (pats, comma_locs, trailing_comma_loc) = state.series_with_follow_info(
              delim=TK_COMMA,
              follow_set=[TK_RPAREN],
              State::parse_pattern,
            )
            let (_, rparen_start, rparen_end) = state.peek()
            let rparen_loc : Location = { start: rparen_start, end: rparen_end }
            if pats is [] {
              first
            } else {
              let comma_locs = @list.from_array(comma_locs).add(first_comma_loc)
              Tuple(
                pats=@list.from_array([first, ..pats]),
                loc=state.loc_start_with(spos),
                lparen_loc=Some(lparen_loc),
                rparen_loc=Some(rparen_loc),
                comma_locs~,
                trailing_comma_loc~,
              )
            }
          }
          COLON => {
            state.skip()
            let ty = state.parse_type()
            @syntax.Pattern::Constraint(
              pat=first,
              ty~,
              loc=state.loc_start_with(spos),
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
      let pats1 = []
      let pats2 = []
      let mut dotdot = NoDotDot
      let mut dotdot_loc = None
      fn push_pat(pat : @syntax.ArrayPattern) {
        if dotdot is NoDotDot {
          pats1.push(pat)
        } else {
          pats2.push(pat)
        }
      }

      let (_, comma_locs, trailing_comma_loc) = self.surround_series_info(
        left=TK_LBRACKET,
        right=TK_RBRACKET,
        delim=TK_COMMA,
        fn(self) {
          if self.peek_token() is DOTDOT {
            let pat_spos = self.peek_spos()
            let spread_dotdot_loc = self.peek_location()
            self.skip()
            match self.peek_token() {
              STRING(str) => {
                let loc = self.peek_location()
                self.skip()
                push_pat(
                  StringSpread(str~, loc~, dotdot_loc=Some(spread_dotdot_loc)),
                )
              }
              BYTES(bytes) => {
                let loc = self.peek_location()
                self.skip()
                push_pat(
                  BytesSpread(bytes~, loc~, dotdot_loc=Some(spread_dotdot_loc)),
                )
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
                    loc~,
                    dotdot_loc=Some(spread_dotdot_loc),
                  ),
                )
              }
              PACKAGE_NAME(pkg) => {
                self.skip()
                let id_loc = self.peek_location()
                let name = self.expect_dot_uident(context="name of constant")
                let binder : @syntax.Binder = { name, loc: id_loc }
                let loc = self.loc_start_with(pat_spos)
                push_pat(
                  ConstSpread(
                    binder~,
                    pkg=Some(pkg),
                    loc~,
                    dotdot_loc=Some(spread_dotdot_loc),
                  ),
                )
              }
              _ =>
                match dotdot {
                  NoBinder | WithBinder(_) | Underscore | WithAsBinder(_) =>
                    self.report_error({
                      loc: self.peek_location(),
                      msg: "At most one `..` is allowed in array pattern.",
                    })
                  NoDotDot => {
                    dotdot_loc = Some(spread_dotdot_loc)
                    dotdot = match self.peek_token() {
                      LIDENT(_) => {
                        let binder = self.parse_binder(context="")
                        WithBinder(binder)
                      }
                      AS(_) => {
                        self.skip()
                        let binder = self.parse_binder(context="variable name")
                        WithAsBinder(binder)
                      }
                      UNDERSCORE => {
                        self.skip()
                        Underscore
                      }
                      _ => NoBinder
                    }
                  }
                }
            }
          } else {
            push_pat(Pattern(self.parse_pattern()))
          }
        },
      )
      let (_, rbracket_start, rbracket_end) = self.previous_triple()
      let rbracket_loc : Location = { start: rbracket_start, end: rbracket_end }
      let pats1 = @list.from_array(pats1)
      let pats2 = @list.from_array(pats2)
      let comma_locs = @list.from_array(comma_locs)
      let pats : @syntax.ArrayPatterns = match dotdot {
        NoDotDot => Closed(pats1)
        NoBinder => Open(pats1, pats2, NoBinder)
        WithBinder(b) => Open(pats1, pats2, Binder(b))
        Underscore => Open(pats1, pats2, Underscore)
        WithAsBinder(b) => Open(pats1, pats2, BinderAs(b))
      }
      let loc = self.loc_start_with(spos)
      Array(
        pats~,
        loc~,
        lbracket_loc=Some(lbracket_loc),
        rbracket_loc=Some(rbracket_loc),
        comma_locs~,
        trailing_comma_loc~,
        dotdot_loc~,
      )
    }
    LBRACE => {
      let spos = self.peek_spos()
      let mut kind : BraceLikeSyntaxKind = Unknown
      let lbrace_loc = self.peek_location()
      self.expect_token(TK_LBRACE)
      self.push_sync(TK_RBRACE)
      let (field_pat_or_dotdots, comma_locs, trailing_comma_loc) = self.series_with_follow_info(
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
              loc,
              question_loc,
              colon_loc: Some(colon_loc),
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
      let comma_locs = @list.from_array(comma_locs)
      let loc = self.loc_start_with(spos)
      match kind {
        Unknown | Record => {
          // record pattern
          fn collect_fields(
            has_dotdot,
            dotdot_loc : Location?,
            acc : List[@syntax.FieldPat],
            field_or_dotdots : ArrayView[MapPatternFragment],
          ) -> @syntax.Pattern {
            match field_or_dotdots {
              [] =>
                Record(
                  fields=acc.rev(),
                  is_closed=!has_dotdot,
                  loc~,
                  lbrace_loc=Some(lbrace_loc),
                  rbrace_loc=Some(rbrace_loc),
                  comma_locs~,
                  trailing_comma_loc~,
                  dotdot_loc~,
                )
              [Dotdot(loc), .. rest] => {
                if !(rest is []) {
                  self.report_error({
                    loc,
                    msg: "Unexpected `..` here, add `, ..` behind the last field to ignore the rest of record.",
                  })
                }
                collect_fields(true, Some(loc), acc, rest)
              }
              [Field(field_pat), .. rest] =>
                collect_fields(has_dotdot, dotdot_loc, acc.add(field_pat), rest)
              [MapElem({ loc, .. }), .. rest] => {
                self.report_error({
                  loc,
                  msg: "Record pattern and map pattern cannot be mixed",
                })
                collect_fields(has_dotdot, dotdot_loc, acc, rest)
              }
              [Error, .. rest] =>
                collect_fields(has_dotdot, dotdot_loc, acc, rest)
            }
          }

          collect_fields(false, None, @list.new(), field_pat_or_dotdots)
        }
        Map => {
          // Error only appears when we fail to parse a contant pattern,
          // so it's fine to assume it is a map pattern in this case
          fn collect_elems(
            has_dotdot,
            dotdot_loc : Location?,
            acc : List[_],
            field_pat_or_dotdots : ArrayView[MapPatternFragment],
          ) -> @syntax.Pattern {
            match field_pat_or_dotdots {
              [] =>
                Map(
                  elems=acc.rev(),
                  is_closed=!has_dotdot,
                  loc~,
                  lbrace_loc=Some(lbrace_loc),
                  rbrace_loc=Some(rbrace_loc),
                  comma_locs~,
                  trailing_comma_loc~,
                  dotdot_loc~,
                )
              [Dotdot(loc), .. rest] => {
                if !(rest is []) {
                  self.report_error({ loc, msg: "Unexpected `..` here" })
                }
                collect_elems(true, Some(loc), acc, rest)
              }
              [Field({ loc, .. }), .. rest] => {
                self.report_error({ loc, msg: "Unexpected `..` here" })
                collect_elems(has_dotdot, dotdot_loc, acc, rest)
              }
              [Error, .. rest] =>
                collect_elems(has_dotdot, dotdot_loc, acc, rest)
              [MapElem(elem), .. rest] =>
                collect_elems(has_dotdot, dotdot_loc, acc.add(elem), rest)
            }
          }

          collect_elems(false, None, @list.new(), field_pat_or_dotdots)
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
      loc,
      colon_loc: None,
    }
  }
  let spos = self.peek_spos()
  let label = self.parse_label(context="struct field")
  if self.peek_token() is COLON {
    let colon_loc = self.peek_location()
    self.skip()
    let pattern = self.parse_pattern()
    let loc = self.loc_start_with(spos)
    { label, pattern, is_pun: false, loc, colon_loc: Some(colon_loc) }
  } else {
    let loc = self.loc_start_with(spos)
    let pattern : @syntax.Pattern = Var({ name: label.name, loc: label.loc })
    { label, pattern, is_pun: true, loc, colon_loc: None }
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
          (pat, Labelled(label), Some(equal_loc))
        }
        COMMA | RPAREN => {
          let loc = Location::trim_last_char(label.loc)
          let pat = @syntax.label_to_pat(loc~, label)
          (pat, LabelledPun(label), None)
        }
        _ => {
          let loc = self.peek_location()
          (Any(loc~), Labelled(label), None)
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
      { pat, kind: Labelled(label), equal_loc: Some(equal_loc) }
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
  NoBinder
  WithBinder(@syntax.Binder)
  Underscore
  WithAsBinder(@syntax.Binder)
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
fn State::parse_loop_expr(self : Self, label~ : @syntax.Label?) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let loop_keyword_loc = self.peek_location()
  self.skip() // skip LOOP token
  let arg = self.with_follow(follow_set=[TK_LBRACE], fn(_) { self.parse_expr() })
  let loop_loc = self.loc_start_with(spos)
  let lbrace_loc = self.peek_location()
  let body = match self.mode {
    Normal =>
      self.surround_series(
        invalid_delims=[TK_COMMA],
        left=TK_LBRACE,
        right=TK_RBRACE,
        delim=TK_SEMI,
        fn(_) { self.parse_case() },
      )
      |> @list.from_array
    Panic(_) => @list.new()
  }
  let (_, rbrace_start, rbrace_end) = self.previous_triple()
  let rbrace_loc : Location = { start: rbrace_start, end: rbrace_end }
  let loop_nobreak = if self.peek_token() is NOBREAK {
    self.with_follow(follow_set=[TK_LBRACE], fn(state) {
      state.expect_token(TK_NOBREAK)
    })
    Some(self.parse_block_expr())
  } else {
    None
  }
  let loc = self.loc_start_with(spos)
  Loop(
    arg~,
    body~,
    loop_nobreak~,
    label~,
    loop_loc~,
    loc~,
    loop_keyword_loc=Some(loop_keyword_loc),
    lbrace_loc=Some(lbrace_loc),
    rbrace_loc=Some(rbrace_loc),
  )
}

///|
fn State::parse_where_clause(self : State) -> @syntax.WhereClause? {
  if self.peek_token() is WHERE {
    let spos = self.peek_spos()
    let where_loc = self.peek_location()
    self.skip()
    let lbrace_loc = self.peek_location()
    self.expect_token(TK_LBRACE)
    let (fields, comma_locs, trailing_comma_loc) = self.series_with_follow_info(
      delim=TK_COMMA,
      follow_set=[TK_RBRACE],
      State::parse_field_def,
    )
    let fields = @list.from_array(fields)
    self.expect_token(TK_RBRACE)
    let (_, rbrace_start, rbrace_end) = self.previous_triple()
    let rbrace_loc : Location = { start: rbrace_start, end: rbrace_end }
    let loc = self.loc_start_with(spos)
    Some({
      fields,
      loc,
      where_loc: Some(where_loc),
      lbrace_loc: Some(lbrace_loc),
      rbrace_loc: Some(rbrace_loc),
      comma_locs: @list.from_array(comma_locs),
      trailing_comma_loc,
    })
  } else {
    None
  }
}

///|
fn State::parse_for_expr(self : Self, label~ : @syntax.Label?) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
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
    let body = self.parse_block_expr()
    let loc = self.loc_start_with(spos)
    For(
      binders=@list.new(),
      condition=None,
      continue_block=@list.new(),
      body~,
      for_else=None,
      label~,
      loc~,
      where_clause=None,
      for_loc=Some(for_loc),
      body_lbrace_loc=None,
      body_rbrace_loc=None,
      nobreak_loc=None,
      nobreak_lbrace_loc=None,
      nobreak_rbrace_loc=None,
      header_semi_locs=@list.empty(),
    )
  }

  match self.peek_token() {
    SEMI(_) | LBRACE => self.parse_for_loop(spos~, label~, for_loc~)
    UNDERSCORE | IN =>
      // The case of `in` is actually syntax-incorrect,
      // but it is very likely a `for .. in` loop,
      // so we treat it as `for .. in` for better error message & recovery.
      self.parse_foreach_loop(spos~, label~, for_loc~)
    LIDENT(_) =>
      match self.peek_token(nth=1) {
        EQUAL => self.parse_for_loop(spos~, label~, for_loc~)
        COMMA | IN => self.parse_foreach_loop(spos~, label~, for_loc~)
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
  for_loc~ : Location,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let (binders, comma_locs, _) = self.with_follow(follow_set=[TK_IN], fn(
    state,
  ) {
    state.series_with_follow_info(delim=TK_COMMA, follow_set=[TK_IN], fn(
      state,
    ) -> @syntax.Binder? {
      match state.peek_token() {
        UNDERSCORE => {
          state.skip()
          None
        }
        LIDENT(name) => {
          let loc = state.peek_location()
          state.skip()
          Some({ name, loc })
        }
        other => {
          let loc = state.peek_location()
          state.report_unexpected(other, [TK_LIDENT, TK_UNDERSCORE], loc)
          state.panic() |> ignore
          None
        }
      }
    })
  })
  let binders = @list.from_array(binders)
  let in_loc = self.peek_location()
  self.expect_token(TK_IN)
  let expr = self.parse_expr()
  let (init, continue_block) = if self.peek_token() is SEMI(_) {
    self.skip()
    let (init, _) = self.series_with_follow(
      delim=TK_COMMA,
      follow_set=[TK_SEMI, TK_LBRACE],
      State::parse_loop_binder_with_value,
    )
    let init = @list.from_array(init)
    let continue_block = if self.peek_token() is SEMI(_) {
      self.skip()
      let (continue_block, _) = self.series_with_follow(
        delim=TK_COMMA,
        follow_set=[TK_LBRACE],
        State::parse_loop_binder_with_value,
      )
      @list.from_array(continue_block)
    } else {
      @list.empty()
    }
    (init, continue_block)
  } else {
    (@list.empty(), @list.empty())
  }
  let body_info = self.parse_block_expr_info()
  let else_block = if self.peek_token() is NOBREAK {
    let nobreak_loc = self.peek_location()
    self.skip()
    let else_info = self.parse_block_expr_info()
    (
      Some(else_info.expr),
      Some(nobreak_loc),
      else_info.lbrace_loc,
      else_info.rbrace_loc,
    )
  } else {
    (None, None, None, None)
  }
  let where_clause = self.parse_where_clause()
  let (else_block, nobreak_loc, nobreak_lbrace_loc, nobreak_rbrace_loc) = else_block
  let loc = self.loc_start_with(spos)
  ForEach(
    binders~,
    expr~,
    init~,
    continue_block~,
    body=body_info.expr,
    else_block~,
    where_clause~,
    loc~,
    label~,
    for_loc=Some(for_loc),
    in_loc=Some(in_loc),
    body_lbrace_loc=body_info.lbrace_loc,
    body_rbrace_loc=body_info.rbrace_loc,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
    comma_locs=@list.from_array(comma_locs),
  )
}

///|
fn State::parse_for_loop(
  self : Self,
  spos~ : Position,
  label~ : @syntax.Label?,
  for_loc~ : Location,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let (binders, _) = self.series_with_follow(
    delim=TK_COMMA,
    follow_set=[TK_SEMI, TK_LBRACE],
    State::parse_loop_binder_with_value,
  )
  let binders = @list.from_array(binders)
  let (condition, continue_block, header_semi_locs) = match self.peek_token() {
    LBRACE => (None, @list.new(), @list.empty())
    SEMI(_) => {
      self.push_sync(TK_LBRACE)
      let first_semi_loc = self.peek_location()
      self.expect_token(TK_SEMI)
      let condition = self.optional(first_expr, State::try_parse_condition_expr)
      let second_semi_loc = self.peek_location()
      self.expect_token(TK_SEMI)
      let (continue_block, _) = self.series_with_follow(
        delim=TK_COMMA,
        follow_set=[TK_LBRACE],
        State::parse_loop_binder_with_value,
      )
      let continue_block = @list.from_array(continue_block)
      self.pop_sync(TK_LBRACE)
      (
        condition,
        continue_block,
        @list.from_array([first_semi_loc, second_semi_loc]),
      )
    }
    other => {
      let other_loc = self.peek_location()
      self.report_unexpected(other, [TK_SEMI, TK_LBRACE], other_loc)
      (None, @list.new(), @list.empty())
    }
  }
  let body_info = self.parse_block_expr_info()
  let (for_else, nobreak_loc, nobreak_lbrace_loc, nobreak_rbrace_loc) = if self.peek_token()
    is NOBREAK {
    let nobreak_loc = self.peek_location()
    self.with_follow(follow_set=[TK_LBRACE], fn(state) {
      state.expect_token(TK_NOBREAK)
    })
    let else_info = self.parse_block_expr_info()
    (
      Some(else_info.expr),
      Some(nobreak_loc),
      else_info.lbrace_loc,
      else_info.rbrace_loc,
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
    body=body_info.expr,
    for_else~,
    label~,
    loc~,
    where_clause~,
    for_loc=Some(for_loc),
    body_lbrace_loc=body_info.lbrace_loc,
    body_rbrace_loc=body_info.rbrace_loc,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
    header_semi_locs~,
  )
}

///|
fn State::parse_while_loop(
  self : Self,
  label~ : @syntax.Label?,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let while_loc = self.peek_location()
  self.expect_token(TK_WHILE)
  let loop_cond = self.with_follow(
    follow_set=[TK_LBRACE],
    State::try_parse_condition_expr,
  )
  let loop_body = self.parse_block_expr_info()
  let (while_else, nobreak_loc, nobreak_lbrace_loc, nobreak_rbrace_loc) = if self.peek_token()
    is NOBREAK {
    let nobreak_loc = self.peek_location()
    self.with_follow(follow_set=[TK_LBRACE], fn(state) {
      state.expect_token(TK_NOBREAK)
    })
    let else_info = self.parse_block_expr_info()
    (
      Some(else_info.expr),
      Some(nobreak_loc),
      else_info.lbrace_loc,
      else_info.rbrace_loc,
    )
  } else {
    (None, None, None, None)
  }
  let loc = self.loc_start_with(spos)
  While(
    loop_cond~,
    loop_body=loop_body.expr,
    while_else~,
    loc~,
    label~,
    while_loc=Some(while_loc),
    loop_lbrace_loc=loop_body.lbrace_loc,
    loop_rbrace_loc=loop_body.rbrace_loc,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
  )
}

///|
fn State::try_parse_pipe_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  match self.peek_token() {
    // an error production to handle `try invalid-pipe-expr`
    IF
    | MATCH
    | LOOP
    | WHILE
    | FOR
    | TRY
    | LET
    | GUARD
    | BREAK
    | RETURN
    | CONTINUE =>
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
  let try_keyword_loc = self.peek_location()
  self.expect_token(TK_TRY)
  let body = self.try_parse_pipe_expr()
  let catch_start = self.peek_spos()
  let catch_keyword_loc = self.peek_location()
  self.expect_token(TK_CATCH)
  let catch_bang_loc = if self.peek_token() is EXCLAMATION {
    let catch_bang_loc = self.peek_location()
    self.skip()
    Some(catch_bang_loc)
  } else {
    None
  }
  let catch_all = catch_bang_loc is Some(_)
  let catch_lbrace_loc = self.peek_location()
  let lbrace_epos = self.peek_epos()
  // the catch_loc is the location of `catch {` or `catch! {`
  let catch_loc : Location = { start: catch_start, end: lbrace_epos }
  let catch_cases = self.surround_series(
      invalid_delims=[TK_COMMA],
      left=TK_LBRACE,
      right=TK_RBRACE,
      delim=TK_SEMI,
      State::parse_case,
    )
    |> @list.from_array
  let (_, catch_rbrace_start, catch_rbrace_end) = self.previous_triple()
  let catch_rbrace_loc : Location = {
    start: catch_rbrace_start,
    end: catch_rbrace_end,
  }
  let (try_else, else_loc, noraise_loc, else_lbrace_loc, else_rbrace_loc) = if self.peek_token()
    is NORAISE {
    let else_spos = self.peek_spos()
    let noraise_loc = self.peek_location()
    self.skip()
    let else_lbrace_loc = self.peek_location()
    let lbrace_epos = self.peek_epos()
    // the else_loc is the location of `noraise {`
    let else_loc : Location = { start: else_spos, end: lbrace_epos }
    let try_else_cases = self.surround_series(
        invalid_delims=[TK_COMMA],
        left=TK_LBRACE,
        right=TK_RBRACE,
        delim=TK_SEMI,
        State::parse_case,
      )
      |> @list.from_array
    let (_, else_rbrace_start, else_rbrace_end) = self.previous_triple()
    (
      Some(try_else_cases),
      else_loc,
      Some(noraise_loc),
      Some(else_lbrace_loc),
      Some(Location::{ start: else_rbrace_start, end: else_rbrace_end }),
    )
  } else {
    (None, @syntax.no_location, None, None, None)
  }
  let loc = self.loc_start_with(spos)
  Try(
    body~,
    catch_=catch_cases,
    catch_all~,
    try_else~,
    has_try=true,
    try_loc=try_keyword_loc,
    catch_loc~,
    else_loc~,
    loc~,
    try_keyword_loc=Some(try_keyword_loc),
    catch_keyword_loc=Some(catch_keyword_loc),
    catch_bang_loc~,
    catch_lbrace_loc=Some(catch_lbrace_loc),
    catch_rbrace_loc=Some(catch_rbrace_loc),
    noraise_loc~,
    else_lbrace_loc~,
    else_rbrace_loc~,
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
  TryOperator(
    body~,
    kind=Question,
    try_loc~,
    loc~,
    question_loc=Some(try_loc.last_char()),
    bang_loc=None,
  )
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
  TryOperator(
    body~,
    kind=Exclamation,
    try_loc~,
    loc~,
    question_loc=None,
    bang_loc=Some(try_loc.last_char()),
  )
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
  let ifso = self.parse_block_expr_info()
  let (ifnot, else_loc, else_lbrace_loc, else_rbrace_loc) = if self.peek_token()
    is ELSE {
    let else_loc = self.peek_location()
    self.skip()
    if self.peek_token() is IF {
      (Some(self.parse_if_expr()), Some(else_loc), None, None)
    } else {
      let else_info = self.parse_block_expr_info()
      (
        Some(else_info.expr),
        Some(else_loc),
        else_info.lbrace_loc,
        else_info.rbrace_loc,
      )
    }
  } else {
    (None, None, None, None)
  }
  let loc = self.loc_start_with(spos)
  If(
    cond~,
    ifso=ifso.expr,
    ifnot~,
    loc~,
    if_loc=Some(if_loc),
    else_loc~,
    if_lbrace_loc=ifso.lbrace_loc,
    if_rbrace_loc=ifso.rbrace_loc,
    else_lbrace_loc~,
    else_rbrace_loc~,
  )
}

///|
fn State::parse_match_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let match_keyword_loc = self.peek_location()
  self.expect_token(TK_MATCH)
  let expr = self.with_follow(follow_set=[TK_LBRACE], fn(_) {
    self.try_parse_condition_expr()
  })
  let lbrace_loc = self.peek_location()
  let lbrace_epos = self.peek_epos()
  // The main_loc is the location of `match expr {`
  let match_loc : Location = { start: spos, end: lbrace_epos }
  let cases = self.surround_series(
      invalid_delims=[TK_COMMA],
      left=TK_LBRACE,
      right=TK_RBRACE,
      delim=TK_SEMI,
      State::parse_case,
    )
    |> @list.from_array
  let (_, rbrace_start, rbrace_end) = self.previous_triple()
  let rbrace_loc : Location = { start: rbrace_start, end: rbrace_end }
  let loc = self.loc_start_with(spos)
  Match(
    expr~,
    cases~,
    match_loc~,
    loc~,
    match_keyword_loc=Some(match_keyword_loc),
    lbrace_loc=Some(lbrace_loc),
    rbrace_loc=Some(rbrace_loc),
  )
}

///|
fn State::parse_case(self : Self) -> @syntax.Case {
  if self.mode is Panic(loc~, ..) {
    return {
      pattern: Any(loc~),
      guard_: None,
      guard_keyword_loc: None,
      body: Hole(loc~, kind=Synthesized),
      fat_arrow_loc: None,
    }
  }
  if self.peek_token() is ELLIPSIS {
    let loc = self.peek_location()
    self.skip()
    {
      pattern: Any(loc~),
      guard_: None,
      guard_keyword_loc: None,
      body: Hole(loc~, kind=Todo),
      fat_arrow_loc: None,
    }
  } else {
    let pat = self.with_syncs([TK_FAT_ARROW, TK_IF], State::parse_pattern)
    let (guard_, guard_keyword_loc) = if self.peek_token() is IF {
      let guard_keyword_loc = self.peek_location()
      self.skip()
      let expr = match self.peek_token() {
        // an error production to handle `pattern if invalid-infix-expr => action`
        IF
        | MATCH
        | LOOP
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
      (Some(expr), Some(guard_keyword_loc))
    } else {
      (None, None)
    }
    let (body, fat_arrow_loc) = self.parse_fat_arrow_action()
    {
      pattern: pat,
      guard_,
      guard_keyword_loc,
      body,
      fat_arrow_loc: Some(fat_arrow_loc),
    }
  }
}

///|
fn State::parse_fat_arrow_action(self : State) -> (@syntax.Expr, Location) {
  let fat_arrow_loc = self.peek_location()
  self.expect_token(TK_FAT_ARROW)
  let body = match self.peek_token() {
    // an error production to handle `pattern => invalid-expr-statement`
    LET | LETREC | GUARD | DEFER =>
      self.handle_unexpected_expr_or_statement(
        context="the action part of a matching case",
      )
    _ => self.parse_expr_stmt()
  }
  (body, fat_arrow_loc)
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
  let lexmatch_loc = self.peek_location()
  self.expect_token(TK_LEXMATCH)
  let (expr, strategy) = self.with_follow(follow_set=[TK_LBRACE], fn(state) {
    let expr = state.try_parse_condition_expr()
    let strategy = state.parse_opt_with_strategy()
    (expr, strategy)
  })
  let (strategy, with_loc) = strategy
  let lbrace_loc = self.peek_location()
  let match_loc = @basic.Location::{ start: spos, end: self.peek_epos() }
  fn parse_body(state : State) -> @list.List[@syntax.LexCase] {
    let (cases, _) = state.series_with_follow(
      invalid_delims=[TK_COMMA],
      delim=TK_SEMI,
      follow_set=[TK_RBRACE],
      State::parse_lexmatch_case,
    )
    @list.from_array(cases)
  }

  let cases = self.surround(left=TK_LBRACE, right=TK_RBRACE, parse_body)
  let (_, rbrace_start, rbrace_end) = self.previous_triple()
  let rbrace_loc : Location = { start: rbrace_start, end: rbrace_end }
  let loc = self.loc_start_with(spos)
  @syntax.Expr::LexMatch(
    strategy~,
    expr~,
    cases~,
    match_loc~,
    loc~,
    lexmatch_loc=Some(lexmatch_loc),
    with_loc~,
    lbrace_loc=Some(lbrace_loc),
    rbrace_loc=Some(rbrace_loc),
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
        guard_keyword_loc: None,
        body: Hole(loc~, kind=Todo),
        fat_arrow_loc: None,
      }
    }
    _ => {
      let (pat, pat_loc) = self.with_syncs(
        [TK_FAT_ARROW, TK_COMMA],
        State::parse_lex_pattern,
      )
      let (guard_, guard_keyword_loc) = self.parse_lex_case_guard()
      let (body, fat_arrow_loc) = self.parse_fat_arrow_action()
      {
        pat,
        pat_loc,
        guard_,
        guard_keyword_loc,
        body,
        fat_arrow_loc: Some(fat_arrow_loc),
      }
    }
  }
}

///|
fn State::parse_lex_case_guard(self : State) -> (@syntax.Expr?, Location?) {
  match self.peek_token() {
    IF => {
      let guard_keyword_loc = self.peek_location()
      self.skip()
      // an error production to handle `lex pattern if invalid-infix-expr => action`
      let expr = match self.peek_token() {
        IF
        | MATCH
        | LOOP
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
      (Some(expr), Some(guard_keyword_loc))
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
  let pats = self.parse_lex_pattern_sequence()
  match pats {
    More(pat, tail=Empty) if self.peek_token() is AS => {
      self.skip()
      let binder = self.parse_binder(context="binder in `as` pattern")
      let loc = self.loc_start_with(spos)
      Alias(pat~, binder~, loc~)
    }
    More(pat, tail=Empty) => return pat
    _ => {
      let loc = self.loc_start_with(spos)
      Sequence(pats~, loc~)
    }
  }
}

///|
fn State::parse_lex_pattern_sequence(
  self : State,
) -> @list.List[@syntax.LexPattern] {
  let first = self.parse_lex_atom_pattern()
  let pats = [first]
  for tok = self.peek_token() {
    match tok {
      SEMI(_)
      | REGEX_LITERAL(_)
      | REGEX_INTERP(_)
      | STRING(_)
      | INTERP(_)
      | UIDENT(_)
      | PACKAGE_NAME(_)
      | LPAREN => {
        if tok is SEMI(_) {
          self.skip()
        }
        let pat = self.parse_lex_atom_pattern()
        pats.push(pat)
        continue self.peek_token()
      }
      _ => break ()
    }
  }
  @list.from_array(pats)
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
      let pats = self.with_syncs([TK_RPAREN], State::parse_lex_pattern_sequence)
      match pats {
        More(pat, tail=Empty) =>
          match self.peek_token() {
            AS => {
              self.skip()
              let binder = self.parse_binder(context="binder in `as` pattern")
              let loc = self.loc_start_with(alias_spos)
              self.expect_token(TK_RPAREN)
              Alias(pat~, binder~, loc~)
            }
            _ => {
              self.expect_token(TK_RPAREN)
              pat
            }
          }
        _ => {
          self.expect_token(TK_RPAREN)
          Sequence(pats~, loc=self.peek_location())
        }
      }
    }
    other => {
      let other_loc = self.peek_location()
      self.report_failed_to_parse(other, "lex atom pattern", other_loc)
      let loc = self.panic()
      Sequence(pats=@list.empty(), loc~)
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
      self.expect_token(TK_RPAREN)
      self.pop_sync(TK_RPAREN)
      let loc = state.loc_start_with(spos)
      @syntax.Expr::Unit(loc~, faked=false)
    }
    Some(first) =>
      match state.peek_token() {
        COLON => {
          state.expect_token(TK_COLON)
          let ty = state.parse_type()
          self.expect_token(TK_RPAREN)
          self.pop_sync(TK_RPAREN)
          let loc = state.loc_start_with(spos)
          Constraint(expr=first, ty~, loc~)
        }
        COMMA => {
          let first_comma_loc = state.peek_location()
          state.expect_token(TK_COMMA)
          let (rest, comma_locs, trailing_comma_loc) = state.series_with_follow_info(
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
              loc~,
              lparen_loc=Some(lparen_loc),
              rparen_loc=Some(rparen_loc),
              lbrace_loc=None,
              rbrace_loc=None,
            )
          } else {
            let rest = @list.from_array(rest)
            let comma_locs = @list.from_array(comma_locs).add(first_comma_loc)
            Tuple(
              exprs=rest.add(first),
              loc~,
              lparen_loc=Some(lparen_loc),
              rparen_loc=Some(rparen_loc),
              comma_locs~,
              trailing_comma_loc~,
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
            loc~,
            lparen_loc=Some(lparen_loc),
            rparen_loc=Some(rparen_loc),
            lbrace_loc=None,
            rbrace_loc=None,
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
  let (expr_or_binders, comma_locs, trailing_comma_loc) = self.series_with_follow_info(
    delim=TK_COMMA,
    follow_set=[TK_RPAREN],
    State::parse_expr_or_binder,
  )
  let rparen_loc = self.peek_location()
  self.expect_token(TK_RPAREN)
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
        func=@syntax.make_arrow_fn(params_loc~, loc~, parameters, body),
      )
    }
    (_, []) => {
      let loc = self.loc_start_with(spos)
      handle_expr(Unit(loc~, faked=false))
    }
    (_, [Binder(expr, ty~, colon_loc=_)]) if trailing_comma_loc is None => {
      let loc = self.loc_start_with(spos)
      handle_expr(Constraint(expr~, ty~, loc~))
    }
    (_, [Expr(expr)]) => {
      let loc = self.loc_start_with(spos)
      handle_expr(
        Group(
          expr~,
          group=Paren,
          loc~,
          lparen_loc=Some(lparen_loc),
          rparen_loc=Some(rparen_loc),
          lbrace_loc=None,
          rbrace_loc=None,
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
      let comma_locs = @list.from_array(comma_locs)
      let loc = self.loc_start_with(spos)
      handle_expr(
        Tuple(
          exprs~,
          loc~,
          lparen_loc=Some(lparen_loc),
          rparen_loc=Some(rparen_loc),
          comma_locs~,
          trailing_comma_loc~,
        ),
      )
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
  let kind : @syntax.PredicateQuantifierKind = match self.peek_token() {
    FORALL => {
      self.skip()
      Forall
    }
    EXISTS => {
      self.skip()
      Exists
    }
    _ => panic()
  }
  let binder = self.parse_binder(context="quantifier binder")
  self.expect_token(TK_COLON)
  let binder_ty = self.parse_type()
  self.expect_token(TK_COMMA)
  let body = match self.peek_token() {
    FORALL | EXISTS => self.parse_quantifier_expr()
    _ => self.parse_infix_expr(parsed=None)
  }
  let loc = self.loc_start_with(spos)
  Quantifier(kind~, binder~, binder_ty~, body~, loc~)
}

///|
fn State::parse_expr(self : Self) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  match self.peek_token() {
    POST_LABEL(label_name) => {
      let label : @syntax.Label = {
        name: label_name,
        loc: self.peek_location(),
      }
      self.skip()
      self.expect_token(TK_COLON)
      match self.peek_token() {
        LOOP => self.parse_loop_expr(label=Some(label))
        FOR => self.parse_for_expr(label=Some(label))
        WHILE => self.parse_while_loop(label=Some(label))
        other => {
          let loc = self.peek_location()
          self.report_unexpected(other, [TK_LOOP, TK_FOR, TK_WHILE], loc)
          self.panic() |> ignore
          Hole(loc~, kind=Synthesized)
        }
      }
    }
    LEXMATCH => self.parse_lexmatch_expr()
    LOOP => self.parse_loop_expr(label=None)
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
      let catch_keyword_loc = self.peek_location()
      self.skip()
      let catch_bang_loc = if self.peek_token() is EXCLAMATION {
        let catch_bang_loc = self.peek_location()
        self.skip()
        Some(catch_bang_loc)
      } else {
        None
      }
      let catch_all = catch_bang_loc is Some(_)
      let catch_lbrace_loc = self.peek_location()
      let catch_loc = self.loc_start_with(catch_start)
      let catch_cases = self.surround_series(
          left=TK_LBRACE,
          right=TK_RBRACE,
          delim=TK_SEMI,
          State::parse_case,
        )
        |> @list.from_array
      let (_, catch_rbrace_start, catch_rbrace_end) = self.previous_triple()
      let catch_rbrace_loc : Location = {
        start: catch_rbrace_start,
        end: catch_rbrace_end,
      }
      let (try_else, else_loc, noraise_loc, else_lbrace_loc, else_rbrace_loc) = if self.peek_token()
        is NORAISE {
        let else_spos = self.peek_spos()
        let noraise_loc = self.peek_location()
        self.skip()
        let else_lbrace_loc = self.peek_location()
        let lbrace_epos = self.peek_epos()
        // the else_loc is the location of `else {` or `noraise {`
        let else_loc : Location = { start: else_spos, end: lbrace_epos }
        let try_else_cases = self.surround_series(
            invalid_delims=[TK_COMMA],
            left=TK_LBRACE,
            right=TK_RBRACE,
            delim=TK_SEMI,
            State::parse_case,
          )
          |> @list.from_array
        let (_, else_rbrace_start, else_rbrace_end) = self.previous_triple()
        (
          Some(try_else_cases),
          else_loc,
          Some(noraise_loc),
          Some(else_lbrace_loc),
          Some(Location::{ start: else_rbrace_start, end: else_rbrace_end }),
        )
      } else {
        (None, @syntax.no_location, None, None, None)
      }
      let loc = self.loc_start_with(spos)
      Try(
        body=e,
        catch_=catch_cases,
        catch_all~,
        try_else~,
        has_try=false,
        try_loc=catch_loc,
        catch_loc~,
        else_loc~,
        loc~,
        try_keyword_loc=None,
        catch_keyword_loc=Some(catch_keyword_loc),
        catch_bang_loc~,
        catch_lbrace_loc=Some(catch_lbrace_loc),
        catch_rbrace_loc=Some(catch_rbrace_loc),
        noraise_loc~,
        else_lbrace_loc~,
        else_rbrace_loc~,
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
      let (expr_or_binders, comma_locs, trailing_comma_loc) = self.series_with_follow_info(
        delim=TK_COMMA,
        follow_set=[TK_RPAREN],
        State::parse_expr_or_binder,
      )
      let rparen_loc = self.peek_location()
      self.expect_token(TK_RPAREN)
      let params_loc = self.loc_start_with(spos)
      match (self.peek_token(), expr_or_binders) {
        (FAT_ARROW, _) => {
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
          Err(Some(@syntax.Expr::Unit(loc~, faked=false)))
        }
        (_, [Binder(expr, ty~, colon_loc=_)]) if trailing_comma_loc is None => {
          let loc = self.loc_start_with(spos)
          Err(Some(@syntax.Expr::Constraint(expr~, ty~, loc~)))
        }
        (_, [Expr(expr)]) => {
          let loc = self.loc_start_with(spos)
          Err(
            Some(
              @syntax.Group(
                expr~,
                group=Paren,
                loc~,
                lparen_loc=Some(lparen_loc),
                rparen_loc=Some(rparen_loc),
                lbrace_loc=None,
                rbrace_loc=None,
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
                loc~,
                lparen_loc=Some(lparen_loc),
                rparen_loc=Some(rparen_loc),
                comma_locs=@list.from_array(comma_locs),
                trailing_comma_loc~,
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
        lhs = Pipe(lhs~, rhs~, loc~)
      }
      PIPE_LEFT => {
        self.skip()
        let rhs = match self.try_parse_arrow_fn_expr(explicit_brace_body=true) {
          Ok(fn_expr) => fn_expr
          Err(parsed) => self.parse_infix_expr(parsed~)
        }
        let loc = self.loc_start_with(spos)
        lhs = RevPipe(lhs~, rhs~, loc~)
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
      Ident(name="→") => @syntax.Expr::Implies(lhs~, rhs~, loc~)
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
) -> (@syntax.Label, @syntax.Binder?) {
  match self.peek_token() {
    LIDENT(name) if self.peek_token(nth=1) is EQUAL => {
      let label : @syntax.Label = { name, loc: self.peek_location() }
      self.skip()
      self.expect_token(TK_EQUAL)
      let binder = self.parse_binder(context="regex match binding")
      (label, Some(binder))
    }
    POST_LABEL(name) => {
      let label : @syntax.Label = { name, loc: self.peek_location() }
      self.skip()
      (label, None)
    }
    other => {
      let loc = self.peek_location()
      self.report_unexpected(other, [TK_LIDENT, TK_POST_LABEL], loc)
      self.panic() |> ignore
      ({ name: "", loc }, None)
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
    self.skip()
    let rhs = self.parse_regex_atom_pattern()
    let loc = self.loc_start_with(spos)
    pat = @syntax.RegexPattern::Sequence(pat1=pat, pat2=rhs, loc~)
  }
  pat
}

///|
fn State::parse_regex_or_pattern(self : Self) -> @syntax.RegexPattern {
  let spos = self.peek_spos()
  let mut pat = self.parse_regex_sequence_pattern()
  while self.peek_token() is BAR {
    self.skip()
    let rhs = self.parse_regex_sequence_pattern()
    let loc = self.loc_start_with(spos)
    pat = @syntax.RegexPattern::Alternation(pat1=pat, pat2=rhs, loc~)
  }
  pat
}

///|
fn State::parse_regex_as_pattern(self : Self) -> @syntax.RegexPattern {
  let spos = self.peek_spos()
  let pat = self.parse_regex_or_pattern()
  if self.peek_token() is AS {
    self.skip()
    let binder = self.parse_binder(context="regex alias")
    let loc = self.loc_start_with(spos)
    @syntax.RegexPattern::Alias(pat~, binder~, loc~)
  } else {
    pat
  }
}

///|
fn State::parse_regex_match_rhs(
  self : Self,
) -> (@syntax.RegexPattern, @list.List[(@syntax.Label, @syntax.Binder?)]) {
  match self.peek_token() {
    LPAREN => {
      self.skip()
      let pat = self.parse_regex_as_pattern()
      let bindings = if self.peek_token() is COMMA {
        self.skip()
        let (bindings, _) = self.series_with_follow(
          delim=TK_COMMA,
          follow_set=[TK_RPAREN],
          State::parse_regex_match_binding,
        )
        @list.from_array(bindings)
      } else {
        @list.empty()
      }
      self.expect_token(TK_RPAREN)
      (pat, bindings)
    }
    _ => (self.parse_regex_atom_pattern(), @list.empty())
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
      self.skip()
      let trait_ = self.parse_type_name()
      let loc = self.loc_start_with(spos)
      As(expr~, trait_~, loc~)
    }
    IS => {
      let is_loc = self.peek_location()
      self.skip()
      let pat = self.parse_range_pattern()
      let loc = self.loc_start_with(spos)
      Is(expr~, pat~, loc~, is_loc=Some(is_loc))
    }
    LEXMATCH_QUESTION => {
      let lexmatch_question_loc = self.peek_location()
      self.skip()
      let (pat, pat_loc) = self.parse_lex_pattern()
      let (strategy, with_loc) = self.parse_opt_with_strategy()
      IsLexMatch(
        expr~,
        strategy~,
        pat~,
        pat_loc~,
        loc=self.loc_start_with(spos),
        lexmatch_loc=Some(lexmatch_question_loc.trim_last_char()),
        question_loc=Some(lexmatch_question_loc.last_char()),
        with_loc~,
      )
    }
    EQ_TILDE => {
      self.skip()
      let (pat, bindings) = self.parse_regex_match_rhs()
      let loc = self.loc_start_with(spos)
      RegexMatch(expr~, pat~, bindings~, loc~)
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
      let op_loc = self.peek_location()
      self.expect_token(TK_PLUS)
      let expr = self.parse_prefix_expr(parsed=None)
      let loc = self.loc_start_with(spos)
      @syntax.make_uplus(loc~, "+", expr, op_loc=Some(op_loc))
    }
    MINUS if parsed is None => {
      let op_loc = self.peek_location()
      self.expect_token(TK_MINUS)
      let expr = self.parse_prefix_expr(parsed=None)
      let loc = self.loc_start_with(spos)
      @syntax.make_uminus(loc~, "-", expr, op_loc=Some(op_loc))
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
          EXCLAMATION | QUESTION | LPAREN => {
            let (attr, bang_loc, question_loc) = self.parse_apply_attr_info()
            let lparen_loc = self.peek_location()
            let (args, comma_locs, trailing_comma_loc) = self.surround_series_info(
              left=TK_LPAREN,
              right=TK_RPAREN,
              delim=TK_COMMA,
              State::parse_argument,
            )
            let (_, rparen_start, rparen_end) = self.previous_triple()
            let rparen_loc : Location = { start: rparen_start, end: rparen_end }
            let args = @list.from_array(args)
            let comma_locs = @list.from_array(comma_locs)
            let loc = self.loc_start_with(spos)
            go(
              Apply(
                func=left,
                args~,
                attr~,
                loc~,
                bang_loc~,
                question_loc~,
                lparen_loc=Some(lparen_loc),
                rparen_loc=Some(rparen_loc),
                comma_locs~,
                trailing_comma_loc~,
              ),
            )
          }
          LBRACKET => {
            fn parse_index() {
              match self.peek_token() {
                COLON => {
                  self.skip()
                  let end_index = self.optional(first_expr, State::parse_expr)
                  SliceIndex(None, end_index)
                }
                _ => {
                  let start_index = self.parse_expr()
                  match self.peek_token() {
                    COLON => {
                      self.skip()
                      let end_index = self.optional(
                        first_expr,
                        State::parse_expr,
                      )
                      SliceIndex(Some(start_index), end_index)
                    }
                    _ => ArrayIndex(start_index)
                  }
                }
              }
            }

            let index_spos = self.peek_spos()
            let index = self.surround(left=TK_LBRACKET, right=TK_RBRACKET, _ => {
              parse_index()
            })
            let loc = self.loc_start_with(index_spos)
            let index_loc = self.loc_start_with(index_spos)
            let array_get_expr : @syntax.Expr = match index {
              ArrayIndex(index) => ArrayGet(loc~, array=left, index~)
              SliceIndex(start_index, end_index) =>
                ArrayGetSlice(
                  loc~,
                  index_loc~,
                  array=left,
                  start_index~,
                  end_index~,
                )
            }
            go(array_get_expr)
          }
          DOT_LIDENT(name) => {
            let label_loc = self.peek_location().trim_first_char()
            let label : @syntax.Label = { name, loc: label_loc }
            self.skip()
            let (attr, bang_loc, question_loc) = self.parse_apply_attr_info()
            match self.peek_token() {
              LPAREN => {
                let lparen_loc = self.peek_location()
                let (args, comma_locs, trailing_comma_loc) = self.surround_series_info(
                  left=TK_LPAREN,
                  right=TK_RPAREN,
                  delim=TK_COMMA,
                  State::parse_argument,
                )
                let (_, rparen_start, rparen_end) = self.previous_triple()
                let rparen_loc : Location = {
                  start: rparen_start,
                  end: rparen_end,
                }
                let args = @list.from_array(args)
                let comma_locs = @list.from_array(comma_locs)
                let loc = self.loc_start_with(spos)
                let expr : @syntax.Expr = DotApply(
                  self=left,
                  method_name=label,
                  args~,
                  return_self=false,
                  attr~,
                  loc~,
                  bang_loc~,
                  question_loc~,
                  lparen_loc=Some(lparen_loc),
                  rparen_loc=Some(rparen_loc),
                  comma_locs~,
                  trailing_comma_loc~,
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
            self.skip()
            let label = self.parse_label(context="method name")
            let (attr, bang_loc, question_loc) = self.parse_apply_attr_info()
            let lparen_loc = self.peek_location()
            let (args, comma_locs, trailing_comma_loc) = self.surround_series_info(
              left=TK_LPAREN,
              right=TK_RPAREN,
              delim=TK_COMMA,
              State::parse_argument,
            )
            let (_, rparen_start, rparen_end) = self.previous_triple()
            let rparen_loc : Location = { start: rparen_start, end: rparen_end }
            let args = @list.from_array(args)
            let comma_locs = @list.from_array(comma_locs)
            let loc = self.loc_start_with(spos)
            let expr : @syntax.Expr = DotApply(
              self=left,
              method_name=label,
              args~,
              return_self=true,
              attr~,
              loc~,
              bang_loc~,
              question_loc~,
              lparen_loc=Some(lparen_loc),
              rparen_loc=Some(rparen_loc),
              comma_locs~,
              trailing_comma_loc~,
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
    Package(_) | TypeNameWithConstrPackage(_) =>
      self.expect_dot_uident(context="constructor name")
    NoExtraInfo | TypeName(_) => self.expect_uident(context="constructor name")
  }
  let name_loc = self.loc_start_with(name_loc_start)
  let loc = self.loc_start_with(spos)
  let constr : @syntax.Constructor = {
    extra_info,
    name: { name: constr_name, loc: name_loc },
    loc: name_loc,
  }
  // constructor with arguments is handled by [Apply],
  // and desugared during ast linting
  Constr(constr~, loc~)
}

///|
priv enum IndexingSyntaxFragment {
  ArrayIndex(@syntax.Expr)
  SliceIndex(@syntax.Expr?, @syntax.Expr?)
}

///|
fn State::parse_argument(self : Self) -> @syntax.Argument {
  if self.mode is Panic(loc~, ..) {
    return {
      value: Hole(loc~, kind=Synthesized),
      kind: @syntax.Positional,
      equal_loc: None,
    }
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
        LabelledOption(label~, question_loc~)
      } else {
        Labelled(label)
      }
      let equal_loc = self.peek_location()
      self.expect_token(TK_EQUAL) // `=`
      let value = self.parse_expr()
      { value, kind, equal_loc: Some(equal_loc) }
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
        self.skip()
        let value = self.parse_expr()
        let loc = self.loc_start_with(spos)
        self.report_error({
          loc,
          msg: "Invalid syntax `label~=value`. Do you mean `label=value`?",
        })
        { value, kind: Labelled(label), equal_loc: None }
      } else {
        let value = @syntax.label_to_expr(
          loc=Location::trim_last_char(label.loc),
          label,
        )
        { value, kind: LabelledPun(label), equal_loc: None }
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
      { value, kind, equal_loc: None }
    }
    _ => {
      let value = self.parse_expr()
      { value, kind: Positional, equal_loc: None }
    }
  }
}

///|
fn State::parse_func(
  self : Self,
  has_error~ : Location?,
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
      has_error,
      is_async,
      loc,
      fn_loc,
      body_lbrace_loc: None,
      body_rbrace_loc: None,
      body_semi_locs: @list.empty(),
      body_trailing_semi_loc: None,
    }
  }
  match self.peek_token() {
    LPAREN => {
      let param_loc_start = self.peek_spos()
      self.push_sync(TK_LBRACE)
      let params = self.surround_series(
        left=TK_LPAREN,
        right=TK_RPAREN,
        delim=TK_COMMA,
        fn(s) -> @syntax.Parameter { s.parse_parameter() },
      )
      let params_loc = self.loc_start_with(param_loc_start)
      let (return_type, error_type) = self.parse_func_return_type()
      self.pop_sync(TK_LBRACE)
      let body_info = self.parse_block_expr_info()
      let func_loc = match fn_loc {
        Some(fn_loc) =>
          Location::merge(
            fn_loc,
            match body_info.rbrace_loc {
              Some(rbrace_loc) => rbrace_loc
              None => body_info.expr.loc()
            },
          )
        None => body_info.expr.loc()
      }
      @syntax.Func::{
        parameters: @list.from_array(params),
        params_loc,
        body: body_info.expr,
        return_type,
        error_type,
        kind: Lambda,
        has_error,
        is_async,
        loc: func_loc,
        fn_loc,
        body_lbrace_loc: body_info.lbrace_loc,
        body_rbrace_loc: body_info.rbrace_loc,
        body_semi_locs: body_info.semi_locs,
        body_trailing_semi_loc: body_info.trailing_semi_loc,
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
        has_error,
        is_async,
        loc,
        fn_loc,
        body_lbrace_loc: None,
        body_rbrace_loc: None,
        body_semi_locs: @list.empty(),
        body_trailing_semi_loc: None,
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
      let colon_loc = if self.peek_token() is COLON {
        Some(self.peek_location())
      } else {
        None
      }
      let param_annot = self.parse_opt_annot()
      @syntax.DiscardPositional(ty=param_annot, loc~, colon_loc~)
    }
    // `label~ : Type` 
    // or `label~ : Type = expr` (same as `label? : Type = expr`, deprecated syntax)
    POST_LABEL(label_name) => {
      let param_binder : @syntax.Binder = {
        name: label_name,
        loc: Location::trim_last_char(self.peek_location()),
      }
      self.skip() // skip POST_LABEL
      let colon_loc = if self.peek_token() is COLON {
        Some(self.peek_location())
      } else {
        None
      }
      let param_annot = self.parse_opt_annot()
      match self.peek_token() {
        EQUAL if self.mode is Normal => {
          let equal_loc = self.peek_location()
          self.skip()
          let default = self.parse_expr()
          Optional(
            default~,
            binder=param_binder,
            ty=param_annot,
            question_loc=None,
            colon_loc~,
            equal_loc=Some(equal_loc),
          )
        }
        _ => Labelled(binder=param_binder, ty=param_annot, colon_loc~)
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
      let colon_loc = if self.peek_token() is COLON {
        Some(self.peek_location())
      } else {
        None
      }
      let param_annot = self.parse_opt_annot()
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
            question_loc=Some(question_loc),
            colon_loc~,
            equal_loc=Some(equal_loc),
          )
        }
        _ =>
          QuestionOptional(
            binder=param_binder,
            ty=param_annot,
            question_loc=Some(question_loc),
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
      let colon_loc = if self.peek_token() is COLON {
        Some(self.peek_location())
      } else {
        None
      }
      let param_annot = self.parse_opt_annot()
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
          question_loc=None,
          colon_loc~,
          equal_loc=Some(eq_loc),
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
fn State::parse_parameters_info(
  self : Self,
) -> (@syntax.Parameters, Location, Location, @list.List[Location], Location?) {
  let lparen_loc = self.peek_location()
  let (params, comma_locs, trailing_comma_loc) = self.surround_series_info(
    left=TK_LPAREN,
    right=TK_RPAREN,
    delim=TK_COMMA,
    State::parse_parameter,
  )
  let (_, rparen_start, rparen_end) = self.previous_triple()
  (
    params |> @list.from_array,
    lparen_loc,
    Location::{ start: rparen_start, end: rparen_end },
    @list.from_array(comma_locs),
    trailing_comma_loc,
  )
}

///|
fn State::parse_apply_attr_info(
  self : Self,
) -> (@syntax.ApplyAttr, Location?, Location?) {
  if self.mode is Panic(..) {
    return (NoAttr, None, None)
  }
  match self.peek_token() {
    EXCLAMATION => {
      let loc = self.peek_location()
      self.skip()
      (NoAttr, Some(loc), None)
    }
    QUESTION => {
      let loc = self.peek_location()
      self.skip()
      (NoAttr, None, Some(loc))
    }
    _ => (NoAttr, None, None)
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
      Constant(c=Bool(true), loc~, uplus_loc=None, uminus_loc=None)
    }
    FALSE => {
      let loc = self.peek_location()
      self.expect_token(TK_FALSE)
      Constant(c=Bool(false), loc~, uplus_loc=None, uminus_loc=None)
    }
    BYTE(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_BYTE)
      Constant(c=Byte(x), loc~, uplus_loc=None, uminus_loc=None)
    }
    CHAR(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_CHAR)
      Constant(c=Char(x), loc~, uplus_loc=None, uminus_loc=None)
    }
    INT(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_INT)
      Constant(c=@syntax.make_int(x), loc~, uplus_loc=None, uminus_loc=None)
    }
    FLOAT(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_FLOAT)
      Constant(c=@syntax.make_float(x), loc~, uplus_loc=None, uminus_loc=None)
    }
    DOUBLE(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_DOUBLE)
      Constant(c=@syntax.make_double(x), loc~, uplus_loc=None, uminus_loc=None)
    }
    STRING(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_STRING)
      Constant(c=String(x), loc~, uplus_loc=None, uminus_loc=None)
    }
    REGEX_LITERAL(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_REGEX_LITERAL)
      Constant(c=Regex(x), loc~)
    }
    BYTES(x) => {
      let loc = self.peek_location()
      self.expect_token(TK_BYTES)
      Constant(c=Bytes(x), loc~, uplus_loc=None, uminus_loc=None)
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
          self.skip()
          self.parse_constr_expr(spos~, extra_info=Package(pkg))
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
      let has_error = self.parse_optional_bang()
      let func = self.parse_func(has_error~, is_async~, fn_loc=Some(fn_loc))
      let loc = self.loc_start_with(spos)
      Function(func~, loc~)
    }
    FN => {
      let spos = self.peek_spos()
      let fn_loc = self.peek_location()
      self.expect_token(TK_FN)
      let has_error = self.parse_optional_bang()
      let func = self.parse_func(has_error~, is_async=None, fn_loc=Some(fn_loc))
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
      let (elems, comma_locs, trailing_comma_loc) = self.surround_series_info(
        left=TK_LBRACKET,
        right=TK_RBRACKET,
        delim=TK_COMMA,
        fn(state) -> @syntax.SpreadableElem {
          match state.peek_token() {
            DOTDOT => {
              to_spread = true
              let spread_spos = state.peek_spos()
              state.expect_token(TK_DOTDOT)
              let expr = state.parse_expr()
              let loc = state.loc_start_with(spread_spos)
              Spread(expr~, loc~)
            }
            _ => Regular(state.parse_expr())
          }
        },
      )
      let (_, rbracket_start, rbracket_end) = self.previous_triple()
      let rbracket_loc : Location = { start: rbracket_start, end: rbracket_end }
      let elems = @list.from_array(elems)
      let comma_locs = @list.from_array(comma_locs)
      let loc = self.loc_start_with(spos)
      if to_spread {
        ArraySpread(
          elems~,
          loc~,
          lbracket_loc=Some(lbracket_loc),
          rbracket_loc=Some(rbracket_loc),
          comma_locs~,
          trailing_comma_loc~,
        )
      } else {
        let exprs = elems.map(elem => {
          match elem {
            Regular(e) => e
            Spread(_) => panic()
          }
        })
        Array(
          exprs~,
          loc~,
          lbracket_loc=Some(lbracket_loc),
          rbracket_loc=Some(rbracket_loc),
          comma_locs~,
          trailing_comma_loc~,
        )
      }
    }
    LBRACE => {
      // to parse a block expr or a record expr ?
      // things got a little bit tricky here ...
      let spos = self.peek_spos()
      match self.peek_token(nth=1) {
        DOTDOT => self.parse_record_expr(type_name=None)
        LIDENT(_) =>
          match self.peek_token(nth=2) {
            // In cases `{ x: ... }` `{ x, }` `{ x; }` `{ x }` the result is a record expression.
            SEMI(_) if self.peek_token(nth=3) is RBRACE =>
              self.parse_semi_trailing_record()
            COLON | COMMA | RBRACE => self.parse_record_expr(type_name=None)
            _ => {
              let lbrace_loc = self.peek_location()
              let expr = self.parse_block_expr()
              let loc = self.loc_start_with(spos)
              Group(
                expr~,
                group=Brace,
                loc~,
                lparen_loc=None,
                rparen_loc=None,
                lbrace_loc=Some(lbrace_loc),
                rbrace_loc=Some(loc.last_char()),
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
          let lbrace_loc = self.peek_location()
          let expr = self.parse_block_expr()
          let loc = self.loc_start_with(spos)
          Group(
            expr~,
            group=Brace,
            loc~,
            lparen_loc=None,
            rparen_loc=None,
            lbrace_loc=Some(lbrace_loc),
            rbrace_loc=Some(loc.last_char()),
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
      Method(
        type_name~,
        method_name~,
        loc~,
        coloncolon_loc=Some(coloncolon_loc),
      )
    }
    UIDENT(_) => self.parse_constr_expr(spos~, extra_info=TypeName(type_name))
    PACKAGE_NAME(pkg) if self.peek_token(nth=1) is DOT_UIDENT(_) => {
      self.skip()
      self.parse_constr_expr(
        spos~,
        extra_info=TypeNameWithConstrPackage(type_name~, pkg~),
      )
    }
    LBRACE => self.parse_record_expr(type_name=Some(type_name))
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
          extra_info: TypeName(type_name),
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
  self.expect_token(TK_LBRACKET)
  let for_spos = self.peek_spos()
  self.expect_token(TK_FOR)
  let kind : @syntax.ListComprehensionKind = if self.next_is_foreach_comprehension() {
    let binders = self.sepby1(delim=TK_COMMA, State::parse_foreach_binder)
      |> @list.from_array
    self.expect_token(TK_IN)
    let expr = self.parse_infix_expr(parsed=None)
    let (init, continue_block) = if self.peek_token() is SEMI(_) {
      self.skip()
      let (init, _) = self.series_with_follow(
        delim=TK_COMMA,
        follow_set=[TK_SEMI, TK_IF, TK_FAT_ARROW],
        State::parse_loop_binder_with_infix_value,
      )
      let init = @list.from_array(init)
      let continue_block = if self.peek_token() is SEMI(_) {
        self.skip()
        let (continue_block, _) = self.series_with_follow(
          delim=TK_COMMA,
          follow_set=[TK_IF, TK_FAT_ARROW],
          State::parse_loop_binder_with_infix_value,
        )
        @list.from_array(continue_block)
      } else {
        @list.empty()
      }
      (init, continue_block)
    } else {
      (@list.empty(), @list.empty())
    }
    Foreach(binders~, expr~, init~, continue_block~)
  } else {
    let (binders, _) = self.series_with_follow(
      delim=TK_COMMA,
      follow_set=[TK_SEMI, TK_IF, TK_FAT_ARROW],
      State::parse_loop_binder_with_infix_value,
    )
    let binders = @list.from_array(binders)
    let (condition, continue_block) = if self.peek_token() is SEMI(_) {
      self.skip()
      let condition = match self.peek_token() {
        SEMI(_) | IF | FAT_ARROW => None
        _ => Some(self.parse_infix_expr(parsed=None))
      }
      let continue_block = if self.peek_token() is SEMI(_) {
        self.skip()
        let (continue_block, _) = self.series_with_follow(
          delim=TK_COMMA,
          follow_set=[TK_IF, TK_FAT_ARROW],
          State::parse_loop_binder_with_infix_value,
        )
        @list.from_array(continue_block)
      } else {
        @list.empty()
      }
      (condition, continue_block)
    } else {
      (None, @list.empty())
    }
    let for_loc = self.loc_start_with(for_spos)
    For(binders~, condition~, continue_block~, for_loc~)
  }
  let guard_ = if self.peek_token() is IF {
    self.skip()
    Some(self.parse_infix_expr(parsed=None))
  } else {
    None
  }
  self.expect_token(TK_FAT_ARROW)
  let body = self.parse_expr()
  self.expect_token(TK_RBRACKET)
  let loc = self.loc_start_with(spos)
  ListComprehension(kind~, guard_~, body~, loc~)
}

///|
fn State::parse_field_def(self : Self) -> @syntax.FieldDef {
  if self.mode is Panic(loc~, ..) {
    return {
      label: { name: "?", loc },
      expr: Hole(loc~, kind=Synthesized),
      is_pun: false,
      loc,
      colon_loc: None,
    }
  }
  let spos = self.peek_spos()
  let label = self.parse_label(context="struct field")
  let (colon_loc, expr_opt) = match self.peek_token() {
    EQUAL => {
      // Error production for `field = expr` in struct expression
      let loc = self.peek_location()
      self.report_error({
        loc,
        msg: "Invalid equal sign in struct expression, use `:` instead",
      })
      self.skip() // skip =
      (None, Some(self.parse_expr()))
    }
    COLON => {
      let colon_loc = self.peek_location()
      self.skip() // skip :
      (Some(colon_loc), Some(self.parse_expr()))
    }
    _ => (None, None)
  }
  let loc = self.loc_start_with(spos)
  match expr_opt {
    Some(expr) => { label, expr, is_pun: false, loc, colon_loc }
    None => {
      let expr = @syntax.label_to_expr(loc=label.loc, label)
      { label, expr, is_pun: true, loc, colon_loc: None }
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
  self.expect_token(TK_SEMI)
  let rbrace_loc = self.peek_location()
  self.expect_token(TK_RBRACE)
  let loc = self.loc_start_with(spos)
  Record(
    type_name=None,
    fields=@list.from_array([field]),
    trailing=Semi,
    loc~,
    lbrace_loc=Some(lbrace_loc),
    rbrace_loc=Some(rbrace_loc),
    comma_locs=@list.empty(),
    trailing_comma_loc=None,
  )
}

///|
fn State::parse_record_expr(
  self : Self,
  type_name~ : @syntax.TypeName?,
) -> @syntax.Expr {
  if self.mode is Panic(loc~, ..) {
    return Hole(loc~, kind=Synthesized)
  }
  let spos = self.peek_spos()
  let lbrace_loc = self.peek_location()
  self.expect_token(TK_LBRACE)
  let (spread_record, fields, comma_locs, trailing_comma_loc, dotdot_loc) = if self.peek_token()
    is DOTDOT {
    let dotdot_loc = self.peek_location()
    self.skip()
    let spread_record = self.parse_expr()
    let (fields, comma_locs, trailing_comma_loc) = if self.peek_token() is COMMA {
      let first_comma_loc = self.peek_location()
      self.expect_token(TK_COMMA)
      let (fields, comma_locs, trailing_comma_loc) = self.series_with_follow_info(
        delim=TK_COMMA,
        follow_set=[TK_RBRACE],
        State::parse_field_def,
      )
      (
        @list.from_array(fields),
        @list.from_array(comma_locs).add(first_comma_loc),
        trailing_comma_loc,
      )
    } else {
      (@list.new(), @list.empty(), None)
    }
    (
      Some(spread_record),
      fields,
      comma_locs,
      trailing_comma_loc,
      Some(dotdot_loc),
    )
  } else {
    let (fields, comma_locs, trailing_comma_loc) = self.series_with_follow_info(
      delim=TK_COMMA,
      follow_set=[TK_RBRACE],
      State::parse_field_def,
    )
    (
      None,
      @list.from_array(fields),
      @list.from_array(comma_locs),
      trailing_comma_loc,
      None,
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
        loc~,
        lbrace_loc=Some(lbrace_loc),
        rbrace_loc=Some(rbrace_loc),
        comma_locs~,
        trailing_comma_loc~,
        dotdot_loc~,
      )
    None => {
      let trailing : @syntax.TrailingMark = if trailing_comma_loc is Some(_) {
        Comma
      } else {
        None
      }
      Record(
        type_name~,
        fields~,
        trailing~,
        loc~,
        lbrace_loc=Some(lbrace_loc),
        rbrace_loc=Some(rbrace_loc),
        comma_locs~,
        trailing_comma_loc~,
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
      Some({ key, expr, key_loc, loc, colon_loc: Some(colon_loc) })
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
  let (elems, comma_locs, trailing_comma_loc) = self.surround_series_info(
    left=TK_LBRACE,
    right=TK_RBRACE,
    delim=TK_COMMA,
    parse_elem,
  )
  let (_, rbrace_start, rbrace_end) = self.previous_triple()
  let rbrace_loc : Location = { start: rbrace_start, end: rbrace_end }
  let elems = elems.filter_map(elem => elem) // TODO: add catMaybes to core?
  let comma_locs = @list.from_array(comma_locs)
  let loc = self.loc_start_with(spos)
  Map(
    elems=@list.from_array(elems),
    loc~,
    lbrace_loc=Some(lbrace_loc),
    rbrace_loc=Some(rbrace_loc),
    comma_locs~,
    trailing_comma_loc~,
  )
}

///|
fn State::parse_opt_annot(self : Self) -> @syntax.Type? {
  if self.mode is Panic(..) {
    return None
  }
  if self.peek_token() is COLON {
    self.skip()
    Some(self.parse_type())
  } else {
    None
  }
}

///|
fn State::parse_block_expr_with_local_types(
  self : Self,
) -> BlockExprWithLocalTypesInfo {
  if self.mode is Panic(loc~, ..) {
    return {
      local_types: @list.new(),
      expr: Hole(loc~, kind=Synthesized),
      lbrace_loc: None,
      rbrace_loc: None,
      semi_locs: @list.empty(),
      trailing_semi_loc: None,
    }
  }
  let spos = self.peek_spos()
  let lbrace_loc = self.peek_location()
  let (local_types, stmts, semi_locs, trailing_semi_loc) = self.surround(
    left=TK_LBRACE,
    right=TK_RBRACE,
    fn(self) {
      let local_types = []
      // TODO: maybe use delimited_with_follow instead
      let mut allow_following_stmt = true
      while self.mode is Normal && self.peek_token() is (STRUCT | ENUM) {
        self.push_sync(TK_SEMI)
        local_types.push(self.parse_local_type_decl())
        self.pop_sync(TK_SEMI)
        allow_following_stmt = if self.peek_token() is SEMI(_) {
          self.skip()
          true
        } else {
          false
        }
      }
      let (stmts, semi_locs, trailing_semi_loc) = if allow_following_stmt {
        let (stmts, semi_locs, trailing_semi_loc) = self.series_with_follow_info(
          delim=TK_SEMI,
          follow_set=[TK_RBRACE],
          State::parse_stmt,
        )
        (
          attach_stmt_semi_locs(stmts, semi_locs, trailing_semi_loc),
          @list.from_array(semi_locs),
          trailing_semi_loc,
        )
      } else {
        (@list.empty(), @list.empty(), None)
      }
      (@list.from_array(local_types), stmts, semi_locs, trailing_semi_loc)
    },
  )
  let (_, rbrace_start, rbrace_end) = self.previous_triple()
  let loc = self.loc_start_with(spos)
  {
    local_types,
    expr: Statement::compact_rev(stmts.rev(), loc~), // FIXME: remove rev
    lbrace_loc: Some(lbrace_loc),
    rbrace_loc: Some(Location::{ start: rbrace_start, end: rbrace_end }),
    semi_locs,
    trailing_semi_loc,
  }
}

///|
fn State::parse_block_expr(self : Self) -> @syntax.Expr {
  self.parse_block_expr_info().expr
}

///|
fn State::parse_block_expr_info(self : Self) -> BlockExprInfo {
  if self.mode is Panic(loc~, ..) {
    return {
      expr: @syntax.Hole(loc~, kind=Synthesized),
      lbrace_loc: None,
      rbrace_loc: None,
      semi_locs: @list.empty(),
      trailing_semi_loc: None,
    }
  }
  let loc_start = self.peek_spos()
  let lbrace_loc = self.peek_location()
  let (stmts, semi_locs, trailing_semi_loc) = self.surround_series_info(
    left=TK_LBRACE,
    right=TK_RBRACE,
    delim=TK_SEMI,
    fn(s) -> Statement { s.parse_stmt() },
  )
  let stmts = attach_stmt_semi_locs(stmts, semi_locs, trailing_semi_loc)
  let (_, rbrace_start, rbrace_end) = self.previous_triple()
  let rbrace_loc : Location = { start: rbrace_start, end: rbrace_end }
  let loc = self.loc_start_with(loc_start)
  {
    expr: Statement::compact_rev(stmts.rev(), loc~),
    lbrace_loc: Some(lbrace_loc),
    rbrace_loc: Some(rbrace_loc),
    semi_locs: @list.from_array(semi_locs),
    trailing_semi_loc,
  }
}

///|
fn attach_stmt_semi_locs(
  stmts : Array[Statement],
  semi_locs : Array[Location],
  trailing_semi_loc : Location?,
) -> @list.List[Statement] {
  if stmts.length() == 0 {
    return @list.empty()
  }
  let result = []
  let last_index = stmts.length() - 1
  for i in 0..<stmts.length() {
    let semi_loc = if i < semi_locs.length() {
      Some(semi_locs[i])
    } else if i == last_index {
      trailing_semi_loc
    } else {
      None
    }
    result.push(stmts[i].with_semi_loc(semi_loc))
  }
  @list.from_array(result)
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
      has_error: None,
      is_async: None,
      loc,
      fn_loc: None,
      body_lbrace_loc: None,
      body_rbrace_loc: None,
      body_semi_locs: @list.empty(),
      body_trailing_semi_loc: None,
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
      let parameters = self.surround_series(
        left=TK_LPAREN,
        right=TK_RPAREN,
        delim=TK_COMMA,
        fn(self) -> (@syntax.ArrowFnParam, @syntax.Type?) {
          let param = parse_arrow_fn_param()
          let ty_opt = self.parse_opt_annot()
          (param, ty_opt)
        },
      )
      let params_loc = self.loc_start_with(spos)
      self.expect_token(TK_FAT_ARROW)
      let body = self.parse_expr_stmt_no_break_continue_return()
      let loc = self.loc_start_with(spos)
      @syntax.make_arrow_fn(
        @list.from_array(parameters),
        body,
        params_loc~,
        loc~,
      )
    }
    LIDENT(_) | UNDERSCORE => {
      let param = parse_arrow_fn_param()
      let params_loc = self.loc_start_with(spos)
      self.expect_token(TK_FAT_ARROW)
      let body = self.parse_expr_stmt_no_break_continue_return()
      let loc = self.loc_start_with(spos)
      @syntax.make_arrow_fn(
        @list.from_array([(param, None)]),
        body,
        params_loc~,
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
      let has_error = self.parse_optional_bang()
      self.parse_func(has_error~, is_async~, fn_loc=Some(fn_loc))
    }
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
        let ty_opt = self.parse_opt_annot()
        let equal_loc = self.peek_location()
        self.expect_token(TK_EQUAL)
        let expr = self.parse_expr()
        let loc = self.loc_start_with(spos)
        StmtLetmut(
          binder~,
          ty_opt~,
          expr~,
          loc~,
          let_loc=Some(let_loc),
          mut_loc=Some(mut_loc),
          equal_loc=Some(equal_loc),
          semi_loc=None,
        )
      } else {
        let pat = self.parse_pattern()
        let ty_opt = self.parse_opt_annot()
        let pat : @syntax.Pattern = match ty_opt {
          Some(ty) => Constraint(pat~, ty~, loc=pat.loc().merge(ty.loc()))
          None => pat
        }
        let equal_loc = self.peek_location()
        self.expect_token(TK_EQUAL)
        let expr = self.parse_expr()
        let loc = self.loc_start_with(spos)
        StmtLet(
          pat~,
          expr~,
          loc~,
          let_loc=Some(let_loc),
          equal_loc=Some(equal_loc),
          semi_loc=None,
        )
      }
    }
    LETREC => {
      let spos = self.peek_spos()
      let letrec_loc = self.peek_location()
      self.expect_token(TK_LETREC)
      let bindings = self.sepby1(delim=TK_AND, fn(self) {
        let binder = self.parse_binder(context="variable name")
        let ty_opt = self.parse_opt_annot()
        self.expect_token(TK_EQUAL)
        let func = self.parse_letand_func()
        (binder, ty_opt, func)
      })
      let loc = self.loc_start_with(spos)
      StmtLetand(
        bindings=@list.from_array(bindings),
        loc~,
        letrec_loc=Some(letrec_loc),
        semi_loc=None,
      )
    }
    ASYNC if self.peek_token(nth=1) is FN && self.peek_token(nth=2) is LIDENT(_) => {
      let is_async = Some(self.peek_location())
      let spos = self.peek_spos()
      self.skip() // async
      let fn_loc = self.peek_location()
      self.expect_token(TK_FN)
      let binder = self.parse_binder(context="")
      let has_error = self.parse_optional_bang()
      let func = self.parse_func(has_error~, is_async~, fn_loc=Some(fn_loc))
      let loc = self.loc_start_with(spos)
      StmtFunc(binder~, func~, loc~, fn_loc=Some(fn_loc), semi_loc=None)
    }
    FN if self.peek_token(nth=1) is LIDENT(_) => {
      let spos = self.peek_spos()
      let fn_loc = self.peek_location()
      self.expect_token(TK_FN)
      let binder = self.parse_binder(context="")
      let has_error = self.parse_optional_bang()
      let func = self.parse_func(has_error~, is_async=None, fn_loc=Some(fn_loc))
      let loc = self.loc_start_with(spos)
      StmtFunc(binder~, func~, loc~, fn_loc=Some(fn_loc), semi_loc=None)
    }
    GUARD => {
      let spos = self.peek_spos()
      let guard_loc = self.peek_location()
      self.skip()
      let cond = self.with_syncs([TK_ELSE], State::try_parse_condition_expr)
      let (
        otherwise,
        else_loc,
        else_lbrace_loc,
        else_rbrace_loc,
        else_semi_locs,
        else_trailing_semi_loc,
      ) = if self.peek_token() is ELSE {
        let else_loc = self.peek_location()
        self.skip()
        let otherwise = self.parse_block_expr_info()
        (
          Some(otherwise.expr),
          Some(else_loc),
          otherwise.lbrace_loc,
          otherwise.rbrace_loc,
          otherwise.semi_locs,
          otherwise.trailing_semi_loc,
        )
      } else {
        (None, None, None, None, @list.empty(), None)
      }
      let loc = self.loc_start_with(spos)
      StmtGuard(
        cond~,
        otherwise~,
        loc~,
        guard_loc=Some(guard_loc),
        else_loc~,
        else_lbrace_loc~,
        else_rbrace_loc~,
        else_semi_locs~,
        else_trailing_semi_loc~,
        semi_loc=None,
      )
    }
    DEFER => {
      let spos = self.peek_spos()
      let defer_loc = self.peek_location()
      self.skip()
      let expr = self.try_parse_pipe_expr()
      let loc = self.loc_start_with(spos)
      StmtDefer(expr~, loc~, defer_loc=Some(defer_loc), semi_loc=None)
    }
    PROOF_ASSERT => {
      let spos = self.peek_spos()
      self.skip()
      let expr = self.parse_expr()
      let loc = self.loc_start_with(spos)
      StmtExpr(expr=ProofAssert(expr~, loc~), semi_loc=None)
    }
    PROOF_LET => {
      let spos = self.peek_spos()
      self.skip()
      let binder = self.parse_binder(context="proof binding")
      self.expect_token(TK_EQUAL)
      let expr = self.parse_expr()
      let loc = self.loc_start_with(spos)
      StmtExpr(expr=ProofLet(binder~, expr~, loc~), semi_loc=None)
    }
    _ => StmtExpr(expr=self.parse_expr_stmt(), semi_loc=None)
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
    let loc = stmt.loc()
    Statement::compact_rev(@list.singleton(stmt), loc~)
  }

  let (actual, expr) = match token {
    IF => ("if expression", self.parse_if_expr())
    MATCH => ("match expression", self.parse_match_expr())
    LOOP => ("loop expression", self.parse_loop_expr(label=None))
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
    | LOOP
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
      self.expect_token(TK_EQUAL)
      let rhs = self.parse_expr()
      let loc = self.loc_start_with(spos)
      match @syntax.make_assign_opt(loc~, lhs, rhs) {
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
      match @syntax.make_augmented_assign_opt(loc~, op, lhs, rhs) {
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
      match @syntax.make_augmented_assign_opt(loc~, op, lhs, rhs) {
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
    LT_PLUS => {
      self.expect_token(TK_LT_PLUS)
      let template = self.parse_template_literal_expr()
      let loc = self.loc_start_with(spos)
      TemplateWriting(expr=lhs, template~, loc~)
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
      @syntax.Raise(err_value=expr, loc=loc_, raise_loc=Some(raise_loc))
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
      let label = if self.peek_token() is POST_LABEL(label_name) {
        let label_loc = self.peek_location()
        self.skip()
        Some(@syntax.Label::{ name: label_name, loc: label_loc })
      } else {
        None
      }
      let arg = self.optional(first_expr, State::parse_expr)
      Break(
        arg~,
        loc=self.loc_start_with(spos),
        label~,
        break_loc=Some(break_loc),
      )
    }
    CONTINUE => {
      let spos = self.peek_spos()
      let continue_loc = self.peek_location()
      self.expect_token(TK_CONTINUE)
      let label : @syntax.Label? = match self.peek_token() {
        //  Consider the case:
        //  ```
        //  continue l~: loop 0 { ... } 
        // ```
        //  Here the label l~ is for the loop instead of the continue statement.
        POST_LABEL(label_name) if !(self.peek_token(nth=1) is COLON) => {
          let label_loc = self.peek_location()
          self.skip()
          Some({ name: label_name, loc: label_loc })
        }
        _ => None
      }
      let args = self.sepby(
          delim=TK_COMMA,
          first_set=first_expr,
          State::parse_expr,
        )
        |> @list.from_array
      Continue(
        args~,
        loc=self.loc_start_with(spos),
        label~,
        continue_loc=Some(continue_loc),
      )
    }
    RETURN => {
      let spos = self.peek_spos()
      let return_loc = self.peek_location()
      self.expect_token(TK_RETURN)
      let expr = self.optional(first_expr, State::parse_expr)
      let loc_ = self.loc_start_with(spos)
      Return(return_value=expr, loc=loc_, return_loc=Some(return_loc))
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
        loc~,
        lbracket_loc=None,
        rbracket_loc=None,
        comma_locs=@list.empty(),
        trailing_comma_loc=None,
      )
    }
    _ => {
      let name = self.parse_qual_luident()
      let loc = self.loc_start_with(spos)
      let constr_id : @syntax.ConstrId = { id: name, loc }
      Name(
        constr_id~,
        tys=@list.new(),
        loc~,
        lbracket_loc=None,
        rbracket_loc=None,
        comma_locs=@list.empty(),
        trailing_comma_loc=None,
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
            loc~,
            lbracket_loc=None,
            rbracket_loc=None,
            comma_locs=@list.empty(),
            trailing_comma_loc=None,
          )
          MaybeError(ty=fake_error, question_loc=Some(question_loc))
        }
        PACKAGE_NAME(_) | UIDENT(_) | LIDENT(_) | UNDERSCORE => {
          let ty = self.parse_error_type()
          ErrorType(ty~)
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
fn State::parse_func_return_type(
  self : Self,
) -> (@syntax.Type?, @syntax.ErrorType) {
  let (return_type, error_type, _) = self.parse_func_return_type_info()
  (return_type, error_type)
}

///|
fn State::parse_func_return_type_info(
  self : Self,
) -> (@syntax.Type?, @syntax.ErrorType, Location?) {
  if self.mode is Panic(..) {
    return (None, NoErrorType, None)
  }
  if self.peek_token() is THIN_ARROW {
    let thin_arrow_loc = self.peek_location()
    self.skip()
    let (ret_ty, err_ty) = self.parse_return_type()
    (Some(ret_ty), err_ty, Some(thin_arrow_loc))
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

  let spos = self.peek_spos()
  match self.peek_token() {
    PACKAGE_NAME(_) | LIDENT(_) | UIDENT(_) => {
      let name_loc = self.peek_location()
      let name = self.parse_qual_luident()
      let arg_info = self.optional([TK_LBRACKET], fn(self) {
        let lbracket_loc = self.peek_location()
        let (args, comma_locs, trailing_comma_loc) = self.surround_series_info(
          left=TK_LBRACKET,
          right=TK_RBRACKET,
          delim=TK_COMMA,
          State::parse_type,
        )
        let (_, rbracket_start, rbracket_end) = self.previous_triple()
        let rbracket_loc : Location = {
          start: rbracket_start,
          end: rbracket_end,
        }
        (
          @list.from_array(args),
          Some(lbracket_loc),
          Some(rbracket_loc),
          @list.from_array(comma_locs),
          trailing_comma_loc,
        )
      })
      let loc = self.loc_start_with(spos)
      let (args, lbracket_loc, rbracket_loc, comma_locs, trailing_comma_loc) = match
        arg_info {
        Some(info) => info
        None => (@list.new(), None, None, @list.empty(), None)
      }
      let ty = @syntax.Type::Name(
        constr_id={ id: name, loc: name_loc },
        tys=args,
        loc~,
        lbracket_loc~,
        rbracket_loc~,
        comma_locs~,
        trailing_comma_loc~,
      )
      parse_question_mark(spos, ty)
    }
    ASYNC => {
      let is_async = Some(self.peek_location())
      self.skip()
      let lparen_loc = self.peek_location()
      let (tys, comma_locs, trailing_comma_loc) = self.surround_series_info(
        left=TK_LPAREN,
        right=TK_RPAREN,
        delim=TK_COMMA,
        State::parse_type,
      )
      let (_, rparen_start, rparen_end) = self.previous_triple()
      let rparen_loc : Location = { start: rparen_start, end: rparen_end }
      let tys = @list.from_array(tys)
      let comma_locs = @list.from_array(comma_locs)
      let thin_arrow_loc = self.peek_location()
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
          loc~,
          lparen_loc=Some(lparen_loc),
          rparen_loc=Some(rparen_loc),
          comma_locs~,
          trailing_comma_loc~,
          thin_arrow_loc=Some(thin_arrow_loc),
        ),
      )
    }
    LPAREN => {
      let lparen_loc = self.peek_location()
      let (tys, comma_locs, trailing_comma_loc) = self.surround_series_info(
        left=TK_LPAREN,
        right=TK_RPAREN,
        delim=TK_COMMA,
        State::parse_type,
      )
      let (_, rparen_start, rparen_end) = self.previous_triple()
      let rparen_loc : Location = { start: rparen_start, end: rparen_end }
      let tys = @list.from_array(tys)
      let comma_locs = @list.from_array(comma_locs)
      if tys is Empty {
        let thin_arrow_loc = self.peek_location()
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
            loc~,
            lparen_loc=Some(lparen_loc),
            rparen_loc=Some(rparen_loc),
            comma_locs~,
            trailing_comma_loc~,
            thin_arrow_loc=Some(thin_arrow_loc),
          ),
        )
      } else {
        match self.peek_token() {
          THIN_ARROW => {
            let thin_arrow_loc = self.peek_location()
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
                loc~,
                lparen_loc=Some(lparen_loc),
                rparen_loc=Some(rparen_loc),
                comma_locs~,
                trailing_comma_loc~,
                thin_arrow_loc=Some(thin_arrow_loc),
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
                  loc~,
                  lparen_loc=Some(lparen_loc),
                  rparen_loc=Some(rparen_loc),
                  comma_locs~,
                  trailing_comma_loc~,
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
      let spos = self.peek_spos()
      let lid = self.parse_qual_luident()
      let loc = self.loc_start_with(spos)
      let ty = @syntax.Type::Object(
        constr_id={ id: lid, loc },
        amp_loc=Some(amp_loc),
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
fn State::parse_opt_typ_params_info(
  self : Self,
) -> (
  @list.List[@syntax.TypeVarBinder],
  Location?,
  Location?,
  @list.List[Location],
  Location?,
) {
  if self.mode is Panic(..) {
    return (@list.new(), None, None, @list.empty(), None)
  }
  self
  .optional([TK_LBRACKET], fn(state) {
    let lbracket_loc = state.peek_location()
    let (params, comma_locs, trailing_comma_loc) = state.surround_series_info(
      left=TK_LBRACKET,
      right=TK_RBRACKET,
      delim=TK_COMMA,
      State::parse_tvar_binder,
    )
    let (_, rbracket_start, rbracket_end) = state.previous_triple()
    (
      params |> @list.from_array,
      Some(lbracket_loc),
      Some(Location::{ start: rbracket_start, end: rbracket_end }),
      comma_locs |> @list.from_array,
      trailing_comma_loc,
    )
  })
  .unwrap_or((@list.new(), None, None, @list.empty(), None))
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
