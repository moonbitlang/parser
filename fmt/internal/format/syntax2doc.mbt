// PLEASE READ BEFORE EDITING:
// 
// 1. The syntax2doc is implemented in a divide-and-conquer style
//
// 2. Do not emit trailing space or newline
//   Due to the behavior of the `nest` primitive, **`Hardline` represents the 
//   start of a line**, not its end. Therefore, each piece of code is responsible 
//   for handling its own leading newline or space, and should NOT emit trailing 
//   newlines and spaces (unless they are explicitly optional in the AST).
//
// 3. Consistent printing style is important for a formatter
//   In MoonBit, we do not strictly enforce the "keep your code DRY" principle.
//   Duplicating code is acceptable if abstraction becomes too complex.
//   However, in formatter, maintaining multiple copies makes it difficult to 
//   ensure consistent printing style. Before duplicating code, please consider 
//   reusing existing helper functions.
//

///|
priv struct Fmt {
  mapper : @comment.Mapper?
  block_line : Bool
}

///|
fn fmt_comment(comment : @comment.Comment) -> Doc {
  match comment.kind {
    Blank => @pp.mandatory_line
    Block(_) => panic()
    Line(l) => @pp.mandatory_space + text(l) + @pp.mandatory_line
  }
}

///|
fn fmt_comments(comments : Array[@comment.Comment]) -> Doc {
  comments.map(fmt_comment).fold(init=empty, Doc::add)
}

///|
fn Fmt::wrap_comment(fmt : Self, key : @comment.Key?, doc : Doc) -> Doc {
  let (a, b) = fmt.comments(key)
  a + doc + b
}

///|
// fn Fmt::has_comment(fmt : Self, key : @comment.Key?) -> Bool {
//   match (fmt.mapper, key) {
//     (Some(mapper), Some(k)) => mapper.has_comment(key)
//     _ => false
//   }
// }

///|
// fn Fmt::has_trailing_comment(fmt : Self, key : @comment.Key?) -> Bool {
//   match fmt.mapper {
//     None => false
//     Some(m) => m.has_leading_comment(key)
//   }
// }

///|
// fn Fmt::has_leading_comment(fmt : Self, key : @comment.Key?) -> Bool {
//   match fmt.mapper {
//     None => false
//     Some(m) => m.has_trailing_comment(key)
//   }
// }

///|
/// Gets the formatted comments after to the given node's key.
fn Fmt::before_comment(fmt : Self, key : @comment.Key?) -> Doc {
  match (fmt.mapper, key) {
    (Some(mapper), Some(k)) =>
      mapper.consume_comments(key, c => c.start < k.loc().start) |> fmt_comments
    _ => empty
  }
}

///|
/// Gets the formatted comments before to the given node's key.
fn Fmt::after_comment(fmt : Self, key : @comment.Key?) -> Doc {
  match (fmt.mapper, key) {
    (Some(mapper), Some(k)) =>
      mapper.consume_comments(key, c => c.start >= k.loc().start)
      |> fmt_comments
    _ => empty
  }
}

///|
/// Gets the formatted comments attached to the given node's key.
/// 
/// Returns a tuple of two `Doc` values representing the comments before and 
/// after the node.
fn Fmt::comments(fmt : Self, key : @comment.Key?) -> (Doc, Doc) {
  let doc1 = fmt.before_comment(key)
  let doc2 = fmt.after_comment(key)
  (doc1, doc2)
}

///|
/// Gets the token table for the node.
fn Fmt::get_table(fmt : Self, node : Location) -> @comment.NodeTable {
  match fmt.mapper {
    Some(tbl) => tbl.get_table(node)
    None => @comment.NodeTable::empty()
  }
}

///|
fn Fmt::new(mapper? : @comment.Mapper, block_line? : Bool = true) -> Self {
  { mapper, block_line }
}

///|
fn Fmt::impls(fmt : Self, impls : List[@syntax.Impl]) -> Doc {
  concat_map(impls, x => fmt.impl_(x) + hardline + hardline) //TODO: handle comments between impls
}

///|
fn Fmt::return_and_error_type_annotation(
  fmt : Self,
  return_ty : @syntax.Type?,
  err_ty : @syntax.ErrorType,
) -> Doc {
  let arrow = if return_ty is None && err_ty is NoErrorType {
    empty
  } else {
    space + text("->")
  }
  let return_type = match return_ty {
    None => empty
    Some(ty) => space + fmt.ty(ty)
  }
  let error_type = fmt.error_type(err_ty)
  arrow + return_type + error_type
}

///|
fn Fmt::impl_(fmt : Self, toplevel : @syntax.Impl) -> Doc {
  match toplevel {
    TopView(
      quantifiers~,
      source_ty~,
      view_type_name~,
      view_type_loc=_,
      view_constrs~,
      view_func_name~,
      parameters~,
      params_loc=_,
      body~,
      vis~,
      loc=_,
      attrs~,
      doc~
    ) => {
      let doc = fmt.docstring(doc, is_toplevel=true)
      let attrs = fmt.attributes(attrs)
      let vis = fmt.visibility(vis)
      let enumview_ = text("enumview")
      let quantifiers = fmt.type_var_binders(
        quantifiers,
        lbracket=None,
        commas=@comment.NodeArray::empty(),
        tvar_tables=@comment.NodeTableArray::empty(),
        rbracket=None,
      )
      let view_name = space + text(view_type_name)
      let semi = switch(char(';'), empty)
      let constrs = if view_constrs.is_empty() {
        space + text("{") + hardline + text("}")
      } else {
        space +
        char('{') +
        nest(
          hardline + series1(view_constrs.map(x => fmt.constr_decl(x)), semi),
        ) +
        hardline +
        char('}')
      }
      let for_ = space + text("for") + space + fmt.ty(source_ty)
      let with_ = space + text("with")
      let func_name = space + fmt.binder(view_func_name)
      let params = group(
        fmt.parameters(
          parameters,
          lparen=None,
          param_tables=@comment.NodeTableArray::empty(),
          commas=@comment.NodeArray::empty(),
          rparen=None,
        ),
      )
      let body = space + fmt.expr_block(body, force_newline=true)
      doc +
      attrs +
      vis +
      enumview_ +
      quantifiers +
      view_name +
      constrs +
      for_ +
      with_ +
      func_name +
      params +
      body
    }
    TopImpl(
      self_ty~,
      trait_~,
      method_name~,
      quantifiers~,
      method_quantifiers~,
      params~,
      ret_ty~,
      err_ty~,
      body~,
      vis~,
      attrs~,
      doc~,
      loc=_
    ) => {
      let doc = fmt.docstring(doc, is_toplevel=true)
      let attrs = fmt.attributes(attrs)
      let vis = fmt.visibility(vis)
      let impl_ = text("impl")
      let quantifiers = fmt.type_var_binders(
        quantifiers,
        lbracket=None,
        commas=@comment.NodeArray::empty(),
        tvar_tables=@comment.NodeTableArray::empty(),
        rbracket=None,
      )
      let trait_ = fmt.type_name(trait_)
      let for_part = match self_ty {
        None => empty
        Some(ty) => space + text("for") + space + fmt.ty(ty)
      }
      let with_ = space + text("with")
      let method_quantifiers = if method_quantifiers.is_empty() {
        empty
      } else {
        space +
        text("fn") +
        fmt.type_var_binders(
          method_quantifiers,
          lbracket=None,
          commas=@comment.NodeArray::empty(),
          tvar_tables=@comment.NodeTableArray::empty(),
          rbracket=None,
        )
      }
      let method_name = space + text(method_name.name)
      let params = group(
        fmt.parameters(
          params,
          lparen=None,
          param_tables=@comment.NodeTableArray::empty(),
          commas=@comment.NodeArray::empty(),
          rparen=None,
        ),
      )
      let result = fmt.return_and_error_type_annotation(ret_ty, err_ty)
      let body = match body {
        DeclNone => empty
        DeclBody(expr~) => space + fmt.expr_block(expr, force_newline=true)
        DeclStubs(stubs) => {
          let stubs = match stubs {
            Import(module_name~, func_name~, ..) => {
              let module_name = double_quoted(text(module_name))
              let func_name = double_quoted(text(func_name))
              space + module_name + space + func_name
            }
            Embedded(language~, code~) => {
              guard language is None else {
                abort("language must be None in impl stub.")
              }
              let code = fmt.embedded_code(code)
              space + code
            }
          }
          space + text("=") + stubs
        }
      }
      doc +
      attrs +
      vis +
      impl_ +
      quantifiers +
      space +
      trait_ +
      for_part +
      with_ +
      method_quantifiers +
      method_name +
      params +
      result +
      body
    }
    TopTrait({ name, supers, methods, vis, attrs, doc, loc: _, is_declare }) => {
      let doc = fmt.docstring(doc, is_toplevel=true)
      let attrs = fmt.attributes(attrs)
      let declare_ = if is_declare { text("declare") + space } else { empty }
      let vis = fmt.visibility(vis)
      let trait_ = text("trait")
      let name = space + fmt.binder(name)
      let supers = match supers {
        Empty => empty
        More(_) =>
          space +
          colon +
          group(
            nest(
              line +
              series1(
                supers.map(x => fmt.type_var_constraint(x)),
                space + char('+'),
                trailing=AlwaysHidden,
              ),
            ),
          )
      }
      let methods = space +
        braces(
          series1(methods.map(x => fmt.trait_method(x)), empty),
          force_newline=true,
        )
      doc + attrs + declare_ + vis + trait_ + name + supers + methods
    }
    TopLetDef(
      binder~,
      ty~,
      expr~,
      vis~,
      is_constant~,
      attrs~,
      doc~,
      is_declare~,
      ..
    ) => {
      let doc = fmt.docstring(doc, is_toplevel=true)
      let attrs = fmt.attributes(attrs)
      let declare_ = if is_declare { text("declare") + space } else { empty }
      let vis = fmt.visibility(vis)
      let let_ = text(if is_constant { "const" } else { "let" })
      let binder = space + fmt.binder(binder)
      let ty = match ty {
        None => empty
        Some(ty) => space + colon + fmt.ty(ty)
      }
      let rhs = space + char('=') + fmt.compose(expr, Expr) // TODO: compose_rhs
      doc + attrs + declare_ + vis + let_ + binder + ty + rhs
    }
    TopTypeDef(typedecl) => fmt.typedecl(typedecl)
    TopFuncDef(fun_decl~, decl_body~, where_clause~, loc~) => {
      let table = fmt.get_table(loc)
      let {
        type_name,
        name,
        is_async,
        decl_params,
        quantifiers,
        return_type,
        error_type,
        vis,
        attrs,
        doc,
        ..,
      } = fun_decl
      let doc = fmt.docstring(doc, is_toplevel=true)
      let attrs = fmt.attributes(attrs)
      let vis = fmt.visibility(vis)
      let fn_ = fmt.wrap_comment(table.get_token("fn"), text("fn"))
      let async_ = if is_async is Some(_) {
        fmt.wrap_comment(table.get_token("async"), text("async") + space)
      } else {
        empty
      }
      let quantifiers = fmt.type_var_binders(
        quantifiers,
        lbracket=table.get_token("lbracket"),
        commas=table.get_array("commas0"),
        tvar_tables=table.get_table_array("type_var_binders"),
        rbracket=table.get_token("rbracket"),
      )
      let type_name_and_binder = match type_name {
        None => space + fmt.binder(name)
        Some(n) => space + fmt.type_name(n) + text("::") + fmt.binder(name)
      }
      let params = match decl_params {
        None => empty
        Some(xs) =>
          group(
            fmt.parameters(
              xs,
              lparen=table.get_token("lparen"),
              param_tables=table.get_table_array("parameters"),
              commas=table.get_array("commas1"),
              rparen=table.get_token("rparen"),
            ),
          )
      }
      let result = fmt.return_and_error_type_annotation(return_type, error_type)
      let (extern_, body) = match decl_body {
        DeclNone => (empty, empty)
        DeclBody(expr~) => (empty, space + fmt.expr_block(expr))
        DeclStubs(Import(module_name~, func_name~, ..)) => {
          let equal = space + char('=')
          let module_name = space + double_quoted(text(module_name))
          let func_name = space + double_quoted(text(func_name))
          (empty, equal + module_name + func_name)
        }
        DeclStubs(Embedded(language~, code~)) => {
          let extern_ = match language {
            None => empty
            Some(s) => text("extern") + space + double_quoted(text(s)) + space
          }
          let equal = space + char('=')
          let code = group(nest(line + fmt.embedded_code(code)))
          (extern_, equal + code)
        }
      }
      let layout = doc +
        attrs +
        vis +
        extern_ +
        async_ +
        fn_ +
        quantifiers +
        type_name_and_binder +
        params +
        result +
        fmt.where_clause(where_clause) +
        body
      fmt.wrap_comment(Some(Node(loc)), layout)
    }
    TopTest(expr~, name~, params~, attrs~, doc~, is_async~, ..) => {
      let doc = fmt.docstring(doc, is_toplevel=true)
      let attrs = fmt.attributes(attrs)
      let test_ = if is_async is Some(_) {
        text("async test")
      } else {
        text("test")
      }
      let name = match name {
        None => empty
        Some((s, _)) => space + double_quoted(text(s))
      }
      let params = match params {
        None => empty
        Some(xs) =>
          space +
          group(
            fmt.parameters(
              xs,
              lparen=None,
              param_tables=@comment.NodeTableArray::empty(),
              commas=@comment.NodeArray::empty(),
              rparen=None,
            ),
          )
      }
      let body = space + fmt.expr_block(expr)
      doc + attrs + test_ + name + params + body
    }
    TopExpr(expr~, is_main~, is_async~, ..) => {
      let fn_ = if is_async is Some(_) { text("async fn") } else { text("fn") }
      let name = space + text(if is_main { "main" } else { "init" })
      let expr = fmt.expr_block(expr)
      fn_ + name + expr
    }
    TopImplRelation(
      self_ty~,
      trait_~,
      quantifiers~,
      vis~,
      attrs~,
      doc~,
      is_declare~,
      ..
    ) => {
      let doc = fmt.docstring(doc, is_toplevel=true)
      let attrs = fmt.attributes(attrs)
      let declare_ = if is_declare { text("declare") + space } else { empty }
      let vis = fmt.visibility(vis)
      let impl_ = text("impl")
      let quantifiers = fmt.type_var_binders(
        quantifiers,
        lbracket=None,
        commas=@comment.NodeArray::empty(),
        tvar_tables=@comment.NodeTableArray::empty(),
        rbracket=None,
      )
      let self = space + fmt.ty(self_ty)
      let for_ = space + text("for")
      let trait_ = space + fmt.type_name(trait_)
      doc + attrs + declare_ + vis + impl_ + quantifiers + trait_ + for_ + self
    }
    TopUsing(pkg~, names~, vis~, attrs~, doc~, loc=_) => {
      let doc = fmt.docstring(doc, is_toplevel=true)
      let attrs = fmt.attributes(attrs)
      let vis = fmt.visibility(vis)
      let using_ = text("using") + space
      let pkg = char('@') + fmt.label(pkg)
      fn using_item(item : (@syntax.AliasTarget, @syntax.UsingKind)) -> Doc {
        let (target, kind) = item
        let kind_prefix = match kind {
          Value => empty
          Type => text("type") + space
          Trait => text("trait") + space
        }
        kind_prefix + fmt.alias_target(target)
      }

      let targets = space + group(braces(series1(names.map(using_item), comma)))
      doc + attrs + vis + using_ + pkg + targets
    }
  }
}

///|
fn Fmt::embedded_code(fmt : Fmt, code : @syntax.EmbeddedCode) -> Doc {
  ignore(fmt) //TODO: use fmt to access comments
  match code {
    CodeString(s) => double_quoted(text(s))
    CodeMultilineString(xs) =>
      group(separate_map(xs, hardline, s => text("#|" + s)))
  }
}

///|
fn Fmt::alias_target(fmt : Fmt, target : @syntax.AliasTarget) -> Doc {
  let { binder, target } = target
  let binder = fmt.binder(binder)
  match target {
    None => binder
    Some(target) => fmt.label(target) + space + text("as") + space + binder
  }
}

///|
fn Fmt::type_var_constraint(
  fmt : Fmt,
  constraint : @syntax.TypeVarConstraint,
) -> Doc {
  ignore(fmt) //TODO: use fmt to access comments
  let { trait_, loc: _ } = constraint
  longident(trait_)
}

///|
fn Fmt::type_var_binder(
  env : Fmt,
  binder : @syntax.TypeVarBinder,
  tvar_table~ : @comment.NodeTable,
) -> Doc {
  let colon = tvar_table.get_token("colon")
  let pluses = tvar_table.get_array("pluses")
  let { name, constraints, name_loc: loc } = binder
  let name = env.wrap_comment(Some(Node(loc)), text(name))
  let constraints = match constraints {
    Empty => empty
    More(_) => {
      let colon = space + env.wrap_comment(colon, char(':'))
      let mut traits = space
      for i, constraint in constraints {
        let plus = if i == 0 {
          empty
        } else {
          env.wrap_comment(pluses[i], space + text("+"))
        }
        let constraint = space + env.type_var_constraint(constraint)
        traits = traits + plus + constraint
      }
      colon + traits
    }
  }
  name + constraints
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
fn Fmt::type_var_binders(
  fmt : Fmt,
  quants : List[@syntax.TypeVarBinder],
  lbracket~ : @comment.Key?,
  commas~ : @comment.NodeArray,
  tvar_tables~ : @comment.NodeTableArray,
  rbracket~ : @comment.Key?,
) -> Doc {
  guard quants is More(_) else { return empty }
  let sep = comma
  let trailing = switch(empty, sep)
  let mut acc = empty
  for i, quant in quants {
    let binder = fmt.type_var_binder(quant, tvar_table=tvar_tables[i])
    let constraints = if i == quants.length() - 1 {
      let trailing_comment = fmt.after_comment(Some(Node(quant.loc())))
      let around_comments = fmt.wrap_comment(commas[i], empty)
      trailing + trailing_comment + around_comments
    } else {
      let sep = fmt.wrap_comment(commas[i], sep)
      sep + line
    }
    acc = acc + binder + constraints
  }
  let lp = char('[')
  let rp = char(']')
  let (before_l, after_l) = fmt.comments(lbracket)
  let (before_r, after_r) = fmt.comments(rbracket)
  group(
    before_l +
    lp +
    nest(after_l + softline + acc + before_r) +
    softline +
    rp +
    after_r,
  )
}

///|
fn Fmt::trait_method(fmt : Fmt, meth : @syntax.TraitMethodDecl) -> Doc {
  let {
    name,
    is_async,
    quantifiers: _,
    params,
    return_type,
    error_type,
    loc: _,
    has_default,
    attrs: _,
  } = meth
  let async_ = if is_async is Some(_) { text("async") + space } else { empty }
  let name = text(name.name)
  let params = fmt.parameters(
    params,
    lparen=None,
    rparen=None,
    commas=@comment.NodeArray::empty(),
    param_tables=@comment.NodeTableArray::empty(),
    kind=TraitMethod,
  )
  let arrow = space + text("->")
  let return_type = match return_type {
    None => empty
    Some(ty) => space + fmt.ty(ty)
  }
  let error_type = fmt.error_type(error_type)
  let has_default = match has_default {
    None => empty
    Some(_) => space + char('=') + space + text("_")
  }
  group(async_ + name + params + arrow + return_type + error_type + has_default)
}

///|
fn Fmt::visibility(fmt : Fmt, vis : @syntax.Visibility) -> Doc {
  match vis {
    Pub(attr=modifier, loc~) => {
      let table = fmt.get_table(loc)
      let pub_ = fmt.wrap_comment(table.get_token("pub"), text("pub"))
      let modifier = match modifier {
        None => empty
        Some(s) => {
          let lparen = fmt.wrap_comment(table.get_token("lparen"), text("("))
          let id = fmt.wrap_comment(table.get_token("lident"), text(s))
          let rparen = fmt.wrap_comment(table.get_token("rparen"), text(")"))
          lparen + id + rparen
        }
      }
      fmt.wrap_comment(Some(Node(loc)), pub_ + modifier) + space
    }
    Priv(loc~) => {
      let table = fmt.get_table(loc)
      let priv_ = fmt.wrap_comment(table.get_token("priv"), text("priv"))
      fmt.wrap_comment(Some(Node(loc)), priv_) + space
    }
    Default => empty
  }
}

///|
fn Fmt::docstring(
  fmt : Fmt,
  doc : @syntax.DocString,
  is_toplevel~ : Bool,
) -> Doc {
  if fmt.block_line && is_toplevel {
    match doc.content {
      Empty => text("///|") + hardline
      More(first_line, tail~) =>
        if first_line.trim(char_set=" \t\n") == "" {
          let first_line = text("///|") + hardline
          let remain_lines = concat_map(tail, x => {
            text("///") + text(x) + hardline
          })
          first_line + remain_lines
        } else {
          let new_first_line = text("///|") + hardline
          let old_first_line = text("///") + text(first_line) + hardline
          let remain_lines = concat_map(tail, x => {
            text("///") + text(x) + hardline
          })
          new_first_line + old_first_line + remain_lines
        }
    }
  } else {
    concat_map(doc.content, x => text("///") + text(x) + hardline)
  }
}

///|
fn Fmt::type_decl_binder(_ : Self, binder : @syntax.TypeDeclBinder) -> Doc {
  let { name, loc: _ } = binder
  match name {
    None => text("_")
    Some(n) => text(n)
  }
}

///|
fn Fmt::type_decl_binders(
  fmt : Self,
  binders : List[@syntax.TypeDeclBinder],
) -> Doc {
  match binders {
    Empty => empty
    More(_) =>
      group(brackets(series1(binders.map(x => fmt.type_decl_binder(x)), comma)))
  }
}

///|
fn Fmt::constr_param(fmt : Self, constr_param : @syntax.ConstrParam) -> Doc {
  let { mut_, label, ty } = constr_param
  let mut_ = if mut_ { text("mut") + space } else { empty }
  let label = match label {
    None => empty
    Some(label) => fmt.label(label) + char('~') + space + char(':') + space
  }
  let ty = fmt.ty(ty)
  mut_ + label + ty
}

///|
fn Fmt::constr_decl(fmt : Self, constr_decl : @syntax.ConstrDecl) -> Doc {
  let { name, args, tag, loc: _, attrs, doc } = constr_decl
  let doc = fmt.docstring(doc, is_toplevel=false)
  let attrs = fmt.attributes(attrs)
  let name = text(name.name)
  let args = match args {
    None => empty
    Some(xs) => group(parens(series1(xs.map(x => fmt.constr_param(x)), comma)))
  }
  let tag = match tag {
    None => empty
    Some((tag, _)) => space + char('=') + space + text(tag)
  }
  doc + attrs + name + args + tag
}

///|
fn Fmt::deriving_directive(
  fmt : Self,
  deriving : @syntax.DerivingDirective,
) -> Doc {
  let { type_name, args, loc: _ } = deriving
  let type_name = fmt.type_name(type_name)
  let args = match args {
    Empty => empty
    More(_) => parens(series1(args.map(x => fmt.argument(x)), comma))
  }
  type_name + args
}

///|
fn Fmt::deriving_directives(
  fmt : Self,
  deriving : List[@syntax.DerivingDirective],
) -> Doc {
  match deriving {
    Empty => empty
    More(_) => {
      let derive_ = text("derive")
      let xs = group(
        parens(series1(deriving.map(x => fmt.deriving_directive(x)), comma)),
      )
      space + derive_ + xs
    }
  }
}

///|
fn Fmt::field_decl(fmt : Self, field_decl : @syntax.FieldDecl) -> Doc {
  let { name, ty, mut_, vis, attrs, doc, loc: _ } = field_decl
  let doc = fmt.docstring(doc, is_toplevel=false)
  let attrs = fmt.attributes(attrs)
  let vis = fmt.visibility(vis)
  let mut_ = if mut_ { text("mut") + space } else { empty }
  let name = text(name.label)
  let ty = fmt.ty(ty)
  doc + attrs + vis + mut_ + name + space + char(':') + space + ty
}

///|
fn Fmt::typedecl(fmt : Fmt, typedecl : @syntax.TypeDecl) -> Doc {
  ignore(fmt) //TODO: use fmt to access comments
  let {
    tycon,
    tycon_loc: _,
    params,
    components,
    attrs,
    doc,
    type_vis,
    deriving,
    loc: _,
    is_declare,
  } = typedecl
  let doc = fmt.docstring(doc, is_toplevel=true)
  let attrs = fmt.attributes(attrs)
  let declare_ = if is_declare { text("declare") + space } else { empty }
  let vis = fmt.visibility(type_vis)
  let semi = switch(char(';'), empty)
  match components {
    Variant(constr_decls) | ExtensibleEnum(constr_decls) => {
      let enum_ = text(
        if components is ExtensibleEnum(_) {
          "extenum"
        } else {
          "enum"
        },
      )
      let tycon = space + text(tycon)
      let params = fmt.type_decl_binders(params)
      let lbrace = space + char('{')
      let constrs = nest(
        hardline + series1(constr_decls.map(x => fmt.constr_decl(x)), semi),
      )
      let rbrace = hardline + char('}')
      let deriving = fmt.deriving_directives(deriving)
      doc +
      attrs +
      declare_ +
      vis +
      enum_ +
      tycon +
      params +
      lbrace +
      constrs +
      rbrace +
      deriving
    }
    Record(fields~) => {
      let struct_ = text("struct")
      let tycon = space + text(tycon)
      let params = fmt.type_decl_binders(params)
      let lbrace = space + char('{')
      let semi = switch(char(';'), empty)
      let fields_doc = nest(
        hardline + series1(fields.map(x => fmt.field_decl(x)), semi),
      )
      let rbrace = hardline + char('}')
      let deriving = fmt.deriving_directives(deriving)
      doc +
      attrs +
      declare_ +
      vis +
      struct_ +
      tycon +
      params +
      lbrace +
      fields_doc +
      rbrace +
      deriving
    }
    Error(exception_decl) => {
      let struct_ = text("suberror")
      let tycon = space + text(tycon)
      let semi = switch(char(';'), empty)
      let payload = match exception_decl {
        NoPayload => empty
        EnumPayload(constr_decls) =>
          space +
          braces(
            series1(constr_decls.map(x => fmt.constr_decl(x)), semi),
            force_newline=true,
          )
      }
      doc + attrs + declare_ + vis + struct_ + tycon + payload
    }
    ExtendEnum(target~, constructors~) => {
      let enum_ = text("extenum")
      let target = space + fmt.type_name(target)
      let plus_equal = space + text("+=")
      let constrs = space +
        braces(
          series1(constructors.map(x => fmt.constr_decl(x)), semi),
          force_newline=true,
        )
      doc + attrs + declare_ + vis + enum_ + target + plus_equal + constrs
    }
    Abstract => {
      let type_ = text("type")
      let tycon = space + text(tycon)
      doc + attrs + declare_ + vis + type_ + tycon
    }
    TupleStruct(tys) => {
      let type_ = text("struct")
      let tycon = space + text(tycon)
      let tys = group(parens(series1(tys.map(x => fmt.ty(x)), comma)))
      let params = fmt.type_decl_binders(params)
      doc + attrs + declare_ + vis + type_ + tycon + params + tys
    }
    Alias(ty) => {
      let type_ = text("typealias")
      let ty = space + fmt.ty(ty)
      let as_ = space + text("as")
      let tycon = space + text(tycon)
      doc + attrs + declare_ + vis + type_ + ty + as_ + tycon
    }
  }
}

///|
fn Fmt::constant(fmt : Fmt, constant : @syntax.Constant) -> Doc {
  ignore(fmt) //TODO: use fmt to access comments
  match constant {
    String(s) => double_quoted(text(s))
    Char(s) => single_quoted(text(s))
    Bytes(s) => char('b') + double_quoted(text(s))
    Byte(s) => char('b') + single_quoted(text(s))
    Bool(b) => @pp.pretty(b)
    BigInt(s) => text(s) + char('N')
    Regex(s) => text("re") + double_quoted(text(s))
    UInt64(s) => text(s) + char('U') + char('L')
    UInt(s) => text(s) + char('U')
    Int64(s) => text(s) + char('L')
    Float(s) => text(s) + char('F')
    Double(s) | Int(s) => text(s)
  }
}

///|
fn Fmt::interps(fmt : Fmt, interps : List[@syntax.InterpElem]) -> Doc {
  interps.fold(init=empty, fn(acc, x) {
    match x {
      Literal(repr~, ..) => acc + text(repr)
      Expr(expr~, ..) => acc + text("\\{") + fmt.compose(expr, Expr) + char('}')
      Source({ source, .. }) => acc + text("\\{") + text(source) + char('}')
    }
  })
}

///|
fn Fmt::multiline_string(
  env : Fmt,
  elems : List[@syntax.MultilineStringElem],
  parens~ : Bool,
  loc~ : Location,
) -> Doc {
  ignore(parens)
  ignore(loc)
  let elems = separate_map(elems, hardline, elem => {
    match elem {
      String(s) => text(s)
      Interp(xs) => env.interps(xs)
    }
  })
  group(elems)
}

///|
fn Fmt::label(fmt : Fmt, label : @syntax.Label) -> Doc {
  ignore(fmt) //TODO: use fmt to access comments
  let { name, loc: _ } = label
  text(name)
}

///|
fn is_pun_eligible_for_expr(
  label : @syntax.Label,
  value : @syntax.Expr,
) -> Bool {
  value is Ident(id={ name: Ident(name~), .. }, ..) && name == label.name
}

///|
fn is_pun_eligible_for_pat(
  label : @syntax.Label,
  pat : @syntax.Pattern,
) -> Bool {
  pat is Var({ name, .. }) && name == label.name
}

///|
fn Fmt::argument(fmt : Fmt, arg : @syntax.Argument) -> Doc {
  //TODO: print in compact style for short expr 

  let { value, kind } = arg
  match kind {
    Positional => fmt.compose(value, Expr)
    Labelled(label) =>
      if is_pun_eligible_for_expr(label, value) {
        fmt.label(label) + char('~')
      } else {
        fmt.label(label) + char('=') + fmt.compose(value, Expr) //TODO: compose_rhs
      }
    LabelledOption(label~, question_loc=_) =>
      if is_pun_eligible_for_expr(label, value) {
        fmt.label(label) + char('?')
      } else {
        fmt.label(label) + text("?=") + fmt.compose(value, Expr) //TODO: compose_rhs
      }
    LabelledPun(label) => fmt.label(label) + char('~')
    LabelledOptionPun(label~, question_loc=_) => fmt.label(label) + char('?')
  }
}

///|
fn Fmt::arguments(fmt : Fmt, args : List[@syntax.Argument]) -> Doc {
  // TODO: optimize list <-> array conversion and readability
  let arr = args.to_array()
  // Bad case:
  //
  // Before applying the trailing block rules, we should ensure the remaining space 
  // is enough for leading arguments and the head of the trailing function.
  //
  // ```
  // f(arg1, arg2, ..., argN, [ elems ])
  //   ^^^^^^^^^^^^^^^^^^^^^^
  // f(arg1, arg2, ..., argN, (x,y) => e)
  //   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // f(arg1, arg2, ..., argN, fn(x,y){ e })
  //   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^~~~~~~~ is this part fits in current remaining space?
  // ```
  // 
  // Consider this bad result:
  // ```
  // long_function(argument1, argument2, argument3, argument4, argument5, fn(
  //   param1, 
  //   param2,
  // ) {
  //   expr
  // })
  // ```
  match arr.pop() {
    Some({ value: trailing_arg_value, kind: Positional } as trailing_arg) => {
      let leading_args = arr.map(x => fmt.argument(x))
      fn trailing_block_style() {
        let leading_doc = series1(
          List(leading_args),
          comma,
          trailing=AlwaysPresent,
        )
        let trailing_doc = fmt.argument(trailing_arg)
        let sep = if leading_args.is_empty() { empty } else { space }
        group(char('(') + (group(leading_doc) + sep + trailing_doc) + char(')'))
      }

      fn fallback_style() {
        leading_args.push(fmt.argument(trailing_arg))
        group(parens(series1(List(leading_args), comma)))
      }

      fn switch_trailing_block(leading_req, total_req) {
        ignore(total_req) //TODO: use total_req to decide the layout
        dynamic(fn(ctx) {
          if ctx.current_remain() >= leading_req {
            trailing_block_style()
          } else {
            fallback_style()
          }
        })
      }

      let leading_args_req = leading_args.fold(init=Requirement::Space(0), fn(
        acc,
        x,
      ) {
        acc + x.requirement() + Space(2)
      })
      match trailing_arg_value {
        Record(_) | Array(_) => {
          guard fmt.expr(trailing_arg_value) is BlockLike(doc)
          let leading_req = leading_args_req
          let total_req = leading_args_req + doc.requirement()
          switch_trailing_block(leading_req, total_req)
        }
        Function(_) => {
          guard fmt.expr(trailing_arg_value) is Collapsible(head, body)
          let leading_req = leading_args_req + head.requirement()
          let total_req = leading_args_req +
            head.requirement() +
            body.requirement()
          switch_trailing_block(leading_req, total_req)
        }
        _ => fallback_style()
      }
    }
    _ => group(parens(series1(args.map(x => fmt.argument(x)), comma)))
  }
}

///|
fn longident(id : @syntax.LongIdent) -> Doc {
  match id {
    Ident(name~) => text(name)
    Dot(pkg~, id~) => text("@" + pkg + "." + id)
  }
}

///|
fn Fmt::var_(fmt : Self, variable : @syntax.Var) -> Doc {
  ignore(fmt) //TODO: use fmt to access comments
  longident(variable.name)
}

///|
fn Fmt::binder(fmt : Self, binder : @syntax.Binder) -> Doc {
  ignore(fmt) //TODO: use fmt to access comments
  let { name, loc: _ } = binder
  text(name)
}

///|
fn Fmt::pattern(fmt : Self, pat : @syntax.Pattern) -> Doc {
  match pat {
    SpecialConstr(_) => abort("unsupported special constr")
    Var(binder) => fmt.binder(binder)
    Any(loc=_) => char('_')
    Tuple(pats~, loc=_) =>
      group(parens(series1(pats.map(x => fmt.pattern(x)), comma)))
    Constr(constr~, args=payload, is_open~, loc=_) => {
      fn constr_pat_arg2doc(arg : @syntax.ConstrPatArg) -> Doc {
        let { pat, kind } = arg
        let pat_doc = fmt.pattern(pat)
        match kind {
          Positional => pat_doc
          Labelled(label) =>
            if is_pun_eligible_for_pat(label, pat) {
              fmt.label(label) + equal + pat_doc
            } else {
              fmt.label(label) + tilde
            }
          LabelledPun(label) => fmt.label(label) + tilde
          LabelledOption(_) | LabelledOptionPun(_) => panic()
        }
      }

      let constr = fmt.constr(constr)
      let payload = match payload {
        None => empty
        Some(args) => {
          let args = args.map(constr_pat_arg2doc)
          let args = if is_open {
            // Replace deprecated @list.of usage with from_array
            args.concat(List([text("..")]))
          } else {
            args
          }
          group(parens(series(args, comma, x => x)))
        }
      }
      group(constr + payload)
    }
    Constant(c~, loc=_) => fmt.constant(c)
    Array(pats~, loc=_) => {
      fn array_pattern2doc(pat : @syntax.ArrayPattern) -> Doc {
        match pat {
          Pattern(pat) => fmt.pattern(pat)
          StringSpread(str~, ..) => text("..") + double_quoted(text(str))
          BytesSpread(bytes~, ..) =>
            text("..") + char('b') + double_quoted(text(bytes))
          ConstSpread(binder~, pkg~, loc=_) => {
            let pkg = match pkg {
              Some(x) => text("@\{x}.")
              None => empty
            }
            let binder = fmt.binder(binder)
            text("..") + pkg + binder
          }
        }
      }

      match pats {
        Closed(elems) =>
          group(brackets(series(elems, comma, array_pattern2doc)))
        Open(left, right, remain) => {
          let elems = left.to_array().map(array_pattern2doc)
          match remain {
            NoBinder => ()
            Binder(b) | BinderAs(b) => elems.push(text("..") + fmt.binder(b))
            Underscore => elems.push(text(".._"))
          }
          elems.push_iter(right.map(array_pattern2doc).iter())
          let elems = @list.List(elems)
          group(brackets(series(elems, comma, x => x)))
        }
      }
    }
    Range(lhs~, rhs~, kind~, loc=_) => {
      let lhs = fmt.pattern(lhs)
      let op = text(
        match kind {
          Inclusive | InclusiveMissingEqual => "..="
          Exclusive => "..<"
        },
      )
      let rhs = fmt.pattern(rhs)
      group(lhs + op + rhs)
    }
    Record(fields~, is_closed~, loc=_) => {
      fn field_pat2doc(field : @syntax.FieldPat) -> Doc {
        let { label, pattern, is_pun, loc: _ } = field
        if is_pun || is_pun_eligible_for_pat(label, pattern) {
          fmt.label(label)
        } else {
          fmt.label(label) + colon + space + fmt.pattern(pattern)
        }
      }

      let elems = fields.to_array().map(field_pat2doc)
      if is_closed {
        elems.push(text(".."))
      }
      let elems = @list.List(elems)
      group(braces(series(elems, comma, x => x)))
    }
    Map(elems~, is_closed~, loc=_) => {
      fn map_pat2doc(elem : @syntax.MapPatElem) -> Doc {
        let { key, pat, match_absent, key_loc: _, loc: _ } = elem
        let key = fmt.constant(key)
        let absent = if match_absent { char('?') } else { empty }
        let pat = space + fmt.pattern(pat)
        key + absent + colon + pat
      }

      let elems = elems.to_array().map(map_pat2doc)
      if is_closed {
        elems.push(text(".."))
      }
      let elems = @list.List(elems)
      group(braces(series(elems, comma, x => x)))
    }
    Constraint(pat~, ty~, loc=_) => {
      let pat = fmt.pattern(pat)
      let colon = space + char(':')
      let ty = space + fmt.ty(ty)
      group(parens(pat + colon + ty))
    }
    Alias(pat~, alias_~, loc=_) => {
      let pat = fmt.pattern(pat)
      let as_ = space + text("as")
      let alias_ = space + fmt.binder(alias_)
      group(pat + as_ + alias_)
    }
    Or(pat1~, pat2~, loc=_) => {
      let sep = line + char('|') + space
      let pats = for acc = @list.List([pat1]), cur = pat2 {
        match cur {
          Or(pat1~, pat2~, loc=_) => continue acc.add(pat1), pat2
          pat => break acc.add(pat).rev()
        }
      }
      let lparen = switch(empty, char('(') + space)
      let pats = separate(pats.map(x => fmt.pattern(x)), sep)
      let rparen = switch(empty, softline + char(')'))
      lparen + pats + rparen
    }
  }
}

///|
/// Convert ErrorType to Document, the result including the leading space.
/// 
/// for example:
/// `` 
/// ` noraise`
/// ` raise TYPE`
/// ` raise?`
fn Fmt::error_type(fmt : Self, err : @syntax.ErrorType) -> Doc {
  match err {
    NoErrorType => empty
    DefaultErrorType(loc=_) => space + text("raise")
    Noraise(loc=_) => space + text("noraise")
    ErrorType(ty~) => space + text("raise") + space + fmt.ty(ty)
    MaybeError(ty=_) => space + text("raise?")
  }
}

///|
fn Fmt::ty(fmt : Self, ty : @syntax.Type) -> Doc {
  match ty {
    Any(loc=_) => char('_')
    Name(constr_id={ id: constr, loc: _ }, tys~, loc=_) => {
      // If the user writes `Option[Int]`, keep it as is.
      let constr = longident(constr)
      let tys = match tys {
        Empty => empty
        More(_) => group(brackets(series1(tys.map(x => fmt.ty(x)), comma)))
      }
      constr + tys
    }
    Tuple(tys~, loc=_) => group(parens(series1(tys.map(x => fmt.ty(x)), comma)))
    Arrow(args~, res~, err~, is_async~, loc=_) => {
      let async_ = if is_async is Some(_) {
        text("async") + space
      } else {
        empty
      }
      let args = group(parens(series1(args.map(x => fmt.ty(x)), comma)))
      let arrow = space + text("->")
      let res = space + fmt.ty(res)
      let err = fmt.error_type(err)
      async_ + args + arrow + res + err
    }
    Option(ty~, loc=_, question_loc=_) => {
      let ty = match ty {
        Arrow(_) => parens(fmt.ty(ty))
        Name(_) | Tuple(_) | Any(_) | Option(_) | Object(_) => fmt.ty(ty)
      }
      ty + char('?')
    }
    Object({ id: constr, loc: _ }) => {
      let constr = longident(constr)
      char('&') + constr
    }
  }
}

///|
fn Fmt::type_name(fmt : Self, typename : @syntax.TypeName) -> Doc {
  ignore(fmt) // TODO: use fmt to get comments
  let { name, is_object, loc: _ } = typename
  let object_mark = if is_object { char('&') } else { empty }
  let name = longident(name)
  object_mark + name
}

///|
fn Fmt::constr(fmt : Self, constr : @syntax.Constructor) -> Doc {
  let { extra_info, name: { name, loc: _ }, loc: _ } = constr
  let extra_info = match extra_info {
    NoExtraInfo => empty
    TypeName(n) => fmt.type_name(n) + text("::")
    TypeNameWithConstrPackage(type_name~, pkg~) =>
      fmt.type_name(type_name) + text("::@" + pkg + ".")
    Package(p) => text("@" + p + ".")
  }
  let name = text(name)
  extra_info + name
}

///|
fn Fmt::parameter(
  fmt : Self,
  parameter : @syntax.Parameter,
  table : @comment.NodeTable,
  kind? : ParameterListKind = Regular,
) -> Doc {
  ignore(table) // TODO: use table for comments
  match parameter {
    Optional(binder~, default~, ty~) => {
      let binder = fmt.binder(binder) + char('?')
      let ty = match ty {
        None => empty
        Some(ty) => space + colon + space + fmt.ty(ty)
      }
      let equal = space + equal
      let default = space + fmt.compose(default, Expr)
      binder + ty + equal + default
    }
    Labelled(binder~, ty~) => {
      let binder = fmt.binder(binder) + char('~')
      let ty = match ty {
        None => empty
        Some(ty) => space + colon + space + fmt.ty(ty)
      }
      binder + ty
    }
    Positional(binder~, ty~) => {
      let binder = fmt.binder(binder)
      let ty = match ty {
        None => empty
        Some(ty) => space + colon + space + fmt.ty(ty)
      }
      binder + ty
    }
    DiscardPositional(ty~, loc=_) =>
      if kind is TraitMethod {
        guard ty is Some(ty) else {
          abort("Trait method parameter must have type annotation.")
        }
        fmt.ty(ty)
      } else {
        let binder = char('_')
        let ty = match ty {
          None => empty
          Some(ty) => space + colon + space + fmt.ty(ty)
        }
        binder + ty
      }
    QuestionOptional(binder~, ty~) => {
      let binder = fmt.binder(binder) + char('?')
      let ty = match ty {
        None => empty
        Some(ty) => space + colon + space + fmt.ty(ty)
      }
      binder + ty
    }
  }
}

///|
priv enum ParameterListKind {
  Regular
  TraitMethod
  ArrowFunction
}

///|
/// 
/// # Parameters
/// 
/// - `kind`: indicates the context of the parameters.
///   If `kind` is:
///   - Regular: print all parameters as is.
///   - TraitMethod: hide the parameter name if it is `DiscardPositional`.
///   - ArrowFunction: omit the parentheses if there is a single parameter.
fn Fmt::parameters(
  fmt : Self,
  parameters : List[@syntax.Parameter],
  lparen~ : @comment.Key?,
  param_tables~ : @comment.NodeTableArray,
  commas~ : @comment.NodeArray,
  rparen~ : @comment.Key?,
  kind? : ParameterListKind = Regular,
) -> Doc {
  ignore(rparen) //TODO: use rparen comment
  let sep = comma
  let trailing = switch(empty, sep)
  let mut acc = empty
  for i, param in parameters {
    acc += fmt.parameter(param, param_tables[i], kind~)
    if i == parameters.length() - 1 {
      let trailing_comment = fmt.after_comment(Some(Node(param.loc())))
      let around_comments = fmt.wrap_comment(commas[i], empty)
      acc += trailing + trailing_comment + around_comments
    } else {
      let sep = fmt.wrap_comment(commas[i], sep)
      acc += sep + line
    }
  }
  let l = softline
  match kind {
    ArrowFunction if parameters is More(_, tail=Empty) => acc
    _ => {
      let lp = fmt.wrap_comment(lparen, char('('))
      let rp = fmt.wrap_comment(lparen, char(')'))
      lp + nest(l + acc) + l + rp
    }
  }
}

///|
fn Fmt::arrow_function(
  fmt : Self,
  parameters~ : @syntax.Parameters,
  body~ : @syntax.Expr,
) -> RawExprDoc {
  let params = group(
    fmt.parameters(
      parameters,
      lparen=None,
      param_tables=@comment.NodeTableArray::empty(),
      commas=@comment.NodeArray::empty(),
      rparen=None,
      kind=ArrowFunction,
    ),
  )
  let arrow = space + text("=>")
  let body = fmt.compose_arrow_rhs(body)
  Collapsible(params + arrow, body)
}

///|
fn Fmt::func(
  fmt : Self,
  func : @syntax.Func,
  binder? : @syntax.Binder,
) -> RawExprDoc {
  let is_annomyous = binder is None
  let { parameters, body, return_type, error_type, kind, is_async, .. } = func
  let async_ = if is_async is Some(_) { text("async") + space } else { empty }
  let fn_ = match kind {
    Arrow => empty
    Lambda => text("fn")
  }
  let binder = match binder {
    None => empty
    Some(b) => space + fmt.binder(b)
  }
  let params = group(
    fmt.parameters(
      parameters,
      lparen=None,
      param_tables=@comment.NodeTableArray::empty(),
      commas=@comment.NodeArray::empty(),
      rparen=None,
    ),
  )
  let has_error_ty = match error_type {
    NoErrorType => false
    DefaultErrorType(_) | Noraise(_) | ErrorType(_) | MaybeError(_) => true
  }
  let arrow = match kind {
    Arrow => space + text("=>")
    Lambda =>
      if return_type is Some(_) || has_error_ty {
        space + text("->")
      } else {
        empty
      }
  }
  let ety = fmt.error_type(error_type)
  let rty = match return_type {
    None => empty
    Some(ty) => space + fmt.ty(ty)
  }
  let body = space + fmt.expr_block(body, force_newline=!is_annomyous)
  Collapsible(async_ + fn_ + binder + params + arrow + rty + ety, body)
}

///|
fn Fmt::field(fmt : Self, field : @syntax.FieldDef) -> Doc {
  let { label, expr, is_pun, loc: _ } = field
  if is_pun || is_pun_eligible_for_expr(label, expr) {
    fmt.label(label)
  } else {
    fmt.label(label) + colon + space + fmt.compose(expr, Expr)
  }
}

///|
/// in cases `(1).f()`, `(-1L).f()` and `(-1.0).f()`, the parens should be reserved 
fn Fmt::expr_before_dot(fmt : Self, expr : @syntax.Expr) -> Doc {
  let doc = fmt.compose(expr, Prefix)
  match expr {
    Constant(c=Int(_), loc=_) => group(parens(doc))
    Constant(c=Double(str) | Int64(str), loc=_) if str.has_prefix("-") =>
      group(parens(doc))
    _ => doc
  }
}

///|
fn Fmt::accessor(self : Self, accessor : @syntax.Accessor) -> Doc {
  match accessor {
    Label(label) => dot + self.label(label)
    Index(tuple_index~, loc=_) => dot + text(tuple_index.to_string())
    Newtype(loc=_) => text(".inner()")
  }
}

///|
fn Fmt::loop_label(
  self : Self,
  label : @syntax.Label?,
  trailing_colon~ : Bool,
) -> Doc {
  match label {
    None => empty
    Some(label) =>
      space +
      self.label(label) +
      tilde +
      (if trailing_colon { colon } else { empty })
  }
}

///|
fn Fmt::lex_pattern(fmt : Self, pat : @syntax.LexPattern) -> Doc {
  match pat {
    Regex(lit~, offset=_, loc=_) => double_quoted(text(lit))
    ConstantRef(lid~, loc=_) => longident(lid)
    RegexInterp(elems~, loc=_) => {
      fn elem2doc(elem : @syntax.InterpElem) -> Doc {
        match elem {
          Literal(repr~, loc=_) => text(repr)
          Expr(expr~, loc=_) =>
            text("\\{") + fmt.compose(expr, Expr) + text("}")
          Source({ source, loc: _ }) => text("\\{") + text(source) + text("}")
        }
      }

      double_quoted(concat_map(elems, elem2doc))
    }
    Alias(pat~, binder~, loc=_) =>
      fmt.lex_pattern(pat) + space + text("as") + space + fmt.binder(binder)
    Sequence(pats~, loc=_) => separate(pats.map(p => fmt.lex_pattern(p)), space)
  }
}

///|
fn Fmt::regex_pattern(fmt : Self, pat : @syntax.RegexPattern) -> Doc {
  match pat {
    Literal(lit~, loc=_) => text("re") + double_quoted(text(lit))
    Reference(lid~, loc=_) => longident(lid)
    Sequence(pat1~, pat2~, loc=_) =>
      fmt.regex_pattern(pat1) +
      space +
      char('+') +
      space +
      fmt.regex_pattern(pat2)
    Alternation(pat1~, pat2~, loc=_) =>
      fmt.regex_pattern(pat1) +
      space +
      char('|') +
      space +
      fmt.regex_pattern(pat2)
    Alias(pat~, binder~, loc=_) =>
      fmt.regex_pattern(pat) + space + text("as") + space + fmt.binder(binder)
  }
}

///|
fn Fmt::lex_top_pattern(fmt : Self, pat : @syntax.LexTopPattern) -> Doc {
  match pat {
    Pattern(p) => fmt.lex_pattern(p)
    Binder(b) => fmt.binder(b)
    Wildcard(loc=_) => text("_")
  }
}

///|
fn Fmt::lex_top_patterns(fmt : Self, pats : List[@syntax.LexTopPattern]) -> Doc {
  match pats {
    More(x, tail=Empty) => fmt.lex_top_pattern(x)
    _ => group(parens(series1(pats.map(p => fmt.lex_top_pattern(p)), comma)))
  }
}

///|
fn Fmt::lex_case(fmt : Self, case_ : @syntax.LexCase) -> Doc {
  let { pat, pat_loc: _, guard_, body } = case_
  let pattern = fmt.lex_top_patterns(pat)
  let guard_ = match guard_ {
    None => empty
    Some(expr) => space + text("if") + space + fmt.compose(expr, Infix(Other))
  }
  let body = space + text("=>") + fmt.compose_case_rhs(body, ExprStmt)
  group(group(pattern) + guard_ + body)
}

///|
fn Fmt::lex_scan_case_pattern(
  fmt : Self,
  pat : @syntax.LexScanCasePattern,
) -> Doc {
  match pat {
    Pattern(p) => fmt.regex_pattern(p)
    Binder(b) => fmt.binder(b)
    Wildcard(loc=_) => text("_")
  }
}

///|
fn Fmt::lex_scan_case(fmt : Self, case_ : @syntax.LexScanCase) -> Doc {
  let { pat, guard_, body } = case_
  let pattern = fmt.lex_scan_case_pattern(pat)
  let guard_ = match guard_ {
    None => empty
    Some(expr) => space + text("if") + space + fmt.compose(expr, Infix(Other))
  }
  let body = space + text("=>") + fmt.compose_case_rhs(body, ExprStmt)
  group(group(pattern) + guard_ + body)
}

///|
fn Fmt::lex_strategy(fmt : Self, strategy : @syntax.Label?) -> Doc {
  match strategy {
    None => empty
    Some(label) => space + text("with") + space + fmt.label(label)
  }
}

///|
fn Fmt::case(fmt : Self, case_ : @syntax.Case) -> Doc {
  let { pattern, guard_, body } = case_
  let pattern = fmt.pattern(pattern)
  let guard_ = match guard_ {
    None => empty
    Some(expr) => space + text("if") + space + fmt.compose(expr, Infix(Other))
  }
  let body = space + text("=>") + fmt.compose_case_rhs(body, ExprStmt) //TODO: compose_rhs
  group(group(pattern) + guard_ + body)
}

///|
fn Fmt::expr_block(
  fmt : Self,
  expr : @syntax.Expr,
  force_newline? : Bool = true,
  grouped? : Bool = true,
) -> Doc {
  let doc = braces(fmt.compose(expr, Stmt), force_newline~)
  if grouped {
    group(doc)
  } else {
    doc
  }
}

///|
fn Fmt::where_clause(fmt : Self, where_clause : @syntax.WhereClause?) -> Doc {
  match where_clause {
    None => empty
    Some({ fields, loc: _ }) =>
      space +
      text("where") +
      space +
      group(braces(series1(fields.map(x => fmt.field(x)), comma)))
  }
}

///|
fn Fmt::list_comprehension_kind(
  fmt : Self,
  kind : @syntax.ListComprehensionKind,
) -> Doc {
  fn binder_expr2doc(pair : (@syntax.Binder, @syntax.Expr)) -> Doc {
    let (binder, expr) = pair
    fmt.binder(binder) + space + equal + space + fmt.compose(expr, Infix(Other))
  }

  fn foreach_binder2doc(binder : @syntax.Binder?) -> Doc {
    match binder {
      None => empty
      Some(x) => fmt.binder(x)
    }
  }

  let semi_or_newline = switch(semi + space, softline)
  match kind {
    Foreach(binders~, expr~, init~, continue_block~) => {
      let init = match init {
        Empty => empty
        More(_) =>
          semi_or_newline +
          series(init, comma, binder_expr2doc, trailing=AlwaysHidden)
      }
      let continue_block = match continue_block {
        Empty => empty
        More(_) =>
          semi_or_newline +
          series(continue_block, comma, binder_expr2doc, trailing=AlwaysHidden)
      }
      text("for") +
      space +
      series(binders, comma, foreach_binder2doc, trailing=AlwaysHidden) +
      space +
      text("in") +
      space +
      fmt.compose(expr, Infix(Other)) +
      init +
      continue_block
    }
    For(binders~, condition~, continue_block~, for_loc=_) => {
      let binders = series(
        binders,
        comma,
        binder_expr2doc,
        trailing=AlwaysHidden,
      )
      let condition = condition.map(x => fmt.compose(x, Infix(Other)))
      let continue_block = match continue_block {
        Empty => None
        More(_) =>
          Some(
            series(
              continue_block,
              comma,
              binder_expr2doc,
              trailing=AlwaysHidden,
            ),
          )
      }
      text("for") +
      space +
      binders +
      semi_or_newline +
      condition.unwrap_or(empty) +
      semi_or_newline +
      continue_block.unwrap_or(empty)
    }
  }
}

///|
fn Fmt::case_block(
  self : Self,
  cases : List[@syntax.Case],
  force_newline? : Bool = true,
) -> Doc {
  group(braces(series1(cases.map(x => self.case(x)), empty), force_newline~))
}

///|
/// Handle chained expression like `self.f().g().field.1`.
/// 
/// Ideally, the chained method calls should be formatted like below:
///
/// ```skip
/// some_object
///   .long_method_call1(arg1, arg2)
///   .long_method_call2(arg1)
/// ```
///
/// However, if the lhs is too short or printing in several lines, it may 
/// cause the entire expression weirdly indented.
/// 
/// Bad case 1: lhs is too short
/// ```skip
/// x
///   .long_method_call1(arg1, arg2)
///   .long_method_call2(arg1)
/// ```
/// 
/// Bad case 2: lhs is printed in several lines, and the last line is too short
/// ```skip
/// {
///   field1: value1,
///   field2: value2,
/// }
///   .long_method_call1(arg1, arg2)
///   .long_method_call2(arg1)
/// ```
/// 
/// Bad case 3: the rhs is only one method call 
/// ```skip
/// object
///   .long_method_call(
///     long_arg1, 
///     long_arg2,
///   )
/// ```
fn Fmt::chained_dot_expr(fmt : Self, e : @syntax.Expr) -> RawExprDoc {
  let xs = for acc = @list.empty(), cur = e {
    match cur {
      DotApply(self~, ..) as apply => continue acc.add(apply), self
      Field(record~, ..) as field => continue acc.add(field), record
      expr => break acc.add(expr)
    }
  }
  guard xs is More(s, tail=chained_exprs)
  let lhs = fmt.expr_before_dot(s)
  let chained = separate_map(chained_exprs, softline, fn(expr) {
    match expr {
      DotApply(self=_, method_name~, args~, return_self~, loc=_) => {
        let dot = if return_self { text("..") } else { text(".") }
        let method_name = fmt.label(method_name)
        let args = fmt.arguments(args)
        dot + method_name + args
      }
      Field(record=_, accessor~, loc=_) => fmt.accessor(accessor)
      _ => panic() // unreachable
    }
  })
  let apps = if chained_exprs is More(_, tail=Empty) {
    group(chained)
  } else {
    group(
      current_column_without_indent(fn(column) {
        if lhs.requirement() < Space(3) {
          // improve style in bad case 1
          nest(chained)
        } else if column < 3 {
          // improve style in bad case 2
          // multiline line lhs but `current_column - current_indent < 3`
          softline + nest(chained)
        } else {
          nest(softline + chained)
        }
      }),
    )
  }
  // The chained `a.b.c` should be `Collapsible`, so that if the expression
  // needs line wrap and occurs in the rhs of `let lhs = rhs` or `lhs = rhs`, the 
  // indentation will be correct.
  // 
  // Bad format: 4 spaces indentation
  // ```
  // let binder = some_long_lhs
  //     .long_method_call1(arg1, arg2)
  //     .long_method_call2(arg1)
  // ```
  //
  // Good format: 2 spaces indentation
  // ```
  // let binder = some_long_lhs
  //   .long_method_call1(arg1, arg2)
  //   .long_method_call2(arg1)
  // ```
  Collapsible(lhs, apps)
}

///|
fn Fmt::expr(fmt : Self, expr : @syntax.Expr) -> RawExprDoc {
  match expr {
    Apply(func~, args~, loc~) => {
      let func = match func {
        // (self.label)(args) 
        // (self.0)(args)
        Field(accessor=Label(_) | Index(_), ..) =>
          parens(fmt.compose(func, Simple))
        _ => fmt.compose(func, Simple)
      }
      let args = fmt.arguments(args)
      let before_comment = fmt.before_comment(Some(Node(loc)))
      let after_comment = fmt.after_comment(Some(Node(loc)))
      Collapsible(before_comment + func, args + after_comment)
    }
    Infix(op~, lhs~, rhs~, loc=_) => {
      let op_str = match op.name {
        Ident(name~) => name
        _ => panic()
      }
      let op = longident(op.name)
      let layout = if op_str is ("..<" | "..=") {
        let lhs = fmt.compose(lhs, Range)
        let rhs = fmt.compose(rhs, Range)
        group(lhs + op + rhs)
      } else {
        let lhs = fmt.compose(
          lhs,
          Infix(InfixOperand(op=op_str, pos=LeftOperand)),
        )
        let rhs = fmt.compose(
          rhs,
          Infix(InfixOperand(op=op_str, pos=RightOperand)),
        )
        // TODO: improve the style for long, nested infix expressions
        lhs + space + op + space + rhs
      }
      Normal(layout)
    }
    Unary(op~, expr~, loc=_) => {
      let op = fmt.var_(op)
      let expr = match expr {
        // -(_) or +(_)
        Hole(kind=_, loc=_) => parens(fmt.compose(expr, Simple))
        _ => fmt.compose(expr, Simple)
      }
      let layout = group(op + expr)
      Normal(layout)
    }
    Array(exprs~, is_iter~, loc~) => {
      let before_comment = fmt.before_comment(Some(Node(loc)))
      let elems = series(exprs, comma, e => fmt.compose(e, Expr))
      let doc = if is_iter {
        if exprs.is_empty() {
          text("[||]")
        } else {
          group(text("[|") + space + elems + space + text("|]"))
        }
      } else {
        group(brackets(elems))
      }
      let after_comment = fmt.after_comment(Some(Node(loc)))
      BlockLike(before_comment + doc + after_comment)
    }
    ArraySpread(elems~, is_iter~, loc=_) => {
      fn spreadable2doc(elem : @syntax.SpreadableElem) -> Doc {
        match elem {
          Regular(expr) => fmt.compose(expr, Expr)
          Spread(expr~, loc=_) => text("..") + fmt.compose(expr, Expr)
        }
      }

      let elems_doc = series(elems, comma, spreadable2doc)
      let doc = if is_iter {
        if elems.is_empty() {
          text("[||]")
        } else {
          group(text("[|") + space + elems_doc + space + text("|]"))
        }
      } else {
        group(brackets(elems_doc))
      }
      Normal(doc)
    }
    ListComprehension(kind~, guard_~, body~, is_iter~, loc=_) => {
      let kind = fmt.list_comprehension_kind(kind)
      let guard_doc = match guard_ {
        None => empty
        Some(expr) =>
          space + text("if") + space + fmt.compose(expr, Infix(Other))
      }
      let body = space + text("=>") + space + fmt.compose(body, Expr)
      let inner = kind + guard_doc + body
      let doc = if is_iter {
        text("[|") + space + inner + space + text("|]")
      } else {
        brackets(inner)
      }
      Normal(group(doc))
    }
    ArrayGet(array~, index~, loc=_) => {
      let array = fmt.compose(array, Simple)
      let index = fmt.compose(index, Expr)
      let doc = group(array + group(brackets(index)))
      Normal(doc)
    }
    ArrayGetSlice(array~, start_index~, end_index~, loc=_, index_loc=_) => {
      let array = fmt.compose(array, Simple)
      let start_index = optional(start_index, empty, e => fmt.compose(e, Expr))
      let end_index = optional(end_index, empty, e => fmt.compose(e, Expr))
      let doc = group(array + group(brackets(start_index + colon + end_index)))
      Normal(doc)
    }
    ArraySet(array~, index~, value~, loc=_) => {
      let op = equal
      let array = fmt.compose(array, Simple)
      let index = fmt.compose(index, Expr)
      let value = fmt.compose_assign_rhs(value, Expr) // TODO: compose_rhs
      let doc = group(
        array + group(brackets(index)) + space + op + space + value,
      )
      Normal(doc)
    }
    ArrayAugmentedSet(op~, array~, index~, value~, loc=_) => {
      let op = fmt.var_(op)
      let array = fmt.compose(array, Simple)
      let index = fmt.compose(index, Expr)
      let value = fmt.compose_assign_rhs(value, Expr) // TODO: compose_rhs
      let doc = group(
        array + group(brackets(index)) + space + op + space + value,
      )
      Normal(doc)
    }
    Constant(c~, loc=_) => {
      let doc = fmt.constant(c)
      Normal(doc)
    }
    MultilineString(elems~, loc~) => {
      let doc = fmt.multiline_string(parens=true, loc~, elems)
      Normal(doc)
    }
    Interp(elems~, loc=_) => {
      fn elem2doc(elem : @syntax.InterpElem) -> Doc {
        match elem {
          Literal(repr~, loc=_) => text(repr)
          Expr(expr~, loc=_) =>
            text("\\{") + fmt.compose(expr, Expr) + text("}")
          Source({ source, loc: _ }) => text("\\{") + text(source) + text("}")
        }
      }

      let elems = concat_map(elems, elem2doc)
      let doc = double_quoted(elems)
      Normal(doc)
    }
    BytesInterp(elems~, loc=_) => {
      fn elem2doc(elem : @syntax.InterpElem) -> Doc {
        match elem {
          Literal(repr~, loc=_) => text(repr)
          Expr(expr~, loc=_) =>
            text("\\{") + fmt.compose(expr, Expr) + text("}")
          Source({ source, loc: _ }) => text("\\{") + text(source) + text("}")
        }
      }

      let elems = concat_map(elems, elem2doc)
      let doc = text("b") + double_quoted(elems)
      Normal(doc)
    }
    Constraint(expr~, ty~, loc=_) => {
      let expr = fmt.compose(expr, Expr)
      let ty = fmt.ty(ty)
      let doc = group(parens(expr + line + colon + space + ty))
      BlockLike(doc)
    }
    Constr(constr~, loc=_) => {
      let doc = fmt.constr(constr)
      Normal(doc)
    }
    While(loop_cond~, loop_body~, while_else~, label~, loc=_) => {
      let label = match label {
        None => empty
        Some(x) => fmt.label(x) + colon
      }
      let while_ = text("while")
      let cond = space + fmt.compose(loop_cond, Infix(Other))
      let body = space + fmt.expr_block(loop_body)
      let else_ = match while_else {
        None => empty
        Some(e) => space + text("nobreak") + space + fmt.expr_block(e)
      }
      let doc = while_ + label + cond + body + else_
      Normal(doc)
    }
    Function(func~, loc=_) =>
      match func.kind {
        Arrow => fmt.arrow_function(parameters=func.parameters, body=func.body)
        Lambda => fmt.func(func)
      }
    Ident(id~, loc~) => {
      let before_comment = fmt.before_comment(Some(Node(loc)))
      let doc = fmt.var_(id)
      let after_comment = fmt.after_comment(Some(Node(loc)))
      Normal(before_comment + doc + after_comment)
    }
    If(cond~, ifso~, ifnot~, loc=_) => {
      let if_ = text("if")
      let cond = space + fmt.compose(cond, Infix(Other))
      let ifso = space +
        fmt.expr_block(ifso, force_newline=false, grouped=false)
      let ifnot = match ifnot {
        None => empty
        Some(If(_) as e) => space + text("else") + space + fmt.compose(e, Stmt)
        Some(e) =>
          space +
          text("else") +
          space +
          fmt.expr_block(e, force_newline=false, grouped=false)
      }
      Collapsible(if_ + cond, ifso + ifnot)
    }
    Guard(cond~, otherwise~, body~, loc=_) => {
      let guard_ = text("guard")
      let cond = space + fmt.compose(cond, Infix(Other))
      let otherwise = match otherwise {
        None => empty
        Some(e) => space + text("else") + space + fmt.expr_block(e)
      }
      let body = hardline + fmt.compose(body, Stmt)
      let doc = guard_ + cond + otherwise + body
      Normal(doc)
    }
    Is(expr~, pat~, loc=_) => {
      // The right-hand side of the `is` expression is a simple pattern.
      // Parentheses should be added around complex patterns.
      let expr = fmt.compose(expr, Expr)
      let is_ = space + text("is")
      let pat = match pat {
        Constant(_)
        | Any(_)
        | Var(_)
        | Constr(_)
        | SpecialConstr(_)
        | Tuple(_)
        | Constraint(_)
        | Array(_)
        | Record(_)
        | Map(_) => fmt.pattern(pat)
        Range(_) | Or(_) | Alias(_) => parens(fmt.pattern(pat))
      }
      let pat = space + pat
      let doc = group(expr + is_ + pat)
      Normal(doc)
    }
    RegexMatch(expr~, pat~, bindings~, loc=_) => {
      fn binding2doc(pair : (@syntax.Label, @syntax.Binder?)) -> Doc {
        let (label, binder) = pair
        match binder {
          None => fmt.label(label)
          Some(binder) => fmt.label(label) + equal + fmt.binder(binder)
        }
      }

      let expr = fmt.compose(expr, Infix(Other))
      let pat = fmt.regex_pattern(pat)
      let pat = if bindings.is_empty() {
        pat
      } else {
        group(parens(pat + comma + line + series(bindings, comma, binding2doc)))
      }
      Normal(group(expr + space + text("=~") + space + pat))
    }
    Defer(expr~, body~, loc=_) => {
      let defer_ = text("defer")
      let expr = space + fmt.compose(expr, Pipeline) // TODO: compose_rhs
      let body = hardline + fmt.compose(body, Stmt)
      let doc = defer_ + expr + body
      Normal(doc)
    }
    LetFn(name=_, func~, body~, loc=_) => { //TODO: fixme
      guard fmt.func(func) is Collapsible(a, b)
      let func = a + b
      let body = hardline + fmt.compose(body, Stmt)
      Normal(func + body)
    }
    LetAnd(bindings~, body~, loc=_) => {
      fn rec_binding2doc(
        triple : (@syntax.Binder, @syntax.Type?, @syntax.Func),
      ) {
        let (binder, ty, func) = triple
        let binder = fmt.binder(binder)
        let ty = match ty {
          None => empty
          Some(ty) => space + colon + space + fmt.ty(ty)
        }
        let { parameters, body, .. } = func
        let params = space +
          group(
            fmt.parameters(
              parameters,
              lparen=None,
              param_tables=@comment.NodeTableArray::empty(),
              commas=@comment.NodeArray::empty(),
              rparen=None,
            ),
          )
        let body = space + fmt.expr_block(body)
        binder + ty + space + equal + params + text("=>") + body
      }

      let letrec_ = text("letrec")
      let bindings = space +
        separate_map(bindings, hardline + text("and") + space, rec_binding2doc)
      let body = hardline + fmt.compose(body, Stmt)
      let doc = letrec_ + bindings + body
      Normal(doc)
    }
    Let(pattern~, expr~, body~, loc=_) => {
      let lhs = if pattern is Constraint(pat~, ty~, loc=_) {
        let pattern = fmt.pattern(pat)
        let ty = fmt.ty(ty)
        pattern + space + colon + ty
      } else {
        fmt.pattern(pattern)
      }
      let expr = fmt.compose_assign_rhs(expr, Expr) //TODO: compose_rhs
      let doc = group(text("let") + space + lhs + space + equal + expr) +
        hardline +
        fmt.compose(body, Stmt)
      Normal(doc)
    }
    Sequence(exprs~, last_expr~, loc~) => {
      let before_comment = fmt.before_comment(Some(Node(loc)))
      let exprs = separate(exprs.map(e => fmt.compose(e, ExprStmt)), hardline)
      let last_expr = hardline + fmt.compose(last_expr, Stmt)
      let after_comment = fmt.after_comment(Some(Node(loc)))
      let doc = before_comment + exprs + last_expr + after_comment
      Normal(doc)
    }
    Tuple(exprs~, loc=_) => {
      let doc = group(parens(series(exprs, comma, e => fmt.compose(e, Expr))))
      Normal(doc)
    }
    Record(type_name~, fields~, trailing=_, loc=_) =>
      // Bad case: if type_name is empty and the record is initialized with 
      // one punning field, a comma should be added for disambiguation.
      //
      // ```
      // { ident }           // block with one expression
      // { field, }          // record with one field
      // TypeName::{ field } 
      // ```
      if type_name is None {
        let record = group(
          if fields is More(_, tail=Empty) {
            braces(
              series1(
                fields.map(x => fmt.field(x)),
                comma,
                trailing=AlwaysPresent,
              ),
            )
          } else {
            braces(series1(fields.map(x => fmt.field(x)), comma))
          },
        )
        BlockLike(record)
      } else {
        let type_name = match type_name {
          None => empty
          Some(type_name) => fmt.type_name(type_name) + text("::")
        }
        let record = group(
          braces(series1(fields.map(x => fmt.field(x)), comma)),
        )
        Collapsible(type_name, record)
      }
    RecordUpdate(type_name~, record~, fields~, loc=_) => {
      let type_name = match type_name {
        None => empty
        Some(x) => fmt.type_name(x)
      }
      let dotdot_record = text("..") + fmt.compose(record, Simple)
      let fields = fields.map(x => fmt.field(x))
      let record = group(
        braces(series(fields.add(dotdot_record), comma, x => x)),
      )
      let doc = group(type_name + text("::") + record)
      Normal(doc)
    }
    Method(type_name~, method_name~, loc=_) => {
      let type_name = fmt.type_name(type_name)
      let method_name = fmt.label(method_name)
      let doc = type_name + text("::") + method_name
      Normal(doc)
    }
    Field(record=_, accessor=_, loc=_) as e => fmt.chained_dot_expr(e)
    DotApply(self=_, method_name=_, args=_, return_self=_, loc=_) as e =>
      fmt.chained_dot_expr(e)
    As(expr~, trait_~, loc=_) => {
      let expr = fmt.compose(expr, Simple)
      let as_ = space + text("as")
      let trait_ = space + fmt.type_name(trait_)
      let doc = group(expr + as_ + trait_)
      Normal(doc)
    }
    Mutate(record~, accessor~, field~, augmented_by~, loc=_) => {
      let record = fmt.expr_before_dot(record)
      let accessor = fmt.accessor(accessor)
      let op = match augmented_by {
        None => space + text("=")
        Some(op) => space + fmt.var_(op)
      }
      let field = space + fmt.compose(field, Expr) //TODO: compose_rhs
      let doc = record + accessor + op + field
      Normal(doc)
    }
    Match(expr~, cases~, match_loc=_, loc=_) => {
      let match_ = text("match")
      let cond = space + fmt.compose(expr, Infix(Other))
      let cases = space +
        group(
          braces(
            series1(cases.map(x => fmt.case(x)), empty),
            force_newline=true,
          ),
        )
      Collapsible(match_ + cond, cases)
    }
    LetMut(binder~, ty~, expr~, body~, loc=_) => {
      let let_ = text("let")
      let binder = space + fmt.binder(binder)
      let ty = match ty {
        None => empty
        Some(ty) => space + fmt.ty(ty)
      }
      let assign = space + equal
      // TODO: handle multiline string
      let expr = space + fmt.compose(expr, Expr) // TODO: compose_rhs
      let body = hardline + fmt.compose(body, Stmt)
      let doc = let_ + binder + ty + assign + expr + body
      Normal(doc)
    }
    Pipe(lhs~, rhs~, loc=_) => {
      fn pipe2doc(lhs : @syntax.Expr, rhs) {
        let lhs = match lhs {
          Pipe(lhs~, rhs~, loc=_) => pipe2doc(lhs, rhs)
          _ => fmt.compose(lhs, Simple)
        }
        let pipe = line + text("|>")
        let rhs = space + fmt.compose(rhs, Infix(Other))
        lhs + pipe + rhs
      }

      Normal(group(pipe2doc(lhs, rhs)))
    }
    RevPipe(lhs~, rhs~, loc=_) => {
      let lhs = fmt.compose(lhs, Infix(Other))
      let rhs = fmt.compose(rhs, Pipeline)
      Normal(group(lhs + space + text("<|") + space + rhs))
    }
    Assign(var_~, expr~, augmented_by~, loc=_) => {
      let var_ = fmt.var_(var_)
      let op = match augmented_by {
        None => space + text("=")
        Some(op) => space + fmt.var_(op)
      }
      let expr = fmt.compose_assign_rhs(expr, Expr) //TODO: compose_rhs
      let doc = var_ + op + expr
      Normal(doc)
    }
    Hole(loc=_, kind=hole) => {
      let doc = match hole {
        Incomplete => text("_")
        Synthesized => text("_") //TODO: sus
        Todo => text("...")
      }
      Normal(doc)
    }
    Return(return_value~, loc=_) => {
      let return_ = text("return")
      let value = space +
        optional(return_value, empty, e => {
          fmt.compose_return_break_continue_rhs(e, Expr)
        }) //TODO: compose_rhs
      let doc = return_ + value
      Normal(doc)
    }
    Raise(err_value~, loc=_) => {
      let raise_ = text("raise")
      let value = space + fmt.compose_assign_rhs(err_value, Expr) //TODO: compose_rhs
      let doc = raise_ + value
      Normal(doc)
    }
    Quantifier(kind~, binder~, binder_ty~, body~, loc=_) => {
      let quantifier = match kind {
        Forall => text("∀")
        Exists => text("∃")
      }
      let doc = quantifier +
        space +
        fmt.binder(binder) +
        space +
        colon +
        space +
        fmt.ty(binder_ty) +
        comma +
        space +
        fmt.compose(body, Expr)
      Normal(group(doc))
    }
    Implies(lhs~, rhs~, loc=_) => {
      let lhs = fmt.compose(lhs, Infix(Other))
      let rhs = fmt.compose(rhs, Infix(Other))
      Normal(group(lhs + space + text("→") + space + rhs))
    }
    ProofAssert(expr~, loc=_) => {
      let expr = space + fmt.compose(expr, Expr)
      Normal(text("proof_assert") + expr)
    }
    ProofLet(binder~, expr~, loc=_) => {
      let expr = space + equal + space + fmt.compose(expr, Expr)
      Normal(text("proof_let") + space + fmt.binder(binder) + expr)
    }
    Unit(faked~, loc=_) => {
      let doc = if faked { empty } else { text("()") }
      Normal(doc)
    }
    Break(arg~, label~, loc=_) => {
      let break_ = text("break")
      let label = fmt.loop_label(label, trailing_colon=false)
      let arg = space + optional(arg, empty, e => fmt.compose(e, Expr)) //TODO: compose_rhs
      let doc = break_ + label + arg
      Normal(doc)
    }
    Continue(args~, label~, loc=_) => {
      let continue_ = text("continue")
      let label = fmt.loop_label(label, trailing_colon=false)
      let args = if args.is_empty() {
        empty
      } else {
        space + series(args, comma, e => fmt.compose(e, Expr))
      }
      let doc = continue_ + label + args
      Normal(doc)
    }
    For(
      binders~,
      condition~,
      continue_block~,
      body~,
      for_else~,
      where_clause~,
      label~,
      ..
    ) => {
      fn binder_expr2doc(pair : (@syntax.Binder, @syntax.Expr)) -> Doc {
        let (binder, expr) = pair
        let binder = fmt.binder(binder)
        let assign = space + equal
        let expr = space + fmt.compose(expr, Infix(Other))
        binder + assign + expr
      }

      let label = fmt.loop_label(label, trailing_colon=true)
      let for_ = text("for")
      let binders_doc = space +
        series(binders, comma, binder_expr2doc, trailing=AlwaysHidden)
      let condition = match condition {
        None => None
        Some(e) => Some(fmt.compose(e, Infix(Other)))
      }
      let continue_block = match continue_block {
        Empty => None
        More(_) =>
          Some(
            series(
              continue_block,
              comma,
              binder_expr2doc,
              trailing=AlwaysHidden,
            ),
          )
      }
      let body = space + fmt.expr_block(body)
      let for_else = match for_else {
        None => empty
        Some(e) => space + text("nobreak") + space + fmt.expr_block(e)
      }
      let semi_or_newline = switch(semi + space, softline)
      let header = match (condition, continue_block) {
        (None, None) =>
          if binders.is_empty() {
            empty
          } else {
            space + binders_doc
          }
        (_, _) =>
          binders_doc +
          semi_or_newline +
          condition.unwrap_or(empty) +
          semi_or_newline +
          continue_block.unwrap_or(empty)
      }
      let where_ = fmt.where_clause(where_clause)
      let doc = label +
        for_ +
        group(nest(softline + header)) +
        body +
        for_else +
        where_
      Normal(doc)
    }
    ForEach(
      binders~,
      expr~,
      init~,
      continue_block~,
      body~,
      else_block~,
      where_clause~,
      label~,
      loc=_
    ) => {
      let label = fmt.loop_label(label, trailing_colon=true)
      let for_ = text("for")
      fn foreach_binder2doc(binder : @syntax.Binder?) -> Doc {
        match binder {
          None => empty
          Some(x) => fmt.binder(x)
        }
      }
      fn binder_expr2doc(pair : (@syntax.Binder, @syntax.Expr)) -> Doc {
        let (binder, expr) = pair
        fmt.binder(binder) +
        space +
        equal +
        space +
        fmt.compose(expr, Infix(Other))
      }

      let binders = space +
        group(series(binders, comma, foreach_binder2doc, trailing=AlwaysHidden))
      let in_ = space + text("in")
      let expr = space + fmt.compose(expr, Infix(Other))
      let semi_or_newline = switch(semi + space, softline)
      let init = match init {
        Empty => empty
        More(_) =>
          semi_or_newline +
          series(init, comma, binder_expr2doc, trailing=AlwaysHidden)
      }
      let continue_block = match continue_block {
        Empty => empty
        More(_) =>
          semi_or_newline +
          series(continue_block, comma, binder_expr2doc, trailing=AlwaysHidden)
      }
      let body = space + fmt.expr_block(body)
      let else_block = match else_block {
        None => empty
        Some(e) => space + text("nobreak") + space + fmt.expr_block(e)
      }
      let where_ = fmt.where_clause(where_clause)
      let header = group(for_ + binders + in_ + expr + init + continue_block)
      let doc = label + header + body + else_block + where_
      Normal(doc)
    }
    Try(
      body~,
      catch_=catch_cases,
      try_else~,
      has_try=_,
      try_loc=_,
      catch_loc=_,
      else_loc=_,
      loc=_
    ) => {
      let try_ = text("try")
      let body = space + fmt.expr_block(body)
      let catch_ = space + text("catch") + fmt.case_block(catch_cases)
      let noraise_ = match try_else {
        None => empty
        Some(cases) => space + text("noraise") + space + fmt.case_block(cases)
      }
      let doc = try_ + body + catch_ + noraise_
      Normal(doc)
    }
    TryOperator(body~, kind=op, try_loc=_, loc=_) => {
      let try_ = match op {
        Question => text("try?")
        Exclamation => text("try!")
      }
      let body = space + fmt.compose(body, Simple)
      let doc = try_ + body
      Normal(doc)
    }
    Map(elems~, loc=_) => {
      fn map_expr_elem2doc(elem : @syntax.MapExprElem) -> Doc {
        let { key, expr, key_loc: _, loc: _ } = elem
        fmt.constant(key) + colon + space + fmt.compose(expr, Expr)
      }

      let doc = group(braces(series(elems, comma, map_expr_elem2doc)))
      Normal(doc)
    }
    TemplateWriting(expr~, template~, is_conditional~, loc=_) => {
      let expr = fmt.compose(expr, Infix(Other))
      let template = fmt.compose(template, Infix(Other))
      let op = if is_conditional { "<?" } else { "<+" }
      Normal(group(expr + space + text(op) + space + template))
    }
    LexMatch(strategy~, expr~, match_loc=_, cases~, loc=_) => {
      let lexmatch_ = text("lexmatch")
      let cond = space + fmt.compose(expr, Infix(Other))
      let strategy = fmt.lex_strategy(strategy)
      let cases = space +
        group(
          braces(
            series1(cases.map(x => fmt.lex_case(x)), empty),
            force_newline=true,
          ),
        )
      Collapsible(lexmatch_ + cond + strategy, cases)
    }
    LexScan(expr~, match_loc=_, cases~, loc=_) => {
      let lexscan_ = text("lexscan")
      let cond = space + fmt.compose(expr, Infix(Other))
      let cases = space +
        group(
          braces(
            series1(cases.map(x => fmt.lex_scan_case(x)), empty),
            force_newline=true,
          ),
        )
      Collapsible(lexscan_ + cond, cases)
    }
    Group(expr~, ..) => abort(expr.to_json().stringify(indent=2))
    StaticAssert(_) => panic()
  }
}
