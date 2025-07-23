///|
priv type AstLintEnv Unit

///|
impl ImplMapVisitor for AstLintEnv with visit_array_get(
  env,
  array~,
  index~,
  loc~,
) {
  let array = env.visit_expr(array)
  let index = env.visit_expr(index)
  desugar_array_get(array, index, loc~)
}

///|
impl ImplMapVisitor for AstLintEnv with visit_array_set(
  env,
  array~,
  index~,
  value~,
  loc~,
) {
  let array = env.visit_expr(array)
  let index = env.visit_expr(index)
  let value = env.visit_expr(value)
  desugar_array_set(array, index, value, loc~)
}

///|
impl ImplMapVisitor for AstLintEnv with visit_array_augmented_set(
  env,
  op~,
  array~,
  index~,
  value~,
  loc~,
) {
  let array = env.visit_expr(array)
  let index = env.visit_expr(index)
  let value = env.visit_expr(value)
  desugar_array_augmented_set(loc, op, array, index, value)
}

///|
impl ImplMapVisitor for AstLintEnv with visit_group(env, expr~, group~, loc~) {
  ignore(group)
  ignore(loc)
  MapVisitorBase(env).visit_expr(expr)
}

///|
impl ImplMapVisitor for AstLintEnv with visit_unary(env, op~, expr~, loc~) {
  match op.name {
    Ident(name="-") => {
      let id = "Neg"
      let pkg = @basic.builtin_package
      let method_name = "op_neg"
      let trait_name : LongIdent = if pkg == @basic.current_package.val {
        Ident(name=id)
      } else {
        Dot(pkg~, id~)
      }
      let method_expr : Expr = Method(
        type_name=TypeName::{
          name: trait_name,
          is_object: false,
          loc: no_location,
        },
        method_name=Label::{ name: method_name, loc: op.loc },
        loc~,
      )
      Expr::Apply(
        loc~,
        func=method_expr,
        args=@list.from_array([
          Argument::{ value: env.visit_expr(expr), kind: Positional },
        ]),
        attr=NoAttr,
      )
    }
    Ident(name="!") => Expr::Unary(op~, expr=env.visit_expr(expr), loc~)
    _ => panic()
  }
}

///|
impl ImplMapVisitor for AstLintEnv with visit_interp(env, elems~, loc~) {
  let AstLintEnv(_) = env
  // TODO: parse interp
  Expr::Interp(elems~, loc~)
}

///|
impl ImplMapVisitor for AstLintEnv with visit_multiline_string(_, elems~, loc~) {
  // TODO: parse interp in multiline string
  Expr::MultilineString(elems~, loc~)
}

///|
fn test_to_func(
  i : Int,
  expr : Expr,
  params : Parameters?,
  local_types : @list.T[LocalTypeDecl],
  loc : Location,
) -> Impl {
  let test_name = "__test_\{@basic.encode_base16(loc.start.fname)}_\{i}"
  let unit_type = Type::Name(
    constr_id=ConstrId::{ id: Ident(name="Unit"), loc: no_location },
    tys=@list.empty(),
    loc=no_location,
  )
  let string_type = Type::Name(
    constr_id=ConstrId::{ id: Ident(name="Error"), loc: no_location },
    tys=@list.empty(),
    loc=no_location,
  )
  let params = match params {
    None => Some(@list.empty())
    Some(_) => params // no check!
  }
  TopFuncDef(
    fun_decl=FunDecl::{
      type_name: None,
      has_error: None,
      is_async: false,
      name: Binder::{ name: test_name, loc: no_location },
      decl_params: params,
      params_loc: no_location,
      quantifiers: @list.empty(),
      return_type: Some(unit_type),
      error_type: ErrorType::ErrorType(ty=string_type),
      vis: Default,
      attrs: @list.empty(),
      doc: DocString::empty(),
    },
    decl_body=DeclBody(expr~, local_types~),
    loc~,
  )
}

///|
pub fn post_process(impls : Impls) -> Impls {
  let env = AstLintEnv(())
  fn go(i : Int, impls : Impls) -> Impls {
    match impls {
      Empty => @list.empty()
      More(impl_, tail=rest) =>
        match env.visit_impl(impl_) {
          TopTest(expr~, params~, local_types~, loc~, ..) => {
            let impl_ = test_to_func(i, expr, params, local_types, loc)
            go(i + 1, rest).add(impl_)
          }
          TopFuncDef(
            fun_decl={ name: { name: "init", .. }, .. },
            decl_body=DeclBody(expr~, local_types~),
            loc~
          ) => {
            let impl_ = TopExpr(expr~, is_main=false, local_types~, loc~)
            go(i, rest).add(impl_)
          }
          TopFuncDef(
            fun_decl={ name: { name: "main", .. }, type_name: None, attrs, .. },
            decl_body=DeclBody(expr~, local_types~),
            loc~
          ) => {
            let is_test_main_attribute = fn(attr : Attribute) {
              match attr {
                { parsed: Some(Ident({ qual: None, name: "test_entry" })), .. } =>
                  true
                { parsed: None, .. } => false // TODO: replace with panic() after complete attribute_parser
                _ => false
              }
            }
            if @basic.test_mode.val && !attrs.any(is_test_main_attribute) {
              go(i, rest)
            } else {
              go(i, rest).add(TopExpr(expr~, is_main=true, local_types~, loc~))
            }
          }
          TopTypeDef(decl) as top => {
            let external = decl.attrs.find(fn(attr : Attribute) {
              match attr.parsed {
                Some(Ident({ qual: None, name: "external" })) => true
                _ => false
              }
            })
            if external is Some(_) {
              let decl = match decl.components {
                Abstract | Extern => { ..decl, components: Extern }
                _ => decl
              }
              go(i, rest).add(TopTypeDef(decl))
            } else {
              go(i, rest).add(top)
            }
          }
          _ => go(i, rest).add(impl_)
        }
    }
  }

  let impls = go(0, impls)
  fn filter_impls_by_cfg(impls : Impls) -> Impls {
    impls
    .rev()
    .fold(init=@list.empty(), fn(acc, impl_) {
      let keep = match impl_ {
        TopExpr(_) | TopTest(_) => true
        TopTypeDef({ attrs, .. })
        | TopFuncDef(fun_decl={ attrs, .. }, ..)
        | TopFuncAlias(attrs~, ..)
        | TopLetDef(attrs~, ..)
        | TopTrait({ attrs, .. })
        | TopBatchTypeAlias(attrs~, ..)
        | TopBatchTraitAlias(attrs~, ..)
        | TopImpl(attrs~, ..)
        | TopView(attrs~, ..)
        | TopImplRelation(attrs~, ..) =>
          attrs.all(fn(attr) {
            match attr {
              {
                parsed: Some(
                  Apply({ qual: None, name: "cfg" }, More(prop, tail=Empty))
                ),
                ..,
              } => {
                ignore(prop)
                true // TODO: implement Eval_cfg
              }
              _ => true
            }
          })
      }
      if keep {
        acc.add(impl_)
      } else {
        acc
      }
    })
  }

  filter_impls_by_cfg(impls)
}
