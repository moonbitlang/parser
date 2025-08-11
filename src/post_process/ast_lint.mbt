///|
priv struct AstLintEnv(Unit)

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
  @syntax.MapVisitorBase(env).visit_expr(expr)
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
          loc: @syntax.no_location,
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
pub fn post_process(impls : Impls) -> Impls {
  let env = AstLintEnv(())
  fn go(impls : Impls) -> Impls {
    match impls {
      Empty => @list.empty()
      More(impl_, tail=rest) =>
        match env.visit_impl(impl_) {
          TopFuncDef(
            fun_decl={ name: { name: "init", .. }, .. },
            decl_body=DeclBody(expr~, local_types~),
            loc~
          ) => {
            let impl_ = Impl::TopExpr(expr~, is_main=false, local_types~, loc~)
            go(rest).add(impl_)
          }
          TopFuncDef(
            fun_decl={ name: { name: "main", .. }, type_name: None, .. },
            decl_body=DeclBody(expr~, local_types~),
            loc~
          ) => go(rest).add(TopExpr(expr~, is_main=true, local_types~, loc~))
          impl_ => go(rest).add(impl_)
        }
    }
  }

  go(impls)
}
