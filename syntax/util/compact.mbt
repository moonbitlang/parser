///|
/// The `Statement` type is not part of MoonBit's AST.
/// In fact, a statement in MoonBit is simply an incomplete expression. For example, consider a
/// block with a let statement:
///
/// ```skip
/// {
///   let _ = a
/// }
/// ```
///
/// The resulting AST is `Expr::Let(pattern=Any, expr=Ident("a"), body=Unit)`,
/// where the result value is the body. Since the body part is missing, we fill it with a `Unit`.
/// 
/// This type provides methods to convert an `Array[Statement]` into an `Expr`.
pub(all) enum Statement {
  /// Single expression
  StmtExpr(expr~ : Expr, semi_loc~ : Location?)
  /// let statement
  /// 
  /// ```skip
  /// let pat = expr
  /// ```
  StmtLet(
    pat~ : Pattern,
    expr~ : Expr,
    let_loc~ : Location,
    annot_colon_loc~ : Location?,
    equal_loc~ : Location,
    semi_loc~ : Location?,
    loc~ : Location
  )
  /// guard statement
  ///
  /// ```skip
  /// guard cond else { otherwise }
  /// ```
  StmtGuard(
    cond~ : Expr,
    otherwise~ : Expr?,
    guard_loc~ : Location,
    else_loc~ : Location?,
    else_lbrace_loc~ : Location?,
    else_rbrace_loc~ : Location?,
    semi_loc~ : Location?,
    loc~ : Location
  )
  /// let mut statement
  ///
  /// ```skip
  /// let mut binder : ty_opt = expr
  /// ```
  StmtLetmut(
    binder~ : Binder,
    ty_opt~ : Type?,
    expr~ : Expr,
    let_loc~ : Location,
    mut_loc~ : Location,
    annot_colon_loc~ : Location?,
    equal_loc~ : Location,
    semi_loc~ : Location?,
    loc~ : Location
  )
  /// local function
  /// 
  /// ```skip
  /// fn local() { ... }
  /// ```
  StmtFunc(
    binder~ : Binder,
    func~ : Func,
    semi_loc~ : Location?,
    loc~ : Location
  )
  /// letand statement
  StmtLetand(
    bindings~ : @list.List[(Binder, Type?, Func)],
    letrec_loc~ : Location,
    and_locs~ : @list.List[Location],
    equal_locs~ : @list.List[Location],
    annot_colon_locs~ : @list.List[Location],
    semi_loc~ : Location?,
    loc~ : Location
  )
  /// defer statement
  StmtDefer(
    expr~ : Expr,
    defer_loc~ : Location,
    semi_loc~ : Location?,
    loc~ : Location
  )
}

///|
pub fn Statement::with_semi_loc(
  self : Statement,
  semi_loc : Location?,
) -> Statement {
  match self {
    StmtExpr(expr~, ..) => StmtExpr(expr~, semi_loc~)
    StmtLet(
      pat~,
      expr~,
      let_loc~,
      annot_colon_loc~,
      equal_loc~,
      loc=stmt_loc,
      ..
    ) =>
      StmtLet(
        pat~,
        expr~,
        let_loc~,
        annot_colon_loc~,
        equal_loc~,
        semi_loc~,
        loc=stmt_loc,
      )
    StmtGuard(
      cond~,
      otherwise~,
      guard_loc~,
      else_loc~,
      else_lbrace_loc~,
      else_rbrace_loc~,
      loc=stmt_loc,
      ..
    ) =>
      StmtGuard(
        cond~,
        otherwise~,
        guard_loc~,
        else_loc~,
        else_lbrace_loc~,
        else_rbrace_loc~,
        semi_loc~,
        loc=stmt_loc,
      )
    StmtLetmut(
      binder~,
      ty_opt~,
      expr~,
      let_loc~,
      mut_loc~,
      annot_colon_loc~,
      equal_loc~,
      loc=stmt_loc,
      ..
    ) =>
      StmtLetmut(
        binder~,
        ty_opt~,
        expr~,
        let_loc~,
        mut_loc~,
        annot_colon_loc~,
        equal_loc~,
        semi_loc~,
        loc=stmt_loc,
      )
    StmtFunc(binder~, func~, loc=stmt_loc, ..) =>
      StmtFunc(binder~, func~, semi_loc~, loc=stmt_loc)
    StmtLetand(
      bindings~,
      letrec_loc~,
      and_locs~,
      equal_locs~,
      annot_colon_locs~,
      loc=stmt_loc,
      ..
    ) =>
      StmtLetand(
        bindings~,
        letrec_loc~,
        and_locs~,
        equal_locs~,
        annot_colon_locs~,
        semi_loc~,
        loc=stmt_loc,
      )
    StmtDefer(expr~, defer_loc~, loc=stmt_loc, ..) =>
      StmtDefer(expr~, defer_loc~, semi_loc~, loc=stmt_loc)
  }
}

///|
pub fn Statement::with_head_semi_loc_rev(
  statements : @list.List[Statement],
  semi_loc : Location?,
) -> @list.List[Statement] {
  match statements {
    Empty => @list.empty()
    More(statement, tail~) => tail.add(statement.with_semi_loc(semi_loc))
  }
}

///|
fn Statement::semi_loc(self : Statement) -> Location? {
  match self {
    StmtExpr(semi_loc~, ..)
    | StmtLet(semi_loc~, ..)
    | StmtGuard(semi_loc~, ..)
    | StmtLetmut(semi_loc~, ..)
    | StmtFunc(semi_loc~, ..)
    | StmtLetand(semi_loc~, ..)
    | StmtDefer(semi_loc~, ..) => semi_loc
  }
}

///|
pub fn Statement::semi_locs_rev(
  statements : @list.List[Statement],
) -> @list.List[Location] {
  fn go(
    todo : @list.List[Statement],
    acc : @list.List[Location],
  ) -> @list.List[Location] {
    match todo {
      Empty => acc
      More(statement, tail~) =>
        match statement.semi_loc() {
          Some(loc) => go(tail, acc.add(loc))
          None => go(tail, acc)
        }
    }
  }
  go(statements, @list.empty())
}

///|
pub fn Statement::compact_rev(
  ls : @list.List[Statement],
  loc~ : Location,
) -> Expr {
  match ls {
    More(StmtExpr(expr~, ..), tail=rest) => Statement::collect_rev(rest, expr)
    rest => {
      let loc = match rest {
        Empty => loc
        More(
          StmtLet(loc~, ..)
          | StmtLetmut(loc~, ..)
          | StmtFunc(loc~, ..)
          | StmtLetand(loc~, ..)
          | StmtDefer(loc~, ..)
          | StmtGuard(loc~, ..),
          ..
        ) => loc
        More(StmtExpr(..), ..) => panic()
      }
      Statement::collect_rev(
        rest,
        Expr::Unit(
          loc~,
          faked=true,
          lparen_loc=@syntax.no_location,
          rparen_loc=@syntax.no_location,
        ),
      )
    }
  }
}

///|
pub fn Statement::collect_rev(
  rest : @list.List[Statement],
  cont : Expr,
) -> Expr {
  match rest {
    Empty => cont
    More(x, tail=xs) =>
      match x {
        StmtFunc(binder~, func~, semi_loc~, loc~) => {
          let loc = loc.merge(loc_of_expression(cont))
          Statement::collect_rev(
            xs,
            Expr::LetFn(name=binder, func~, semi_loc~, loc~, body=cont),
          )
        }
        StmtExpr(..) =>
          Statement::collect_sequence(
            loc=loc_of_expression(cont),
            @list.empty(),
            @list.empty(),
            rest,
            cont,
          )
        _ => {
          let acc = match x {
            StmtLet(
              pat~,
              expr~,
              let_loc~,
              annot_colon_loc~,
              equal_loc~,
              semi_loc~,
              loc~
            ) => {
              let loc = loc.merge(cont.loc())
              Expr::Let(
                pattern=pat,
                expr~,
                let_loc~,
                annot_colon_loc~,
                equal_loc~,
                semi_loc~,
                loc~,
                body=cont,
              )
            }
            StmtGuard(
              cond~,
              otherwise~,
              guard_loc~,
              else_loc~,
              else_lbrace_loc~,
              else_rbrace_loc~,
              semi_loc~,
              loc~
            ) => {
              let loc = loc.merge(cont.loc())
              Expr::Guard(
                cond~,
                otherwise~,
                guard_loc~,
                else_loc~,
                else_lbrace_loc~,
                else_rbrace_loc~,
                semi_loc~,
                loc~,
                body=cont,
              )
            }
            StmtLetmut(
              binder~,
              ty_opt~,
              expr~,
              let_loc~,
              mut_loc~,
              annot_colon_loc~,
              equal_loc~,
              semi_loc~,
              loc~
            ) => {
              let loc = loc.merge(cont.loc())
              Expr::LetMut(
                binder~,
                ty=ty_opt,
                expr~,
                let_loc~,
                mut_loc~,
                annot_colon_loc~,
                equal_loc~,
                semi_loc~,
                loc~,
                body=cont,
              )
            }
            StmtLetand(
              bindings~,
              letrec_loc~,
              and_locs~,
              equal_locs~,
              annot_colon_locs~,
              semi_loc~,
              loc~
            ) => {
              let loc = loc.merge(loc_of_expression(cont))
              Expr::LetAnd(
                bindings~,
                letrec_loc~,
                and_locs~,
                equal_locs~,
                annot_colon_locs~,
                semi_loc~,
                loc~,
                body=cont,
              )
            }
            StmtDefer(expr~, defer_loc~, semi_loc~, loc~) => {
              let loc = loc.merge(loc_of_expression(cont))
              Expr::Defer(expr~, defer_loc~, semi_loc~, loc~, body=cont)
            }
            StmtExpr(..) | StmtFunc(..) => panic()
          }
          Statement::collect_rev(xs, acc)
        }
      }
  }
}

///|
fn Statement::collect_sequence(
  loc~ : Location,
  acc : @list.List[Expr],
  semi_locs : @list.List[Location],
  todo : @list.List[Statement],
  cont : Expr,
) -> Expr {
  match todo {
    More(StmtExpr(expr~, semi_loc~), tail=rest) => {
      let loc = loc.merge(expr.loc())
      let semi_locs = match semi_loc {
        Some(loc) => semi_locs.add(loc)
        None => semi_locs
      }
      Statement::collect_sequence(loc~, acc.add(expr), semi_locs, rest, cont)
    }
    _ =>
      Statement::collect_rev(
        todo,
        Expr::Sequence(exprs=acc, last_expr=cont, semi_locs~, loc~),
      )
  }
}
