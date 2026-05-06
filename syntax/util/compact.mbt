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
    loc~ : Location,
    let_loc~ : Location?,
    equal_loc~ : Location?,
    semi_loc~ : Location?
  )
  /// guard statement
  ///
  /// ```skip
  /// guard cond else { otherwise }
  /// ```
  StmtGuard(
    cond~ : Expr,
    otherwise~ : Expr?,
    loc~ : Location,
    guard_loc~ : Location?,
    else_loc~ : Location?,
    else_lbrace_loc~ : Location?,
    else_rbrace_loc~ : Location?,
    else_semi_locs~ : @list.List[Location],
    else_trailing_semi_loc~ : Location?,
    semi_loc~ : Location?
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
    loc~ : Location,
    let_loc~ : Location?,
    mut_loc~ : Location?,
    equal_loc~ : Location?,
    semi_loc~ : Location?
  )
  /// local function
  /// 
  /// ```skip
  /// fn local() { ... }
  /// ```
  StmtFunc(
    binder~ : Binder,
    func~ : Func,
    loc~ : Location,
    fn_loc~ : Location?,
    semi_loc~ : Location?
  )
  /// letand statement
  StmtLetand(
    bindings~ : @list.List[(Binder, Type?, Func)],
    loc~ : Location,
    letrec_loc~ : Location?,
    semi_loc~ : Location?
  )
  /// defer statement
  StmtDefer(
    expr~ : Expr,
    loc~ : Location,
    defer_loc~ : Location?,
    semi_loc~ : Location?
  )
}

///|
pub fn Statement::with_semi_loc(
  self : Statement,
  semi_loc : Location?,
) -> Statement {
  match self {
    StmtExpr(expr~, semi_loc=_) => StmtExpr(expr~, semi_loc~)
    StmtLet(pat~, expr~, loc~, let_loc~, equal_loc~, semi_loc=_) =>
      StmtLet(pat~, expr~, loc~, let_loc~, equal_loc~, semi_loc~)
    StmtGuard(
      cond~,
      otherwise~,
      loc~,
      guard_loc~,
      else_loc~,
      else_lbrace_loc~,
      else_rbrace_loc~,
      else_semi_locs~,
      else_trailing_semi_loc~,
      semi_loc=_
    ) =>
      StmtGuard(
        cond~,
        otherwise~,
        loc~,
        guard_loc~,
        else_loc~,
        else_lbrace_loc~,
        else_rbrace_loc~,
        else_semi_locs~,
        else_trailing_semi_loc~,
        semi_loc~,
      )
    StmtLetmut(
      binder~,
      ty_opt~,
      expr~,
      loc~,
      let_loc~,
      mut_loc~,
      equal_loc~,
      semi_loc=_
    ) =>
      StmtLetmut(
        binder~,
        ty_opt~,
        expr~,
        loc~,
        let_loc~,
        mut_loc~,
        equal_loc~,
        semi_loc~,
      )
    StmtFunc(binder~, func~, loc~, fn_loc~, semi_loc=_) =>
      StmtFunc(binder~, func~, loc~, fn_loc~, semi_loc~)
    StmtLetand(bindings~, loc~, letrec_loc~, semi_loc=_) =>
      StmtLetand(bindings~, loc~, letrec_loc~, semi_loc~)
    StmtDefer(expr~, loc~, defer_loc~, semi_loc=_) =>
      StmtDefer(expr~, loc~, defer_loc~, semi_loc~)
  }
}

///|
pub fn Statement::loc(self : Statement) -> Location {
  match self {
    StmtExpr(expr~, ..) => expr.loc()
    StmtLet(loc~, ..)
    | StmtGuard(loc~, ..)
    | StmtLetmut(loc~, ..)
    | StmtFunc(loc~, ..)
    | StmtLetand(loc~, ..)
    | StmtDefer(loc~, ..) => loc
  }
}

///|
pub fn Statement::attach_semi_locs(
  stmts : @list.List[Statement],
  semi_locs : @list.List[Location],
  trailing_semi_loc : Location?,
) -> @list.List[Statement] {
  match stmts {
    Empty => @list.empty()
    More(stmt, tail=Empty) =>
      @list.singleton(stmt.with_semi_loc(trailing_semi_loc))
    More(stmt, tail=rest) =>
      match semi_locs {
        More(semi_loc, tail=semi_tail) =>
          @list.cons(
            stmt.with_semi_loc(Some(semi_loc)),
            Statement::attach_semi_locs(rest, semi_tail, trailing_semi_loc),
          )
        Empty =>
          @list.cons(
            stmt.with_semi_loc(None),
            Statement::attach_semi_locs(rest, @list.empty(), trailing_semi_loc),
          )
      }
  }
}

///|
pub fn Statement::compact_rev(
  ls : @list.List[Statement],
  loc~ : Location,
) -> Expr {
  match ls {
    // Preserve the historical AST shape for a trailing expression statement.
    // The trailing `;` is carried by the surrounding block owner instead of
    // forcing `expr;` into `Sequence([expr], Unit)` in JSON-visible AST.
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
        More(StmtExpr(expr~, ..), ..) => expr.loc()
      }
      Statement::collect_rev(rest, Expr::Unit(loc~, faked=true))
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
        StmtFunc(binder~, func~, loc~, fn_loc~, semi_loc~) => {
          let loc = loc.merge(loc_of_expression(cont))
          Statement::collect_rev(
            xs,
            Expr::LetFn(name=binder, func~, loc~, body=cont, fn_loc~, semi_loc~),
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
            StmtLet(pat~, expr~, loc~, let_loc~, equal_loc~, semi_loc~) => {
              let loc = loc.merge(cont.loc())
              Expr::Let(
                pattern=pat,
                expr~,
                loc~,
                body=cont,
                let_loc~,
                equal_loc~,
                semi_loc~,
              )
            }
            StmtGuard(
              cond~,
              otherwise~,
              loc~,
              guard_loc~,
              else_loc~,
              else_lbrace_loc~,
              else_rbrace_loc~,
              else_semi_locs~,
              else_trailing_semi_loc~,
              semi_loc~
            ) => {
              let loc = loc.merge(cont.loc())
              Expr::Guard(
                cond~,
                otherwise~,
                loc~,
                body=cont,
                guard_loc~,
                else_loc~,
                else_lbrace_loc~,
                else_rbrace_loc~,
                else_semi_locs~,
                else_trailing_semi_loc~,
                semi_loc~,
              )
            }
            StmtLetmut(
              binder~,
              ty_opt~,
              expr~,
              loc~,
              let_loc~,
              mut_loc~,
              equal_loc~,
              semi_loc~
            ) => {
              let loc = loc.merge(cont.loc())
              Expr::LetMut(
                binder~,
                ty=ty_opt,
                expr~,
                loc~,
                body=cont,
                let_loc~,
                mut_loc~,
                equal_loc~,
                semi_loc~,
              )
            }
            StmtLetand(bindings~, loc~, letrec_loc~, semi_loc~) => {
              let loc = loc.merge(loc_of_expression(cont))
              Expr::LetAnd(bindings~, loc~, body=cont, letrec_loc~, semi_loc~)
            }
            StmtDefer(expr~, loc~, defer_loc~, semi_loc~) => {
              let loc = loc.merge(loc_of_expression(cont))
              Expr::Defer(expr~, loc~, body=cont, defer_loc~, semi_loc~)
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
  semi_acc : @list.List[Location],
  todo : @list.List[Statement],
  cont : Expr,
) -> Expr {
  match todo {
    More(StmtExpr(expr~, semi_loc~), tail=rest) => {
      let loc = loc.merge(expr.loc())
      let semi_acc = match semi_loc {
        Some(semi_loc) => semi_acc.add(semi_loc)
        None => semi_acc
      }
      Statement::collect_sequence(loc~, acc.add(expr), semi_acc, rest, cont)
    }
    _ =>
      Statement::collect_rev(
        todo,
        Expr::Sequence(exprs=acc, last_expr=cont, loc~, semi_locs=semi_acc),
      )
  }
}
