///|
/// Represents the Doc of an expression that cannot be directly composed with 
/// other expression parts.
/// 
/// For example, in `(1 + 2) * 4`, when handling the left-hand side `(1 + 2)` 
/// with `Fmt::expr`, it returns a `RawExprDoc` representing `1 + 2`, without 
/// parentheses or braces included. Concatenating a `RawExprDoc` directly with 
/// the `* 4` part or using `Fmt::expr` would produce incorrect formatting.
/// Instead, use the `Fmt::compose` or `Fmt::compose_*` functions to combine 
/// expressions properly.
///
/// See design.md#compose-expression for more details.
priv enum RawExprDoc {
  /// Any syntatic structure that better to split into two part. 
  ///
  /// For example:
  /// - f(args)
  /// - if cond { a } else { b }
  Collapsible(Doc, Doc)
  /// { x }, [ x ], ( x ), (value : TypeAnnotation) etc. 
  BlockLike(Doc)
  /// Other case 
  Normal(Doc)
}

///|
fn RawExprDoc::to_doc(self : Self) -> Doc {
  match self {
    Normal(doc) | BlockLike(doc) => doc
    Collapsible(doc1, doc2) => doc1 + doc2
  }
}

///|
fn Fmt::compose(fmt : Self, expr : @syntax.Expr, ctx : Context) -> Doc {
  let kind = expr.classify()
  let doc = fmt.expr(expr).to_doc()
  match infer_parens(ctx, kind) {
    NoParen => doc
    NeedBraces => braces(doc)
    NeedParens => parens(doc)
    GroupedParens => group(parens(doc))
  }
}

///|
fn Fmt::compose_assign_rhs(
  fmt : Self,
  expr : @syntax.Expr,
  ctx : Context,
) -> Doc {
  let kind = expr.classify()
  let raw = fmt.expr(expr)
  let rhs = match infer_parens(ctx, kind) {
    NoParen =>
      match raw {
        Normal(doc) | BlockLike(doc) => doc
        Collapsible(head, body) => {
          let rhs_req = head.requirement() + body.requirement()
          let head_req = head.requirement()
          dynamic(ctx => {
            let current_remain = Requirement::Space(
              maximum(0, ctx.width - ctx.indent),
            )
            let next_remain = Requirement::Space(
              maximum(0, ctx.width - ctx.next_indent),
            )
            if current_remain >= rhs_req {
              // flatten style
              group(head + body)
            } else if current_remain >= head_req &&
              !(next_remain >= rhs_req + Space(2)) {
              // Composed style
              //
              // Bad case: 
              //
              // 1. To avoid following bad cases, if the head is not empty and 
              // rhs fits in next line, use dangling style
              //
              // ```
              // VeryLongPattern | VeryLongPattern | LongPattern | LongPattern => func(
              //   x,
              // ) 
              // VeryLongPattern | VeryLongPattern | LongPattern | LongPattern => 
              //   [a, b, c, d, e, f, g, h]
              // ```
              //
              // 2. To avoid following bad cases, if the rhs need more than one line, 
              // use normal style
              //
              // ```
              // match e {
              //   P => {
              //     long_expr
              //   }
              //   VeryLongPattern | VeryLongPattern | LongPattern => if condition {
              //     long_expr
              //   } else {
              //     long_expr
              //   }
              // }
              // ```
              group(head + body)
            } else if next_remain >= head_req + Space(2) {
              // dangling style
              nest(hardline + group(head + body))
            } else {
              // normal style
              group(braces(head + body, force_newline=true))
            }
          })
        }
      }
    NeedBraces => braces(raw.to_doc())
    NeedParens => parens(raw.to_doc())
    GroupedParens => group(parens(raw.to_doc()))
  }
  space + rhs
}

///|
fn Fmt::compose_return_break_continue_rhs(
  fmt : Self,
  expr : @syntax.Expr,
  ctx : Context,
) -> Doc {
  fmt.compose(expr, ctx)
}

///|
fn flatten_style(doc : Doc) -> Doc {
  group(doc)
}

///|
fn composed_style(doc : Doc) -> Doc {
  group(doc)
}

///|
fn dangling_style(doc : Doc) -> Doc {
  nest(hardline + group(doc))
}

///|
fn normal_style(doc : Doc) -> Doc {
  group(braces(doc, force_newline=true))
}

///|
fn Fmt::compose_case_rhs(fmt : Self, expr : @syntax.Expr, ctx : Context) -> Doc {
  let kind = expr.classify()
  let raw = fmt.expr(expr)
  let rhs = match infer_parens(ctx, kind) {
    NoParen =>
      match raw {
        // For a normal RawExprDoc, we always wrap it with braces when it cannot 
        // compose with `=>` in the same line.
        //
        // Bad case: 
        // If the case rhs is a identifier, to avoid ambiguous with record containing 
        // one punning field, we should avoid the `{}`.
        Normal(doc) =>
          if expr is Ident(_) {
            nest(softline + doc)
          } else {
            group(braces_if_not_flatten(doc))
          }

        // For a block-like RawExprDoc, we always compose it directly with `composed_style`.
        // This is because block-like structures usually already have their own paren/braces;
        // adding extra braces or line wraps causes redundant indentation.
        //
        // Bad case:
        // ```
        // match e {
        //   VeryLongPattern | VeryLongPattern | LongPattern | LongPattern => 
        //     [a, b]
        //   VeryLongPattern | VeryLongPattern | LongPattern | LongPattern => 
        //     [
        //       long_a, 
        //       long_b,
        //     ]
        // }
        // ```
        // 
        // Ideally:
        // ```
        // match e {
        //   VeryLongPattern | VeryLongPattern | LongPattern | LongPattern => [
        //     a, 
        //     b, 
        //   ]
        //   VeryLongPattern | VeryLongPattern | LongPattern | LongPattern => [
        //     long_a, 
        //     long_b,
        //   ]
        // }
        // ```
        BlockLike(doc) => composed_style(doc)

        // For a collapsible RawExprDoc, we choose different styles based on the
        // remaining space in the current and next lines.
        //
        // Bad case: 
        // 1. if rhs fits in next line, use dangling style
        //
        //    Bad format result:
        //    ```
        //    VeryLongPattern | VeryLongPattern | LongPattern | LongPattern => func(
        //      x,
        //    ) 
        //    ```
        //    
        //    Ideal format result:
        //    ```  
        //    VeryLongPattern | VeryLongPattern | LongPattern | LongPattern => 
        //      func(x)
        //    ```
        //
        // 2. if the rhs need more than one line, use normal style
        //
        //    This avoids confusing layouts, especially in nested control 
        //    flow expressions, when the control flow bodies have more than one 
        //    part that looks like a block.
        // 
        //
        //    Bad format result:
        //    ```
        //    match e {
        //      VeryLongPattern | VeryLongPattern => try {
        //        long_expr
        //      } catch {
        //        long_expr
        //      } noraise {
        //        long_expr
        //      }
        //    }
        //    ```
        //
        //    Ideal format result:
        //    ```
        //    match e {
        //      VeryLongPattern | VeryLongPattern => {
        //        try {
        //          long_expr
        //        } catch {
        //          long_expr
        //        } noraise {
        //          long_expr
        //        }
        //      }
        //    }
        //    ```
        Collapsible(head, body) => {
          let rhs_req = (head + body).requirement()
          dynamic(ctx => {
            let current_remain = ctx.current_remain()
            let next_remain = ctx.next_remain()
            if current_remain >= rhs_req {
              flatten_style(head + body)
            } else if next_remain >= rhs_req + Space(2) {
              dangling_style(head + body)
            } else {
              normal_style(head + body)
            }
          })
        }
      }
    NeedBraces => braces(raw.to_doc())
    NeedParens => parens(raw.to_doc())
    GroupedParens => group(parens(raw.to_doc()))
  }
  space + rhs
}

///|
fn Fmt::compose_arrow_rhs(fmt : Self, expr : @syntax.Expr) -> Doc {
  let kind = expr.classify()
  let raw = fmt.expr(expr)
  let rhs = match infer_parens(ExprStmt, kind) {
    NoParen =>
      match raw {
        // Bad case:
        // In some cases, the trailing callback rule applies. If the callback is an arrow
        // function and the body is too long to fit on the current line, it will wrap
        // inside. This looks strange if the body is a control flow expression with more
        // than one `{}`, or a nested function call:
        //
        // ```
        // function_call(arg1, arg2, (x) => if cond {
        //   long_expr
        // } else { 
        //   long_expr
        // })
        //  
        // function_call(arg1, arg2, (x) => 
        //   callback(
        //     long_arg1,
        //     long_arg2,
        //   ))
        // ```
        //
        // We should always add `{}` if the body requires more than one line to make 
        // it look better:
        //
        // ```
        // function_call(arg1, arg2, (x) => {
        //   if cond { 
        //     long_expr
        //   } else {
        //     long_expr
        //   }
        // })  
        //
        // function_call(arg1, arg2, (x) => {
        //   callback(
        //     long_arg1,
        //     long_arg2,
        //   )
        // })
        // ```
        // Therefore we didn't use the composed style here.
        Normal(_) | Collapsible(_, _) => {
          let doc = raw.to_doc()
          match expr {
            // Bad case: In arrow function: the body cannot be break, continue or return
            Break(_) | Continue(_) | Return(_) =>
              group(braces(doc, force_newline=false))
            // Bad case: ambiguous with record containing one punning field
            // ```
            // function_call(arg1, arg2, (x) => {
            //   long_id 
            // })
            // ```
            // The long_id need to be surround with `()`.
            Ident(_) => {
              let lb = switch(empty, char('('))
              let rb = switch(empty, char(')'))
              group(braces_if_not_flatten(lb + doc + rb))
            }
            _ => group(braces_if_not_flatten(doc))
          }
        }
        BlockLike(doc) => composed_style(doc)
      }
    NeedBraces => braces(raw.to_doc())
    NeedParens | GroupedParens => panic()
  }
  space + rhs
}
