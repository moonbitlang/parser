///|
fn parse_cst_source(
  source : String,
  name : String,
  parser : @parser.Parser,
) -> (@syntax.Impls, Json) raise Error {
  let (impls, diagnostics) = @parser.parse_string(source, name~, parser~)
  if !diagnostics.is_empty() {
    fail(
      "parsing \{name} failed due to diagnostics: \{diagnostics.to_json().stringify(indent=2)}",
    )
  }
  let json = @basic.show_loc.protect(@basic.String, fn() {
    impls.map(impl_ => impl_.json_repr()).to_json()
  })
  (impls, json)
}

///|
fn expect_same_json(
  name : String,
  hand_json : Json,
  yacc_json : Json,
) -> Unit raise Error {
  if hand_json != yacc_json {
    fail(
      "\{name} produced different ASTs between handrolled and yacc parsers.\nhandrolled:\n\{hand_json.stringify(indent=2)}\n\nyacc:\n\{yacc_json.stringify(indent=2)}",
    )
  }
}

///|
fn expect_some_loc(
  loc : @basic.Location?,
  content : String,
) -> Unit raise Error {
  match loc {
    Some(loc) => inspect(loc, content~)
    None => fail("expected location \{content}")
  }
}

///|
fn expect_none(loc : @basic.Location?) -> Unit raise Error {
  match loc {
    None => ()
    Some(_) => fail("expected no location")
  }
}

///|
fn expect_one_loc(
  locs : @list.List[@basic.Location],
  content : String,
) -> Unit raise Error {
  match locs {
    More(loc, tail=Empty) => inspect(loc, content~)
    _ => fail("expected exactly one location")
  }
}

///|
fn expect_two_locs(
  locs : @list.List[@basic.Location],
  first : String,
  second : String,
) -> Unit raise Error {
  match locs {
    More(loc1, tail=More(loc2, tail=Empty)) => {
      inspect(loc1, content=first)
      inspect(loc2, content=second)
    }
    _ => fail("expected exactly two locations")
  }
}

///|
fn loc_count(locs : @list.List[@basic.Location]) -> Int {
  match locs {
    Empty => 0
    More(_, tail~) => 1 + loc_count(tail)
  }
}

///|
fn expect_empty_locs(locs : @list.List[@basic.Location]) -> Unit raise Error {
  if !locs.is_empty() {
    fail("expected no locations")
  }
}

///|
fn expect_loc_count(
  locs : @list.List[@basic.Location],
  count : Int,
) -> Unit raise Error {
  if loc_count(locs) != count {
    fail("expected \{count} locations")
  }
}

///|
fn find_top_let(
  impls : @syntax.Impls,
  name : String,
) -> (@syntax.Expr, @basic.Location?) raise Error {
  for impl_ in impls {
    match impl_ {
      TopLetDef(binder={ name: binder_name, .. }, expr~, trailing_semi_loc~, ..) if binder_name ==
        name => return (expr, trailing_semi_loc)
      _ => ()
    }
  }
  fail("failed to find top-level let \{name}")
}

///|
fn find_top_match_cases(
  impls : @syntax.Impls,
  name : String,
) -> @list.List[@syntax.Case] raise Error {
  for impl_ in impls {
    match impl_ {
      TopFuncDef(
        fun_decl={ name: { name: fn_name, .. }, .. },
        decl_body=DeclBody(expr=Match(cases~, ..), ..),
        ..
      ) if fn_name == name => return cases
      _ => ()
    }
  }
  fail("failed to find match cases for function \{name}")
}

///|
fn find_top_fun_decl(
  impls : @syntax.Impls,
  name : String,
) -> @syntax.FunDecl raise Error {
  for impl_ in impls {
    match impl_ {
      TopFuncDef(fun_decl~, ..) =>
        if fun_decl.name.name == name {
          return fun_decl
        }
      _ => ()
    }
  }
  fail("failed to find top-level function \{name}")
}

///|
fn find_top_fun_body(
  impls : @syntax.Impls,
  name : String,
) -> @syntax.DeclBody raise Error {
  for impl_ in impls {
    match impl_ {
      TopFuncDef(fun_decl~, decl_body~, ..) =>
        if fun_decl.name.name == name {
          return decl_body
        }
      _ => ()
    }
  }
  fail("failed to find top-level function body \{name}")
}

///|
fn find_top_trait_method(
  impls : @syntax.Impls,
  trait_name : String,
  method_name : String,
) -> @syntax.TraitMethodDecl raise Error {
  for impl_ in impls {
    match impl_ {
      TopTrait({ name: { name: current_trait, .. }, methods, .. }, ..) if current_trait ==
        trait_name =>
        for method_decl in methods {
          if method_decl.name.name == method_name {
            return method_decl
          }
        }
      _ => ()
    }
  }
  fail("failed to find trait method \{trait_name}::\{method_name}")
}

///|
fn find_top_type_decl(
  impls : @syntax.Impls,
  name : String,
) -> @syntax.TypeDecl raise Error {
  for impl_ in impls {
    match impl_ {
      TopTypeDef(type_decl, ..) =>
        if type_decl.tycon == name {
          return type_decl
        }
      _ => ()
    }
  }
  fail("failed to find top-level type \{name}")
}

///|
fn find_top_trait_decl(
  impls : @syntax.Impls,
  name : String,
) -> @syntax.TraitDecl raise Error {
  for impl_ in impls {
    match impl_ {
      TopTrait(trait_decl, ..) =>
        if trait_decl.name.name == name {
          return trait_decl
        }
      _ => ()
    }
  }
  fail("failed to find top-level trait \{name}")
}

///|
fn find_top_impl_by_method(
  impls : @syntax.Impls,
  method_name : String,
) -> @syntax.Impl raise Error {
  for impl_ in impls {
    match impl_ {
      TopImpl(method_name={ name, .. }, ..) if name == method_name =>
        return impl_
      _ => ()
    }
  }
  fail("failed to find impl method \{method_name}")
}

///|
fn assert_remaining_gap_locs(impls : @syntax.Impls) -> Unit raise Error {
  match find_top_fun_body(impls, "top_tail") {
    DeclBody(lbrace_loc~, rbrace_loc~, semi_locs~, trailing_semi_loc~, ..) => {
      expect_some_loc(lbrace_loc, "cst_remaining.mbt-1:16-1:17")
      expect_some_loc(rbrace_loc, "cst_remaining.mbt-1:30-1:31")
      expect_one_loc(semi_locs, "cst_remaining.mbt-1:22-1:23")
      expect_some_loc(trailing_semi_loc, "cst_remaining.mbt-1:28-1:29")
    }
    _ => fail("expected top_tail to have a declaration body")
  }

  match find_top_impl_by_method(impls, "run") {
    TopImpl(
      body=DeclBody(
        lbrace_loc~,
        rbrace_loc~,
        semi_locs~,
        trailing_semi_loc~,
        ..
      ),
      ..
    ) => {
      expect_some_loc(lbrace_loc, "cst_remaining.mbt-2:39-2:40")
      expect_some_loc(rbrace_loc, "cst_remaining.mbt-2:53-2:54")
      expect_one_loc(semi_locs, "cst_remaining.mbt-2:45-2:46")
      expect_some_loc(trailing_semi_loc, "cst_remaining.mbt-2:51-2:52")
    }
    _ => fail("expected run impl")
  }

  let (local_fn_expr, _) = find_top_let(impls, "local_fn")
  match local_fn_expr {
    Group(expr=LetFn(func~, fn_loc~, semi_loc~, ..), ..) => {
      expect_some_loc(fn_loc, "cst_remaining.mbt-3:19-3:21")
      expect_some_loc(semi_loc, "cst_remaining.mbt-3:45-3:46")
      expect_some_loc(func.fn_loc, "cst_remaining.mbt-3:19-3:21")
      expect_some_loc(func.body_lbrace_loc, "cst_remaining.mbt-3:30-3:31")
      expect_some_loc(func.body_rbrace_loc, "cst_remaining.mbt-3:44-3:45")
      expect_one_loc(func.body_semi_locs, "cst_remaining.mbt-3:36-3:37")
      expect_some_loc(
        func.body_trailing_semi_loc,
        "cst_remaining.mbt-3:42-3:43",
      )
    }
    _ => fail("expected local_fn to parse as LetFn")
  }

  let (lex_q_expr, _) = find_top_let(impls, "lex_q")
  match lex_q_expr {
    IsLexMatch(lexmatch_loc~, question_loc~, with_loc~, ..) => {
      expect_some_loc(lexmatch_loc, "cst_remaining.mbt-4:22-4:30")
      expect_some_loc(question_loc, "cst_remaining.mbt-4:30-4:31")
      expect_none(with_loc)
    }
    _ => fail("expected lex_q to parse as IsLexMatch")
  }

  let (lex_with_expr, _) = find_top_let(impls, "lex_with")
  match lex_with_expr {
    IsLexMatch(lexmatch_loc~, question_loc~, with_loc~, ..) => {
      expect_some_loc(lexmatch_loc, "cst_remaining.mbt-5:25-5:33")
      expect_some_loc(question_loc, "cst_remaining.mbt-5:33-5:34")
      expect_some_loc(with_loc, "cst_remaining.mbt-5:39-5:43")
    }
    _ => fail("expected lex_with to parse as IsLexMatch")
  }

  let (dot_call_expr, _) = find_top_let(impls, "dot_call")
  match dot_call_expr {
    DotApply(method_name={ loc, .. }, ..) =>
      inspect(loc, content="cst_remaining.mbt-6:23-6:26")
    _ => fail("expected dot_call to parse as DotApply")
  }

  let (let_stmt_expr, _) = find_top_let(impls, "let_stmt")
  match let_stmt_expr {
    Group(expr=Let(let_loc~, equal_loc~, semi_loc~, ..), ..) => {
      expect_some_loc(let_loc, "cst_remaining.mbt-7:19-7:22")
      expect_some_loc(equal_loc, "cst_remaining.mbt-7:29-7:30")
      expect_some_loc(semi_loc, "cst_remaining.mbt-7:35-7:36")
    }
    _ => fail("expected let_stmt to parse as Let")
  }

  let (let_mut_stmt_expr, _) = find_top_let(impls, "let_mut_stmt")
  match let_mut_stmt_expr {
    Group(expr=LetMut(let_loc~, mut_loc~, equal_loc~, semi_loc~, ..), ..) => {
      expect_some_loc(let_loc, "cst_remaining.mbt-8:23-8:26")
      expect_some_loc(mut_loc, "cst_remaining.mbt-8:27-8:30")
      expect_some_loc(equal_loc, "cst_remaining.mbt-8:37-8:38")
      expect_some_loc(semi_loc, "cst_remaining.mbt-8:43-8:44")
    }
    _ => fail("expected let_mut_stmt to parse as LetMut")
  }

  let (letrec_stmt_expr, _) = find_top_let(impls, "letrec_stmt")
  match letrec_stmt_expr {
    Group(expr=LetAnd(bindings~, letrec_loc~, semi_loc~, ..), ..) => {
      expect_some_loc(letrec_loc, "cst_remaining.mbt-9:22-9:28")
      expect_some_loc(semi_loc, "cst_remaining.mbt-9:68-9:69")
      match bindings {
        More((_, _, first_func), tail=More((_, _, second_func), tail=Empty)) => {
          expect_some_loc(first_func.fn_loc, "cst_remaining.mbt-9:33-9:35")
          expect_some_loc(second_func.fn_loc, "cst_remaining.mbt-9:55-9:57")
        }
        _ => fail("expected two letrec bindings")
      }
    }
    _ => fail("expected letrec_stmt to parse as LetAnd")
  }

  let (guard_stmt_expr, _) = find_top_let(impls, "guard_stmt")
  match guard_stmt_expr {
    Group(
      expr=Guard(
        guard_loc~,
        else_loc~,
        else_lbrace_loc~,
        else_rbrace_loc~,
        else_semi_locs~,
        else_trailing_semi_loc~,
        semi_loc~,
        ..
      ),
      ..
    ) => {
      expect_some_loc(guard_loc, "cst_remaining.mbt-10:21-10:26")
      expect_some_loc(else_loc, "cst_remaining.mbt-10:32-10:36")
      expect_some_loc(else_lbrace_loc, "cst_remaining.mbt-10:37-10:38")
      expect_some_loc(else_rbrace_loc, "cst_remaining.mbt-10:49-10:50")
      expect_one_loc(else_semi_locs, "cst_remaining.mbt-10:42-10:43")
      expect_some_loc(else_trailing_semi_loc, "cst_remaining.mbt-10:47-10:48")
      expect_some_loc(semi_loc, "cst_remaining.mbt-10:50-10:51")
    }
    _ => fail("expected guard_stmt to parse as Guard")
  }

  let (defer_stmt_expr, _) = find_top_let(impls, "defer_stmt")
  match defer_stmt_expr {
    Group(expr=Defer(defer_loc~, semi_loc~, ..), ..) => {
      expect_some_loc(defer_loc, "cst_remaining.mbt-11:21-11:26")
      expect_some_loc(semi_loc, "cst_remaining.mbt-11:34-11:35")
    }
    _ => fail("expected defer_stmt to parse as Defer")
  }

  let typed_decl = find_top_fun_decl(impls, "typed")
  match typed_decl.decl_params {
    Some(params) =>
      match params {
        More(
          Positional(
            colon_loc=Some(colon_loc),
            ty=Some(Object(constr_id=_, amp_loc~)),
            ..
          ),
          tail=Empty
        ) => {
          inspect(colon_loc, content="cst_remaining.mbt-12:13-12:14")
          expect_some_loc(amp_loc, "cst_remaining.mbt-12:15-12:16")
        }
        _ => fail("expected typed to have one typed positional parameter")
      }
    None => fail("expected typed to have parameters")
  }

  let range_cases = find_top_match_cases(impls, "range_pat")
  match range_cases {
    More(case, tail=Empty) =>
      match case.pattern {
        Range(lhs=_, rhs=_, kind=_, loc=_, op_loc~) =>
          expect_some_loc(op_loc, "cst_remaining.mbt-13:37-13:40")
        _ => fail("expected range_pat case to parse as Pattern::Range")
      }
    _ => fail("expected range_pat to have one match case")
  }

  let (is_expr, _) = find_top_let(impls, "is_expr")
  match is_expr {
    Is(is_loc~, ..) => expect_some_loc(is_loc, "cst_remaining.mbt-14:22-14:24")
    _ => fail("expected is_expr to parse as Is")
  }

  let (call_bang_expr, _) = find_top_let(impls, "call_bang")
  match call_bang_expr {
    Apply(bang_loc~, question_loc~, ..) => {
      expect_some_loc(bang_loc, "cst_remaining.mbt-15:19-15:20")
      expect_none(question_loc)
    }
    _ => fail("expected call_bang to parse as Apply")
  }

  let (call_q_expr, _) = find_top_let(impls, "call_q")
  match call_q_expr {
    Apply(bang_loc~, question_loc~, ..) => {
      expect_none(bang_loc)
      expect_some_loc(question_loc, "cst_remaining.mbt-16:16-16:17")
    }
    _ => fail("expected call_q to parse as Apply")
  }

  let (dot_bang_expr, _) = find_top_let(impls, "dot_bang")
  match dot_bang_expr {
    DotApply(bang_loc~, question_loc~, ..) => {
      expect_some_loc(bang_loc, "cst_remaining.mbt-17:26-17:27")
      expect_none(question_loc)
    }
    _ => fail("expected dot_bang to parse as DotApply")
  }

  let (dot_q_expr, _) = find_top_let(impls, "dot_q")
  match dot_q_expr {
    DotApply(bang_loc~, question_loc~, ..) => {
      expect_none(bang_loc)
      expect_some_loc(question_loc, "cst_remaining.mbt-18:23-18:24")
    }
    _ => fail("expected dot_q to parse as DotApply")
  }

  let (method_ref_expr, _) = find_top_let(impls, "method_ref")
  match method_ref_expr {
    Method(coloncolon_loc~, ..) =>
      expect_some_loc(coloncolon_loc, "cst_remaining.mbt-19:24-19:26")
    _ => fail("expected method_ref to parse as Method")
  }

  let maybe_decl = find_top_fun_decl(impls, "maybe")
  match maybe_decl.error_type {
    MaybeError(question_loc~, ..) =>
      expect_some_loc(question_loc, "cst_remaining.mbt-20:19-20:20")
    _ => fail("expected maybe to have MaybeError error type")
  }

  let open_array_cases = find_top_match_cases(impls, "open_array")
  match open_array_cases {
    More(case, tail=Empty) => {
      expect_some_loc(case.fat_arrow_loc, "cst_remaining.mbt-21:48-21:50")
      match case.pattern {
        Array(dotdot_loc~, ..) =>
          expect_some_loc(dotdot_loc, "cst_remaining.mbt-21:40-21:42")
        _ => fail("expected open_array case to parse as Pattern::Array")
      }
    }
    _ => fail("expected open_array to have one match case")
  }

  let string_spread_cases = find_top_match_cases(impls, "string_spread")
  match string_spread_cases {
    More(case, tail=Empty) =>
      match case.pattern {
        Array(pats~, dotdot_loc~, ..) => {
          expect_none(dotdot_loc)
          match pats {
            Closed(More(StringSpread(dotdot_loc~, ..), tail=Empty)) =>
              expect_some_loc(dotdot_loc, "cst_remaining.mbt-22:37-22:39")
            _ => fail("expected string_spread to use StringSpread")
          }
        }
        _ => fail("expected string_spread case to parse as Pattern::Array")
      }
    _ => fail("expected string_spread to have one match case")
  }

  let bytes_spread_cases = find_top_match_cases(impls, "bytes_spread")
  match bytes_spread_cases {
    More(case, tail=Empty) =>
      match case.pattern {
        Array(pats~, dotdot_loc~, ..) => {
          expect_none(dotdot_loc)
          match pats {
            Closed(More(BytesSpread(dotdot_loc~, ..), tail=Empty)) =>
              expect_some_loc(dotdot_loc, "cst_remaining.mbt-23:36-23:38")
            _ => fail("expected bytes_spread to use BytesSpread")
          }
        }
        _ => fail("expected bytes_spread case to parse as Pattern::Array")
      }
    _ => fail("expected bytes_spread to have one match case")
  }

  let const_spread_cases = find_top_match_cases(impls, "const_spread")
  match const_spread_cases {
    More(case, tail=Empty) =>
      match case.pattern {
        Array(pats~, dotdot_loc~, ..) => {
          expect_none(dotdot_loc)
          match pats {
            Closed(More(ConstSpread(dotdot_loc~, ..), tail=Empty)) =>
              expect_some_loc(dotdot_loc, "cst_remaining.mbt-24:36-24:38")
            _ => fail("expected const_spread to use ConstSpread")
          }
        }
        _ => fail("expected const_spread case to parse as Pattern::Array")
      }
    _ => fail("expected const_spread to have one match case")
  }

  let guarded_cases = find_top_match_cases(impls, "guarded")
  match guarded_cases {
    More(first_case, tail=More(second_case, tail=Empty)) => {
      expect_some_loc(
        first_case.guard_keyword_loc,
        "cst_remaining.mbt-25:30-25:32",
      )
      expect_some_loc(first_case.fat_arrow_loc, "cst_remaining.mbt-25:36-25:38")
      expect_none(second_case.guard_keyword_loc)
      expect_some_loc(
        second_case.fat_arrow_loc,
        "cst_remaining.mbt-25:44-25:46",
      )
    }
    _ => fail("expected guarded to have two match cases")
  }

  let (lexed_guard_expr, _) = find_top_let(impls, "lexed_guard")
  match lexed_guard_expr {
    LexMatch(cases~, ..) =>
      match cases {
        More(first_case, tail=More(second_case, tail=Empty)) => {
          expect_some_loc(
            first_case.guard_keyword_loc,
            "cst_remaining.mbt-26:41-26:43",
          )
          expect_some_loc(
            first_case.fat_arrow_loc,
            "cst_remaining.mbt-26:47-26:49",
          )
          expect_none(second_case.guard_keyword_loc)
          expect_some_loc(
            second_case.fat_arrow_loc,
            "cst_remaining.mbt-26:57-26:59",
          )
        }
        _ => fail("expected lexed_guard to have two lex cases")
      }
    _ => fail("expected lexed_guard to parse as LexMatch")
  }

  let quantified_decl = find_top_fun_decl(impls, "quantified")
  match quantified_decl.quantifiers {
    More(binder, tail=Empty) => {
      expect_some_loc(binder.colon_loc, "cst_remaining.mbt-27:18-27:19")
      expect_one_loc(binder.plus_locs, "cst_remaining.mbt-27:25-27:26")
    }
    _ => fail("expected quantified to have one type variable binder")
  }

  let demo_decl = find_top_type_decl(impls, "Demo")
  match demo_decl.components {
    Variant(More(constr, tail=Empty)) =>
      match constr.args {
        Some(More(first_param, tail=More(second_param, tail=Empty))) => {
          inspect(first_param.loc, content="cst_remaining.mbt-28:18-28:25")
          expect_some_loc(first_param.mut_loc, "cst_remaining.mbt-28:18-28:21")
          expect_none(first_param.colon_loc)
          inspect(second_param.loc, content="cst_remaining.mbt-28:27-28:42")
          expect_none(second_param.mut_loc)
          expect_some_loc(
            second_param.colon_loc,
            "cst_remaining.mbt-28:34-28:35",
          )
        }
        _ => fail("expected Demo.One to have two constructor parameters")
      }
    _ => fail("expected Demo to parse as a variant type")
  }
}

///|
fn find_top_view(
  impls : @syntax.Impls,
  view_name : String,
) -> @syntax.Impl raise Error {
  for impl_ in impls {
    match impl_ {
      TopView(view_type_name~, ..) if view_type_name == view_name =>
        return impl_
      _ => ()
    }
  }
  fail("failed to find enumview \{view_name}")
}

///|
fn find_top_using(
  impls : @syntax.Impls,
  pkg_name : String,
) -> @syntax.Impl raise Error {
  for impl_ in impls {
    match impl_ {
      TopUsing(pkg={ name, .. }, ..) if name == pkg_name => return impl_
      _ => ()
    }
  }
  fail("failed to find using \{pkg_name}")
}

///|
fn assert_expr_separator_locs(impls : @syntax.Impls) -> Unit raise Error {
  let (call_expr, _) = find_top_let(impls, "call")
  match call_expr {
    Apply(lparen_loc~, rparen_loc~, comma_locs~, trailing_comma_loc~, ..) => {
      expect_some_loc(lparen_loc, "cst_exprs.mbt-1:14-1:15")
      expect_some_loc(rparen_loc, "cst_exprs.mbt-1:20-1:21")
      expect_one_loc(comma_locs, "cst_exprs.mbt-1:16-1:17")
      expect_some_loc(trailing_comma_loc, "cst_exprs.mbt-1:19-1:20")
    }
    _ => fail("expected call to parse as Apply")
  }

  let (tuple_expr, _) = find_top_let(impls, "tuple")
  match tuple_expr {
    Tuple(lparen_loc~, rparen_loc~, comma_locs~, trailing_comma_loc~, ..) => {
      expect_some_loc(lparen_loc, "cst_exprs.mbt-2:14-2:15")
      expect_some_loc(rparen_loc, "cst_exprs.mbt-2:19-2:20")
      expect_one_loc(comma_locs, "cst_exprs.mbt-2:16-2:17")
      expect_none(trailing_comma_loc)
    }
    _ => fail("expected tuple to parse as Tuple")
  }

  let (array_expr, _) = find_top_let(impls, "array")
  match array_expr {
    ArraySpread(
      lbracket_loc~,
      rbracket_loc~,
      comma_locs~,
      trailing_comma_loc~,
      ..
    ) => {
      expect_some_loc(lbracket_loc, "cst_exprs.mbt-3:14-3:15")
      expect_some_loc(rbracket_loc, "cst_exprs.mbt-3:25-3:26")
      expect_two_locs(
        comma_locs, "cst_exprs.mbt-3:16-3:17", "cst_exprs.mbt-3:21-3:22",
      )
      expect_some_loc(trailing_comma_loc, "cst_exprs.mbt-3:24-3:25")
    }
    _ => fail("expected array to parse as ArraySpread")
  }

  let (record_expr, _) = find_top_let(impls, "record")
  match record_expr {
    Record(
      fields~,
      lbrace_loc~,
      rbrace_loc~,
      comma_locs~,
      trailing_comma_loc~,
      ..
    ) => {
      expect_some_loc(lbrace_loc, "cst_exprs.mbt-4:15-4:16")
      expect_some_loc(rbrace_loc, "cst_exprs.mbt-4:26-4:27")
      expect_one_loc(comma_locs, "cst_exprs.mbt-4:21-4:22")
      expect_some_loc(trailing_comma_loc, "cst_exprs.mbt-4:24-4:25")
      match fields {
        More({ colon_loc: Some(colon_loc), .. }, tail=_) =>
          inspect(colon_loc, content="cst_exprs.mbt-4:18-4:19")
        _ => fail("expected record field colon loc")
      }
    }
    _ => fail("expected record to parse as Record")
  }

  let (_, trailing_semi_loc) = find_top_let(impls, "first")
  expect_some_loc(trailing_semi_loc, "cst_exprs.mbt-5:15-5:16")

  let (pipe_group_expr, _) = find_top_let(impls, "pipe_group")
  match pipe_group_expr {
    Pipe(rhs=Group(lparen_loc~, rparen_loc~, ..), ..) => {
      expect_some_loc(lparen_loc, "cst_exprs.mbt-7:24-7:25")
      expect_some_loc(rparen_loc, "cst_exprs.mbt-7:26-7:27")
    }
    _ => fail("expected pipe_group rhs to parse as Group")
  }

  let (pipe_tuple_expr, _) = find_top_let(impls, "pipe_tuple")
  match pipe_tuple_expr {
    Pipe(
      rhs=Tuple(lparen_loc~, rparen_loc~, comma_locs~, trailing_comma_loc~, ..),
      ..
    ) => {
      expect_some_loc(lparen_loc, "cst_exprs.mbt-8:24-8:25")
      expect_some_loc(rparen_loc, "cst_exprs.mbt-8:30-8:31")
      expect_one_loc(comma_locs, "cst_exprs.mbt-8:26-8:27")
      expect_some_loc(trailing_comma_loc, "cst_exprs.mbt-8:29-8:30")
    }
    _ => fail("expected pipe_tuple rhs to parse as Tuple")
  }
}

///|
fn assert_pattern_separator_locs(impls : @syntax.Impls) -> Unit raise Error {
  let cases = find_top_match_cases(impls, "pat")
  match cases {
    More(case1, tail=More(case2, tail=Empty)) => {
      match case1.pattern {
        Constr(
          args~,
          lparen_loc~,
          rparen_loc~,
          comma_locs~,
          trailing_comma_loc~,
          dotdot_loc~,
          ..
        ) => {
          expect_some_loc(lparen_loc, "cst_patterns.mbt-3:6-3:7")
          expect_some_loc(rparen_loc, "cst_patterns.mbt-3:21-3:22")
          expect_two_locs(
            comma_locs, "cst_patterns.mbt-3:8-3:9", "cst_patterns.mbt-3:17-3:18",
          )
          expect_none(trailing_comma_loc)
          expect_some_loc(dotdot_loc, "cst_patterns.mbt-3:19-3:21")
          match args {
            Some(More(_, tail=More(arg, tail=Empty))) =>
              match arg.equal_loc {
                Some(equal_loc) =>
                  inspect(equal_loc, content="cst_patterns.mbt-3:15-3:16")
                None => fail("expected labelled constructor pattern argument")
              }
            _ => fail("expected labelled constructor pattern argument")
          }
        }
        _ => fail("expected first match case to be constructor pattern")
      }

      match case2.pattern {
        Map(
          elems~,
          is_closed=_,
          loc=_,
          lbrace_loc~,
          rbrace_loc~,
          comma_locs~,
          trailing_comma_loc~,
          dotdot_loc~
        ) => {
          expect_some_loc(lbrace_loc, "cst_patterns.mbt-4:2-4:3")
          expect_some_loc(rbrace_loc, "cst_patterns.mbt-4:14-4:15")
          expect_one_loc(comma_locs, "cst_patterns.mbt-4:10-4:11")
          expect_none(trailing_comma_loc)
          expect_some_loc(dotdot_loc, "cst_patterns.mbt-4:12-4:14")
          match elems {
            More({ question_loc, colon_loc, .. }, tail=Empty) => {
              expect_some_loc(question_loc, "cst_patterns.mbt-4:6-4:7")
              expect_some_loc(colon_loc, "cst_patterns.mbt-4:7-4:8")
            }
            _ => fail("expected one map pattern element")
          }
        }
        _ => fail("expected second match case to be map pattern")
      }
    }
    _ => fail("expected exactly two match cases")
  }
}

///|
fn assert_control_flow_locs(impls : @syntax.Impls) -> Unit raise Error {
  let (branch_expr, _) = find_top_let(impls, "branch")
  match branch_expr {
    If(
      ifnot~,
      if_loc~,
      else_loc~,
      if_lbrace_loc~,
      if_rbrace_loc~,
      else_lbrace_loc~,
      else_rbrace_loc~,
      ..
    ) => {
      expect_some_loc(if_loc, "cst_control.mbt-1:15-1:17")
      expect_some_loc(else_loc, "cst_control.mbt-1:26-1:30")
      expect_some_loc(if_lbrace_loc, "cst_control.mbt-1:20-1:21")
      expect_some_loc(if_rbrace_loc, "cst_control.mbt-1:24-1:25")
      expect_none(else_lbrace_loc)
      expect_none(else_rbrace_loc)
      match ifnot {
        Some(
          If(
            if_loc=inner_if_loc,
            else_loc=inner_else_loc,
            if_lbrace_loc=inner_if_lbrace_loc,
            if_rbrace_loc=inner_if_rbrace_loc,
            else_lbrace_loc=inner_else_lbrace_loc,
            else_rbrace_loc=inner_else_rbrace_loc,
            ..
          )
        ) => {
          expect_some_loc(inner_if_loc, "cst_control.mbt-1:31-1:33")
          expect_some_loc(inner_else_loc, "cst_control.mbt-1:42-1:46")
          expect_some_loc(inner_if_lbrace_loc, "cst_control.mbt-1:36-1:37")
          expect_some_loc(inner_if_rbrace_loc, "cst_control.mbt-1:40-1:41")
          expect_some_loc(inner_else_lbrace_loc, "cst_control.mbt-1:47-1:48")
          expect_some_loc(inner_else_rbrace_loc, "cst_control.mbt-1:51-1:52")
        }
        _ => fail("expected else-if chain to parse as nested If")
      }
    }
    _ => fail("expected branch to parse as If")
  }

  let (while_expr, _) = find_top_let(impls, "while_case")
  match while_expr {
    While(
      loop_body~,
      while_else~,
      while_loc~,
      loop_lbrace_loc~,
      loop_rbrace_loc~,
      nobreak_loc~,
      nobreak_lbrace_loc~,
      nobreak_rbrace_loc~,
      ..
    ) => {
      expect_some_loc(while_loc, "cst_control.mbt-2:19-2:24")
      expect_some_loc(loop_lbrace_loc, "cst_control.mbt-2:30-2:31")
      expect_some_loc(loop_rbrace_loc, "cst_control.mbt-2:42-2:43")
      expect_some_loc(nobreak_loc, "cst_control.mbt-2:44-2:51")
      expect_some_loc(nobreak_lbrace_loc, "cst_control.mbt-2:52-2:53")
      expect_some_loc(nobreak_rbrace_loc, "cst_control.mbt-2:59-2:60")
      match loop_body {
        Sequence(semi_locs~, ..) =>
          expect_one_loc(semi_locs, "cst_control.mbt-2:36-2:37")
        _ => fail("expected while body to preserve inner semicolon loc")
      }
      match while_else {
        Some(Unit(loc=_, faked=false)) => ()
        Some(_) => ()
        None => fail("expected nobreak block to parse")
      }
    }
    _ => fail("expected while_case to parse as While")
  }

  let (counted_expr, _) = find_top_let(impls, "counted")
  match counted_expr {
    For(
      body~,
      for_else~,
      where_clause~,
      for_loc~,
      body_lbrace_loc~,
      body_rbrace_loc~,
      nobreak_loc~,
      nobreak_lbrace_loc~,
      nobreak_rbrace_loc~,
      header_semi_locs~,
      ..
    ) => {
      expect_some_loc(for_loc, "cst_control.mbt-3:16-3:19")
      expect_some_loc(body_lbrace_loc, "cst_control.mbt-3:45-3:46")
      expect_some_loc(body_rbrace_loc, "cst_control.mbt-3:60-3:61")
      expect_some_loc(nobreak_loc, "cst_control.mbt-3:62-3:69")
      expect_some_loc(nobreak_lbrace_loc, "cst_control.mbt-3:70-3:71")
      expect_some_loc(nobreak_rbrace_loc, "cst_control.mbt-3:77-3:78")
      expect_two_locs(
        header_semi_locs, "cst_control.mbt-3:28-3:29", "cst_control.mbt-3:34-3:35",
      )
      match body {
        Sequence(semi_locs~, ..) =>
          expect_one_loc(semi_locs, "cst_control.mbt-3:52-3:53")
        _ => fail("expected for body to preserve inner semicolon loc")
      }
      match for_else {
        Some(Unit(loc=_, faked=false)) => ()
        Some(_) => ()
        None => fail("expected for nobreak block to parse")
      }
      match where_clause {
        Some(
          {
            fields,
            where_loc,
            lbrace_loc,
            rbrace_loc,
            comma_locs,
            trailing_comma_loc,
            ..,
          }
        ) => {
          expect_some_loc(where_loc, "cst_control.mbt-3:79-3:84")
          expect_some_loc(lbrace_loc, "cst_control.mbt-3:85-3:86")
          expect_some_loc(rbrace_loc, "cst_control.mbt-3:111-3:112")
          expect_one_loc(comma_locs, "cst_control.mbt-3:97-3:98")
          expect_some_loc(trailing_comma_loc, "cst_control.mbt-3:109-3:110")
          match fields {
            More({ colon_loc: Some(colon_loc), .. }, tail=_) =>
              inspect(colon_loc, content="cst_control.mbt-3:90-3:91")
            _ => fail("expected where clause field colon loc")
          }
        }
        None => fail("expected for where clause")
      }
    }
    _ => fail("expected counted to parse as For")
  }

  let (each_expr, _) = find_top_let(impls, "each")
  match each_expr {
    ForEach(
      for_loc~,
      in_loc~,
      body_lbrace_loc~,
      body_rbrace_loc~,
      nobreak_loc~,
      nobreak_lbrace_loc~,
      nobreak_rbrace_loc~,
      comma_locs~,
      ..
    ) => {
      expect_some_loc(for_loc, "cst_control.mbt-4:13-4:16")
      expect_some_loc(in_loc, "cst_control.mbt-4:28-4:30")
      expect_some_loc(body_lbrace_loc, "cst_control.mbt-4:36-4:37")
      expect_some_loc(body_rbrace_loc, "cst_control.mbt-4:43-4:44")
      expect_some_loc(nobreak_loc, "cst_control.mbt-4:45-4:52")
      expect_some_loc(nobreak_lbrace_loc, "cst_control.mbt-4:53-4:54")
      expect_some_loc(nobreak_rbrace_loc, "cst_control.mbt-4:60-4:61")
      expect_one_loc(comma_locs, "cst_control.mbt-4:20-4:21")
    }
    _ => fail("expected each to parse as ForEach")
  }

  let (matched_expr, _) = find_top_let(impls, "matched")
  match matched_expr {
    Match(match_keyword_loc~, lbrace_loc~, rbrace_loc~, ..) => {
      expect_some_loc(match_keyword_loc, "cst_control.mbt-5:16-5:21")
      expect_some_loc(lbrace_loc, "cst_control.mbt-5:28-5:29")
      expect_some_loc(rbrace_loc, "cst_control.mbt-5:45-5:46")
    }
    _ => fail("expected matched to parse as Match")
  }

  let (lexed_expr, _) = find_top_let(impls, "lexed")
  match lexed_expr {
    LexMatch(lexmatch_loc~, with_loc~, lbrace_loc~, rbrace_loc~, ..) => {
      expect_some_loc(lexmatch_loc, "cst_control.mbt-6:14-6:22")
      expect_some_loc(with_loc, "cst_control.mbt-6:29-6:33")
      expect_some_loc(lbrace_loc, "cst_control.mbt-6:42-6:43")
      expect_some_loc(rbrace_loc, "cst_control.mbt-6:53-6:54")
    }
    _ => fail("expected lexed to parse as LexMatch")
  }

  let (handled_expr, _) = find_top_let(impls, "handled")
  match handled_expr {
    Try(
      try_keyword_loc~,
      catch_keyword_loc~,
      catch_bang_loc~,
      catch_lbrace_loc~,
      catch_rbrace_loc~,
      noraise_loc~,
      else_lbrace_loc~,
      else_rbrace_loc~,
      ..
    ) => {
      expect_some_loc(try_keyword_loc, "cst_control.mbt-7:16-7:19")
      expect_some_loc(catch_keyword_loc, "cst_control.mbt-7:25-7:30")
      expect_some_loc(catch_bang_loc, "cst_control.mbt-7:30-7:31")
      expect_some_loc(catch_lbrace_loc, "cst_control.mbt-7:32-7:33")
      expect_some_loc(catch_rbrace_loc, "cst_control.mbt-7:45-7:46")
      expect_some_loc(noraise_loc, "cst_control.mbt-7:47-7:54")
      expect_some_loc(else_lbrace_loc, "cst_control.mbt-7:55-7:56")
      expect_some_loc(else_rbrace_loc, "cst_control.mbt-7:68-7:69")
    }
    _ => fail("expected handled to parse as Try")
  }
}

///|
fn assert_symbol_locs(impls : @syntax.Impls) -> Unit raise Error {
  let (try_q_expr, _) = find_top_let(impls, "try_q")
  match try_q_expr {
    TryOperator(question_loc~, bang_loc~, ..) => {
      expect_some_loc(question_loc, "cst_control.mbt-8:17-8:18")
      expect_none(bang_loc)
    }
    _ => fail("expected try_q to parse as TryOperator")
  }

  let (try_b_expr, _) = find_top_let(impls, "try_b")
  match try_b_expr {
    TryOperator(question_loc~, bang_loc~, ..) => {
      expect_none(question_loc)
      expect_some_loc(bang_loc, "cst_control.mbt-9:17-9:18")
    }
    _ => fail("expected try_b to parse as TryOperator")
  }

  let (plus_expr, _) = find_top_let(impls, "plus")
  match plus_expr {
    Constant(uplus_loc~, uminus_loc~, ..) => {
      expect_some_loc(uplus_loc, "cst_control.mbt-10:13-10:14")
      expect_none(uminus_loc)
    }
    _ => fail("expected plus to parse as Constant")
  }

  let (minus_expr, _) = find_top_let(impls, "minus")
  match minus_expr {
    Constant(uplus_loc~, uminus_loc~, ..) => {
      expect_none(uplus_loc)
      expect_some_loc(uminus_loc, "cst_control.mbt-11:14-11:15")
    }
    _ => fail("expected minus to parse as Constant")
  }
}

///|
fn assert_decl_header_locs(impls : @syntax.Impls) -> Unit raise Error {
  let fun_decl = find_top_fun_decl(impls, "decl")
  expect_some_loc(fun_decl.fn_loc, "cst_decls.mbt-1:2-1:4")
  expect_some_loc(fun_decl.type_params_lbracket_loc, "cst_decls.mbt-1:5-1:6")
  expect_some_loc(fun_decl.type_params_rbracket_loc, "cst_decls.mbt-1:11-1:12")
  expect_one_loc(fun_decl.type_param_comma_locs, "cst_decls.mbt-1:7-1:8")
  expect_some_loc(
    fun_decl.type_param_trailing_comma_loc,
    "cst_decls.mbt-1:10-1:11",
  )
  expect_some_loc(fun_decl.params_lparen_loc, "cst_decls.mbt-1:17-1:18")
  expect_some_loc(fun_decl.params_rparen_loc, "cst_decls.mbt-1:35-1:36")
  expect_one_loc(fun_decl.param_comma_locs, "cst_decls.mbt-1:25-1:26")
  expect_some_loc(fun_decl.trailing_param_comma_loc, "cst_decls.mbt-1:34-1:35")
  expect_some_loc(fun_decl.thin_arrow_loc, "cst_decls.mbt-1:37-1:39")

  let trait_method = find_top_trait_method(impls, "Demo", "method")
  expect_some_loc(
    trait_method.type_params_lbracket_loc,
    "cst_decls.mbt-3:8-3:9",
  )
  expect_some_loc(
    trait_method.type_params_rbracket_loc,
    "cst_decls.mbt-3:14-3:15",
  )
  expect_one_loc(trait_method.type_param_comma_locs, "cst_decls.mbt-3:10-3:11")
  expect_some_loc(
    trait_method.type_param_trailing_comma_loc,
    "cst_decls.mbt-3:13-3:14",
  )
  expect_some_loc(trait_method.params_lparen_loc, "cst_decls.mbt-3:15-3:16")
  expect_some_loc(trait_method.params_rparen_loc, "cst_decls.mbt-3:33-3:34")
  expect_one_loc(trait_method.param_comma_locs, "cst_decls.mbt-3:23-3:24")
  expect_some_loc(
    trait_method.trailing_param_comma_loc,
    "cst_decls.mbt-3:32-3:33",
  )
  expect_some_loc(trait_method.thin_arrow_loc, "cst_decls.mbt-3:35-3:37")
  expect_some_loc(trait_method.equal_loc, "cst_decls.mbt-3:42-3:43")
}

///|
fn assert_fun_modifier_locs(impls : @syntax.Impls) -> Unit raise Error {
  let declared_fun = find_top_fun_decl(impls, "declared")
  expect_some_loc(declared_fun.declare_loc, "cst_fun_mods.mbt-1:2-1:9")
  expect_some_loc(declared_fun.fn_loc, "cst_fun_mods.mbt-1:10-1:12")
  expect_some_loc(
    declared_fun.type_params_lbracket_loc,
    "cst_fun_mods.mbt-1:13-1:14",
  )
  expect_empty_locs(declared_fun.type_param_comma_locs)
  expect_some_loc(
    declared_fun.type_param_trailing_comma_loc,
    "cst_fun_mods.mbt-1:15-1:16",
  )
  expect_some_loc(declared_fun.params_lparen_loc, "cst_fun_mods.mbt-1:26-1:27")
  expect_some_loc(declared_fun.params_rparen_loc, "cst_fun_mods.mbt-1:35-1:36")
  expect_empty_locs(declared_fun.param_comma_locs)
  expect_some_loc(
    declared_fun.trailing_param_comma_loc,
    "cst_fun_mods.mbt-1:34-1:35",
  )
  expect_some_loc(declared_fun.thin_arrow_loc, "cst_fun_mods.mbt-1:37-1:39")

  let foreign_fun = find_top_fun_decl(impls, "foreign")
  expect_some_loc(foreign_fun.extern_loc, "cst_fun_mods.mbt-2:2-2:8")
  expect_some_loc(foreign_fun.fn_loc, "cst_fun_mods.mbt-2:14-2:16")
  expect_none(foreign_fun.type_params_lbracket_loc)
  expect_none(foreign_fun.type_params_rbracket_loc)
  expect_empty_locs(foreign_fun.type_param_comma_locs)
  expect_none(foreign_fun.type_param_trailing_comma_loc)
  expect_some_loc(foreign_fun.params_lparen_loc, "cst_fun_mods.mbt-2:24-2:25")
  expect_some_loc(foreign_fun.params_rparen_loc, "cst_fun_mods.mbt-2:33-2:34")
  expect_empty_locs(foreign_fun.param_comma_locs)
  expect_some_loc(
    foreign_fun.trailing_param_comma_loc,
    "cst_fun_mods.mbt-2:32-2:33",
  )
  expect_some_loc(foreign_fun.thin_arrow_loc, "cst_fun_mods.mbt-2:35-2:37")
}

///|
fn assert_type_impl_header_locs(impls : @syntax.Impls) -> Unit raise Error {
  let alias_decl = find_top_type_decl(impls, "Alias")
  expect_some_loc(alias_decl.keyword_loc, "cst_headers.mbt-1:2-1:6")
  expect_some_loc(
    alias_decl.type_params_lbracket_loc,
    "cst_headers.mbt-1:12-1:13",
  )
  expect_some_loc(
    alias_decl.type_params_rbracket_loc,
    "cst_headers.mbt-1:18-1:19",
  )
  expect_one_loc(alias_decl.type_param_comma_locs, "cst_headers.mbt-1:14-1:15")
  expect_some_loc(
    alias_decl.type_param_trailing_comma_loc,
    "cst_headers.mbt-1:17-1:18",
  )
  expect_some_loc(alias_decl.equal_loc, "cst_headers.mbt-1:20-1:21")
  expect_some_loc(alias_decl.derive_loc, "cst_headers.mbt-1:26-1:32")
  expect_some_loc(alias_decl.derive_lparen_loc, "cst_headers.mbt-1:32-1:33")
  expect_some_loc(alias_decl.derive_rparen_loc, "cst_headers.mbt-1:41-1:42")
  expect_one_loc(alias_decl.derive_comma_locs, "cst_headers.mbt-1:35-1:36")

  let pair_decl = find_top_type_decl(impls, "Pair")
  expect_some_loc(pair_decl.keyword_loc, "cst_headers.mbt-2:2-2:8")
  expect_some_loc(
    pair_decl.type_params_lbracket_loc,
    "cst_headers.mbt-2:13-2:14",
  )
  expect_some_loc(
    pair_decl.type_params_rbracket_loc,
    "cst_headers.mbt-2:19-2:20",
  )
  expect_one_loc(pair_decl.type_param_comma_locs, "cst_headers.mbt-2:15-2:16")
  expect_some_loc(
    pair_decl.type_param_trailing_comma_loc,
    "cst_headers.mbt-2:18-2:19",
  )
  expect_some_loc(pair_decl.body_lparen_loc, "cst_headers.mbt-2:20-2:21")
  expect_some_loc(pair_decl.body_rparen_loc, "cst_headers.mbt-2:31-2:32")
  expect_one_loc(pair_decl.body_comma_locs, "cst_headers.mbt-2:24-2:25")
  expect_some_loc(
    pair_decl.body_trailing_comma_loc,
    "cst_headers.mbt-2:30-2:31",
  )
  expect_some_loc(pair_decl.derive_loc, "cst_headers.mbt-2:33-2:39")

  let fancy_trait = find_top_trait_decl(impls, "Fancy")
  expect_some_loc(fancy_trait.trait_loc, "cst_headers.mbt-3:2-3:7")
  expect_some_loc(fancy_trait.colon_loc, "cst_headers.mbt-3:14-3:15")
  expect_one_loc(fancy_trait.plus_locs, "cst_headers.mbt-3:19-3:20")
  expect_some_loc(fancy_trait.lbrace_loc, "cst_headers.mbt-3:26-3:27")
  expect_some_loc(fancy_trait.rbrace_loc, "cst_headers.mbt-3:72-3:73")

  match find_top_impl_by_method(impls, "call") {
    TopImpl(
      impl_loc~,
      type_params_lbracket_loc~,
      type_params_rbracket_loc~,
      type_param_comma_locs~,
      type_param_trailing_comma_loc~,
      for_loc~,
      with_loc~,
      params_lparen_loc~,
      params_rparen_loc~,
      param_comma_locs~,
      trailing_param_comma_loc~,
      thin_arrow_loc~,
      ..
    ) => {
      expect_some_loc(impl_loc, "cst_headers.mbt-4:2-4:6")
      expect_some_loc(type_params_lbracket_loc, "cst_headers.mbt-4:6-4:7")
      expect_some_loc(type_params_rbracket_loc, "cst_headers.mbt-4:12-4:13")
      expect_one_loc(type_param_comma_locs, "cst_headers.mbt-4:8-4:9")
      expect_some_loc(
        type_param_trailing_comma_loc, "cst_headers.mbt-4:11-4:12",
      )
      expect_some_loc(for_loc, "cst_headers.mbt-4:20-4:23")
      expect_some_loc(with_loc, "cst_headers.mbt-4:28-4:32")
      expect_some_loc(params_lparen_loc, "cst_headers.mbt-4:37-4:38")
      expect_some_loc(params_rparen_loc, "cst_headers.mbt-4:55-4:56")
      expect_one_loc(param_comma_locs, "cst_headers.mbt-4:45-4:46")
      expect_some_loc(trailing_param_comma_loc, "cst_headers.mbt-4:54-4:55")
      expect_some_loc(thin_arrow_loc, "cst_headers.mbt-4:57-4:59")
    }
    _ => fail("expected top impl")
  }

  match find_top_view(impls, "View") {
    TopView(
      enumview_loc~,
      type_params_lbracket_loc~,
      type_params_rbracket_loc~,
      type_param_comma_locs~,
      type_param_trailing_comma_loc~,
      constr_lbrace_loc~,
      constr_rbrace_loc~,
      for_loc~,
      with_loc~,
      params_lparen_loc~,
      params_rparen_loc~,
      param_comma_locs~,
      trailing_param_comma_loc~,
      ..
    ) => {
      expect_some_loc(enumview_loc, "cst_headers.mbt-5:2-5:10")
      expect_some_loc(type_params_lbracket_loc, "cst_headers.mbt-5:10-5:11")
      expect_some_loc(type_params_rbracket_loc, "cst_headers.mbt-5:13-5:14")
      expect_empty_locs(type_param_comma_locs)
      expect_some_loc(
        type_param_trailing_comma_loc, "cst_headers.mbt-5:12-5:13",
      )
      expect_some_loc(constr_lbrace_loc, "cst_headers.mbt-5:20-5:21")
      expect_some_loc(constr_rbrace_loc, "cst_headers.mbt-5:27-5:28")
      expect_some_loc(for_loc, "cst_headers.mbt-5:29-5:32")
      expect_some_loc(with_loc, "cst_headers.mbt-5:37-5:41")
      expect_some_loc(params_lparen_loc, "cst_headers.mbt-5:46-5:47")
      expect_some_loc(params_rparen_loc, "cst_headers.mbt-5:64-5:65")
      expect_one_loc(param_comma_locs, "cst_headers.mbt-5:54-5:55")
      expect_some_loc(trailing_param_comma_loc, "cst_headers.mbt-5:63-5:64")
    }
    _ => fail("expected top enumview")
  }

  match find_top_using(impls, "pkg") {
    TopUsing(
      using_loc~,
      lbrace_loc~,
      rbrace_loc~,
      comma_locs~,
      trailing_comma_loc~,
      ..
    ) => {
      expect_some_loc(using_loc, "cst_headers.mbt-6:2-6:7")
      expect_some_loc(lbrace_loc, "cst_headers.mbt-6:13-6:14")
      expect_some_loc(rbrace_loc, "cst_headers.mbt-6:59-6:60")
      expect_loc_count(comma_locs, 3)
      expect_some_loc(trailing_comma_loc, "cst_headers.mbt-6:57-6:58")
    }
    _ => fail("expected top using")
  }
}

///|
test "expr separator loc parity" {
  let source =
    #| let call = f(a, b,)
    #| let tuple = (a, b)
    #| let array = [a, ..b, c,]
    #| let record = { x: y, z, }
    #| let first = 1;
    #| let second = 2
    #| let pipe_group = a |> (b)
    #| let pipe_tuple = a |> (b, c,)
  let (hand_impls, hand_json) = parse_cst_source(
    source,
    "cst_exprs.mbt",
    @parser.Handrolled,
  )
  let (yacc_impls, yacc_json) = parse_cst_source(
    source,
    "cst_exprs.mbt",
    @parser.MoonYacc,
  )
  expect_same_json("cst_exprs.mbt", hand_json, yacc_json)
  assert_expr_separator_locs(hand_impls)
  assert_expr_separator_locs(yacc_impls)
}

///|
test "pattern separator loc parity" {
  let source =
    #| fn pat(v : Int) {
    #| match v {
    #| Ctor(a, label=b, ..) => ()
    #| {"k"?: v, ..} => ()
    #| }
    #| }
  let (hand_impls, hand_json) = parse_cst_source(
    source,
    "cst_patterns.mbt",
    @parser.Handrolled,
  )
  let (yacc_impls, yacc_json) = parse_cst_source(
    source,
    "cst_patterns.mbt",
    @parser.MoonYacc,
  )
  expect_same_json("cst_patterns.mbt", hand_json, yacc_json)
  assert_pattern_separator_locs(hand_impls)
  assert_pattern_separator_locs(yacc_impls)
}

///|
test "control flow loc parity" {
  let source =
    #| let branch = if a { b } else if c { d } else { e }
    #| let while_case = while cond { step; end } nobreak { fail }
    #| let counted = for i = init; cond; i = next { body1; body2 } nobreak { fail } where { key: value, other: pun, }
    #| let each = for key, value in data { body } nobreak { done }
    #| let matched = match value { A => a; B => b }
    #| let lexed = lexmatch value with longest { "a" => a }
    #| let handled = try work catch! { err => bad } noraise { ok => good }
    #| let try_q = try? work
    #| let try_b = try! work
    #| let plus = +1
    #| let minus = -1
  let (hand_impls, hand_json) = parse_cst_source(
    source,
    "cst_control.mbt",
    @parser.Handrolled,
  )
  let (yacc_impls, yacc_json) = parse_cst_source(
    source,
    "cst_control.mbt",
    @parser.MoonYacc,
  )
  expect_same_json("cst_control.mbt", hand_json, yacc_json)
  assert_control_flow_locs(hand_impls)
  assert_control_flow_locs(yacc_impls)
  assert_symbol_locs(hand_impls)
  assert_symbol_locs(yacc_impls)
}

///|
test "decl header loc parity" {
  let source =
    #| fn [T, U,] decl(x : Int, y : Int,) -> Int { x }
    #| trait Demo {
    #| method[T, U,](x : Int, y : Int,) -> Int = _
    #| }
  let (hand_impls, hand_json) = parse_cst_source(
    source,
    "cst_decls.mbt",
    @parser.Handrolled,
  )
  let (yacc_impls, yacc_json) = parse_cst_source(
    source,
    "cst_decls.mbt",
    @parser.MoonYacc,
  )
  expect_same_json("cst_decls.mbt", hand_json, yacc_json)
  assert_decl_header_locs(hand_impls)
  assert_decl_header_locs(yacc_impls)
}

///|
test "type impl header loc parity" {
  let source =
    #| type Alias[T, U,] = Int derive(Eq, Show)
    #| struct Pair[T, U,](Int, Bool,) derive(Eq, Show)
    #| trait Fancy : Eq + Show { method[T, U,](x : Int, y : Int,) -> Int = _ }
    #| impl[T, U,] Trait for Int with call(x : Int, y : Int,) -> Int = "stub"
    #| enumview[T,] View { A; B } for Src with make(x : Int, y : Int,) { x }
    #| using @pkg { value, TypeName, type Alias, trait EqTrait, }
  let (hand_impls, hand_json) = parse_cst_source(
    source,
    "cst_headers.mbt",
    @parser.Handrolled,
  )
  let (yacc_impls, yacc_json) = parse_cst_source(
    source,
    "cst_headers.mbt",
    @parser.MoonYacc,
  )
  expect_same_json("cst_headers.mbt", hand_json, yacc_json)
  assert_type_impl_header_locs(hand_impls)
  assert_type_impl_header_locs(yacc_impls)
}

///|
test "fun modifier loc parity" {
  let source =
    #| declare fn [T,] declared(x : Int,) -> Int
    #| extern "js" fn foreign(x : Int,) -> Int = "f"
  let (hand_impls, hand_json) = parse_cst_source(
    source,
    "cst_fun_mods.mbt",
    @parser.Handrolled,
  )
  let (yacc_impls, yacc_json) = parse_cst_source(
    source,
    "cst_fun_mods.mbt",
    @parser.MoonYacc,
  )
  expect_same_json("cst_fun_mods.mbt", hand_json, yacc_json)
  assert_fun_modifier_locs(hand_impls)
  assert_fun_modifier_locs(yacc_impls)
}

///|
test "local letrec fn loc order parity" {
  let source =
    #| let local_rec = { fn f() { a }; fn g() { b }; tail }
  let (hand_impls, hand_json) = parse_cst_source(
    source,
    "cst_local_rec.mbt",
    @parser.Handrolled,
  )
  let (yacc_impls, yacc_json) = parse_cst_source(
    source,
    "cst_local_rec.mbt",
    @parser.MoonYacc,
  )
  expect_same_json("cst_local_rec.mbt", hand_json, yacc_json)
  for impls in [hand_impls, yacc_impls] {
    let (expr, _) = find_top_let(impls, "local_rec")
    match expr {
      Group(expr=LetRec(bindings~, fn_locs~, ..), ..) => {
        match bindings {
          More(
            ({ name: "f", .. }, _),
            tail=More(({ name: "g", .. }, _), tail=Empty)
          ) => ()
          _ => fail("expected local_rec LetRec bindings in source order")
        }
        expect_two_locs(
          fn_locs, "cst_local_rec.mbt-1:20-1:22", "cst_local_rec.mbt-1:34-1:36",
        )
      }
      _ => fail("expected local_rec to parse as LetRec")
    }
  }
}

///|
test "remaining CST gap loc parity" {
  let source =
    #| fn top_tail() { prep; done; }
    #| impl Trait for Int with run() -> Int { prep; done; }
    #| let local_fn = { fn inner() { prep; done; }; after }
    #| let lex_q = subject lexmatch? "a"
    #| let lex_with = subject lexmatch? "a" with longest
    #| let dot_call = recv.call(arg)
    #| let let_stmt = { let value = init; tail }
    #| let let_mut_stmt = { let mut value = init; tail }
    #| let letrec_stmt = { letrec f = fn() { body } and g = fn() { body }; tail }
    #| let guard_stmt = { guard cond else { alt; end; }; tail }
    #| let defer_stmt = { defer cleanup; tail }
    #| fn typed(x : &Thing) -> Int { x }
    #| fn range_pat(v : Int) { match v { 1..=3 => 0 } }
    #| let is_expr = value is Some(_)
    #| let call_bang = f!(x)
    #| let call_q = f?(x)
    #| let dot_bang = recv.call!(arg)
    #| let dot_q = recv.call?(arg)
    #| let method_ref = Thing::run
    #| fn maybe() raise ? { body }
    #| fn open_array(xs) { match xs { [head, ..rest] => 0 } }
    #| fn string_spread(xs) { match xs { [.."s"] => 0 } }
    #| fn bytes_spread(xs) { match xs { [..b"s"] => 0 } }
    #| fn const_spread(xs) { match xs { [..SomeConst] => 0 } }
    #| fn guarded(v) { match v { 0 if ok => 1; 1 => 2 } }
    #| let lexed_guard = lexmatch token { "a" if ok => 1; "b" => 2 }
    #| fn quantified[T : Show + Eq]() { body }
    #| enum Demo { One(mut Int, label~ : String) }
  let (hand_impls, hand_json) = parse_cst_source(
    source,
    "cst_remaining.mbt",
    @parser.Handrolled,
  )
  let (yacc_impls, yacc_json) = parse_cst_source(
    source,
    "cst_remaining.mbt",
    @parser.MoonYacc,
  )
  expect_same_json("cst_remaining.mbt", hand_json, yacc_json)
  assert_remaining_gap_locs(hand_impls)
  assert_remaining_gap_locs(yacc_impls)
}
