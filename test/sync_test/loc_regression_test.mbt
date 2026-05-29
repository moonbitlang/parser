///|
let loc_regression_source =
  #| let global: Int = {
  #|   1
  #| }
  #|
  #| fn init {
  #|   let _ = global
  #| }
  #|
  #| fn sample(x : Int, y : Int) -> Int {
  #|   let f = fn(a : Int, b? : Int = 1) -> Int {
  #|     if a > 0 {
  #|       { a: a, b: b }
  #|     } else {
  #|       b
  #|     }
  #|   }
  #|   let semis = { x; }
  #|   f(x, b=y)
  #| }
  #|
  #| fn statements(a : Int) -> Int {
  #|   let x : Int = 1;
  #|   let mut y : Int = x
  #|   guard y > 0 else {
  #|     y
  #|   };
  #|   defer y;
  #|   fn local() -> Int {
  #|     y
  #|   };
  #|   y;
  #|   y
  #| }
  #|
  #| fn call_tokens {
  #|   id(1, 2,)
  #| }
  #|
  #| fn array_tokens {
  #|   [1, ..xs, 3]
  #| }
  #|
  #| fn array_get_tokens {
  #|   xs[1]
  #| }
  #|
  #| fn slice_tokens {
  #|   xs[1:3]
  #| }
  #|
  #| fn set_tokens {
  #|   xs[0] = 4
  #| }
  #|
  #| fn aug_set_tokens {
  #|   xs[1] += 5
  #| }
  #|
  #| fn tuple_tokens {
  #|   (1, 2,)
  #| }
  #|
  #| fn map_tokens {
  #|   { 1: 2, 3: 4, }
  #| }
  #|
  #| fn group_tokens {
  #|   (id(1))
  #| }
  #|
  #| fn brace_group_tokens {
  #|   { id(1) + 2 }
  #| }
  #|
  #| fn is_tokens {
  #|   value is _
  #| }
  #|
  #| fn pipe_tokens {
  #|   value |> id
  #| }
  #|
  #| fn rev_pipe_tokens {
  #|   id <| value
  #| }
  #|
  #| fn return_tokens {
  #|   return value
  #| }
  #|
  #| fn raise_tokens {
  #|   raise value
  #| }
  #|
  #| fn record_update_tokens {
  #|   { ..base, x: 1, y: 2, }
  #| }
  #|
  #| fn typed_record_update_tokens {
  #|   Point::{ ..base }
  #| }
  #|
  #| fn method_tokens {
  #|   Point::make
  #| }
  #|
  #| fn dot_apply_tokens {
  #|   value.method(1, 2,)
  #| }
  #|
  #| fn dotdot_apply_tokens {
  #|   value..tap(1,)
  #| }
  #|
  #| fn as_tokens {
  #|   value as Trait
  #| }
  #|
  #| fn break_label_tokens {
  #|   break loop~ value
  #| }
  #|
  #| fn continue_label_tokens {
  #|   continue loop~ x, y
  #| }
  #|
  #| fn template_tokens {
  #|   value <+ "template"
  #| }
  #|
  #| fn lexmatch_question_tokens {
  #|   value lexmatch? "x" with longest
  #| }
  #|
  #| fn regex_match_tokens {
  #|   value =~ (re"x", before=head, after~)
  #| }
  #|
  #| fn quantifier_tokens {
  #|   ∀ i : Int, holds(i)
  #| }
  #|
  #| fn implies_tokens {
  #|   a → b
  #| }
  #|
  #| fn match_tokens {
  #|   match value {
  #|     1 => one;
  #|     _ => other;
  #|   }
  #| }
  #|
  #| fn lexmatch_tokens {
  #|   lexmatch value with longest {
  #|     "x" => one;
  #|     _ => other;
  #|   }
  #| }
  #|
  #| fn try_tokens {
  #|   try value catch {
  #|     _ => fallback;
  #|   } noraise {
  #|     _ => ok;
  #|   }
  #| }
  #|
  #| fn while_tokens {
  #|   outer~: while value {
  #|     body
  #|   } nobreak {
  #|     done
  #|   }
  #| }
  #|
  #| fn foreach_comprehension_tokens {
  #|   [for x, _ in xs; i = 0, j = 1; i = i + 1, j = j + 1 if keep => item]
  #| }
  #|
  #| fn for_comprehension_tokens {
  #|   [for i = 0, j = 1; i < 10; i = i + 1, j = j + 1 if keep => item]
  #| }
  #|
  #| fn for_loop_tokens {
  #|   loop~: for i = 0, j = 1; i < 10; i = i + 1, j = j + 1 {
  #|     body
  #|   } nobreak {
  #|     done
  #|   }
  #| }
  #|
  #| fn foreach_loop_tokens {
  #|   loop~: for x, _ in xs; i = 0, j = 1; i = i + 1, j = j + 1 {
  #|     body
  #|   } nobreak {
  #|     done
  #|   }
  #| }
  #|
  #| fn type_tokens(
  #|   pkg_value : @pkg.Type[Int, String],
  #|   object_value : &Object,
  #|   fn_value : (Int, String) -> Unit raise Error,
  #|   tuple_value : (Int, String),
  #| ) -> Unit raise ? {
  #|   ()
  #| }
  #|
  #| suberror SharedError {
  #|   Failure(mut reason~ : String, Int) = 7;
  #|   Empty;
  #| }
  #|
  #| struct SharedRecord {
  #|   mut first : Int;
  #|   second : String;
  #| }
  #|
  #| fn constructor_pattern_tokens(value : Result[Int, Error]) -> Unit {
  #|   match value {
  #|     Some(value=_) => ()
  #|     _ => ()
  #|   }
  #| }
  #|
  #| fn constructor_extra_tokens {
  #|   let _ = @pkg.Constr
  #|   let _ = Type::Constr
  #|   let _ = Type::@pkg.Constr
  #| }
  #|
  #| using @pkg {
  #|   alias_value as renamed_value,
  #|   type AliasType as RenamedType,
  #|   trait AliasTrait as RenamedTrait,
  #| }
  #|
  #| fn lex_regex_pattern_tokens {
  #|   let _ = lexmatch value {
  #|     (re"a"; re"b", re"c" as tail) if keep => result;
  #|     ...;
  #|   }
  #|   let _ = value =~ (re"a" + re"b" | re"c", before=head)
  #|   let _ = value =~ (re"d" as alias, before=head)
  #| }
  #|
  #| fn pattern_container_tokens {
  #|   let _ = match value {
  #|     { first: _, second, .. } => ()
  #|     _ => ()
  #|   }
  #|   let _ = match value {
  #|     { "x"?: _, "y": _, .. } => ()
  #|     _ => ()
  #|   }
  #| }
  #|
  #| fn where_clause_tokens {
  #|   for i = 0; i < 1; i = i + 1 {
  #|     body
  #|   } where { item: value, other: value, }
  #| }
  #|
  #| fn simple_pattern_operator_tokens {
  #|   let _ = match value {
  #|     (_ : Int) => ()
  #|     (a, b,) => ()
  #|     1..=9 => ()
  #|     left | right as alias => ()
  #|     _ => ()
  #|   }
  #| }
  #|
  #| fn array_constr_pattern_tokens(value : Result[Int, Error]) -> Unit {
  #|   match value {
  #|     [head, .. as rest, tail,] => ()
  #|     [..@pkg.Const, .."s", ..b"bytes"] => ()
  #|     Some(a, b, ..) => ()
  #|     i32(a, b,) => ()
  #|     _ => ()
  #|   }
  #| }
  #|
  #| extern "js" fn imported_stub(x : Int) -> Int = "Math" "round"
  #|
  #| fn embedded_string_stub(x : Int) -> Int = "x + 1"
  #|
  #| fn embedded_multiline_stub(x : Int) -> Int = #|line1
  #|  #|line2
  #|

///|
let type_decl_loc_source =
  #| declare type DeclaredAlias[A, B]
  #| type AliasWithDerive[A, B] = Result[A, B] derive(Show, FromJson(prop1="foo", prop2="bar",))
  #| enum DerivedEnum[T, U] {
  #|   CaseA;
  #|   CaseB;
  #| } derive(Eq, Compare)

///|
let fun_decl_loc_source =
  #| declare fn[A, B] Owner::declared(x : A, y : B,) -> B
  #| extern "js" fn[A, B] Owner::external(x : A, y : B,) -> B = "M" "f"
  #| fn[A, B] Owner::defined(x : A, y : B,) -> B { y }

///|
let trait_decl_loc_source =
  #| declare trait Derived : SuperA + SuperB {
  #| fn[A, B] method(x : A, y : B,) -> B = _;
  #| plain(x : Int) -> Int;
  #| }

///|
let top_let_test_loc_source =
  #| declare let declared_value : Int = 1
  #| const ConstantValue : Int = 2
  #| async test "named" (x : Int, y : Int,) { x }
  #| test { () }

///|
let impl_decl_loc_source =
  #| declare impl[A, B] Trait for SelfType
  #| impl[A, B] Trait for SelfType with fn method(x : A, y : B,) -> B { y }
  #| impl[A, B] Trait with method(x : A, y : B,) -> B = "body"

///|
let enumview_loc_source =
  #| enumview[A, B] V {
  #|   C;
  #|   D;
  #| } for Source[A, B] with view(x, y,) {
  #|   C
  #| }

///|
let type_desc_loc_source =
  #| type Alias = Int
  #| enum E {
  #|   A;
  #|   B;
  #| }
  #| struct R {
  #|   x : Int;
  #|   y : String;
  #| }
  #| struct T(Int, String)
  #| extenum X {
  #|   C;
  #| }
  #| extenum X += {
  #|   D;
  #| }

///|
let tvar_binder_loc_source =
  #| fn[A : TraitA + TraitB, B] constrained(x : A) -> B { x }

///|
fn parse_loc_regression(
  parser : @parser.Parser,
) -> (@syntax.Impls, @basic.Location, @basic.Location, Json) raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    loc_regression_source,
    name="loc_regression.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail("\{to_repr(diagnostics)}")
  }
  let mut brace_group_loc = None
  let mut params_loc = None
  for impl_ in impls {
    match impl_ {
      TopLetDef(expr=Group(group=Brace, loc~, ..), ..) =>
        brace_group_loc = Some(loc)
      TopFuncDef(fun_decl~, ..) =>
        if fun_decl.name.name == "init" {
          params_loc = Some(fun_decl.params_loc)
        }
      _ => ()
    }
  }
  let json = @basic.show_loc.protect(@basic.String, fn() {
    impls.map(impl_ => impl_.json_repr()).to_json()
  })
  match (brace_group_loc, params_loc) {
    (Some(brace_group_loc), Some(params_loc)) =>
      (impls, brace_group_loc, params_loc, json)
    _ => fail("failed to find loc regression fixtures in parsed AST")
  }
}

///|
test "loc parity for brace group and empty fn params" {
  let (_, hand_group_loc, hand_params_loc, hand_json) = parse_loc_regression(
    @parser.Handrolled,
  )
  let (_, yacc_group_loc, yacc_params_loc, yacc_json) = parse_loc_regression(
    @parser.MoonYacc,
  )
  inspect(hand_group_loc, content="loc_regression.mbt-1:20-3:3")
  inspect(hand_params_loc, content="loc_regression.mbt-5:9-5:9")
  inspect(yacc_group_loc, content="loc_regression.mbt-1:20-3:3")
  inspect(yacc_params_loc, content="loc_regression.mbt-5:9-5:9")
  if hand_json != yacc_json {
    fail(
      "loc-sensitive AST mismatch between handrolled and yacc parsers.\nhandrolled:\n\{hand_json.stringify(indent=2)}\n\nyacc:\n\{yacc_json.stringify(indent=2)}",
    )
  }
}

///|
fn require_some_loc(
  value : @basic.Location?,
  context : String,
) -> @basic.Location raise Error {
  match value {
    Some(loc) => loc
    None => fail("missing loc for \{context}")
  }
}

///|
fn single_loc(
  locs : @list.List[@basic.Location],
  context : String,
) -> @basic.Location raise Error {
  match locs {
    More(loc, tail=Empty) => loc
    _ => fail("expected exactly one loc for \{context}")
  }
}

///|
fn two_locs(
  locs : @list.List[@basic.Location],
  context : String,
) -> (@basic.Location, @basic.Location) raise Error {
  match locs {
    More(first, tail=More(second, tail=Empty)) => (first, second)
    _ => fail("expected exactly two locs for \{context}")
  }
}

///|
fn three_locs(
  locs : @list.List[@basic.Location],
  context : String,
) -> (@basic.Location, @basic.Location, @basic.Location) raise Error {
  match locs {
    More(first, tail=More(second, tail=More(third, tail=Empty))) =>
      (first, second, third)
    _ => fail("expected exactly three locs for \{context}")
  }
}

///|
fn expect_no_locs(
  locs : @list.List[@basic.Location],
  context : String,
) -> Unit raise Error {
  match locs {
    Empty => ()
    _ => fail("expected no locs for \{context}")
  }
}

///|
fn sample_body(impls : @syntax.Impls) -> @syntax.Expr raise Error {
  let mut body = None
  for impl_ in impls {
    match impl_ {
      TopFuncDef(fun_decl~, decl_body=DeclBody(expr~, ..), ..) =>
        if fun_decl.name.name == "sample" {
          body = Some(expr)
        }
      _ => ()
    }
  }
  match body {
    Some(body) => body
    None => fail("failed to find sample function in loc regression fixture")
  }
}

///|
fn function_body(
  impls : @syntax.Impls,
  name : String,
) -> @syntax.Expr raise Error {
  let mut body = None
  for impl_ in impls {
    match impl_ {
      TopFuncDef(fun_decl~, decl_body=DeclBody(expr~, ..), ..) =>
        if fun_decl.name.name == name {
          body = Some(expr)
        }
      _ => ()
    }
  }
  match body {
    Some(body) => body
    None => fail("failed to find \{name} function in loc regression fixture")
  }
}

///|
fn function_decl(
  impls : @syntax.Impls,
  name : String,
) -> @syntax.FunDecl raise Error {
  let mut decl = None
  for impl_ in impls {
    match impl_ {
      TopFuncDef(fun_decl~, ..) =>
        if fun_decl.name.name == name {
          decl = Some(fun_decl)
        }
      _ => ()
    }
  }
  match decl {
    Some(decl) => decl
    None => fail("failed to find \{name} function declaration")
  }
}

///|
fn top_func_def(
  impls : @syntax.Impls,
  name : String,
) -> @syntax.Impl raise Error {
  for impl_ in impls {
    match impl_ {
      TopFuncDef(fun_decl~, ..) =>
        if fun_decl.name.name == name {
          return impl_
        }
      _ => ()
    }
  }
  fail("failed to find \{name} top-level function")
}

///|
fn type_decl(
  impls : @syntax.Impls,
  name : String,
) -> @syntax.TypeDecl raise Error {
  let mut decl = None
  for impl_ in impls {
    match impl_ {
      TopTypeDef(type_decl) =>
        if type_decl.tycon == name {
          decl = Some(type_decl)
        }
      _ => ()
    }
  }
  match decl {
    Some(decl) => decl
    None => fail("failed to find \{name} type declaration")
  }
}

///|
fn trait_decl(
  impls : @syntax.Impls,
  name : String,
) -> @syntax.TraitDecl raise Error {
  let mut decl = None
  for impl_ in impls {
    match impl_ {
      TopTrait(trait_decl) =>
        if trait_decl.name.name == name {
          decl = Some(trait_decl)
        }
      _ => ()
    }
  }
  match decl {
    Some(decl) => decl
    None => fail("failed to find \{name} trait declaration")
  }
}

///|
fn top_let_def(
  impls : @syntax.Impls,
  name : String,
) -> @syntax.Impl raise Error {
  for impl_ in impls {
    match impl_ {
      TopLetDef(binder~, ..) => if binder.name == name { return impl_ }
      _ => ()
    }
  }
  fail("failed to find \{name} top-level let declaration")
}

///|
fn check_vertical_slice_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  match sample_body(impls) {
    Let(
      expr=Function(func~, ..),
      body=Let(expr=semi_record, body=Apply(args=call_args, ..), ..),
      ..
    ) => {
      inspect(
        require_some_loc(func.fn_loc, "anonymous fn keyword"),
        content="loc_regression.mbt-10:12-10:14",
      )
      inspect(
        require_some_loc(func.lparen_loc, "anonymous fn lparen"),
        content="loc_regression.mbt-10:14-10:15",
      )
      inspect(
        require_some_loc(func.rparen_loc, "anonymous fn rparen"),
        content="loc_regression.mbt-10:36-10:37",
      )
      inspect(
        single_loc(func.comma_locs, "anonymous fn param comma"),
        content="loc_regression.mbt-10:22-10:23",
      )
      inspect(
        require_some_loc(func.return_arrow_loc, "anonymous fn return arrow"),
        content="loc_regression.mbt-10:38-10:40",
      )
      inspect(
        require_some_loc(func.body_lbrace_loc, "anonymous fn body lbrace"),
        content="loc_regression.mbt-10:45-10:46",
      )
      inspect(
        require_some_loc(func.body_rbrace_loc, "anonymous fn body rbrace"),
        content="loc_regression.mbt-16:4-16:5",
      )
      match func.parameters {
        More(
          Positional(colon_loc=first_colon_loc, ..),
          tail=More(
            Optional(question_loc~, colon_loc=second_colon_loc, equal_loc~, ..),
            tail=Empty
          )
        ) => {
          inspect(
            require_some_loc(first_colon_loc, "first positional colon"),
            content="loc_regression.mbt-10:17-10:18",
          )
          inspect(
            require_some_loc(question_loc, "optional param question"),
            content="loc_regression.mbt-10:25-10:26",
          )
          inspect(
            require_some_loc(second_colon_loc, "optional param colon"),
            content="loc_regression.mbt-10:27-10:28",
          )
          inspect(equal_loc, content="loc_regression.mbt-10:33-10:34")
        }
        _ => fail("unexpected anonymous function parameter shape")
      }
      match func.body {
        If(
          if_loc~,
          ifso=Record(
            lbrace_loc=record_lbrace_loc,
            rbrace_loc=record_rbrace_loc,
            comma_locs=record_comma_locs,
            semi_locs=record_semi_locs,
            fields=record_fields,
            ..
          ),
          then_lbrace_loc~,
          then_rbrace_loc~,
          else_loc~,
          else_lbrace_loc~,
          else_rbrace_loc~,
          ..
        ) => {
          inspect(if_loc, content="loc_regression.mbt-11:6-11:8")
          inspect(then_lbrace_loc, content="loc_regression.mbt-11:15-11:16")
          inspect(then_rbrace_loc, content="loc_regression.mbt-13:6-13:7")
          inspect(
            require_some_loc(else_loc, "if else keyword"),
            content="loc_regression.mbt-13:8-13:12",
          )
          inspect(
            require_some_loc(else_lbrace_loc, "if else lbrace"),
            content="loc_regression.mbt-13:13-13:14",
          )
          inspect(
            require_some_loc(else_rbrace_loc, "if else rbrace"),
            content="loc_regression.mbt-15:6-15:7",
          )
          inspect(record_lbrace_loc, content="loc_regression.mbt-12:8-12:9")
          inspect(record_rbrace_loc, content="loc_regression.mbt-12:21-12:22")
          inspect(
            single_loc(record_comma_locs, "record field comma"),
            content="loc_regression.mbt-12:14-12:15",
          )
          expect_no_locs(record_semi_locs, "comma separated record")
          match record_fields {
            More(
              { colon_loc: Some(first_field_colon), .. },
              tail=More({ colon_loc: Some(second_field_colon), .. }, tail=Empty)
            ) => {
              inspect(
                first_field_colon,
                content="loc_regression.mbt-12:11-12:12",
              )
              inspect(
                second_field_colon,
                content="loc_regression.mbt-12:17-12:18",
              )
            }
            _ => fail("unexpected record field shape")
          }
        }
        _ => fail("expected anonymous function body to be an if expression")
      }
      match semi_record {
        Record(
          lbrace_loc~,
          rbrace_loc~,
          comma_locs~,
          semi_locs~,
          trailing=Semi,
          fields=More({ colon_loc: None, .. }, tail=Empty),
          ..
        ) => {
          inspect(lbrace_loc, content="loc_regression.mbt-17:16-17:17")
          inspect(rbrace_loc, content="loc_regression.mbt-17:21-17:22")
          expect_no_locs(comma_locs, "semicolon record comma locs")
          inspect(
            single_loc(semi_locs, "semicolon record semi"),
            content="loc_regression.mbt-17:19-17:20",
          )
        }
        _ => fail("expected semicolon fixture to parse as a record")
      }
      match call_args {
        More(
          _,
          tail=More(
            { kind: Labelled(equal_loc=arg_equal_loc, ..), .. },
            tail=Empty
          )
        ) => inspect(arg_equal_loc, content="loc_regression.mbt-18:10-18:11")
        _ => fail("expected sample call to include one labelled argument")
      }
    }
    _ => fail("unexpected sample body shape")
  }
}

///|
test "vertical slice token locs for functions, params, if, and records" {
  check_vertical_slice_locs(@parser.Handrolled)
  check_vertical_slice_locs(@parser.MoonYacc)
}

///|
fn check_statement_expr_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  let parser_name = match parser {
    Handrolled => "handrolled"
    MoonYacc => "moonyacc"
  }
  match function_body(impls, "statements") {
    Let(
      let_loc~,
      annot_colon_loc~,
      equal_loc~,
      semi_loc~,
      body=LetMut(
        let_loc=mut_let_loc,
        mut_loc~,
        annot_colon_loc=mut_annot_colon_loc,
        equal_loc=mut_equal_loc,
        semi_loc=mut_semi_loc,
        body=Guard(
          guard_loc~,
          else_loc~,
          else_lbrace_loc~,
          else_rbrace_loc~,
          semi_loc=guard_semi_loc,
          body=Defer(
            defer_loc~,
            semi_loc=defer_semi_loc,
            body=LetFn(
              semi_loc=local_fn_semi_loc,
              body=Sequence(semi_locs=sequence_semi_locs, ..),
              ..
            ),
            ..
          ),
          ..
        ),
        ..
      ),
      ..
    ) => {
      inspect(let_loc, content="loc_regression.mbt-22:4-22:7")
      inspect(
        require_some_loc(annot_colon_loc, "let annotation colon"),
        content="loc_regression.mbt-22:10-22:11",
      )
      inspect(equal_loc, content="loc_regression.mbt-22:16-22:17")
      inspect(
        require_some_loc(semi_loc, "\{parser_name} explicit let semicolon"),
        content="loc_regression.mbt-22:19-22:20",
      )
      inspect(mut_let_loc, content="loc_regression.mbt-23:4-23:7")
      inspect(mut_loc, content="loc_regression.mbt-23:8-23:11")
      inspect(
        require_some_loc(mut_annot_colon_loc, "let mut annotation colon"),
        content="loc_regression.mbt-23:14-23:15",
      )
      inspect(mut_equal_loc, content="loc_regression.mbt-23:20-23:21")
      match mut_semi_loc {
        None => ()
        Some(loc) => fail("ASI semicolon should not have a stored loc: \{loc}")
      }
      inspect(guard_loc, content="loc_regression.mbt-24:4-24:9")
      inspect(
        require_some_loc(else_loc, "guard else keyword"),
        content="loc_regression.mbt-24:16-24:20",
      )
      inspect(
        require_some_loc(else_lbrace_loc, "guard else lbrace"),
        content="loc_regression.mbt-24:21-24:22",
      )
      inspect(
        require_some_loc(else_rbrace_loc, "guard else rbrace"),
        content="loc_regression.mbt-26:4-26:5",
      )
      inspect(
        require_some_loc(guard_semi_loc, "guard explicit semicolon"),
        content="loc_regression.mbt-26:5-26:6",
      )
      inspect(defer_loc, content="loc_regression.mbt-27:4-27:9")
      inspect(
        require_some_loc(defer_semi_loc, "defer explicit semicolon"),
        content="loc_regression.mbt-27:11-27:12",
      )
      inspect(
        require_some_loc(local_fn_semi_loc, "local fn explicit semicolon"),
        content="loc_regression.mbt-30:5-30:6",
      )
      inspect(
        single_loc(
          sequence_semi_locs, "statement expression sequence semicolon",
        ),
        content="loc_regression.mbt-31:5-31:6",
      )
    }
    _ => fail("unexpected statements function body shape")
  }
}

///|
test "statement-derived expression token locs" {
  check_statement_expr_locs(@parser.Handrolled)
  check_statement_expr_locs(@parser.MoonYacc)
}

///|
fn check_top_decl_body_stub_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  match top_func_def(impls, "sample") {
    TopFuncDef(
      equal_loc=None,
      decl_body=DeclBody(
        lbrace_loc=Some(body_lbrace_loc),
        rbrace_loc=Some(body_rbrace_loc),
        ..
      ),
      ..
    ) => {
      inspect(body_lbrace_loc, content="loc_regression.mbt-9:37-9:38")
      inspect(body_rbrace_loc, content="loc_regression.mbt-19:2-19:3")
    }
    _ => fail("expected sample function to carry body brace locs")
  }
  match top_func_def(impls, "imported_stub") {
    TopFuncDef(
      equal_loc=Some(top_equal_loc),
      decl_body=DeclStubs(
        Import(
          language_loc=Some(language_loc),
          module_name_loc~,
          func_name_loc~,
          ..
        ),
        equal_loc=decl_equal_loc
      ),
      ..
    ) => {
      inspect(top_equal_loc, content="loc_regression.mbt-285:47-285:48")
      inspect(decl_equal_loc, content="loc_regression.mbt-285:47-285:48")
      inspect(language_loc, content="loc_regression.mbt-285:9-285:13")
      inspect(module_name_loc, content="loc_regression.mbt-285:49-285:55")
      inspect(func_name_loc, content="loc_regression.mbt-285:56-285:63")
    }
    _ => fail("expected imported_stub to carry import stub locs")
  }
  match top_func_def(impls, "embedded_string_stub") {
    TopFuncDef(
      equal_loc=Some(top_equal_loc),
      decl_body=DeclStubs(
        Embedded(
          language_loc=None,
          code=CodeString(_, loc=string_loc),
          code_loc~,
          ..
        ),
        equal_loc=decl_equal_loc
      ),
      ..
    ) => {
      inspect(top_equal_loc, content="loc_regression.mbt-287:42-287:43")
      inspect(decl_equal_loc, content="loc_regression.mbt-287:42-287:43")
      inspect(string_loc, content="loc_regression.mbt-287:44-287:51")
      inspect(code_loc, content="loc_regression.mbt-287:44-287:51")
    }
    _ => fail("expected embedded_string_stub to carry string stub locs")
  }
  match top_func_def(impls, "embedded_multiline_stub") {
    TopFuncDef(
      equal_loc=Some(top_equal_loc),
      decl_body=DeclStubs(
        Embedded(
          language_loc=None,
          code=CodeMultilineString(_, string_locs~),
          code_loc~,
          ..
        ),
        equal_loc=decl_equal_loc
      ),
      ..
    ) => {
      inspect(top_equal_loc, content="loc_regression.mbt-289:45-289:46")
      inspect(decl_equal_loc, content="loc_regression.mbt-289:45-289:46")
      let (first_string_loc, second_string_loc) = two_locs(
        string_locs, "multiline embedded code string locs",
      )
      inspect(first_string_loc, content="loc_regression.mbt-289:47-289:54")
      inspect(second_string_loc, content="loc_regression.mbt-290:3-290:10")
      inspect(code_loc, content="loc_regression.mbt-289:47-290:10")
    }
    _ => fail("expected embedded_multiline_stub to carry multiline stub locs")
  }
}

///|
test "top-level declaration body and stub token locs" {
  check_top_decl_body_stub_locs(@parser.Handrolled)
  check_top_decl_body_stub_locs(@parser.MoonYacc)
}

///|
test "top-level declaration stub JSON parity" {
  let source =
    #| extern "js" fn imported_stub(x : Int) -> Int = "Math" "round"
    #| fn embedded_string_stub(x : Int) -> Int = "x + 1"
    #| fn embedded_multiline_stub(x : Int) -> Int = #|line1
    #|  #|line2
  fn parse_json(parser : @parser.Parser) -> Json raise Error {
    let (impls, diagnostics) = @parser.parse_string(
      source,
      name="top_decl_stub_tokens.mbt",
      parser~,
    )
    if !diagnostics.is_empty() {
      fail("\{to_repr(diagnostics)}")
    }
    @basic.show_loc.protect(@basic.String, fn() {
      impls.map(impl_ => impl_.json_repr()).to_json()
    })
  }

  let hand_json = parse_json(@parser.Handrolled)
  let yacc_json = parse_json(@parser.MoonYacc)
  if hand_json != yacc_json {
    fail(
      "top-level stub JSON mismatch.\nhandrolled:\n\{hand_json.stringify(indent=2)}\n\nyacc:\n\{yacc_json.stringify(indent=2)}",
    )
  }
}

///|
fn check_fun_decl_token_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    fun_decl_loc_source,
    name="fun_decl_locs.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail(diagnostics.to_json().stringify(indent=2))
  }
  let declared = function_decl(impls, "declared")
  inspect(
    require_some_loc(declared.declare_loc, "function declare keyword"),
    content="fun_decl_locs.mbt-1:2-1:9",
  )
  inspect(declared.fn_loc, content="fun_decl_locs.mbt-1:10-1:12")
  inspect(
    require_some_loc(
      declared.type_params_lbracket_loc,
      "function type params lbracket",
    ),
    content="fun_decl_locs.mbt-1:12-1:13",
  )
  inspect(
    single_loc(declared.type_param_comma_locs, "function type parameter comma"),
    content="fun_decl_locs.mbt-1:14-1:15",
  )
  inspect(
    require_some_loc(
      declared.type_params_rbracket_loc,
      "function type params rbracket",
    ),
    content="fun_decl_locs.mbt-1:17-1:18",
  )
  inspect(
    require_some_loc(declared.coloncolon_loc, "function receiver coloncolon"),
    content="fun_decl_locs.mbt-1:24-1:26",
  )
  inspect(
    require_some_loc(declared.params_lparen_loc, "function params lparen"),
    content="fun_decl_locs.mbt-1:34-1:35",
  )
  let (declared_param_comma, declared_trailing_comma) = two_locs(
    declared.param_comma_locs,
    "function parameter comma locs",
  )
  inspect(declared_param_comma, content="fun_decl_locs.mbt-1:40-1:41")
  inspect(declared_trailing_comma, content="fun_decl_locs.mbt-1:47-1:48")
  inspect(
    require_some_loc(declared.params_rparen_loc, "function params rparen"),
    content="fun_decl_locs.mbt-1:48-1:49",
  )
  inspect(
    require_some_loc(declared.return_arrow_loc, "function return arrow"),
    content="fun_decl_locs.mbt-1:50-1:52",
  )

  let external = function_decl(impls, "external")
  inspect(
    require_some_loc(external.extern_loc, "extern function keyword"),
    content="fun_decl_locs.mbt-2:2-2:8",
  )
  inspect(external.fn_loc, content="fun_decl_locs.mbt-2:14-2:16")
  inspect(
    single_loc(
      external.type_param_comma_locs,
      "extern function type parameter comma",
    ),
    content="fun_decl_locs.mbt-2:18-2:19",
  )
  inspect(
    require_some_loc(
      external.coloncolon_loc,
      "extern function receiver coloncolon",
    ),
    content="fun_decl_locs.mbt-2:28-2:30",
  )
  let (external_param_comma, external_trailing_comma) = two_locs(
    external.param_comma_locs,
    "extern function parameter comma locs",
  )
  inspect(external_param_comma, content="fun_decl_locs.mbt-2:44-2:45")
  inspect(external_trailing_comma, content="fun_decl_locs.mbt-2:51-2:52")
  inspect(
    require_some_loc(external.return_arrow_loc, "extern function return arrow"),
    content="fun_decl_locs.mbt-2:54-2:56",
  )

  let defined = function_decl(impls, "defined")
  inspect(defined.fn_loc, content="fun_decl_locs.mbt-3:2-3:4")
  inspect(
    single_loc(
      defined.type_param_comma_locs,
      "defined function type parameter comma",
    ),
    content="fun_decl_locs.mbt-3:6-3:7",
  )
  inspect(
    require_some_loc(
      defined.coloncolon_loc,
      "defined function receiver coloncolon",
    ),
    content="fun_decl_locs.mbt-3:16-3:18",
  )
  let (defined_param_comma, defined_trailing_comma) = two_locs(
    defined.param_comma_locs,
    "defined function parameter comma locs",
  )
  inspect(defined_param_comma, content="fun_decl_locs.mbt-3:31-3:32")
  inspect(defined_trailing_comma, content="fun_decl_locs.mbt-3:38-3:39")
  inspect(
    require_some_loc(defined.return_arrow_loc, "defined function return arrow"),
    content="fun_decl_locs.mbt-3:41-3:43",
  )
}

///|
test "function declaration token locs" {
  check_fun_decl_token_locs(@parser.Handrolled)
  check_fun_decl_token_locs(@parser.MoonYacc)
}

///|
fn check_trait_decl_token_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    trait_decl_loc_source,
    name="trait_decl_locs.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail(diagnostics.to_json().stringify(indent=2))
  }
  let derived = trait_decl(impls, "Derived")
  inspect(
    require_some_loc(derived.declare_loc, "trait declare keyword"),
    content="trait_decl_locs.mbt-1:2-1:9",
  )
  inspect(derived.trait_loc, content="trait_decl_locs.mbt-1:10-1:15")
  inspect(
    require_some_loc(derived.colon_loc, "trait supers colon"),
    content="trait_decl_locs.mbt-1:24-1:25",
  )
  inspect(
    single_loc(derived.super_plus_locs, "trait super plus loc"),
    content="trait_decl_locs.mbt-1:33-1:34",
  )
  inspect(derived.lbrace_loc, content="trait_decl_locs.mbt-1:42-1:43")
  let (first_method_semi, second_method_semi) = two_locs(
    derived.semi_locs,
    "trait method semicolon locs",
  )
  inspect(first_method_semi, content="trait_decl_locs.mbt-2:41-2:42")
  inspect(second_method_semi, content="trait_decl_locs.mbt-3:23-3:24")
  inspect(derived.rbrace_loc, content="trait_decl_locs.mbt-4:2-4:3")

  match derived.methods {
    More(trait_method, tail=More(plain, tail=Empty)) => {
      inspect(
        require_some_loc(trait_method.fn_loc, "trait method fn keyword"),
        content="trait_decl_locs.mbt-2:2-2:4",
      )
      inspect(
        require_some_loc(
          trait_method.type_params_lbracket_loc,
          "trait method type params lbracket",
        ),
        content="trait_decl_locs.mbt-2:4-2:5",
      )
      inspect(
        single_loc(
          trait_method.type_param_comma_locs,
          "trait method type parameter comma",
        ),
        content="trait_decl_locs.mbt-2:6-2:7",
      )
      inspect(
        require_some_loc(
          trait_method.type_params_rbracket_loc,
          "trait method type params rbracket",
        ),
        content="trait_decl_locs.mbt-2:9-2:10",
      )
      inspect(
        trait_method.params_lparen_loc,
        content="trait_decl_locs.mbt-2:17-2:18",
      )
      let (method_param_comma, method_trailing_comma) = two_locs(
        trait_method.param_comma_locs,
        "trait method parameter comma locs",
      )
      inspect(method_param_comma, content="trait_decl_locs.mbt-2:23-2:24")
      inspect(method_trailing_comma, content="trait_decl_locs.mbt-2:30-2:31")
      inspect(
        trait_method.params_rparen_loc,
        content="trait_decl_locs.mbt-2:31-2:32",
      )
      inspect(
        require_some_loc(
          trait_method.return_arrow_loc,
          "trait method return arrow",
        ),
        content="trait_decl_locs.mbt-2:33-2:35",
      )
      inspect(
        require_some_loc(
          trait_method.default_equal_loc,
          "trait method default equal",
        ),
        content="trait_decl_locs.mbt-2:38-2:39",
      )
      inspect(plain.params_lparen_loc, content="trait_decl_locs.mbt-3:7-3:8")
      inspect(plain.params_rparen_loc, content="trait_decl_locs.mbt-3:15-3:16")
      inspect(
        require_some_loc(
          plain.return_arrow_loc,
          "plain trait method return arrow",
        ),
        content="trait_decl_locs.mbt-3:17-3:19",
      )
    }
    _ => fail("expected two trait methods on Derived")
  }
}

///|
test "trait declaration and method token locs" {
  check_trait_decl_token_locs(@parser.Handrolled)
  check_trait_decl_token_locs(@parser.MoonYacc)
}

///|
fn check_top_let_test_token_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    top_let_test_loc_source,
    name="top_let_test_locs.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail(diagnostics.to_json().stringify(indent=2))
  }
  match top_let_def(impls, "declared_value") {
    TopLetDef(
      declare_loc=Some(declare_loc),
      let_or_const_loc~,
      annot_colon_loc=Some(annot_colon_loc),
      equal_loc~,
      ..
    ) => {
      inspect(declare_loc, content="top_let_test_locs.mbt-1:2-1:9")
      inspect(let_or_const_loc, content="top_let_test_locs.mbt-1:10-1:13")
      inspect(annot_colon_loc, content="top_let_test_locs.mbt-1:29-1:30")
      inspect(equal_loc, content="top_let_test_locs.mbt-1:35-1:36")
    }
    _ => fail("expected declared_value top let locs")
  }
  match top_let_def(impls, "ConstantValue") {
    TopLetDef(
      declare_loc=None,
      let_or_const_loc~,
      annot_colon_loc=Some(annot_colon_loc),
      equal_loc~,
      ..
    ) => {
      inspect(let_or_const_loc, content="top_let_test_locs.mbt-2:2-2:7")
      inspect(annot_colon_loc, content="top_let_test_locs.mbt-2:22-2:23")
      inspect(equal_loc, content="top_let_test_locs.mbt-2:28-2:29")
    }
    _ => fail("expected ConstantValue top const locs")
  }

  let mut named_test = None
  let mut anonymous_test = None
  for impl_ in impls {
    match impl_ {
      TopTest(name=Some(("named", _)), ..) => named_test = Some(impl_)
      TopTest(name=None, ..) => anonymous_test = Some(impl_)
      _ => ()
    }
  }
  match named_test {
    Some(
      TopTest(
        test_loc~,
        params_lparen_loc=Some(params_lparen_loc),
        params_rparen_loc=Some(params_rparen_loc),
        param_comma_locs~,
        body_lbrace_loc~,
        body_rbrace_loc~,
        ..
      )
    ) => {
      inspect(test_loc, content="top_let_test_locs.mbt-3:8-3:12")
      inspect(params_lparen_loc, content="top_let_test_locs.mbt-3:21-3:22")
      let (param_comma, trailing_param_comma) = two_locs(
        param_comma_locs, "top test parameter comma locs",
      )
      inspect(param_comma, content="top_let_test_locs.mbt-3:29-3:30")
      inspect(trailing_param_comma, content="top_let_test_locs.mbt-3:38-3:39")
      inspect(params_rparen_loc, content="top_let_test_locs.mbt-3:39-3:40")
      inspect(body_lbrace_loc, content="top_let_test_locs.mbt-3:41-3:42")
      inspect(body_rbrace_loc, content="top_let_test_locs.mbt-3:45-3:46")
    }
    _ => fail("expected named top test locs")
  }
  match anonymous_test {
    Some(TopTest(test_loc~, body_lbrace_loc~, body_rbrace_loc~, ..)) => {
      inspect(test_loc, content="top_let_test_locs.mbt-4:2-4:6")
      inspect(body_lbrace_loc, content="top_let_test_locs.mbt-4:7-4:8")
      inspect(body_rbrace_loc, content="top_let_test_locs.mbt-4:12-4:13")
    }
    _ => fail("expected anonymous top test locs")
  }
}

///|
test "top-level let and test token locs" {
  check_top_let_test_token_locs(@parser.Handrolled)
  check_top_let_test_token_locs(@parser.MoonYacc)
}

///|
fn check_impl_decl_token_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    impl_decl_loc_source,
    name="impl_decl_locs.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail(diagnostics.to_json().stringify(indent=2))
  }
  match impls {
    More(
      TopImplRelation(
        declare_loc=Some(declare_loc),
        impl_loc=relation_impl_loc,
        type_params_lbracket_loc=Some(relation_lbracket_loc),
        type_params_rbracket_loc=Some(relation_rbracket_loc),
        type_param_comma_locs=relation_comma_locs,
        for_loc=relation_for_loc,
        ..
      ),
      tail=More(
        TopImpl(
          impl_loc=method_impl_loc,
          type_params_lbracket_loc=Some(method_lbracket_loc),
          type_params_rbracket_loc=Some(method_rbracket_loc),
          type_param_comma_locs=method_type_param_comma_locs,
          for_loc=Some(method_for_loc),
          with_loc=method_with_loc,
          fn_loc=Some(method_fn_loc),
          params_lparen_loc=method_lparen_loc,
          params_rparen_loc=method_rparen_loc,
          param_comma_locs=method_param_comma_locs,
          return_arrow_loc=Some(method_return_arrow_loc),
          body=DeclBody(
            lbrace_loc=Some(method_body_lbrace_loc),
            rbrace_loc=Some(method_body_rbrace_loc),
            ..
          ),
          ..
        ),
        tail=More(
          TopImpl(
            impl_loc=stub_impl_loc,
            type_params_lbracket_loc=Some(stub_lbracket_loc),
            type_params_rbracket_loc=Some(stub_rbracket_loc),
            type_param_comma_locs=stub_type_param_comma_locs,
            for_loc=None,
            with_loc=stub_with_loc,
            fn_loc=None,
            params_lparen_loc=stub_lparen_loc,
            params_rparen_loc=stub_rparen_loc,
            param_comma_locs=stub_param_comma_locs,
            return_arrow_loc=Some(stub_return_arrow_loc),
            body=DeclStubs(_, equal_loc=stub_equal_loc),
            ..
          ),
          tail=Empty
        )
      )
    ) => {
      inspect(declare_loc, content="impl_decl_locs.mbt-1:2-1:9")
      inspect(relation_impl_loc, content="impl_decl_locs.mbt-1:10-1:14")
      inspect(relation_lbracket_loc, content="impl_decl_locs.mbt-1:14-1:15")
      inspect(
        single_loc(relation_comma_locs, "impl relation type parameter comma"),
        content="impl_decl_locs.mbt-1:16-1:17",
      )
      inspect(relation_rbracket_loc, content="impl_decl_locs.mbt-1:19-1:20")
      inspect(relation_for_loc, content="impl_decl_locs.mbt-1:27-1:30")

      inspect(method_impl_loc, content="impl_decl_locs.mbt-2:2-2:6")
      inspect(method_lbracket_loc, content="impl_decl_locs.mbt-2:6-2:7")
      inspect(
        single_loc(
          method_type_param_comma_locs, "impl method type parameter comma",
        ),
        content="impl_decl_locs.mbt-2:8-2:9",
      )
      inspect(method_rbracket_loc, content="impl_decl_locs.mbt-2:11-2:12")
      inspect(method_for_loc, content="impl_decl_locs.mbt-2:19-2:22")
      inspect(method_with_loc, content="impl_decl_locs.mbt-2:32-2:36")
      inspect(method_fn_loc, content="impl_decl_locs.mbt-2:37-2:39")
      inspect(method_lparen_loc, content="impl_decl_locs.mbt-2:46-2:47")
      let (method_param_comma, method_trailing_comma) = two_locs(
        method_param_comma_locs, "impl method parameter comma locs",
      )
      inspect(method_param_comma, content="impl_decl_locs.mbt-2:52-2:53")
      inspect(method_trailing_comma, content="impl_decl_locs.mbt-2:59-2:60")
      inspect(method_rparen_loc, content="impl_decl_locs.mbt-2:60-2:61")
      inspect(method_return_arrow_loc, content="impl_decl_locs.mbt-2:62-2:64")
      inspect(method_body_lbrace_loc, content="impl_decl_locs.mbt-2:67-2:68")
      inspect(method_body_rbrace_loc, content="impl_decl_locs.mbt-2:71-2:72")

      inspect(stub_impl_loc, content="impl_decl_locs.mbt-3:2-3:6")
      inspect(stub_lbracket_loc, content="impl_decl_locs.mbt-3:6-3:7")
      inspect(
        single_loc(
          stub_type_param_comma_locs, "default impl type parameter comma",
        ),
        content="impl_decl_locs.mbt-3:8-3:9",
      )
      inspect(stub_rbracket_loc, content="impl_decl_locs.mbt-3:11-3:12")
      inspect(stub_with_loc, content="impl_decl_locs.mbt-3:19-3:23")
      inspect(stub_lparen_loc, content="impl_decl_locs.mbt-3:30-3:31")
      let (stub_param_comma, stub_trailing_comma) = two_locs(
        stub_param_comma_locs, "default impl parameter comma locs",
      )
      inspect(stub_param_comma, content="impl_decl_locs.mbt-3:36-3:37")
      inspect(stub_trailing_comma, content="impl_decl_locs.mbt-3:43-3:44")
      inspect(stub_rparen_loc, content="impl_decl_locs.mbt-3:44-3:45")
      inspect(stub_return_arrow_loc, content="impl_decl_locs.mbt-3:46-3:48")
      inspect(stub_equal_loc, content="impl_decl_locs.mbt-3:51-3:52")
    }
    _ => fail("expected impl relation, regular impl, and default impl fixtures")
  }
}

///|
test "impl and impl relation token locs" {
  check_impl_decl_token_locs(@parser.Handrolled)
  check_impl_decl_token_locs(@parser.MoonYacc)
}

///|
fn check_enumview_token_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    enumview_loc_source,
    name="enumview_locs.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail(diagnostics.to_json().stringify(indent=2))
  }
  match impls {
    More(
      TopView(
        enumview_loc~,
        type_params_lbracket_loc=Some(type_params_lbracket_loc),
        type_params_rbracket_loc=Some(type_params_rbracket_loc),
        type_param_comma_locs~,
        lbrace_loc~,
        rbrace_loc~,
        constructor_semi_locs~,
        for_loc~,
        with_loc~,
        params_lparen_loc~,
        params_rparen_loc~,
        param_comma_locs~,
        body_lbrace_loc~,
        body_rbrace_loc~,
        ..
      ),
      tail=Empty
    ) => {
      inspect(enumview_loc, content="enumview_locs.mbt-1:2-1:10")
      inspect(type_params_lbracket_loc, content="enumview_locs.mbt-1:10-1:11")
      inspect(
        single_loc(type_param_comma_locs, "enumview type parameter comma"),
        content="enumview_locs.mbt-1:12-1:13",
      )
      inspect(type_params_rbracket_loc, content="enumview_locs.mbt-1:15-1:16")
      inspect(lbrace_loc, content="enumview_locs.mbt-1:19-1:20")
      let (first_constructor_semi, second_constructor_semi) = two_locs(
        constructor_semi_locs, "enumview constructor semicolon locs",
      )
      inspect(first_constructor_semi, content="enumview_locs.mbt-2:5-2:6")
      inspect(second_constructor_semi, content="enumview_locs.mbt-3:5-3:6")
      inspect(rbrace_loc, content="enumview_locs.mbt-4:2-4:3")
      inspect(for_loc, content="enumview_locs.mbt-4:4-4:7")
      inspect(with_loc, content="enumview_locs.mbt-4:21-4:25")
      inspect(params_lparen_loc, content="enumview_locs.mbt-4:30-4:31")
      let (param_comma, trailing_param_comma) = two_locs(
        param_comma_locs, "enumview parameter comma locs",
      )
      inspect(param_comma, content="enumview_locs.mbt-4:32-4:33")
      inspect(trailing_param_comma, content="enumview_locs.mbt-4:35-4:36")
      inspect(params_rparen_loc, content="enumview_locs.mbt-4:36-4:37")
      inspect(body_lbrace_loc, content="enumview_locs.mbt-4:38-4:39")
      inspect(body_rbrace_loc, content="enumview_locs.mbt-6:2-6:3")
    }
    _ => fail("expected one enumview fixture")
  }
}

///|
test "enumview token locs" {
  check_enumview_token_locs(@parser.Handrolled)
  check_enumview_token_locs(@parser.MoonYacc)
}

///|
fn check_type_desc_token_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    type_desc_loc_source,
    name="type_desc_locs.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail(diagnostics.to_json().stringify(indent=2))
  }
  match type_decl(impls, "Alias").components {
    Alias(equal_loc~, ..) =>
      inspect(equal_loc, content="type_desc_locs.mbt-1:13-1:14")
    _ => fail("expected Alias type declaration")
  }
  match type_decl(impls, "E").components {
    Variant(lbrace_loc~, rbrace_loc~, semi_locs~, ..) => {
      inspect(lbrace_loc, content="type_desc_locs.mbt-2:9-2:10")
      let (first_semi, second_semi) = two_locs(
        semi_locs, "enum constructor semicolon locs",
      )
      inspect(first_semi, content="type_desc_locs.mbt-3:5-3:6")
      inspect(second_semi, content="type_desc_locs.mbt-4:5-4:6")
      inspect(rbrace_loc, content="type_desc_locs.mbt-5:2-5:3")
    }
    _ => fail("expected variant enum declaration")
  }
  match type_decl(impls, "R").components {
    Record(lbrace_loc~, rbrace_loc~, semi_locs~, ..) => {
      inspect(lbrace_loc, content="type_desc_locs.mbt-6:11-6:12")
      let (first_semi, second_semi) = two_locs(
        semi_locs, "record field semicolon locs",
      )
      inspect(first_semi, content="type_desc_locs.mbt-7:11-7:12")
      inspect(second_semi, content="type_desc_locs.mbt-8:14-8:15")
      inspect(rbrace_loc, content="type_desc_locs.mbt-9:2-9:3")
    }
    _ => fail("expected record struct declaration")
  }
  match type_decl(impls, "T").components {
    TupleStruct(lparen_loc~, rparen_loc~, comma_locs~, ..) => {
      inspect(lparen_loc, content="type_desc_locs.mbt-10:10-10:11")
      inspect(
        single_loc(comma_locs, "tuple struct comma loc"),
        content="type_desc_locs.mbt-10:14-10:15",
      )
      inspect(rparen_loc, content="type_desc_locs.mbt-10:22-10:23")
    }
    _ => fail("expected tuple struct declaration")
  }
  let mut extensible = None
  let mut extension = None
  for impl_ in impls {
    match impl_ {
      TopTypeDef({ components: ExtensibleEnum(..), .. } as decl) =>
        extensible = Some(decl)
      TopTypeDef({ components: ExtendEnum(..), .. } as decl) =>
        extension = Some(decl)
      _ => ()
    }
  }
  match extensible {
    Some(
      {
        components: ExtensibleEnum(lbrace_loc~, rbrace_loc~, semi_locs~, ..),
        ..,
      }
    ) => {
      inspect(lbrace_loc, content="type_desc_locs.mbt-11:12-11:13")
      inspect(
        single_loc(semi_locs, "extensible enum semicolon loc"),
        content="type_desc_locs.mbt-12:5-12:6",
      )
      inspect(rbrace_loc, content="type_desc_locs.mbt-13:2-13:3")
    }
    _ => fail("expected extensible enum declaration")
  }
  match extension {
    Some(
      {
        components: ExtendEnum(
          plus_equal_loc~,
          lbrace_loc~,
          rbrace_loc~,
          semi_locs~,
          ..
        ),
        ..,
      }
    ) => {
      inspect(plus_equal_loc, content="type_desc_locs.mbt-14:12-14:14")
      inspect(lbrace_loc, content="type_desc_locs.mbt-14:15-14:16")
      inspect(
        single_loc(semi_locs, "extended enum semicolon loc"),
        content="type_desc_locs.mbt-15:5-15:6",
      )
      inspect(rbrace_loc, content="type_desc_locs.mbt-16:2-16:3")
    }
    _ => fail("expected enum extension declaration")
  }
}

///|
test "type description token locs" {
  check_type_desc_token_locs(@parser.Handrolled)
  check_type_desc_token_locs(@parser.MoonYacc)
}

///|
fn check_type_var_binder_token_locs(
  parser : @parser.Parser,
) -> Unit raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    tvar_binder_loc_source,
    name="tvar_binder_locs.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail(diagnostics.to_json().stringify(indent=2))
  }
  let constrained = function_decl(impls, "constrained")
  match constrained.quantifiers {
    More(
      { colon_loc: Some(colon_loc), plus_locs, .. },
      tail=More({ colon_loc: None, plus_locs: Empty, .. }, tail=Empty)
    ) => {
      inspect(colon_loc, content="tvar_binder_locs.mbt-1:7-1:8")
      inspect(
        single_loc(plus_locs, "type variable constraint plus loc"),
        content="tvar_binder_locs.mbt-1:16-1:17",
      )
      inspect(
        single_loc(
          constrained.type_param_comma_locs,
          "constrained type parameter comma",
        ),
        content="tvar_binder_locs.mbt-1:24-1:25",
      )
    }
    _ => fail("expected constrained type variable binders")
  }
}

///|
test "type variable binder token locs" {
  check_type_var_binder_token_locs(@parser.Handrolled)
  check_type_var_binder_token_locs(@parser.MoonYacc)
}

///|
fn check_type_decl_and_deriving_locs(
  parser : @parser.Parser,
) -> Unit raise Error {
  let (impls, diagnostics) = @parser.parse_string(
    type_decl_loc_source,
    name="type_decl_locs.mbt",
    parser~,
  )
  if !diagnostics.is_empty() {
    fail(diagnostics.to_json().stringify(indent=2))
  }
  let declared_type = type_decl(impls, "DeclaredAlias")
  inspect(
    require_some_loc(
      declared_type.declare_loc,
      "type declaration declare keyword",
    ),
    content="type_decl_locs.mbt-1:2-1:9",
  )
  inspect(declared_type.keyword_loc, content="type_decl_locs.mbt-1:10-1:14")
  inspect(
    require_some_loc(declared_type.params_lbracket_loc, "type params lbracket"),
    content="type_decl_locs.mbt-1:28-1:29",
  )
  inspect(
    single_loc(declared_type.param_comma_locs, "type parameter comma loc"),
    content="type_decl_locs.mbt-1:30-1:31",
  )
  inspect(
    require_some_loc(declared_type.params_rbracket_loc, "type params rbracket"),
    content="type_decl_locs.mbt-1:33-1:34",
  )
  let alias_decl = type_decl(impls, "AliasWithDerive")
  inspect(alias_decl.keyword_loc, content="type_decl_locs.mbt-2:2-2:6")
  inspect(
    require_some_loc(alias_decl.params_lbracket_loc, "alias params lbracket"),
    content="type_decl_locs.mbt-2:22-2:23",
  )
  inspect(
    single_loc(alias_decl.param_comma_locs, "alias parameter comma loc"),
    content="type_decl_locs.mbt-2:24-2:25",
  )
  inspect(
    require_some_loc(alias_decl.params_rbracket_loc, "alias params rbracket"),
    content="type_decl_locs.mbt-2:27-2:28",
  )
  inspect(
    require_some_loc(alias_decl.derive_loc, "derive keyword"),
    content="type_decl_locs.mbt-2:44-2:50",
  )
  inspect(
    require_some_loc(alias_decl.derive_lparen_loc, "derive lparen"),
    content="type_decl_locs.mbt-2:50-2:51",
  )
  inspect(
    single_loc(alias_decl.derive_comma_locs, "derive directive comma"),
    content="type_decl_locs.mbt-2:55-2:56",
  )
  inspect(
    require_some_loc(alias_decl.derive_rparen_loc, "derive rparen"),
    content="type_decl_locs.mbt-2:92-2:93",
  )
  match alias_decl.deriving {
    More(
      {
        args_lparen_loc: None,
        args_rparen_loc: None,
        arg_comma_locs: Empty,
        ..,
      },
      tail=More(
        {
          args_lparen_loc: Some(args_lparen_loc),
          args_rparen_loc: Some(args_rparen_loc),
          arg_comma_locs,
          ..,
        },
        tail=Empty
      )
    ) => {
      inspect(args_lparen_loc, content="type_decl_locs.mbt-2:65-2:66")
      let (arg_comma, trailing_arg_comma) = two_locs(
        arg_comma_locs, "derive directive argument comma locs",
      )
      inspect(arg_comma, content="type_decl_locs.mbt-2:77-2:78")
      inspect(trailing_arg_comma, content="type_decl_locs.mbt-2:90-2:91")
      inspect(args_rparen_loc, content="type_decl_locs.mbt-2:91-2:92")
    }
    _ => fail("expected two deriving directives on AliasWithDerive")
  }

  let enum_decl = type_decl(impls, "DerivedEnum")
  inspect(enum_decl.keyword_loc, content="type_decl_locs.mbt-3:2-3:6")
  inspect(
    require_some_loc(enum_decl.params_lbracket_loc, "enum params lbracket"),
    content="type_decl_locs.mbt-3:18-3:19",
  )
  inspect(
    single_loc(enum_decl.param_comma_locs, "enum parameter comma loc"),
    content="type_decl_locs.mbt-3:20-3:21",
  )
  inspect(
    require_some_loc(enum_decl.params_rbracket_loc, "enum params rbracket"),
    content="type_decl_locs.mbt-3:23-3:24",
  )
  inspect(
    require_some_loc(enum_decl.derive_loc, "enum derive keyword"),
    content="type_decl_locs.mbt-6:4-6:10",
  )
  inspect(
    single_loc(enum_decl.derive_comma_locs, "enum derive directive comma"),
    content="type_decl_locs.mbt-6:13-6:14",
  )
}

///|
test "type declaration and deriving token locs" {
  check_type_decl_and_deriving_locs(@parser.Handrolled)
  check_type_decl_and_deriving_locs(@parser.MoonYacc)
}

///|
fn check_type_token_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  let decl = function_decl(impls, "type_tokens")
  match decl.decl_params {
    Some(
      More(
        Positional(
          ty=Some(
            Name(
              constr_id={ id: Dot(pkg_loc~, dot_id_loc~, ..), .. },
              type_args_lbracket_loc=Some(type_args_lbracket_loc),
              type_args_rbracket_loc=Some(type_args_rbracket_loc),
              type_arg_comma_locs~,
              ..
            )
          ),
          colon_loc=Some(first_colon_loc),
          ..
        ),
        tail=More(
          Positional(
            ty=Some(Object(amp_loc=object_amp_loc, ..)),
            colon_loc=Some(second_colon_loc),
            ..
          ),
          tail=More(
            Positional(
              ty=Some(
                Arrow(
                  lparen_loc=arrow_lparen_loc,
                  rparen_loc=arrow_rparen_loc,
                  arrow_loc~,
                  comma_locs=arrow_comma_locs,
                  err=ErrorType(raise_loc=arrow_raise_loc, ..),
                  ..
                )
              ),
              colon_loc=Some(third_colon_loc),
              ..
            ),
            tail=More(
              Positional(
                ty=Some(
                  Tuple(
                    lparen_loc=tuple_lparen_loc,
                    rparen_loc=tuple_rparen_loc,
                    comma_locs=tuple_comma_locs,
                    ..
                  )
                ),
                colon_loc=Some(fourth_colon_loc),
                ..
              ),
              tail=Empty
            )
          )
        )
      )
    ) => {
      inspect(first_colon_loc, content="loc_regression.mbt-202:14-202:15")
      inspect(pkg_loc, content="loc_regression.mbt-202:16-202:20")
      inspect(dot_id_loc, content="loc_regression.mbt-202:21-202:25")
      inspect(
        type_args_lbracket_loc,
        content="loc_regression.mbt-202:25-202:26",
      )
      inspect(
        single_loc(type_arg_comma_locs, "type argument comma loc"),
        content="loc_regression.mbt-202:29-202:30",
      )
      inspect(
        type_args_rbracket_loc,
        content="loc_regression.mbt-202:37-202:38",
      )
      inspect(second_colon_loc, content="loc_regression.mbt-203:17-203:18")
      inspect(object_amp_loc, content="loc_regression.mbt-203:19-203:20")
      inspect(third_colon_loc, content="loc_regression.mbt-204:13-204:14")
      inspect(arrow_lparen_loc, content="loc_regression.mbt-204:15-204:16")
      inspect(
        single_loc(arrow_comma_locs, "function type argument comma loc"),
        content="loc_regression.mbt-204:19-204:20",
      )
      inspect(arrow_rparen_loc, content="loc_regression.mbt-204:27-204:28")
      inspect(arrow_loc, content="loc_regression.mbt-204:29-204:31")
      inspect(arrow_raise_loc, content="loc_regression.mbt-204:37-204:42")
      inspect(fourth_colon_loc, content="loc_regression.mbt-205:16-205:17")
      inspect(tuple_lparen_loc, content="loc_regression.mbt-205:18-205:19")
      inspect(
        single_loc(tuple_comma_locs, "tuple type comma loc"),
        content="loc_regression.mbt-205:22-205:23",
      )
      inspect(tuple_rparen_loc, content="loc_regression.mbt-205:30-205:31")
    }
    _ => fail("unexpected type_tokens parameter shape")
  }
  match decl.error_type {
    MaybeError(raise_loc~, question_loc~, loc~, ..) => {
      inspect(raise_loc, content="loc_regression.mbt-206:12-206:17")
      inspect(question_loc, content="loc_regression.mbt-206:18-206:19")
      inspect(loc, content="loc_regression.mbt-206:12-206:19")
    }
    _ => fail("expected type_tokens to use raise ?")
  }
}

///|
test "type token locs for shared type nodes" {
  check_type_token_locs(@parser.Handrolled)
  check_type_token_locs(@parser.MoonYacc)
}

///|
fn check_shared_decl_component_locs(
  parser : @parser.Parser,
) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  let error_decl = type_decl(impls, "SharedError")
  match error_decl.components {
    Error(EnumPayload(constrs~, lbrace_loc~, rbrace_loc~, semi_locs~)) => {
      inspect(lbrace_loc, content="loc_regression.mbt-210:23-210:24")
      inspect(rbrace_loc, content="loc_regression.mbt-213:2-213:3")
      inspect(semi_locs.length(), content="2")
      inspect(
        semi_locs.head().unwrap(),
        content="loc_regression.mbt-211:42-211:43",
      )
      inspect(
        semi_locs.last().unwrap(),
        content="loc_regression.mbt-212:9-212:10",
      )
      guard constrs is More(first_constr, tail=More(second_constr, tail=Empty)) else {
        fail("expected two shared error constructors")
      }
      match first_constr {
        {
          name: { name: "Failure", loc: failure_name_loc },
          args: Some(
            More(
              {
                mut_loc: Some(param_mut_loc),
                post_label_loc: Some(param_post_label_loc),
                colon_loc: Some(param_colon_loc),
                label: Some(param_label),
                ..,
              },
              tail=More(
                {
                  mut_loc: None,
                  post_label_loc: None,
                  colon_loc: None,
                  label: None,
                  ..,
                },
                tail=Empty
              )
            )
          ),
          tag: Some((tag, tag_loc)),
          args_lparen_loc: Some(args_lparen_loc),
          args_rparen_loc: Some(args_rparen_loc),
          arg_comma_locs,
          tag_equal_loc: Some(tag_equal_loc),
          ..,
        } => {
          inspect(failure_name_loc, content="loc_regression.mbt-211:4-211:11")
          inspect(args_lparen_loc, content="loc_regression.mbt-211:11-211:12")
          inspect(param_mut_loc, content="loc_regression.mbt-211:12-211:15")
          inspect(param_label.loc, content="loc_regression.mbt-211:16-211:22")
          inspect(
            param_post_label_loc,
            content="loc_regression.mbt-211:16-211:23",
          )
          inspect(param_colon_loc, content="loc_regression.mbt-211:24-211:25")
          inspect(
            single_loc(arg_comma_locs, "constructor argument comma loc"),
            content="loc_regression.mbt-211:32-211:33",
          )
          inspect(args_rparen_loc, content="loc_regression.mbt-211:37-211:38")
          inspect(tag_equal_loc, content="loc_regression.mbt-211:39-211:40")
          inspect(tag, content="7")
          inspect(tag_loc, content="loc_regression.mbt-211:41-211:42")
        }
        _ => fail("unexpected shared constructor shape")
      }
      match second_constr {
        {
          name: { name: "Empty", loc: empty_name_loc },
          args: None,
          args_lparen_loc: None,
          args_rparen_loc: None,
          tag_equal_loc: None,
          ..,
        } => inspect(empty_name_loc, content="loc_regression.mbt-212:4-212:9")
        _ => fail("unexpected empty constructor shape")
      }
    }
    _ => fail("expected SharedError to be a payload suberror")
  }
  let record_decl = type_decl(impls, "SharedRecord")
  match record_decl.components {
    Record(fields~, ..) => {
      guard fields is More(first_field, tail=More(second_field, tail=Empty)) else {
        fail("expected two shared record fields")
      }
      match first_field {
        {
          mut_loc: Some(field_mut_loc),
          colon_loc: first_colon_loc,
          name: { loc: first_name_loc, .. },
          ..,
        } => {
          inspect(field_mut_loc, content="loc_regression.mbt-216:4-216:7")
          inspect(first_name_loc, content="loc_regression.mbt-216:8-216:13")
          inspect(first_colon_loc, content="loc_regression.mbt-216:14-216:15")
        }
        _ => fail("unexpected first field shape")
      }
      match second_field {
        {
          mut_loc: None,
          colon_loc: second_colon_loc,
          name: { loc: second_name_loc, .. },
          ..,
        } => {
          inspect(second_name_loc, content="loc_regression.mbt-217:4-217:10")
          inspect(second_colon_loc, content="loc_regression.mbt-217:11-217:12")
        }
        _ => fail("unexpected second field shape")
      }
    }
    _ => fail("expected SharedRecord to be a record")
  }
}

///|
test "shared declaration component token locs" {
  check_shared_decl_component_locs(@parser.Handrolled)
  check_shared_decl_component_locs(@parser.MoonYacc)
}

///|
fn check_pattern_argument_token_locs(
  parser : @parser.Parser,
) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  match function_body(impls, "constructor_pattern_tokens") {
    Match(
      cases=More(
        {
          pattern: Constr(
            args=Some(
              More(
                {
                  kind: Labelled(equal_loc=kind_equal_loc, ..),
                  equal_loc: Some(arg_equal_loc),
                  ..,
                },
                tail=Empty
              )
            ),
            ..
          ),
          ..,
        },
        ..
      ),
      ..
    ) => {
      inspect(kind_equal_loc, content="loc_regression.mbt-222:16-222:17")
      inspect(arg_equal_loc, content="loc_regression.mbt-222:16-222:17")
    }
    _ => fail("expected labelled constructor pattern argument")
  }
}

///|
test "pattern argument token locs" {
  check_pattern_argument_token_locs(@parser.Handrolled)
  check_pattern_argument_token_locs(@parser.MoonYacc)
}

///|
fn check_constructor_extra_info_locs(
  parser : @parser.Parser,
) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  match function_body(impls, "constructor_extra_tokens") {
    Let(
      expr=Constr(
        constr={ extra_info: Package(pkg_loc~, dot_constr_loc~, ..), .. },
        ..
      ),
      body=Let(
        expr=Constr(
          constr={
            extra_info: TypeName(coloncolon_loc=type_coloncolon_loc, ..),
            ..,
          },
          ..
        ),
        body=Let(
          expr=Constr(
            constr={
              extra_info: TypeNameWithConstrPackage(
                coloncolon_loc=pkg_coloncolon_loc,
                pkg_loc=type_pkg_loc,
                dot_constr_loc=type_dot_constr_loc,
                ..
              ),
              ..,
            },
            ..
          ),
          ..
        ),
        ..
      ),
      ..
    ) => {
      inspect(pkg_loc, content="loc_regression.mbt-228:12-228:16")
      inspect(dot_constr_loc, content="loc_regression.mbt-228:17-228:23")
      inspect(type_coloncolon_loc, content="loc_regression.mbt-229:16-229:18")
      inspect(pkg_coloncolon_loc, content="loc_regression.mbt-230:16-230:18")
      inspect(type_pkg_loc, content="loc_regression.mbt-230:18-230:22")
      inspect(type_dot_constr_loc, content="loc_regression.mbt-230:23-230:29")
    }
    _ => fail("expected constructor extra-info fixtures")
  }
}

///|
test "constructor extra-info token locs" {
  check_constructor_extra_info_locs(@parser.Handrolled)
  check_constructor_extra_info_locs(@parser.MoonYacc)
}

///|
fn check_alias_target_token_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  let mut using_info = None
  for impl_ in impls {
    match impl_ {
      TopUsing(
        pkg={ name: "pkg", .. },
        names=using_names,
        using_loc~,
        lbrace_loc~,
        rbrace_loc~,
        comma_locs~,
        ..
      ) =>
        using_info = Some(
          (using_names, using_loc, lbrace_loc, rbrace_loc, comma_locs),
        )
      _ => ()
    }
  }
  match using_info {
    Some(
      (
        More(
          (value_target, Value),
          tail=More(
            (type_target, Type(loc=type_keyword_loc)),
            tail=More((trait_target, Trait(loc=trait_keyword_loc)), tail=Empty)
          )
        ),
        using_loc,
        lbrace_loc,
        rbrace_loc,
        comma_locs,
      )
    ) => {
      inspect(using_loc, content="loc_regression.mbt-233:2-233:7")
      inspect(lbrace_loc, content="loc_regression.mbt-233:13-233:14")
      inspect(type_keyword_loc, content="loc_regression.mbt-235:4-235:8")
      inspect(trait_keyword_loc, content="loc_regression.mbt-236:4-236:9")
      let (value_comma, type_comma, trait_comma) = three_locs(
        comma_locs, "using declaration comma locs",
      )
      inspect(value_comma, content="loc_regression.mbt-234:32-234:33")
      inspect(type_comma, content="loc_regression.mbt-235:33-235:34")
      inspect(trait_comma, content="loc_regression.mbt-236:36-236:37")
      inspect(rbrace_loc, content="loc_regression.mbt-237:2-237:3")
      inspect(
        require_some_loc(value_target.as_loc, "value alias as keyword"),
        content="loc_regression.mbt-234:16-234:18",
      )
      inspect(
        require_some_loc(type_target.as_loc, "type alias as keyword"),
        content="loc_regression.mbt-235:19-235:21",
      )
      inspect(
        require_some_loc(trait_target.as_loc, "trait alias as keyword"),
        content="loc_regression.mbt-236:21-236:23",
      )
    }
    _ => fail("expected using alias target fixtures")
  }
}

///|
test "alias target token locs" {
  check_alias_target_token_locs(@parser.Handrolled)
  check_alias_target_token_locs(@parser.MoonYacc)
}

///|
fn check_lex_regex_pattern_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  match function_body(impls, "lex_regex_pattern_tokens") {
    Let(
      expr=LexMatch(
        cases=More(
          {
            pat: More(
              Pattern(Sequence(semi_locs=lex_sequence_semi_locs, ..)),
              tail=More(Pattern(Alias(as_loc=lex_alias_as_loc, ..)), tail=Empty)
            ),
            if_loc: Some(lex_case_if_loc),
            fat_arrow_loc: Some(lex_case_fat_arrow_loc),
            ..,
          },
          tail=More({ ellipsis_loc: Some(ellipsis_loc), .. }, tail=Empty)
        ),
        ..
      ),
      body=Let(
        expr=RegexMatch(
          pat=Alternation(
            pat1=Sequence(plus_loc=regex_plus_loc, ..),
            bar_loc=regex_bar_loc,
            ..
          ),
          ..
        ),
        body=Let(
          expr=RegexMatch(pat=Alias(as_loc=regex_alias_as_loc, ..), ..),
          ..
        ),
        ..
      ),
      ..
    ) => {
      inspect(
        single_loc(lex_sequence_semi_locs, "lex pattern sequence semicolon"),
        content="loc_regression.mbt-241:12-241:13",
      )
      inspect(lex_alias_as_loc, content="loc_regression.mbt-241:27-241:29")
      inspect(lex_case_if_loc, content="loc_regression.mbt-241:36-241:38")
      inspect(
        lex_case_fat_arrow_loc,
        content="loc_regression.mbt-241:44-241:46",
      )
      inspect(ellipsis_loc, content="loc_regression.mbt-242:6-242:9")
      inspect(regex_plus_loc, content="loc_regression.mbt-244:28-244:29")
      inspect(regex_bar_loc, content="loc_regression.mbt-244:36-244:37")
      inspect(regex_alias_as_loc, content="loc_regression.mbt-245:28-245:30")
    }
    _ => fail("expected lex/regex pattern token loc fixtures")
  }
}

///|
test "lex and regex pattern token locs" {
  check_lex_regex_pattern_locs(@parser.Handrolled)
  check_lex_regex_pattern_locs(@parser.MoonYacc)
}

///|
fn check_pattern_container_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  match function_body(impls, "pattern_container_tokens") {
    Let(
      expr=Match(
        cases=More(
          {
            pattern: Record(
              fields=More(
                { colon_loc: Some(field_colon_loc), .. },
                tail=More({ colon_loc: None, .. }, tail=Empty)
              ),
              lbrace_loc=record_lbrace_loc,
              rbrace_loc=record_rbrace_loc,
              comma_locs=record_comma_locs,
              dotdot_loc=Some(record_dotdot_loc),
              ..
            ),
            ..,
          },
          ..
        ),
        ..
      ),
      body=Let(
        expr=Match(
          cases=More(
            {
              pattern: Map(
                elems=More(
                  {
                    question_loc: Some(map_question_loc),
                    colon_loc: first_map_colon_loc,
                    ..,
                  },
                  tail=More({ colon_loc: second_map_colon_loc, .. }, tail=Empty)
                ),
                lbrace_loc=map_lbrace_loc,
                rbrace_loc=map_rbrace_loc,
                comma_locs=map_comma_locs,
                dotdot_loc=Some(map_dotdot_loc),
                ..
              ),
              ..,
            },
            ..
          ),
          ..
        ),
        ..
      ),
      ..
    ) => {
      inspect(record_lbrace_loc, content="loc_regression.mbt-250:6-250:7")
      inspect(field_colon_loc, content="loc_regression.mbt-250:13-250:14")
      let (record_first_comma, record_dotdot_comma) = two_locs(
        record_comma_locs, "record pattern comma locs",
      )
      inspect(record_first_comma, content="loc_regression.mbt-250:16-250:17")
      inspect(record_dotdot_comma, content="loc_regression.mbt-250:24-250:25")
      inspect(record_dotdot_loc, content="loc_regression.mbt-250:26-250:28")
      inspect(record_rbrace_loc, content="loc_regression.mbt-250:29-250:30")
      inspect(map_lbrace_loc, content="loc_regression.mbt-254:6-254:7")
      inspect(map_question_loc, content="loc_regression.mbt-254:11-254:12")
      inspect(first_map_colon_loc, content="loc_regression.mbt-254:12-254:13")
      inspect(second_map_colon_loc, content="loc_regression.mbt-254:20-254:21")
      let (map_first_comma, map_dotdot_comma) = two_locs(
        map_comma_locs, "map pattern comma locs",
      )
      inspect(map_first_comma, content="loc_regression.mbt-254:15-254:16")
      inspect(map_dotdot_comma, content="loc_regression.mbt-254:23-254:24")
      inspect(map_dotdot_loc, content="loc_regression.mbt-254:25-254:27")
      inspect(map_rbrace_loc, content="loc_regression.mbt-254:28-254:29")
    }
    _ => fail("expected record and map pattern container fixtures")
  }
}

///|
test "record and map pattern container token locs" {
  check_pattern_container_locs(@parser.Handrolled)
  check_pattern_container_locs(@parser.MoonYacc)
}

///|
fn check_where_clause_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  match function_body(impls, "where_clause_tokens") {
    For(
      where_clause=Some({ where_loc, lbrace_loc, rbrace_loc, comma_locs, .. }),
      ..
    ) => {
      inspect(where_loc, content="loc_regression.mbt-262:6-262:11")
      inspect(lbrace_loc, content="loc_regression.mbt-262:12-262:13")
      let (first_comma, trailing_comma) = two_locs(
        comma_locs, "where clause comma locs",
      )
      inspect(first_comma, content="loc_regression.mbt-262:25-262:26")
      inspect(trailing_comma, content="loc_regression.mbt-262:39-262:40")
      inspect(rbrace_loc, content="loc_regression.mbt-262:41-262:42")
    }
    _ => fail("expected for expression with where clause fixture")
  }
}

///|
test "where clause token locs" {
  check_where_clause_locs(@parser.Handrolled)
  check_where_clause_locs(@parser.MoonYacc)
}

///|
fn check_simple_pattern_operator_locs(
  parser : @parser.Parser,
) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  match function_body(impls, "simple_pattern_operator_tokens") {
    Let(
      expr=Match(
        cases=More(
          {
            pattern: Constraint(
              lparen_loc=Some(constraint_lparen_loc),
              colon_loc=constraint_colon_loc,
              rparen_loc=Some(constraint_rparen_loc),
              ..
            ),
            ..,
          },
          tail=More(
            {
              pattern: Tuple(
                lparen_loc=tuple_lparen_loc,
                rparen_loc=tuple_rparen_loc,
                comma_locs=tuple_comma_locs,
                ..
              ),
              ..,
            },
            tail=More(
              { pattern: Range(range_op_loc~, ..), .. },
              tail=More(
                { pattern: Alias(pat=Or(bar_loc~, ..), as_loc~, ..), .. },
                ..
              )
            )
          )
        ),
        ..
      ),
      ..
    ) => {
      inspect(constraint_lparen_loc, content="loc_regression.mbt-267:6-267:7")
      inspect(constraint_colon_loc, content="loc_regression.mbt-267:9-267:10")
      inspect(constraint_rparen_loc, content="loc_regression.mbt-267:14-267:15")
      inspect(tuple_lparen_loc, content="loc_regression.mbt-268:6-268:7")
      let (tuple_first_comma, tuple_trailing_comma) = two_locs(
        tuple_comma_locs, "tuple pattern comma locs",
      )
      inspect(tuple_first_comma, content="loc_regression.mbt-268:8-268:9")
      inspect(tuple_trailing_comma, content="loc_regression.mbt-268:11-268:12")
      inspect(tuple_rparen_loc, content="loc_regression.mbt-268:12-268:13")
      inspect(range_op_loc, content="loc_regression.mbt-269:7-269:10")
      inspect(bar_loc, content="loc_regression.mbt-270:11-270:12")
      inspect(as_loc, content="loc_regression.mbt-270:19-270:21")
    }
    _ => fail("expected simple pattern operator fixtures")
  }
}

///|
test "simple pattern operator token locs" {
  check_simple_pattern_operator_locs(@parser.Handrolled)
  check_simple_pattern_operator_locs(@parser.MoonYacc)
}

///|
fn check_array_constructor_pattern_locs(
  parser : @parser.Parser,
) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  match function_body(impls, "array_constr_pattern_tokens") {
    Match(
      cases=More(
        {
          pattern: Array(
            pats=Open(
              _,
              _,
              BinderAs(
                binder={ name: "rest", .. },
                dotdot_loc=open_dotdot_loc,
                as_loc=open_as_loc
              ),
              left_comma_locs=open_left_comma_locs,
              dotdot_comma_loc=Some(open_dotdot_comma_loc),
              right_comma_locs=open_right_comma_locs
            ),
            lbracket_loc=open_lbracket_loc,
            rbracket_loc=open_rbracket_loc,
            ..
          ),
          ..,
        },
        tail=More(
          {
            pattern: Array(
              pats=Closed(
                More(
                  ConstSpread(
                    dotdot_loc=const_dotdot_loc,
                    pkg_loc=Some(const_pkg_loc),
                    dot_uident_loc=Some(const_dot_uident_loc),
                    ..
                  ),
                  tail=More(
                    StringSpread(
                      dotdot_loc=string_dotdot_loc,
                      loc=string_loc,
                      ..
                    ),
                    tail=More(
                      BytesSpread(
                        dotdot_loc=bytes_dotdot_loc,
                        loc=bytes_loc,
                        ..
                      ),
                      tail=Empty
                    )
                  )
                ),
                comma_locs=spread_comma_locs
              ),
              lbracket_loc=spread_lbracket_loc,
              rbracket_loc=spread_rbracket_loc,
              ..
            ),
            ..,
          },
          tail=More(
            {
              pattern: Constr(
                args_lparen_loc=Some(constr_lparen_loc),
                args_rparen_loc=Some(constr_rparen_loc),
                arg_comma_locs=constr_comma_locs,
                open_dotdot_loc=Some(constr_dotdot_loc),
                ..
              ),
              ..,
            },
            tail=More(
              {
                pattern: SpecialConstr(
                  lparen_loc=special_lparen_loc,
                  rparen_loc=special_rparen_loc,
                  comma_locs=special_comma_locs,
                  ..
                ),
                ..,
              },
              ..
            )
          )
        )
      ),
      ..
    ) => {
      inspect(open_lbracket_loc, content="loc_regression.mbt-277:6-277:7")
      inspect(open_dotdot_loc, content="loc_regression.mbt-277:13-277:15")
      inspect(open_as_loc, content="loc_regression.mbt-277:16-277:18")
      inspect(
        single_loc(open_left_comma_locs, "open array left comma"),
        content="loc_regression.mbt-277:11-277:12",
      )
      inspect(open_dotdot_comma_loc, content="loc_regression.mbt-277:23-277:24")
      inspect(
        single_loc(open_right_comma_locs, "open array right comma"),
        content="loc_regression.mbt-277:29-277:30",
      )
      inspect(open_rbracket_loc, content="loc_regression.mbt-277:30-277:31")
      inspect(spread_lbracket_loc, content="loc_regression.mbt-278:6-278:7")
      inspect(const_dotdot_loc, content="loc_regression.mbt-278:7-278:9")
      inspect(const_pkg_loc, content="loc_regression.mbt-278:9-278:13")
      inspect(const_dot_uident_loc, content="loc_regression.mbt-278:14-278:19")
      inspect(string_dotdot_loc, content="loc_regression.mbt-278:21-278:23")
      inspect(string_loc, content="loc_regression.mbt-278:23-278:26")
      inspect(bytes_dotdot_loc, content="loc_regression.mbt-278:28-278:30")
      inspect(bytes_loc, content="loc_regression.mbt-278:30-278:38")
      let (spread_first_comma, spread_second_comma) = two_locs(
        spread_comma_locs, "spread array comma locs",
      )
      inspect(spread_first_comma, content="loc_regression.mbt-278:19-278:20")
      inspect(spread_second_comma, content="loc_regression.mbt-278:26-278:27")
      inspect(spread_rbracket_loc, content="loc_regression.mbt-278:38-278:39")
      inspect(constr_lparen_loc, content="loc_regression.mbt-279:10-279:11")
      inspect(constr_rparen_loc, content="loc_regression.mbt-279:19-279:20")
      let (constr_first_comma, constr_second_comma) = two_locs(
        constr_comma_locs, "constructor pattern comma locs",
      )
      inspect(constr_first_comma, content="loc_regression.mbt-279:12-279:13")
      inspect(constr_second_comma, content="loc_regression.mbt-279:15-279:16")
      inspect(constr_dotdot_loc, content="loc_regression.mbt-279:17-279:19")
      inspect(special_lparen_loc, content="loc_regression.mbt-280:9-280:10")
      inspect(special_rparen_loc, content="loc_regression.mbt-280:15-280:16")
      let (special_first_comma, special_trailing_comma) = two_locs(
        special_comma_locs, "special constructor pattern comma locs",
      )
      inspect(special_first_comma, content="loc_regression.mbt-280:11-280:12")
      inspect(
        special_trailing_comma,
        content="loc_regression.mbt-280:14-280:15",
      )
    }
    _ => fail("expected array and constructor pattern fixtures")
  }
}

///|
test "array and constructor pattern token locs" {
  check_array_constructor_pattern_locs(@parser.Handrolled)
  check_array_constructor_pattern_locs(@parser.MoonYacc)
}

///|
test "array and constructor pattern JSON parity" {
  let source =
    #| fn array_constr_pattern_tokens(value : Result[Int, Error]) -> Unit {
    #|   match value {
    #|     [head, .. as rest, tail,] => ()
    #|     [..@pkg.Const, .."s", ..b"bytes"] => ()
    #|     Some(a, b, ..) => ()
    #|     i32(a, b,) => ()
    #|     _ => ()
    #|   }
    #| }
  fn parse_json(parser : @parser.Parser) -> Json raise Error {
    let (impls, diagnostics) = @parser.parse_string(
      source,
      name="array_constr_pattern_tokens.mbt",
      parser~,
    )
    if !diagnostics.is_empty() {
      fail("\{to_repr(diagnostics)}")
    }
    @basic.show_loc.protect(@basic.String, fn() {
      impls.map(impl_ => impl_.json_repr()).to_json()
    })
  }

  let hand_json = parse_json(@parser.Handrolled)
  let yacc_json = parse_json(@parser.MoonYacc)
  if hand_json != yacc_json {
    fail(
      "array/constructor pattern JSON mismatch.\nhandrolled:\n\{hand_json.stringify(indent=2)}\n\nyacc:\n\{yacc_json.stringify(indent=2)}",
    )
  }
}

///|
test "simple pattern operator JSON parity" {
  let source =
    #| fn simple_pattern_operator_tokens {
    #|   let _ = match value {
    #|     (_ : Int) => ()
    #|     (a, b,) => ()
    #|     1..=9 => ()
    #|     left | right as alias => ()
    #|     _ => ()
    #|   }
    #| }
  fn parse_json(parser : @parser.Parser) -> Json raise Error {
    let (impls, diagnostics) = @parser.parse_string(
      source,
      name="simple_pattern_operator_tokens.mbt",
      parser~,
    )
    if !diagnostics.is_empty() {
      fail("\{to_repr(diagnostics)}")
    }
    @basic.show_loc.protect(@basic.String, fn() {
      impls.map(impl_ => impl_.json_repr()).to_json()
    })
  }

  let hand_json = parse_json(@parser.Handrolled)
  let yacc_json = parse_json(@parser.MoonYacc)
  if hand_json != yacc_json {
    fail(
      "simple pattern JSON mismatch.\nhandrolled:\n\{hand_json.stringify(indent=2)}\n\nyacc:\n\{yacc_json.stringify(indent=2)}",
    )
  }
}

///|
fn check_expression_token_locs(parser : @parser.Parser) -> Unit raise Error {
  let (impls, _, _, _) = parse_loc_regression(parser)
  match function_body(impls, "call_tokens") {
    Apply(lparen_loc~, rparen_loc~, comma_locs~, ..) => {
      inspect(lparen_loc, content="loc_regression.mbt-36:6-36:7")
      inspect(rparen_loc, content="loc_regression.mbt-36:12-36:13")
      let (first_comma, trailing_comma) = two_locs(
        comma_locs, "call comma locs",
      )
      inspect(first_comma, content="loc_regression.mbt-36:8-36:9")
      inspect(trailing_comma, content="loc_regression.mbt-36:11-36:12")
    }
    _ => fail("expected call_tokens body to be Apply")
  }
  match function_body(impls, "array_tokens") {
    ArraySpread(elems~, lbracket_loc~, rbracket_loc~, comma_locs~, ..) => {
      inspect(lbracket_loc, content="loc_regression.mbt-40:4-40:5")
      inspect(rbracket_loc, content="loc_regression.mbt-40:15-40:16")
      let (first_comma, second_comma) = two_locs(comma_locs, "array comma locs")
      inspect(first_comma, content="loc_regression.mbt-40:6-40:7")
      inspect(second_comma, content="loc_regression.mbt-40:12-40:13")
      match elems {
        More(_, tail=More(Spread(dotdot_loc~, ..), tail=More(_, tail=Empty))) =>
          inspect(dotdot_loc, content="loc_regression.mbt-40:8-40:10")
        _ => fail("expected spread element in array_tokens")
      }
    }
    _ => fail("expected array_tokens body to be ArraySpread")
  }
  match function_body(impls, "array_get_tokens") {
    ArrayGet(lbracket_loc~, rbracket_loc~, ..) => {
      inspect(lbracket_loc, content="loc_regression.mbt-44:6-44:7")
      inspect(rbracket_loc, content="loc_regression.mbt-44:8-44:9")
    }
    _ => fail("expected array_get_tokens body to be ArrayGet")
  }
  match function_body(impls, "slice_tokens") {
    ArrayGetSlice(lbracket_loc~, colon_loc~, rbracket_loc~, ..) => {
      inspect(lbracket_loc, content="loc_regression.mbt-48:6-48:7")
      inspect(colon_loc, content="loc_regression.mbt-48:8-48:9")
      inspect(rbracket_loc, content="loc_regression.mbt-48:10-48:11")
    }
    _ => fail("expected slice_tokens body to be ArrayGetSlice")
  }
  match function_body(impls, "set_tokens") {
    ArraySet(lbracket_loc~, rbracket_loc~, equal_loc~, ..) => {
      inspect(lbracket_loc, content="loc_regression.mbt-52:6-52:7")
      inspect(rbracket_loc, content="loc_regression.mbt-52:8-52:9")
      inspect(equal_loc, content="loc_regression.mbt-52:10-52:11")
    }
    _ => fail("expected set_tokens body to be ArraySet")
  }
  match function_body(impls, "aug_set_tokens") {
    ArrayAugmentedSet(lbracket_loc~, rbracket_loc~, augmented_assign_loc~, ..) => {
      inspect(lbracket_loc, content="loc_regression.mbt-56:6-56:7")
      inspect(rbracket_loc, content="loc_regression.mbt-56:8-56:9")
      inspect(augmented_assign_loc, content="loc_regression.mbt-56:10-56:12")
    }
    _ => fail("expected aug_set_tokens body to be ArrayAugmentedSet")
  }
  match function_body(impls, "tuple_tokens") {
    Tuple(lparen_loc~, rparen_loc~, comma_locs~, ..) => {
      inspect(lparen_loc, content="loc_regression.mbt-60:4-60:5")
      inspect(rparen_loc, content="loc_regression.mbt-60:10-60:11")
      let (first_comma, trailing_comma) = two_locs(
        comma_locs, "tuple comma locs",
      )
      inspect(first_comma, content="loc_regression.mbt-60:6-60:7")
      inspect(trailing_comma, content="loc_regression.mbt-60:9-60:10")
    }
    _ => fail("expected tuple_tokens body to be Tuple")
  }
  match function_body(impls, "map_tokens") {
    Map(elems~, lbrace_loc~, rbrace_loc~, comma_locs~, ..) => {
      inspect(lbrace_loc, content="loc_regression.mbt-64:4-64:5")
      inspect(rbrace_loc, content="loc_regression.mbt-64:18-64:19")
      let (first_comma, trailing_comma) = two_locs(comma_locs, "map comma locs")
      inspect(first_comma, content="loc_regression.mbt-64:10-64:11")
      inspect(trailing_comma, content="loc_regression.mbt-64:16-64:17")
      match elems {
        More(
          { colon_loc: first_colon, .. },
          tail=More({ colon_loc: second_colon, .. }, tail=Empty)
        ) => {
          inspect(first_colon, content="loc_regression.mbt-64:7-64:8")
          inspect(second_colon, content="loc_regression.mbt-64:13-64:14")
        }
        _ => fail("expected two map elements")
      }
    }
    _ => fail("expected map_tokens body to be Map")
  }
  match function_body(impls, "group_tokens") {
    Group(group=Paren, left_delim_loc~, right_delim_loc~, ..) => {
      inspect(left_delim_loc, content="loc_regression.mbt-68:4-68:5")
      inspect(right_delim_loc, content="loc_regression.mbt-68:10-68:11")
    }
    _ => fail("expected group_tokens body to be paren Group")
  }
  match function_body(impls, "brace_group_tokens") {
    Group(group=Brace, left_delim_loc~, right_delim_loc~, ..) => {
      inspect(left_delim_loc, content="loc_regression.mbt-72:4-72:5")
      inspect(right_delim_loc, content="loc_regression.mbt-72:16-72:17")
    }
    _ => fail("expected brace_group_tokens body to be brace Group")
  }
  match function_body(impls, "is_tokens") {
    Is(is_loc~, ..) => inspect(is_loc, content="loc_regression.mbt-76:10-76:12")
    _ => fail("expected is_tokens body to be Is")
  }
  match function_body(impls, "pipe_tokens") {
    Pipe(pipe_loc~, ..) =>
      inspect(pipe_loc, content="loc_regression.mbt-80:10-80:12")
    _ => fail("expected pipe_tokens body to be Pipe")
  }
  match function_body(impls, "rev_pipe_tokens") {
    RevPipe(rev_pipe_loc~, ..) =>
      inspect(rev_pipe_loc, content="loc_regression.mbt-84:7-84:9")
    _ => fail("expected rev_pipe_tokens body to be RevPipe")
  }
  match function_body(impls, "return_tokens") {
    Return(return_loc~, ..) =>
      inspect(return_loc, content="loc_regression.mbt-88:4-88:10")
    _ => fail("expected return_tokens body to be Return")
  }
  match function_body(impls, "raise_tokens") {
    Raise(raise_loc~, ..) =>
      inspect(raise_loc, content="loc_regression.mbt-92:4-92:9")
    _ => fail("expected raise_tokens body to be Raise")
  }
  match function_body(impls, "record_update_tokens") {
    RecordUpdate(
      coloncolon_loc~,
      lbrace_loc~,
      dotdot_loc~,
      rbrace_loc~,
      comma_locs~,
      ..
    ) => {
      match coloncolon_loc {
        None => ()
        Some(_) => fail("expected untyped record update")
      }
      inspect(lbrace_loc, content="loc_regression.mbt-96:4-96:5")
      inspect(dotdot_loc, content="loc_regression.mbt-96:6-96:8")
      inspect(rbrace_loc, content="loc_regression.mbt-96:26-96:27")
      let (spread_comma, field_comma, trailing_comma) = three_locs(
        comma_locs, "record update comma locs",
      )
      inspect(spread_comma, content="loc_regression.mbt-96:12-96:13")
      inspect(field_comma, content="loc_regression.mbt-96:18-96:19")
      inspect(trailing_comma, content="loc_regression.mbt-96:24-96:25")
    }
    _ => fail("expected record_update_tokens body to be RecordUpdate")
  }
  match function_body(impls, "typed_record_update_tokens") {
    RecordUpdate(
      coloncolon_loc=Some(coloncolon_loc),
      lbrace_loc~,
      dotdot_loc~,
      rbrace_loc~,
      comma_locs~,
      ..
    ) => {
      inspect(coloncolon_loc, content="loc_regression.mbt-100:9-100:11")
      inspect(lbrace_loc, content="loc_regression.mbt-100:11-100:12")
      inspect(dotdot_loc, content="loc_regression.mbt-100:13-100:15")
      inspect(rbrace_loc, content="loc_regression.mbt-100:20-100:21")
      expect_no_locs(comma_locs, "typed record update comma locs")
    }
    _ => fail("expected typed_record_update_tokens body to be RecordUpdate")
  }
  match function_body(impls, "method_tokens") {
    Method(coloncolon_loc~, ..) =>
      inspect(coloncolon_loc, content="loc_regression.mbt-104:9-104:11")
    _ => fail("expected method_tokens body to be Method")
  }
  match function_body(impls, "dot_apply_tokens") {
    DotApply(
      return_self=false,
      dot_loc~,
      lparen_loc~,
      rparen_loc~,
      comma_locs~,
      ..
    ) => {
      inspect(dot_loc, content="loc_regression.mbt-108:10-108:16")
      inspect(lparen_loc, content="loc_regression.mbt-108:16-108:17")
      inspect(rparen_loc, content="loc_regression.mbt-108:22-108:23")
      let (first_comma, trailing_comma) = two_locs(
        comma_locs, "dot apply comma locs",
      )
      inspect(first_comma, content="loc_regression.mbt-108:18-108:19")
      inspect(trailing_comma, content="loc_regression.mbt-108:21-108:22")
    }
    _ => fail("expected dot_apply_tokens body to be DotApply")
  }
  match function_body(impls, "dotdot_apply_tokens") {
    DotApply(
      return_self=true,
      dot_loc~,
      lparen_loc~,
      rparen_loc~,
      comma_locs~,
      ..
    ) => {
      inspect(dot_loc, content="loc_regression.mbt-112:9-112:11")
      inspect(lparen_loc, content="loc_regression.mbt-112:14-112:15")
      inspect(rparen_loc, content="loc_regression.mbt-112:17-112:18")
      inspect(
        single_loc(comma_locs, "dotdot apply comma loc"),
        content="loc_regression.mbt-112:16-112:17",
      )
    }
    _ => fail("expected dotdot_apply_tokens body to be returning-self DotApply")
  }
  match function_body(impls, "as_tokens") {
    As(as_loc~, ..) =>
      inspect(as_loc, content="loc_regression.mbt-116:10-116:12")
    _ => fail("expected as_tokens body to be As")
  }
  match function_body(impls, "break_label_tokens") {
    Break(break_loc~, post_label_loc~, ..) => {
      inspect(break_loc, content="loc_regression.mbt-120:4-120:9")
      inspect(
        require_some_loc(post_label_loc, "break post label"),
        content="loc_regression.mbt-120:10-120:15",
      )
    }
    _ => fail("expected break_label_tokens body to be Break")
  }
  match function_body(impls, "continue_label_tokens") {
    Continue(continue_loc~, post_label_loc~, comma_locs~, ..) => {
      inspect(continue_loc, content="loc_regression.mbt-124:4-124:12")
      inspect(
        require_some_loc(post_label_loc, "continue post label"),
        content="loc_regression.mbt-124:13-124:18",
      )
      inspect(
        single_loc(comma_locs, "continue comma loc"),
        content="loc_regression.mbt-124:20-124:21",
      )
    }
    _ => fail("expected continue_label_tokens body to be Continue")
  }
  match function_body(impls, "template_tokens") {
    TemplateWriting(lt_plus_loc~, ..) =>
      inspect(lt_plus_loc, content="loc_regression.mbt-128:10-128:12")
    _ => fail("expected template_tokens body to be TemplateWriting")
  }
  match function_body(impls, "lexmatch_question_tokens") {
    IsLexMatch(lexmatch_question_loc~, with_loc~, ..) => {
      inspect(lexmatch_question_loc, content="loc_regression.mbt-132:10-132:19")
      inspect(
        require_some_loc(with_loc, "lexmatch with loc"),
        content="loc_regression.mbt-132:24-132:28",
      )
    }
    _ => fail("expected lexmatch_question_tokens body to be IsLexMatch")
  }
  match function_body(impls, "regex_match_tokens") {
    RegexMatch(
      eq_tilde_loc~,
      rhs_lparen_loc~,
      rhs_rparen_loc~,
      binding_comma_locs~,
      binding_equal_locs~,
      ..
    ) => {
      inspect(eq_tilde_loc, content="loc_regression.mbt-136:10-136:12")
      inspect(
        require_some_loc(rhs_lparen_loc, "regex rhs lparen"),
        content="loc_regression.mbt-136:13-136:14",
      )
      inspect(
        require_some_loc(rhs_rparen_loc, "regex rhs rparen"),
        content="loc_regression.mbt-136:40-136:41",
      )
      let (pattern_comma, binding_comma) = two_locs(
        binding_comma_locs, "regex binding comma locs",
      )
      inspect(pattern_comma, content="loc_regression.mbt-136:19-136:20")
      inspect(binding_comma, content="loc_regression.mbt-136:32-136:33")
      inspect(
        single_loc(binding_equal_locs, "regex binding equal loc"),
        content="loc_regression.mbt-136:27-136:28",
      )
    }
    _ => fail("expected regex_match_tokens body to be RegexMatch")
  }
  match function_body(impls, "quantifier_tokens") {
    Quantifier(quantifier_loc~, colon_loc~, comma_loc~, ..) => {
      inspect(quantifier_loc, content="loc_regression.mbt-140:4-140:5")
      inspect(colon_loc, content="loc_regression.mbt-140:8-140:9")
      inspect(comma_loc, content="loc_regression.mbt-140:13-140:14")
    }
    _ => fail("expected quantifier_tokens body to be Quantifier")
  }
  match function_body(impls, "implies_tokens") {
    Implies(implies_loc~, ..) =>
      inspect(implies_loc, content="loc_regression.mbt-144:6-144:7")
    _ => fail("expected implies_tokens body to be Implies")
  }
  match function_body(impls, "match_tokens") {
    Match(lbrace_loc~, rbrace_loc~, semi_locs~, ..) => {
      inspect(lbrace_loc, content="loc_regression.mbt-148:16-148:17")
      inspect(rbrace_loc, content="loc_regression.mbt-151:4-151:5")
      let (first_semi, trailing_semi) = two_locs(
        semi_locs, "match case semicolon locs",
      )
      inspect(first_semi, content="loc_regression.mbt-149:14-149:15")
      inspect(trailing_semi, content="loc_regression.mbt-150:16-150:17")
    }
    _ => fail("expected match_tokens body to be Match")
  }
  match function_body(impls, "lexmatch_tokens") {
    LexMatch(with_loc~, lbrace_loc~, rbrace_loc~, semi_locs~, ..) => {
      inspect(
        require_some_loc(with_loc, "lexmatch expression with loc"),
        content="loc_regression.mbt-155:19-155:23",
      )
      inspect(lbrace_loc, content="loc_regression.mbt-155:32-155:33")
      inspect(rbrace_loc, content="loc_regression.mbt-158:4-158:5")
      let (first_semi, trailing_semi) = two_locs(
        semi_locs, "lexmatch case semicolon locs",
      )
      inspect(first_semi, content="loc_regression.mbt-156:16-156:17")
      inspect(trailing_semi, content="loc_regression.mbt-157:16-157:17")
    }
    _ => fail("expected lexmatch_tokens body to be LexMatch")
  }
  match function_body(impls, "try_tokens") {
    Try(
      catch_lbrace_loc~,
      catch_rbrace_loc~,
      catch_semi_locs~,
      noraise_lbrace_loc~,
      noraise_rbrace_loc~,
      noraise_semi_locs~,
      ..
    ) => {
      inspect(catch_lbrace_loc, content="loc_regression.mbt-162:20-162:21")
      inspect(catch_rbrace_loc, content="loc_regression.mbt-164:4-164:5")
      inspect(
        single_loc(catch_semi_locs, "try catch semicolon loc"),
        content="loc_regression.mbt-163:19-163:20",
      )
      inspect(
        require_some_loc(noraise_lbrace_loc, "try noraise lbrace"),
        content="loc_regression.mbt-164:14-164:15",
      )
      inspect(
        require_some_loc(noraise_rbrace_loc, "try noraise rbrace"),
        content="loc_regression.mbt-166:4-166:5",
      )
      inspect(
        single_loc(noraise_semi_locs, "try noraise semicolon loc"),
        content="loc_regression.mbt-165:13-165:14",
      )
    }
    _ => fail("expected try_tokens body to be Try")
  }
  match function_body(impls, "while_tokens") {
    While(
      label_colon_loc~,
      while_loc~,
      body_lbrace_loc~,
      body_rbrace_loc~,
      nobreak_loc~,
      nobreak_lbrace_loc~,
      nobreak_rbrace_loc~,
      ..
    ) => {
      inspect(
        require_some_loc(label_colon_loc, "while label colon loc"),
        content="loc_regression.mbt-170:4-170:11",
      )
      inspect(while_loc, content="loc_regression.mbt-170:12-170:17")
      inspect(body_lbrace_loc, content="loc_regression.mbt-170:24-170:25")
      inspect(body_rbrace_loc, content="loc_regression.mbt-172:4-172:5")
      inspect(
        require_some_loc(nobreak_loc, "while nobreak loc"),
        content="loc_regression.mbt-172:6-172:13",
      )
      inspect(
        require_some_loc(nobreak_lbrace_loc, "while nobreak lbrace loc"),
        content="loc_regression.mbt-172:14-172:15",
      )
      inspect(
        require_some_loc(nobreak_rbrace_loc, "while nobreak rbrace loc"),
        content="loc_regression.mbt-174:4-174:5",
      )
    }
    _ => fail("expected while_tokens body to be While")
  }
  match function_body(impls, "foreach_comprehension_tokens") {
    ListComprehension(
      kind=Foreach(
        for_loc~,
        in_loc~,
        binder_comma_locs~,
        semi_locs~,
        init_comma_locs~,
        init_equal_locs~,
        continue_comma_locs~,
        continue_equal_locs~,
        ..
      ),
      lbracket_loc~,
      rbracket_loc~,
      if_loc~,
      fat_arrow_loc~,
      ..
    ) => {
      inspect(lbracket_loc, content="loc_regression.mbt-178:4-178:5")
      inspect(for_loc, content="loc_regression.mbt-178:5-178:8")
      inspect(
        single_loc(binder_comma_locs, "foreach comprehension binder comma"),
        content="loc_regression.mbt-178:10-178:11",
      )
      inspect(in_loc, content="loc_regression.mbt-178:14-178:16")
      let (init_semi, continue_semi) = two_locs(
        semi_locs, "foreach comprehension semicolon locs",
      )
      inspect(init_semi, content="loc_regression.mbt-178:19-178:20")
      inspect(continue_semi, content="loc_regression.mbt-178:33-178:34")
      inspect(
        single_loc(init_comma_locs, "foreach comprehension init comma"),
        content="loc_regression.mbt-178:26-178:27",
      )
      let (first_init_equal, second_init_equal) = two_locs(
        init_equal_locs, "foreach comprehension init equals",
      )
      inspect(first_init_equal, content="loc_regression.mbt-178:23-178:24")
      inspect(second_init_equal, content="loc_regression.mbt-178:30-178:31")
      inspect(
        single_loc(continue_comma_locs, "foreach comprehension continue comma"),
        content="loc_regression.mbt-178:44-178:45",
      )
      let (first_continue_equal, second_continue_equal) = two_locs(
        continue_equal_locs, "foreach comprehension continue equals",
      )
      inspect(first_continue_equal, content="loc_regression.mbt-178:37-178:38")
      inspect(second_continue_equal, content="loc_regression.mbt-178:48-178:49")
      inspect(
        require_some_loc(if_loc, "foreach comprehension if loc"),
        content="loc_regression.mbt-178:56-178:58",
      )
      inspect(fat_arrow_loc, content="loc_regression.mbt-178:64-178:66")
      inspect(rbracket_loc, content="loc_regression.mbt-178:71-178:72")
    }
    _ =>
      fail(
        "expected foreach_comprehension_tokens body to be ListComprehension::Foreach",
      )
  }
  match function_body(impls, "for_comprehension_tokens") {
    ListComprehension(
      kind=For(
        for_loc~,
        binder_comma_locs~,
        semi_locs~,
        binder_equal_locs~,
        continue_comma_locs~,
        continue_equal_locs~,
        ..
      ),
      lbracket_loc~,
      rbracket_loc~,
      if_loc~,
      fat_arrow_loc~,
      ..
    ) => {
      inspect(lbracket_loc, content="loc_regression.mbt-182:4-182:5")
      inspect(for_loc, content="loc_regression.mbt-182:5-182:8")
      inspect(
        single_loc(binder_comma_locs, "for comprehension binder comma"),
        content="loc_regression.mbt-182:14-182:15",
      )
      let (condition_semi, continue_semi) = two_locs(
        semi_locs, "for comprehension semicolon locs",
      )
      inspect(condition_semi, content="loc_regression.mbt-182:21-182:22")
      inspect(continue_semi, content="loc_regression.mbt-182:29-182:30")
      let (first_binder_equal, second_binder_equal) = two_locs(
        binder_equal_locs, "for comprehension binder equals",
      )
      inspect(first_binder_equal, content="loc_regression.mbt-182:11-182:12")
      inspect(second_binder_equal, content="loc_regression.mbt-182:18-182:19")
      inspect(
        single_loc(continue_comma_locs, "for comprehension continue comma"),
        content="loc_regression.mbt-182:40-182:41",
      )
      let (first_continue_equal, second_continue_equal) = two_locs(
        continue_equal_locs, "for comprehension continue equals",
      )
      inspect(first_continue_equal, content="loc_regression.mbt-182:33-182:34")
      inspect(second_continue_equal, content="loc_regression.mbt-182:44-182:45")
      inspect(
        require_some_loc(if_loc, "for comprehension if loc"),
        content="loc_regression.mbt-182:52-182:54",
      )
      inspect(fat_arrow_loc, content="loc_regression.mbt-182:60-182:62")
      inspect(rbracket_loc, content="loc_regression.mbt-182:67-182:68")
    }
    _ =>
      fail(
        "expected for_comprehension_tokens body to be ListComprehension::For",
      )
  }
  match function_body(impls, "for_loop_tokens") {
    For(
      label_colon_loc~,
      for_loc~,
      binder_comma_locs~,
      binder_equal_locs~,
      semi_locs~,
      continue_comma_locs~,
      continue_equal_locs~,
      body_lbrace_loc~,
      body_rbrace_loc~,
      nobreak_loc~,
      nobreak_lbrace_loc~,
      nobreak_rbrace_loc~,
      ..
    ) => {
      inspect(
        require_some_loc(label_colon_loc, "for loop label colon"),
        content="loc_regression.mbt-186:4-186:10",
      )
      inspect(for_loc, content="loc_regression.mbt-186:11-186:14")
      inspect(
        single_loc(binder_comma_locs, "for loop binder comma"),
        content="loc_regression.mbt-186:20-186:21",
      )
      let (first_binder_equal, second_binder_equal) = two_locs(
        binder_equal_locs, "for loop binder equals",
      )
      inspect(first_binder_equal, content="loc_regression.mbt-186:17-186:18")
      inspect(second_binder_equal, content="loc_regression.mbt-186:24-186:25")
      let (condition_semi, continue_semi) = two_locs(
        semi_locs, "for loop semicolon locs",
      )
      inspect(condition_semi, content="loc_regression.mbt-186:27-186:28")
      inspect(continue_semi, content="loc_regression.mbt-186:35-186:36")
      inspect(
        single_loc(continue_comma_locs, "for loop continue comma"),
        content="loc_regression.mbt-186:46-186:47",
      )
      let (first_continue_equal, second_continue_equal) = two_locs(
        continue_equal_locs, "for loop continue equals",
      )
      inspect(first_continue_equal, content="loc_regression.mbt-186:39-186:40")
      inspect(second_continue_equal, content="loc_regression.mbt-186:50-186:51")
      inspect(body_lbrace_loc, content="loc_regression.mbt-186:58-186:59")
      inspect(body_rbrace_loc, content="loc_regression.mbt-188:4-188:5")
      inspect(
        require_some_loc(nobreak_loc, "for loop nobreak"),
        content="loc_regression.mbt-188:6-188:13",
      )
      inspect(
        require_some_loc(nobreak_lbrace_loc, "for loop nobreak lbrace"),
        content="loc_regression.mbt-188:14-188:15",
      )
      inspect(
        require_some_loc(nobreak_rbrace_loc, "for loop nobreak rbrace"),
        content="loc_regression.mbt-190:4-190:5",
      )
    }
    _ => fail("expected for_loop_tokens body to be For")
  }
  match function_body(impls, "foreach_loop_tokens") {
    ForEach(
      label_colon_loc~,
      for_loc~,
      binder_comma_locs~,
      in_loc~,
      semi_locs~,
      init_comma_locs~,
      init_equal_locs~,
      continue_comma_locs~,
      continue_equal_locs~,
      body_lbrace_loc~,
      body_rbrace_loc~,
      nobreak_loc~,
      nobreak_lbrace_loc~,
      nobreak_rbrace_loc~,
      ..
    ) => {
      inspect(
        require_some_loc(label_colon_loc, "foreach loop label colon"),
        content="loc_regression.mbt-194:4-194:10",
      )
      inspect(for_loc, content="loc_regression.mbt-194:11-194:14")
      inspect(
        single_loc(binder_comma_locs, "foreach loop binder comma"),
        content="loc_regression.mbt-194:16-194:17",
      )
      inspect(in_loc, content="loc_regression.mbt-194:20-194:22")
      let (init_semi, continue_semi) = two_locs(
        semi_locs, "foreach loop semicolon locs",
      )
      inspect(init_semi, content="loc_regression.mbt-194:25-194:26")
      inspect(continue_semi, content="loc_regression.mbt-194:39-194:40")
      inspect(
        single_loc(init_comma_locs, "foreach loop init comma"),
        content="loc_regression.mbt-194:32-194:33",
      )
      let (first_init_equal, second_init_equal) = two_locs(
        init_equal_locs, "foreach loop init equals",
      )
      inspect(first_init_equal, content="loc_regression.mbt-194:29-194:30")
      inspect(second_init_equal, content="loc_regression.mbt-194:36-194:37")
      inspect(
        single_loc(continue_comma_locs, "foreach loop continue comma"),
        content="loc_regression.mbt-194:50-194:51",
      )
      let (first_continue_equal, second_continue_equal) = two_locs(
        continue_equal_locs, "foreach loop continue equals",
      )
      inspect(first_continue_equal, content="loc_regression.mbt-194:43-194:44")
      inspect(second_continue_equal, content="loc_regression.mbt-194:54-194:55")
      inspect(body_lbrace_loc, content="loc_regression.mbt-194:62-194:63")
      inspect(body_rbrace_loc, content="loc_regression.mbt-196:4-196:5")
      inspect(
        require_some_loc(nobreak_loc, "foreach loop nobreak"),
        content="loc_regression.mbt-196:6-196:13",
      )
      inspect(
        require_some_loc(nobreak_lbrace_loc, "foreach loop nobreak lbrace"),
        content="loc_regression.mbt-196:14-196:15",
      )
      inspect(
        require_some_loc(nobreak_rbrace_loc, "foreach loop nobreak rbrace"),
        content="loc_regression.mbt-198:4-198:5",
      )
    }
    _ => fail("expected foreach_loop_tokens body to be ForEach")
  }
}

///|
test "expression delimiter and operator token locs for handrolled parser" {
  check_expression_token_locs(@parser.Handrolled)
}

///|
test "expression delimiter and operator token locs for moonyacc parser" {
  check_expression_token_locs(@parser.MoonYacc)
}
