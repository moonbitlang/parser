///|
test "parser path: foreach recovery terminates and keeps later declarations" {
  for
    source in [
      "fn f() { for S::{} in xs {} }", "fn f() { for S::{} in xs {} }\nfn later() -> Int { 42 }",
    ] {
    let parsed = @untyped_cst.parse_structure(source)
    @test.assert_eq(parsed.diagnostics_view().is_empty(), false)
    boundary_assert_source(parsed.root(), source)
    let (_, reports) = parsed.to_impls()
    @test.assert_eq(reports.is_empty(), false)
    if source.has_suffix("42 }") {
      guard api_find_node(parsed.root(), node => {
          node.kind.is_impl() &&
          node.text(source) == "fn later() -> Int { 42 }"[:]
        })
        is Some(_) else {
        fail("expected the later declaration in the recovered CST")
      }
    }
  }
}

///|
test "parser path: constructor packages match all parsers" {
  for name in ["T::@p.A", "@a.T::@b.C"] {
    for args in ["", "(x)", "(x, label~, ..)"] {
      let pat = name + args
      for
        body in [
          "value is " + pat,
          "match value { " + pat + " => (); _ => () }",
          "let " + pat + " = value; ()",
          "for " + pat + " in xs {}",
        ] {
        let source = "fn f() { " + body + " }"
        assert_ast_equiv(source, label=source)
        boundary_assert_source(
          @untyped_cst.parse_structure(source).root(),
          source,
        )
      }
    }
  }
}

///|
test "parser path: nested return effects match all parsers" {
  for
    effect in [
      "raise", "raise E", "raise ?", "noraise", "nocancel", "raise + nocancel", "raise E + nocancel",
      "raise ? + nocancel", "noraise + nocancel", "nocancel + raise", "nocancel + raise E",
      "nocancel + raise ?", "nocancel + noraise",
    ] {
    for
      ty in [
        "() -> () -> Int", "() -> () -> () -> Int", "async () -> () -> Int", "() -> async () -> Int",
        "() -> (() -> Int)", "(() -> () -> Int)", "// return type\n () -> () -> Int",
        "() -> // nested return\n () -> Int", "async // async type\n () -> () -> Int",
        "// return type\n (Int, String) -> // nested\n (Bool) -> Int",
      ] {
      let ret = ty + " " + effect
      for
        source in [
          "fn f() -> " + ret + " {}",
          "fn outer() { fn local() -> " + ret + " {}; () }",
          "fn outer() { let f = fn() -> " + ret + " {}; () }",
          "fn T::method(self : T) -> " + ret + " {}",
          "trait T { fn method(Self) -> " + ret + " }",
          "impl T for S with fn method(self : S) -> " + ret + " {}",
        ] {
        assert_ast_equiv(source, label=source)
        boundary_assert_source(
          @untyped_cst.parse_structure(source).root(),
          source,
        )
      }
    }
  }
}

///|
test "parser path: constructor type package and name have independent ranges" {
  for
    (name, type_text, type_pkg, constr_pkg, constr_name) in [
      ("T::@p.A", "T", "", "p", "A"),
      ("@a.T::@b.C", "@a.T", "a", "b", "C"),
    ] {
    let prefix = "value is "
    let source = prefix + name + "(x)"
    guard boundary_expr(source)
      is Is(pat=Constr(constr~, args=Some(args), ..), ..) &&
      constr.extra_info is TypeNameWithConstrPackage(type_name~, pkg~) else {
      fail(
        "expected a constructor pattern with separate type and constructor packages",
      )
    }
    @test.assert_eq(pkg, constr_pkg)
    @test.assert_eq(constr.name.name, constr_name)
    @test.assert_eq(args.length(), 1)
    @test.assert_eq(type_name.is_object, false)
    @test.assert_eq(type_name.loc.start.cnum, prefix.length())
    @test.assert_eq(
      type_name.loc.end.cnum,
      prefix.length() + type_text.length(),
    )
    @test.assert_eq(constr.loc.start.cnum, prefix.length())
    @test.assert_eq(constr.loc.end.cnum, prefix.length() + name.length())
    @test.assert_eq(
      constr.name.loc.start.cnum,
      prefix.length() + name.length() - 1,
    )
    @test.assert_eq(constr.name.loc.end.cnum, constr.loc.end.cnum)
    match type_name.name {
      Ident(name~) => {
        @test.assert_eq(type_pkg, "")
        @test.assert_eq(name, "T")
      }
      Dot(pkg~, id~) => {
        @test.assert_eq(pkg, type_pkg)
        @test.assert_eq(id, "T")
      }
    }
    let root = @untyped_cst.parse_expression(source).root()
    boundary_assert_source(root, source)
    let pat = api_find_node(root, node => node.kind is Pattern_Constr).unwrap()
    @test.assert_eq(
      pat.first_child_of_kind(kind => {
        kind is ConstructorExtraInfo_TypeNameWithConstrPackage
      })
      is Some(_),
      true,
    )
    let names = pat.children_with_label("name")
    let pkg_node = names[names.length() - 2]
    @test.assert_eq(pkg_node.text(source), ("@" + constr_pkg)[:])
    @test.assert_eq(
      pkg_node.loc.start.cnum,
      prefix.length() + type_text.length() + 2,
    )
    @test.assert_eq(pkg_node.loc.end.cnum, constr.name.loc.start.cnum - 1)
    let _ = recovery_check_source_spans(pat, source, 0)
  }
}

///|
fn path_function_decl(source : String) -> @syntax.FunDecl raise {
  assert_ast_equiv(source, label=source)
  let parsed = @untyped_cst.parse_structure(source)
  boundary_assert_source(parsed.root(), source)
  let (impls, _) = parsed.to_impls()
  guard impls.to_array() is [TopFuncDef(fun_decl~, ..)] else {
    fail("expected one function declaration")
  }
  fun_decl
}

///|
fn path_error_name(error : @syntax.ErrorType) -> String {
  match error {
    NoErrorType => ""
    DefaultErrorType(..) => "raise"
    Noraise(..) => "noraise"
    MaybeError(..) => "raise ?"
    ErrorType(ty=Name(constr_id={ id: Ident(name="E"), .. }, ..)) => "raise E"
    _ => "unexpected error type"
  }
}

///|
test "parser path: effects bind to the innermost unparenthesized return" {
  for
    (ty, depth_count, annotated_depth) in [
      ("() -> () -> Int", 2, 1),
      ("async () -> async () -> () -> Int", 3, 2),
      ("() -> (() -> Int)", 2, 0),
      ("(() -> () -> Int)", 2, -1),
      ("// return\n () -> // inner\n () -> Int", 2, 1),
    ] {
    for error in ["raise", "raise E", "raise ?", "noraise", ""] {
      for has_nocancel in [false, true] {
        if error == "" && !has_nocancel {
          continue
        }
        let effect = if !has_nocancel {
          error
        } else if error == "" {
          "nocancel"
        } else {
          error + " + nocancel"
        }
        let source = "fn f() -> " + ty + " " + effect + " {}"
        let decl = path_function_decl(source)
        @test.assert_eq(
          path_error_name(decl.error_type),
          if annotated_depth == -1 {
            error
          } else {
            ""
          },
        )
        @test.assert_eq(
          decl.nocancel_ is Some(_),
          annotated_depth == -1 && has_nocancel,
        )
        let mut current = decl.return_type.unwrap()
        for depth in 0..<depth_count {
          guard current is Arrow(res~, err~, nocancel_~, loc~, ..) else {
            fail("expected a function return type at depth \{depth}")
          }
          @test.assert_eq(
            path_error_name(err),
            if depth == annotated_depth {
              error
            } else {
              ""
            },
          )
          @test.assert_eq(
            nocancel_ is Some(_),
            depth == annotated_depth && has_nocancel,
          )
          if depth <= annotated_depth {
            @test.assert_eq(loc.end.cnum, source.length() - " {}".length())
          }
          if nocancel_ is Some(anchor) {
            @test.assert_eq(
              source[anchor.start.cnum:anchor.end.cnum],
              "nocancel"[:],
            )
          }
          current = res
        }
        @test.assert_eq(
          current is Name(constr_id={ id: Ident(name="Int"), .. }, ..),
          true,
        )
      }
    }
  }
}

///|
test "parser path: optional suffixes retain both question locations" {
  let source = "fn f() -> Int // first\n ? // second\n ? {}"
  let decl = path_function_decl(source)
  guard decl.return_type
    is Some(
      Option(
        ty=Option(ty=Name(constr_id~, ..), loc=inner_loc, question_loc=first),
        loc=outer_loc,
        question_loc=second
      )
    ) else {
    fail("expected two optional suffixes")
  }
  @test.assert_eq(constr_id.id is Ident(name="Int"), true)
  @test.assert_eq(inner_loc.start, constr_id.loc.start)
  @test.assert_eq(outer_loc.start, constr_id.loc.start)
  @test.assert_eq(inner_loc.end, first.end)
  @test.assert_eq(outer_loc.end, second.end)
  @test.assert_eq(first.start.lnum, 2)
  @test.assert_eq(second.start.lnum, 3)
  @test.assert_eq(first.end.cnum - first.start.cnum, 1)
  @test.assert_eq(second.end.cnum - second.start.cnum, 1)
  @test.assert_eq(source[first.start.cnum:first.end.cnum], "?"[:])
  @test.assert_eq(source[second.start.cnum:second.end.cnum], "?"[:])
  let _ = recovery_check_source_spans(
    @untyped_cst.parse_structure(source).root(),
    source,
    0,
  )
}

///|
test "parser path: optional type suffix accepts trivia" {
  for gap in [" ", "\n", " // optional\n ", "\n // one\n // two 🐇\n "] {
    for ty in ["Int" + gap + "?", "Int" + gap + "?" + gap + "?"] {
      for
        source in [
          "fn f(x : " + ty + ") {}",
          "fn f() -> " + ty + " {}",
          "type T = " + ty,
          "type T = (" + ty + ", String" + gap + "?)",
          "type T = Array[" + ty + "]",
        ] {
        assert_ast_equiv(source, label=source)
        boundary_assert_source(
          @untyped_cst.parse_structure(source).root(),
          source,
        )
      }
    }
  }
}

///|
test "parser path: optional suffix does not change error annotations" {
  for
    source in [
      "fn f() -> Int ? raise E {}", "fn f() -> Int ? raise ? {}", "fn f() -> () -> Int ? raise E + nocancel {}",
      "type F = () -> Int ? raise E",
    ] {
    assert_ast_equiv(source, label=source)
  }
  // The error-annotation recovery convention remains separate from Type_Option.
  for source in ["fn f() -> Unit raise E? {}", "fn f() -> Unit raise E ? {}"] {
    assert_handrolled_cst_ast_equiv(source)
  }
  for
    source in [
      "fn f() -> Int ?Unit {}", "fn f() -> Int ? @p.Error {}", "fn f() -> Int // old annotation\n ? Unit {}",
    ] {
    assert_all_parsers_reject(source)
    assert_handrolled_cst_structure_equiv("removed error annotation", source)
    boundary_assert_source(@untyped_cst.parse_structure(source).root(), source)
  }
}
