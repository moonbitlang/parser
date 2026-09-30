// Syntax aligned with the upstream `parsing_parser.mly` grammar that is only
// implemented by the moonyacc parser so far.

///|
fn yacc_parse(source : String) -> @syntax.Impls raise {
  let (impls, reports) = @parser.parse_string(source, parser=MoonYacc)
  if !reports.is_empty() {
    fail(
      "moonyacc parser rejected:\n\{source}\n\{reports.to_json().stringify(indent=2)}",
    )
  }
  impls
}

///|
fn yacc_json(source : String) -> String raise {
  yacc_parse(source).map(impl_ => impl_.json_repr()).to_json().stringify()
}

///|
fn expect_yacc_rejected(source : String) -> Unit raise {
  let (_, reports) = @parser.parse_string(source, parser=MoonYacc)
  if reports.is_empty() {
    fail("moonyacc parser unexpectedly accepted:\n\{source}")
  }
}

///|
test "yacc: `extend` is a keyword" {
  expect_yacc_rejected("fn f(extend : Int) -> Unit {}")
  expect_yacc_rejected("fn f() -> Unit { let extend = 1 }")
  expect_yacc_rejected("fn f(x : X) -> Unit { x..extend() }")
  expect_yacc_rejected("pub(extend) fn g() -> Unit {}")
}

///|
test "yacc: array elements cannot be bare `for` loops" {
  expect_yacc_rejected("fn f() -> Unit { let _ = [for x in xs { }] }")
}

///|
test "yacc: declare let/struct/enum" {
  let impls = yacc_parse(
    (
      #|declare let x : Int
      #|declare pub struct S { x : Int }
      #|declare enum E { A }
      #|declare pub(all) struct T(Int)
    ),
  )
  guard impls
    is More(
      TopLetDef(is_declare=true, expr=Hole(kind=Synthesized, ..), ..),
      tail=More(
        TopTypeDef({ is_declare: true, components: Record(_), .. }),
        tail=More(
          TopTypeDef({ is_declare: true, components: Variant(_), .. }),
          tail=More(
            TopTypeDef({ is_declare: true, components: TupleStruct(_), .. }),
            tail=Empty
          )
        )
      )
    ) else {
    fail("unexpected declare items")
  }
}

///|
test "yacc: `label~ = value` arguments and constructor patterns" {
  let json = yacc_json(
    (
      #|fn f(x : T) -> Unit {
      #|  g(x~ = 1)
      #|  match x {
      #|    C(y~ = z) => ()
      #|  }
      #|}
    ),
  )
  if !json.contains("ArgumentKind::Labelled") {
    fail("expected a labelled argument")
  }
}
