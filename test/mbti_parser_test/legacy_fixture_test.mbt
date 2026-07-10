///|
using @mbti_ast {
  type Attributes,
  type QualifiedName,
  type Type,
  type Visibility,
}

///|
fn parse_fixture(name : String) -> Mbti raise {
  let content = @fs.read_file_to_string(fixtures_path + "/" + name + ".mbti")
  let (mbti, reports) = @mbti_parser.parse_string(content, name~)
  if !reports.is_empty() {
    fail(
      "parsing fixture \{name}.mbti failed with \{reports.length()} diagnostics",
    )
  }
  mbti
}

///|
fn sig_at(mbti : Mbti, index : Int) -> Sig raise {
  match mbti.sigs.nth(index) {
    Some((sig, _)) => sig
    None => fail("expected signature at index \{index}")
  }
}

///|
fn sig_name(sig : Sig) -> String {
  match sig {
    Func({ name, .. })
    | Type({ name, .. })
    | Trait({ name, .. })
    | Const({ name, .. })
    | Value({ name, .. }) => name.name
    Alias(TypeAlias(name~, ..)) | Alias(FuncAlias(name~, ..)) => name.name
    Alias(Using(name~, target~, ..)) =>
      name.map_or(target.name, fn(actual) { actual.name })
    Impl({ trait_name, .. }) => qname_to_string(trait_name)
  }
}

///|
fn qname_to_string(name : QualifiedName) -> String {
  match name.name {
    Ident(name~) => name
    Dot(pkg~, id~) => "\{pkg}.\{id}"
  }
}

///|
fn expect_sig_count(mbti : Mbti, count : Int) -> Unit raise {
  if mbti.sigs.length() != count {
    fail("expected \{count} signatures, got \{mbti.sigs.length()}")
  }
}

///|
fn expect_sig_name(mbti : Mbti, index : Int, name : String) -> Unit raise {
  inspect(sig_name(sig_at(mbti, index)), content=name)
}

///|
fn expect_type_name(ty : Type, name : String) -> Unit raise {
  guard ty is Name(constr_id={ id: Ident(name=actual), .. }, ..) else {
    fail("expected type \{name}")
  }
  inspect(actual, content=name)
}

///|
fn expect_pub_attr(vis : Visibility, attr : String?) -> Unit raise {
  guard vis is Pub(attr=actual, ..) else { fail("expected public visibility") }
  match (actual, attr) {
    (None, None) => ()
    (Some(actual), Some(attr)) => inspect(actual, content=attr)
    _ => fail("unexpected public visibility attribute")
  }
}

///|
fn expect_one_attr(attrs : Attributes, raw : String) -> Unit raise {
  if attrs.length() != 1 {
    fail("expected exactly one attribute, got \{attrs.length()}")
  }
  inspect(attrs.head().unwrap().raw, content=raw)
}

///|
fn expect_constructor_name(
  constrs : @list.List[@mbti_ast.ConstrDecl],
  index : Int,
  name : String,
) -> Unit raise {
  guard constrs.nth(index) is Some(constr) else {
    fail("expected constructor at index \{index}")
  }
  inspect(constr.name.name, content=name)
}

///|
test "parse legacy mbti simple fixture" {
  let mbti = parse_fixture("simple")
  inspect(mbti.package_name, content="username/hello/lib")
  assert_eq(mbti.imports.length(), 2)
  expect_sig_count(mbti, 11)
  expect_sig_name(mbti, 0, "g")
  expect_sig_name(mbti, 1, "h")
  expect_sig_name(mbti, 2, "ZERO")
  expect_sig_name(mbti, 3, "zero")
  expect_sig_name(mbti, 4, "f")
  expect_sig_name(mbti, 5, "i_am_async")
  expect_sig_name(mbti, 6, "custom_err")
  expect_sig_name(mbti, 7, "default_err")
  expect_sig_name(mbti, 8, "err_poly")
  expect_sig_name(mbti, 9, "NoPayload")
  expect_sig_name(mbti, 10, "SinglePayload")
  guard sig_at(mbti, 0)
    is Func({ type_params, params, return_: (_, NoErrorType), .. }) else {
    fail("expected g to be a generic function with no error")
  }
  inspect(type_params.head().unwrap().name.name, content="X")
  inspect(
    qname_to_string(type_params.head().unwrap().constraints.head().unwrap()),
    content="I",
  )
  guard params.head().unwrap() is DiscardPositional(ty=Some(param_ty), ..) else {
    fail("expected g to have a positional typed parameter")
  }
  expect_type_name(param_ty, "A")
  guard sig_at(mbti, 4) is Func({ type_name: Some(type_name), type_params, .. }) else {
    fail("expected T::f to be a method")
  }
  inspect(type_name.name, content="T")
  assert_eq(type_name.is_object, false)
  inspect(type_params.head().unwrap().name.name, content="A")
  guard sig_at(mbti, 5) is Func({ is_async: Some(_), .. }) else {
    fail("expected async function")
  }
  guard sig_at(mbti, 6) is Func({ return_: (_, ErrorType(ty=err_ty)), .. }) else {
    fail("expected custom error return")
  }
  expect_type_name(err_ty, "Failure")
  guard sig_at(mbti, 7) is Func({ return_: (_, DefaultErrorType(_)), .. }) else {
    fail("expected default error return")
  }
  guard sig_at(mbti, 8) is Func({ return_: (_, MaybeError(_)), .. }) else {
    fail("expected polymorphic error return")
  }
  guard sig_at(mbti, 9) is Type({ components: Error(NoPayload), .. }) else {
    fail("expected no-payload suberror")
  }
  guard sig_at(mbti, 10)
    is Type({ components: Error(EnumPayload(constrs)), .. }) else {
    fail("expected single-constructor suberror")
  }
  expect_constructor_name(constrs, 0, "SinglePayload")
  guard constrs.head().unwrap().args is Some(params) else {
    fail("expected single-constructor suberror payload")
  }
  expect_type_name(params.head().unwrap().ty, "Int")
}

///|
test "parse legacy mbti trait fixture" {
  let mbti = parse_fixture("trait")
  inspect(mbti.package_name, content="compiler/mbti_parsing_test/trait")
  expect_sig_count(mbti, 3)
  guard sig_at(mbti, 0)
    is Trait({ name: { name: "I1", .. }, vis: Default, methods, .. }) else {
    fail("expected default-visible I1 trait")
  }
  guard methods.head().unwrap()
    is {
      name: { name: "f", .. },
      has_default: true,
      return_: (_, NoErrorType),
      ..,
    } else {
    fail("expected I1.f default method")
  }
  guard sig_at(mbti, 1) is Trait({ name: { name: "I2", .. }, vis, methods, .. }) else {
    fail("expected public I2 trait")
  }
  expect_pub_attr(vis, None)
  guard methods.head().unwrap() is { name: { name: "g", .. }, params, .. } else {
    fail("expected I2.g method")
  }
  assert_eq(params.length(), 2)
  guard sig_at(mbti, 2) is Trait({ name: { name: "I3", .. }, vis, methods, .. }) else {
    fail("expected pub(open) I3 trait")
  }
  expect_pub_attr(vis, Some("open"))
  guard methods.nth(1)
    is Some({ name: { name: "i", .. }, is_async: Some(_), .. }) else {
    fail("expected async I3.i method")
  }
}

///|
test "parse legacy mbti attributes fixture" {
  let mbti = parse_fixture("attributes")
  expect_sig_count(mbti, 5)
  guard sig_at(mbti, 0) is Trait({ methods, .. }) else {
    fail("expected attributed trait")
  }
  let trait_method = methods.head().unwrap()
  expect_one_attr(trait_method.attrs, "#callsite(autofill(f))")
  guard trait_method.params.nth(1)
    is Some(Labelled(binder={ name: "f", .. }, ..)) else {
    fail("expected labelled f~ parameter")
  }
  guard sig_at(mbti, 1) is Type({ components: Variant(constructors), .. }) else {
    fail("expected enum E")
  }
  expect_one_attr(constructors.head().unwrap().attrs, "#attribute")
  guard sig_at(mbti, 2) is Type({ attrs, components: Record(fields~), .. }) else {
    fail("expected attributed struct S")
  }
  expect_one_attr(attrs, "#deprecated")
  expect_one_attr(fields.head().unwrap().attrs, "#attribute")
  guard sig_at(mbti, 3) is Func({ attrs, params, .. }) else {
    fail("expected attributed allow_positional function")
  }
  expect_one_attr(
    attrs, "#callsite(migration(a, allow_positional=true), migration(d, allow_positional=true))",
  )
  assert_eq(params.length(), 4)
  guard params.nth(0) is Some(Labelled(..)) else {
    fail("expected labelled first parameter")
  }
  guard params.nth(1) is Some(DiscardPositional(..)) else {
    fail("expected positional second parameter")
  }
  guard params.nth(2) is Some(QuestionOptional(..)) else {
    fail("expected optional third parameter")
  }
  guard params.nth(3) is Some(Labelled(..)) else {
    fail("expected labelled fourth parameter")
  }
  guard sig_at(mbti, 4) is Value({ attrs, .. }) else {
    fail("expected attributed value")
  }
  expect_one_attr(attrs, "#hidden")
}

///|
test "parse legacy mbti extensible enum inline case" {
  let source =
    #|package "compiler/mbti_parsing_test/extenum"
    #|
    #|pub(all) extenum Event[T] {
    #|  Base(T)
    #|  Done
    #|}
    #|
    #|pub extenum @base.Event[U] += {
    #|  Added(U)
    #|}
    #|
  let mbti = parse_ok("legacy_extenum.mbti", source)
  expect_sig_count(mbti, 2)
  guard sig_at(mbti, 0)
    is Type(
      {
        name: { name: "Event", .. },
        vis,
        type_params,
        components: ExtensibleEnum(constructors),
        ..,
      }
    ) else {
    fail("expected base extensible enum")
  }
  expect_pub_attr(vis, Some("all"))
  guard type_params.head().unwrap() is Name({ name: "T", .. }) else {
    fail("expected Event[T]")
  }
  expect_constructor_name(constructors, 0, "Base")
  expect_constructor_name(constructors, 1, "Done")
  guard sig_at(mbti, 1)
    is Type(
      {
        name: { name: "Event", .. },
        vis,
        type_params,
        components: ExtendEnum(target~, constructors~),
        ..,
      }
    ) else {
    fail("expected extensible enum extension")
  }
  expect_pub_attr(vis, None)
  guard type_params.head().unwrap() is Name({ name: "U", .. }) else {
    fail("expected Event[U] extension")
  }
  inspect(qname_to_string(target), content="base.Event")
  expect_constructor_name(constructors, 0, "Added")
}
