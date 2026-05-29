///|
struct Identity(Unit)

///|
impl MapVisitor for Identity

///|
test "Pattern" {
  let Identity(_) = Identity(())
  let binder = Binder::{ name: "test_binder", loc: dummy_loc }
  let any_pattern = Pattern::Any(loc=dummy_loc)
  let int_constant = Int("42")
  let constant_pattern = Pattern::Constant(c=int_constant, loc=dummy_loc)
  let int_type = Name(
    constr_id=ConstrId::{ id: LongIdent::Ident(name="Int"), loc: dummy_loc },
    tys=@list.from_array([]),
    type_args_lbracket_loc=None,
    type_args_rbracket_loc=None,
    type_arg_comma_locs=@list.empty(),
    loc=dummy_loc,
  )
  let constr_name = ConstrName::{ name: "TestConstructor", loc: dummy_loc }
  let constr = Constructor::{
    name: constr_name,
    extra_info: NoExtraInfo,
    loc: dummy_loc,
  }
  let label = Label::{ name: "test_field", loc: dummy_loc }
  let field_pat = FieldPat::{
    label,
    pattern: any_pattern,
    is_pun: false,
    colon_loc: Some(dummy_loc),
    loc: dummy_loc,
  }
  let map_elem = MapPatElem::{
    key: int_constant,
    pat: any_pattern,
    match_absent: false,
    key_loc: dummy_loc,
    question_loc: None,
    colon_loc: dummy_loc,
    loc: dummy_loc,
  }
  let constr_pat_arg = ConstrPatArg::{
    pat: any_pattern,
    kind: ArgumentKind::Positional,
    equal_loc: None,
  }
  let array_patterns = ArrayPatterns::Closed(
    @list.from_array([]),
    comma_locs=@list.empty(),
  )
  let tuple_pattern = Pattern::Tuple(
    pats=@list.from_array([any_pattern, constant_pattern]),
    lparen_loc=dummy_loc,
    rparen_loc=dummy_loc,
    comma_locs=@list.singleton(dummy_loc),
    loc=dummy_loc,
  )
  let any_with_alias = Pattern::Alias(
    pat=any_pattern,
    alias_=binder,
    as_loc=dummy_loc,
    loc=dummy_loc,
  )
  let array_patterns = Pattern::Array(
    pats=array_patterns,
    lbracket_loc=dummy_loc,
    rbracket_loc=dummy_loc,
    loc=dummy_loc,
  )
  let or_pattern = Or(
    pat1=any_pattern,
    pat2=constant_pattern,
    bar_loc=dummy_loc,
    loc=dummy_loc,
  )
  let range_pattern = Range(
    lhs=any_pattern,
    rhs=constant_pattern,
    kind=Inclusive,
    range_op_loc=dummy_loc,
    loc=dummy_loc,
  )
  let constant_pattern = Expr::Constant(c=int_constant, loc=dummy_loc)
  let constraint_pattern = Pattern::Constraint(
    pat=any_pattern,
    ty=int_type,
    lparen_loc=Some(dummy_loc),
    colon_loc=dummy_loc,
    rparen_loc=Some(dummy_loc),
    loc=dummy_loc,
  )
  let constr_pattern = Pattern::Constr(
    constr~,
    args=Some(@list.from_array([constr_pat_arg])),
    is_open=false,
    args_lparen_loc=Some(dummy_loc),
    args_rparen_loc=Some(dummy_loc),
    arg_comma_locs=@list.empty(),
    open_dotdot_loc=None,
    loc=dummy_loc,
  )
  let record_pattern = Pattern::Record(
    fields=@list.from_array([field_pat]),
    is_closed=true,
    lbrace_loc=dummy_loc,
    rbrace_loc=dummy_loc,
    comma_locs=@list.empty(),
    dotdot_loc=None,
    loc=dummy_loc,
  )
  let map_pattern = Pattern::Map(
    elems=@list.from_array([map_elem]),
    is_closed=false,
    lbrace_loc=dummy_loc,
    rbrace_loc=dummy_loc,
    comma_locs=@list.empty(),
    dotdot_loc=Some(dummy_loc),
    loc=dummy_loc,
  )
  @test.assert_eq(
    any_with_alias.json_repr(),
    MapVisitorBase::visit_Pattern(MapVisitorBase(Identity(())), any_with_alias).json_repr(),
  )
  @test.assert_eq(
    array_patterns.json_repr(),
    MapVisitorBase::visit_Pattern(MapVisitorBase(Identity(())), array_patterns).json_repr(),
  )
  @test.assert_eq(
    constant_pattern.json_repr(),
    MapVisitorBase::visit_Expr(MapVisitorBase(Identity(())), constant_pattern).json_repr(),
  )
  @test.assert_eq(
    constraint_pattern.json_repr(),
    MapVisitorBase::visit_Pattern(
      MapVisitorBase(Identity(())),
      constraint_pattern,
    ).json_repr(),
  )
  @test.assert_eq(
    constr_pattern.json_repr(),
    MapVisitorBase::visit_Pattern(MapVisitorBase(Identity(())), constr_pattern).json_repr(),
  )
  @test.assert_eq(
    or_pattern.json_repr(),
    MapVisitorBase::visit_Pattern(MapVisitorBase(Identity(())), or_pattern).json_repr(),
  )
  @test.assert_eq(
    tuple_pattern.json_repr(),
    MapVisitorBase::visit_Pattern(MapVisitorBase(Identity(())), tuple_pattern).json_repr(),
  )
  @test.assert_eq(
    record_pattern.json_repr(),
    MapVisitorBase::visit_Pattern(MapVisitorBase(Identity(())), record_pattern).json_repr(),
  )
  @test.assert_eq(
    map_pattern.json_repr(),
    MapVisitorBase::visit_Pattern(MapVisitorBase(Identity(())), map_pattern).json_repr(),
  )
  @test.assert_eq(
    range_pattern.json_repr(),
    MapVisitorBase::visit_Pattern(MapVisitorBase(Identity(())), range_pattern).json_repr(),
  )
}
