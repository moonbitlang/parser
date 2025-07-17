///|
let null : Json = Json::null()

///|
pub impl ToJson for Label with to_json(self) {
  { "type": "Label", "name": self.name, "loc": self.loc.to_json() }
}

///| 
// Helper function to convert optional values to JSON
fn[T : ToJson] option_to_json(opt : T?) -> Json {
  match opt {
    Some(value) => value.to_json()
    None => null
  }
}

///|
// Helper function to convert lists to JSON arrays
fn[T : ToJson] list_to_json(list : @list.T[T]) -> Json {
  let array = []
  list.each(fn(item) { array.push(item.to_json()) })
  Json::array(array)
}

///| 
// Helper function to convert tuples to JSON arrays
fn[A : ToJson, B : ToJson] tuple_to_json(tuple : (A, B)) -> Json {
  let (a, b) = tuple
  Json::array([a.to_json(), b.to_json()])
}

///|
// Helper function to convert list of tuples to JSON array
fn[A : ToJson, B : ToJson] list_tuple_to_json(list : @list.T[(A, B)]) -> Json {
  let array = []
  list.each(fn(binding) {
    let (a, b) = binding
    array.push(Json::array([a.to_json(), b.to_json()]))
  })
  Json::array(array)
}

///|
// Helper function for LetAnd bindings specifically
fn letand_bindings_to_json(list : @list.T[(Binder, Type?, Func)]) -> Json {
  let array = []
  list.each(fn(binding) {
    let (binder, ty, func) = binding
    array.push(
      Json::array([binder.to_json(), option_to_json(ty), func.to_json()]),
    )
  })
  Json::array(array)
}

///|
// Visibility
pub impl ToJson for Visibility with to_json(self) {
  match self {
    Default => { "type": "Default" }
    Pub(attr~, loc~) =>
      { "type": "Pub", "attr": option_to_json(attr), "loc": loc.to_json() }
    Priv(loc~) => { "type": "Priv", "loc": loc.to_json() }
  }
}

///|
// Constant 
pub impl ToJson for Constant with to_json(self) {
  match self {
    Bool(b) => { "type": "Bool", "0": b.to_json() }
    Byte(s) => { "type": "Byte", "0": s.to_json() }
    Bytes(s) => { "type": "Bytes", "0": s.to_json() }
    Char(s) => { "type": "Char", "0": s.to_json() }
    Int(s) => { "type": "Int", "0": s.to_json() }
    Int64(s) => { "type": "Int64", "0": s.to_json() }
    UInt(s) => { "type": "UInt", "0": s.to_json() }
    UInt64(s) => { "type": "UInt64", "0": s.to_json() }
    Float(s) => { "type": "Float", "0": s.to_json() }
    Double(s) => { "type": "Double", "0": s.to_json() }
    String(s) => { "type": "String", "0": s.to_json() }
    BigInt(s) => { "type": "BigInt", "0": s.to_json() }
  }
}

///|
// ConstrName
pub impl ToJson for ConstrName with to_json(self) {
  { "type": "ConstrName", "name": self.name, "loc": self.loc.to_json() }
}

///|
// LongIdent
pub impl ToJson for LongIdent with to_json(self) {
  match self {
    Ident(name~) => { "type": "Ident", "name": name }
    Dot(pkg~, id~) => { "type": "Dot", "pkg": pkg, "id": id }
  }
}

///|
// TypeName
pub impl ToJson for TypeName with to_json(self) {
  {
    "type": "TypeName",
    "name": self.name.to_json(),
    "is_object": self.is_object,
    "loc": self.loc.to_json(),
  }
}

///|
// ConstrId
pub impl ToJson for ConstrId with to_json(self) {
  { "type": "ConstrId", "id": self.id.to_json(), "loc": self.loc.to_json() }
}

///|
pub impl ToJson for ErrorType with to_json(self) {
  match self {
    ErrorType(ty~) => { "type": "ErrorType::ErrorType", "ty": ty }
    DefaultErrorType(loc~) =>
      { "type": "ErrorType::DefaultErrorType", "loc": loc }
    NoErrorType => { "type": "ErrorType::NoErrorType" }
    Noraise(loc~) => { "type": "ErrorType::Noraise", "loc": loc }
    MaybeError(ty~) => { "type": "ErrorType::MaybeError", "ty": ty }
  }
}

///|
pub impl ToJson for Type with to_json(self) {
  match self {
    Any(loc~) => { "type": "Type::Any", "loc": loc }
    Arrow(args~, res~, err~, is_async~, loc~) =>
      {
        "type": "Type::Arrow",
        "args": list_to_json(args),
        "res": res,
        "err": err,
        "is_async": is_async,
        "loc": loc,
      }
    Tuple(tys~, loc~) =>
      { "type": "Type::Tuple", "tys": list_to_json(tys), "loc": loc }
    Name(constr_id~, tys~, loc~) =>
      {
        "type": "Type::Name",
        "constr_id": constr_id,
        "tys": list_to_json(tys),
        "loc": loc,
      }
    Option(ty~, loc~, question_loc~) =>
      {
        "type": "Type::Option",
        "ty": ty,
        "loc": loc,
        "question_loc": question_loc,
      }
    Object(constr_id) => { "type": "Type::Object", "0": constr_id }
  }
}

///|
pub impl ToJson for ConstrParam with to_json(self) {
  let { ty, mut_, label } = self
  {
    "type": "ConstrParam",
    "ty": ty,
    "mut_": mut_,
    "label": option_to_json(label),
  }
}

///|
pub impl ToJson for ConstrDecl with to_json(self) {
  let { name, args, tag, loc, doc } = self
  {
    "type": "ConstrDecl",
    "name": name,
    "args": option_to_json(args.map(list_to_json)),
    "tag": option_to_json(tag.map(tuple_to_json)),
    "doc": doc,
    "loc": loc,
  }
}

///|
pub impl ToJson for ExceptionDecl with to_json(self) {
  match self {
    NoPayload => { "type": "ExceptionDecl::NoPayload" }
    SinglePayload(ty) => { "type": "ExceptionDecl::SinglePayload", "0": ty }
    EnumPayload(constrs) =>
      { "type": "ExceptionDecl::EnumPayload", "0": list_to_json(constrs) }
  }
}

///|
pub impl ToJson for FieldName with to_json(self) {
  let { label, loc } = self
  { "type": "FieldName", "label": label, "loc": loc }
}

///|
pub impl ToJson for FieldDecl with to_json(self) {
  let { name, ty, mut_, vis, loc } = self
  {
    "type": "FieldDecl",
    "name": name,
    "ty": ty,
    "mut_": mut_,
    "vis": vis,
    "loc": loc,
  }
}

///|
pub impl ToJson for TypeDesc with to_json(self) {
  match self {
    Abstract => { "type": "TypeDesc::Abstract" }
    Extern => { "type": "TypeDesc::Extern" }
    Newtype(ty) => { "type": "TypeDesc::Newtype", "0": ty }
    Error(exception_decl) => { "type": "TypeDesc::Error", "0": exception_decl }
    Variant(constrs) =>
      { "type": "TypeDesc::Variant", "0": list_to_json(constrs) }
    Record(fields) => { "type": "TypeDesc::Record", "0": list_to_json(fields) }
    Alias(ty) => { "type": "TypeDesc::Alias", "0": ty }
  }
}

///|
pub impl ToJson for Hole with to_json(self) {
  match self {
    Synthesized => { "type": "Hole::Synthesized" }
    Incomplete => { "type": "Hole::Incomplete" }
    Todo => { "type": "Hole::Todo" }
  }
}

///|
pub impl ToJson for ArgumentKind with to_json(self) {
  match self {
    Positional => { "type": "ArgumentKind::Positional" }
    Labelled(label) => { "type": "ArgumentKind::Labelled", "0": label }
    LabelledPun(label) => { "type": "ArgumentKind::LabelledPun", "0": label }
    LabelledOption(label~, question_loc~) =>
      {
        "type": "ArgumentKind::LabelledOption",
        "label": label,
        "question_loc": question_loc,
      }
    LabelledOptionPun(label~, question_loc~) =>
      {
        "type": "ArgumentKind::LabelledOptionPun",
        "label": label,
        "question_loc": question_loc,
      }
  }
}

///|
pub impl ToJson for FnKind with to_json(self) {
  match self {
    Lambda => { "type": "FnKind::Lambda" }
    Matrix => { "type": "FnKind::Matrix" }
    Arrow => { "type": "FnKind::Arrow" }
  }
}

///|
pub impl ToJson for Group with to_json(self) {
  match self {
    Brace => { "type": "Group::Brace" }
    Paren => { "type": "Group::Paren" }
  }
}

///|
pub impl ToJson for TrailingMark with to_json(self) {
  match self {
    Comma => { "type": "Trailing::Comma" }
    Semi => { "type": "Trailing::Semi" }
    None => { "type": "Trailing::None" }
  }
}

///|
pub impl ToJson for ApplyAttr with to_json(self) {
  match self {
    NoAttr => { "type": "ApplyAttr::NoAttr" }
    Exclamation => { "type": "ApplyAttr::Exclamation" }
    Question => { "type": "ApplyAttr::Question" }
  }
}

///|
pub impl ToJson for TypeVarConstraint with to_json(self) {
  let { trait_, loc } = self
  { "type": "TypeVarConstraint", "trait_": trait_, "loc": loc }
}

///|
pub impl ToJson for TypeVarBinder with to_json(self) {
  let { name, constraints, loc } = self
  {
    "type": "TypeVarBinder",
    "name": name,
    "constraints": list_to_json(constraints),
    "loc": loc,
  }
}

///|
pub impl ToJson for TypeDeclBinder with to_json(self) {
  let { name, loc } = self
  { "type": "TypeDeclBinder", "name": option_to_json(name), "loc": loc }
}

///|
pub impl ToJson for Binder with to_json(self) {
  let { name, loc } = self
  { "type": "Binder", "name": name, "loc": loc }
}

///|
pub impl ToJson for Var with to_json(self) {
  let { name, loc } = self
  { "type": "Var", "name": name, "loc": loc }
}

///|
pub impl ToJson for ConstructorExtraInfo with to_json(self) {
  match self {
    TypeName(type_name) =>
      { "type": "ConstructorExtraInfo::TypeName", "0": type_name }
    Package(pkg) => { "type": "ConstructorExtraInfo::Package", "0": pkg }
    NoExtraInfo => { "type": "ConstructorExtraInfo::NoExtraInfo" }
  }
}

///|
pub impl ToJson for Constructor with to_json(self) {
  let { name, extra_info, loc } = self
  { "type": "Constructor", "name": name, "extra_info": extra_info, "loc": loc }
}

///|
pub impl ToJson for Accessor with to_json(self) {
  match self {
    Label(label) => { "type": "Accessor::Label", "0": label }
    Index(tuple_index~, loc~) =>
      { "type": "Accessor::Index", "tuple_index": tuple_index, "loc": loc }
    Newtype(loc~) => { "type": "Accessor::Newtype", "loc": loc }
  }
}

///|
pub impl ToJson for AliasTarget with to_json(self) {
  let { binder, target } = self
  { "type": "AliasTarget", "binder": binder, "target": option_to_json(target) }
}

///|
pub impl ToJson for Argument with to_json(self) {
  let { value, kind } = self
  { "type": "Argument", "value": value, "kind": kind }
}

///|
pub impl ToJson for Parameter with to_json(self) {
  match self {
    DiscardPositional(ty~, loc~) =>
      {
        "type": "Parameter::DiscardPositional",
        "ty": option_to_json(ty),
        "loc": loc,
      }
    Positional(binder~, ty~) =>
      {
        "type": "Parameter::Positional",
        "binder": binder,
        "ty": option_to_json(ty),
      }
    Labelled(binder~, ty~) =>
      {
        "type": "Parameter::Labelled",
        "binder": binder,
        "ty": option_to_json(ty),
      }
    Optional(binder~, default~, ty~) =>
      {
        "type": "Parameter::Optional",
        "binder": binder,
        "default": default,
        "ty": option_to_json(ty),
      }
    QuestionOptional(binder~, ty~) =>
      {
        "type": "Parameter::QuestionOptional",
        "binder": binder,
        "ty": option_to_json(ty),
      }
  }
}

///|
pub impl ToJson for Case with to_json(self) {
  let { pattern, guard_, body } = self
  {
    "type": "Case",
    "pattern": pattern,
    "guard_": option_to_json(guard_),
    "body": body,
  }
}

///|
pub impl ToJson for MultiArgCase with to_json(self) {
  let { patterns, guard_, body } = self
  {
    "type": "MultiArgCase",
    "patterns": list_to_json(patterns),
    "guard_": option_to_json(guard_),
    "body": body,
  }
}

///|
pub impl ToJson for SpreadableElem with to_json(self) {
  match self {
    Regular(expr) => { "type": "SpreadableElem::Regular", "0": expr }
    Spread(expr~, loc~) =>
      { "type": "SpreadableElem::Spread", "expr": expr, "loc": loc }
  }
}

///|
pub impl ToJson for MapExprElem with to_json(self) {
  let { key, expr, key_loc, loc } = self
  {
    "type": "MapExprElem",
    "key": key,
    "expr": expr,
    "key_loc": key_loc,
    "loc": loc,
  }
}

///|
pub impl ToJson for StaticAssertion with to_json(self) {
  let { ty, trait_, loc, msg } = self
  {
    "type": "StaticAssertion",
    "ty": ty,
    "trait_": trait_,
    "loc": loc,
    "msg": msg,
  }
}

///|
pub impl ToJson for Func with to_json(self) {
  match self {
    Lambda(
      parameters~,
      params_loc~,
      body~,
      return_type~,
      error_type~,
      kind~,
      has_error~,
      is_async~,
      loc~
    ) =>
      {
        "type": "Func::Lambda",
        "parameters": list_to_json(parameters),
        "params_loc": params_loc,
        "body": body,
        "return_type": option_to_json(return_type),
        "error_type": error_type,
        "kind": kind,
        "has_error": option_to_json(has_error),
        "is_async": is_async,
        "loc": loc,
      }
    Match(cases~, has_error~, is_async~, fn_loc~, loc~) =>
      {
        "type": "Func::Match",
        "cases": list_to_json(cases),
        "has_error": option_to_json(has_error),
        "is_async": is_async,
        "fn_loc": fn_loc,
        "loc": loc,
      }
  }
}

///|
pub impl ToJson for FieldDef with to_json(self) {
  let { label, expr, is_pun, loc } = self
  {
    "type": "FieldDef",
    "label": label,
    "expr": expr,
    "is_pun": is_pun,
    "loc": loc,
  }
}

///|
pub impl ToJson for InterpElem with to_json(self) {
  match self {
    Literal(repr~, loc~) =>
      { "type": "InterpElem::Literal", "repr": repr, "loc": loc.to_json() }
    Expr(expr~, loc~) =>
      {
        "type": "InterpElem::Expr",
        "expr": expr.to_json(),
        "loc": loc.to_json(),
      }
    Source(source) => { "type": "InterpSource", "source": source.to_json() }
  }
}

///|
pub impl ToJson for MultilineStringElem with to_json(self) {
  match self {
    String(s) => { "type": "MultilineString::String", "0": s }
    Interp(elems) =>
      { "type": "MultilineString::Interp", "0": list_to_json(elems) }
  }
}

///|
pub impl ToJson for DotDotBinder with to_json(self) {
  match self {
    Underscore => { "type": "DotDotBinder::Underscore" }
    NoBinder => { "type": "DotDotBinder::NoBinder" }
    BinderAs(binder) => { "type": "DotDotBinder::BinderAs", "0": binder }
    Binder(binder) => { "type": "DotDotBinder::Binder", "0": binder }
  }
}

///|
pub impl ToJson for ArrayPattern with to_json(self) {
  match self {
    Pattern(pattern) => { "type": "ArrayPattern::Pattern", "0": pattern }
    StringSpread(s) => { "type": "ArrayPattern::StringSpread", "0": s }
    BytesSpread(s) => { "type": "ArrayPattern::BytesSpread", "0": s }
    ConstSpread(binder~, pkg~, loc~) =>
      {
        "type": "ArrayPattern::ConstSpread",
        "binder": binder.to_json(),
        "pkg": option_to_json(pkg),
        "loc": loc.to_json(),
      }
  }
}

///|
pub impl ToJson for ArrayPatterns with to_json(self) {
  match self {
    Closed(patterns) =>
      { "type": "ArrayPatterns::Closed", "0": list_to_json(patterns) }
    Open(start_patterns, end_patterns, dot_dot_binder) =>
      {
        "type": "ArrayPatterns::Open",
        "0": list_to_json(start_patterns),
        "1": list_to_json(end_patterns),
        "2": dot_dot_binder.to_json(),
      }
  }
}

///|
pub impl ToJson for FieldPat with to_json(self) {
  let { label, pattern, is_pun, loc } = self
  {
    "type": "FieldPat",
    "label": label,
    "pattern": pattern,
    "is_pun": is_pun,
    "loc": loc,
  }
}

///|
pub impl ToJson for ConstrPatArg with to_json(self) {
  let { pat, kind } = self
  { "type": "ConstrPatArg", "pat": pat, "kind": kind }
}

///|
pub impl ToJson for MapPatElem with to_json(self) {
  let { key, pat, match_absent, key_loc, loc } = self
  {
    "type": "MapPatElem",
    "key": key,
    "pat": pat,
    "match_absent": match_absent,
    "key_loc": key_loc,
    "loc": loc,
  }
}

///|
pub impl ToJson for Pattern with to_json(self) {
  match self {
    Alias(pat~, alias_~, loc~) =>
      { "type": "Pattern::Alias", "pat": pat, "alias": alias_, "loc": loc }
    Any(loc~) => { "type": "Pattern::Any", "loc": loc }
    Array(pats~, loc~) => { "type": "Pattern::Array", "pats": pats, "loc": loc }
    Constant(c~, loc~) => { "type": "Pattern::Constant", "c": c, "loc": loc }
    Constraint(pat~, ty~, loc~) =>
      { "type": "Pattern::Constraint", "pat": pat, "ty": ty, "loc": loc }
    Constr(constr~, args~, is_open~, loc~) =>
      {
        "type": "Pattern::Constr",
        "constr": constr,
        "args": option_to_json(args.map(list_to_json)),
        "is_open": is_open,
        "loc": loc,
      }
    Or(pat1~, pat2~, loc~) =>
      { "type": "Pattern::Or", "pat1": pat1, "pat2": pat2, "loc": loc }
    Tuple(pats~, loc~) =>
      { "type": "Pattern::Tuple", "pats": list_to_json(pats), "loc": loc }
    Var(binder) => { "type": "Pattern::Var", "0": binder }
    Record(fields~, is_closed~, loc~) =>
      {
        "type": "Pattern::Record",
        "fields": list_to_json(fields),
        "is_closed": is_closed,
        "loc": loc,
      }
    Map(elems~, is_closed~, loc~) =>
      {
        "type": "Pattern::Map",
        "elems": list_to_json(elems),
        "is_closed": is_closed,
        "loc": loc,
      }
    Range(lhs~, rhs~, inclusive~, loc~) =>
      {
        "type": "Pattern::Range",
        "lhs": lhs,
        "rhs": rhs,
        "inclusive": inclusive,
        "loc": loc,
      }
  }
}

///|
pub impl ToJson for LocalTypeDecl with to_json(self) {
  let { tycon, tycon_loc, components, deriving } = self
  {
    "type": "LocalTypeDecl",
    "tycon": tycon,
    "tycon_loc": tycon_loc,
    "components": components,
    "deriving": list_to_json(deriving),
  }
}

///|
pub impl ToJson for DerivingDirective with to_json(self) {
  let { type_name, args, loc } = self
  {
    "type": "DerivingDirective",
    "type_name": type_name,
    "args": list_to_json(args),
    "loc": loc,
  }
}

///|
pub impl ToJson for TypeDecl with to_json(self) {
  let {
    tycon,
    tycon_loc,
    params,
    components,
    attrs,
    doc,
    type_vis,
    deriving,
    loc,
  } = self
  {
    "type": "TypeDecl",
    "tycon": tycon,
    "tycon_loc": tycon_loc,
    "params": list_to_json(params),
    "components": components,
    "attrs": list_to_json(attrs),
    "doc": doc,
    "type_vis": type_vis,
    "deriving": list_to_json(deriving),
    "loc": loc,
  }
}

///|
pub impl ToJson for FuncStubs with to_json(self) {
  match self {
    Import(module_name~, func_name~) =>
      {
        "type": "FuncStubs::Import",
        "module_name": module_name,
        "func_name": func_name,
      }
    Embedded(language~, code~) =>
      {
        "type": "FuncStubs::Embedded",
        "language": option_to_json(language),
        "code": code,
      }
  }
}

///|
pub impl ToJson for EmbeddedCode with to_json(self) {
  match self {
    CodeString(s) => { "type": "EmbeddedCode::CodeString", "0": s }
    CodeMultilineString(lines) =>
      { "type": "EmbeddedCode::CodeMultilineString", "0": list_to_json(lines) }
  }
}

///|
pub impl ToJson for DeclBody with to_json(self) {
  match self {
    DeclBody(local_types~, expr~) =>
      {
        "type": "DeclBody::DeclBody",
        "local_types": list_to_json(local_types),
        "expr": expr,
      }
    DeclStubs(stubs) => { "type": "DeclBody::DeclStubs", "0": stubs }
  }
}

///|
pub impl ToJson for FunDecl with to_json(self) {
  let {
    type_name,
    name,
    has_error,
    is_async,
    decl_params,
    params_loc,
    quantifiers,
    return_type,
    error_type,
    vis,
    attrs,
    doc,
  } = self
  {
    "type": "FunDecl",
    "type_name": option_to_json(type_name),
    "name": name,
    "has_error": option_to_json(has_error),
    "is_async": is_async,
    "decl_params": option_to_json(decl_params.map(list_to_json)),
    "params_loc": params_loc,
    "quantifiers": list_to_json(quantifiers),
    "return_type": option_to_json(return_type),
    "error_type": error_type,
    "vis": vis,
    "attrs": list_to_json(attrs),
    "doc": doc,
  }
}

///|
pub impl ToJson for TraitMethodDecl with to_json(self) {
  let {
    name,
    has_error,
    is_async,
    quantifiers,
    params,
    return_type,
    error_type,
    has_default,
    loc,
  } = self
  {
    "type": "TraitMethodDecl",
    "name": name,
    "has_error": option_to_json(has_error),
    "is_async": is_async,
    "quantifiers": list_to_json(quantifiers),
    "params": list_to_json(params),
    "return_type": option_to_json(return_type),
    "error_type": error_type,
    "has_default": option_to_json(has_default),
    "loc": loc,
  }
}

///|
pub impl ToJson for TraitDecl with to_json(self) {
  let { name, supers, methods, vis, loc, attrs, doc } = self
  {
    "type": "TraitDecl",
    "name": name,
    "supers": list_to_json(supers),
    "methods": list_to_json(methods),
    "vis": vis,
    "loc": loc,
    "attrs": list_to_json(attrs),
    "doc": doc,
  }
}

///|
// Expr - This is the most complex type with many variants
pub impl ToJson for Expr with to_json(self) {
  match self {
    Apply(func~, args~, attr~, loc~) =>
      {
        "type": "Expr::Apply",
        "func": func,
        "args": list_to_json(args),
        "attr": attr,
        "loc": loc,
      }
    Infix(op~, lhs~, rhs~, loc~) =>
      { "type": "Expr::Infix", "op": op, "lhs": lhs, "rhs": rhs, "loc": loc }
    Unary(op~, expr~, loc~) =>
      { "type": "Expr::Unary", "op": op, "expr": expr, "loc": loc }
    Array(exprs~, loc~) =>
      { "type": "Expr::Array", "exprs": list_to_json(exprs), "loc": loc }
    ArraySpread(elems~, loc~) =>
      { "type": "Expr::ArraySpread", "elems": list_to_json(elems), "loc": loc }
    ArrayGet(array~, index~, loc~) =>
      { "type": "Expr::ArrayGet", "array": array, "index": index, "loc": loc }
    ArrayGetSlice(array~, start_index~, end_index~, index_loc~, loc~) =>
      {
        "type": "Expr::ArrayGetSlice",
        "array": array,
        "start_index": option_to_json(start_index),
        "end_index": option_to_json(end_index),
        "index_loc": index_loc,
        "loc": loc,
      }
    ArraySet(array~, index~, value~, loc~) =>
      {
        "type": "Expr::ArraySet",
        "array": array,
        "index": index,
        "value": value,
        "loc": loc,
      }
    ArrayAugmentedSet(op~, array~, index~, value~, loc~) =>
      {
        "type": "Expr::ArrayAugmentedSet",
        "op": op,
        "array": array,
        "index": index,
        "value": value,
        "loc": loc,
      }
    Constant(c~, loc~) => { "type": "Expr::Constant", "c": c, "loc": loc }
    MultilineString(elems~, loc~) =>
      {
        "type": "Expr::MultilineString",
        "elems": list_to_json(elems),
        "loc": loc,
      }
    Interp(elems~, loc~) =>
      { "type": "Expr::Interp", "elems": list_to_json(elems), "loc": loc }
    Constraint(expr~, ty~, loc~) =>
      { "type": "Expr::Constraint", "expr": expr, "ty": ty, "loc": loc }
    Constr(constr~, loc~) =>
      { "type": "Expr::Constr", "constr": constr, "loc": loc }
    While(loop_cond~, loop_body~, while_else~, label~, loc~) =>
      {
        "type": "Expr::While",
        "loop_cond": loop_cond,
        "loop_body": loop_body,
        "while_else": option_to_json(while_else),
        "label": option_to_json(label),
        "loc": loc,
      }
    Function(func~, loc~) =>
      { "type": "Expr::Function", "func": func, "loc": loc }
    Ident(id~, loc~) => { "type": "Expr::Ident", "id": id, "loc": loc }
    If(cond~, ifso~, ifnot~, loc~) =>
      {
        "type": "Expr::If",
        "cond": cond,
        "ifso": ifso,
        "ifnot": option_to_json(ifnot),
        "loc": loc,
      }
    Guard(cond~, otherwise~, body~, loc~) =>
      {
        "type": "Expr::Guard",
        "cond": cond,
        "otherwise": option_to_json(otherwise),
        "body": body,
        "loc": loc,
      }
    Is(expr~, pat~, loc~) =>
      { "type": "Expr::Is", "expr": expr, "pat": pat, "loc": loc }
    Defer(expr~, body~, loc~) =>
      { "type": "Expr::Defer", "expr": expr, "body": body, "loc": loc }
    LetFn(name~, func~, body~, loc~) =>
      {
        "type": "Expr::LetFn",
        "name": name,
        "func": func,
        "body": body,
        "loc": loc,
      }
    LetRec(bindings~, body~, loc~) =>
      {
        "type": "Expr::LetRec",
        "bindings": list_tuple_to_json(bindings),
        "body": body,
        "loc": loc,
      }
    LetAnd(bindings~, body~, loc~) =>
      {
        "type": "Expr::LetAnd",
        "bindings": letand_bindings_to_json(bindings),
        "body": body,
        "loc": loc,
      }
    Let(pattern~, expr~, body~, loc~) =>
      {
        "type": "Expr::Let",
        "pattern": pattern,
        "expr": expr,
        "body": body,
        "loc": loc,
      }
    Sequence(exprs~, last_expr~, loc~) =>
      {
        "type": "Expr::Sequence",
        "exprs": list_to_json(exprs),
        "last_expr": last_expr,
        "loc": loc,
      }
    Tuple(exprs~, loc~) =>
      { "type": "Expr::Tuple", "exprs": list_to_json(exprs), "loc": loc }
    Record(type_name~, fields~, trailing~, loc~) =>
      {
        "type": "Expr::Record",
        "type_name": option_to_json(type_name),
        "fields": list_to_json(fields),
        "trailing": trailing,
        "loc": loc,
      }
    RecordUpdate(type_name~, record~, fields~, loc~) =>
      {
        "type": "Expr::RecordUpdate",
        "type_name": option_to_json(type_name),
        "record": record,
        "fields": list_to_json(fields),
        "loc": loc,
      }
    Field(record~, accessor~, loc~) =>
      {
        "type": "Expr::Field",
        "record": record,
        "accessor": accessor,
        "loc": loc,
      }
    Method(type_name~, method_name~, loc~) =>
      {
        "type": "Expr::Method",
        "type_name": type_name,
        "method_name": method_name,
        "loc": loc,
      }
    DotApply(self~, method_name~, args~, return_self~, attr~, loc~) =>
      {
        "type": "Expr::DotApply",
        "self": self,
        "method_name": method_name,
        "args": list_to_json(args),
        "return_self": return_self,
        "attr": attr,
        "loc": loc,
      }
    As(expr~, trait_~, loc~) =>
      { "type": "Expr::As", "expr": expr, "trait_": trait_, "loc": loc }
    Mutate(record~, accessor~, field~, augmented_by~, loc~) =>
      {
        "type": "Expr::Mutate",
        "record": record,
        "accessor": accessor,
        "field": field,
        "augmented_by": option_to_json(augmented_by),
        "loc": loc.to_json(),
      }
    Match(expr~, cases~, match_loc~, using_~, loc~) =>
      {
        "type": "Expr::Match",
        "expr": expr,
        "cases": list_to_json(cases),
        "match_loc": match_loc,
        "using_": option_to_json(using_),
        "loc": loc,
      }
    LetMut(binder~, ty~, expr~, body~, loc~) =>
      {
        "type": "Expr::LetMut",
        "binder": binder,
        "ty": option_to_json(ty),
        "expr": expr,
        "body": body,
        "loc": loc,
      }
    Pipe(lhs~, rhs~, loc~) =>
      { "type": "Expr::Pipe", "lhs": lhs, "rhs": rhs, "loc": loc }
    Assign(var_~, expr~, augmented_by~, loc~) =>
      {
        "type": "Expr::Assign",
        "var_": var_,
        "expr": expr,
        "augmented_by": option_to_json(augmented_by),
        "loc": loc,
      }
    Hole(loc~, kind~) => { "type": "Expr::Hole", "loc": loc, "kind": kind }
    Return(return_value~, loc~) =>
      {
        "type": "Expr::Return",
        "return_value": option_to_json(return_value),
        "loc": loc,
      }
    Raise(err_value~, loc~) =>
      { "type": "Expr::Raise", "err_value": err_value, "loc": loc }
    Unit(loc~, faked~) => { "type": "Expr::Unit", "loc": loc, "faked": faked }
    Break(arg~, label~, loc~) =>
      {
        "type": "Expr::Break",
        "arg": option_to_json(arg),
        "label": option_to_json(label),
        "loc": loc,
      }
    Continue(args~, label~, loc~) =>
      {
        "type": "Expr::Continue",
        "args": list_to_json(args),
        "label": option_to_json(label),
        "loc": loc,
      }
    Loop(args~, body~, label~, loop_loc~, loc~) =>
      {
        "type": "Expr::Loop",
        "args": list_to_json(args),
        "body": list_to_json(body),
        "label": option_to_json(label),
        "loop_loc": loop_loc,
        "loc": loc,
      }
    For(binders~, condition~, continue_block~, body~, for_else~, label~, loc~) =>
      {
        "type": "Expr::For",
        "binders": list_tuple_to_json(binders),
        "condition": option_to_json(condition),
        "continue_block": list_tuple_to_json(continue_block),
        "body": body,
        "for_else": option_to_json(for_else),
        "label": option_to_json(label),
        "loc": loc,
      }
    ForEach(binders~, expr~, body~, else_block~, label~, loc~) =>
      {
        "type": "Expr::ForEach",
        "binders": list_to_json(binders.map(option_to_json)),
        "expr": expr,
        "body": body,
        "else_block": option_to_json(else_block),
        "label": option_to_json(label),
        "loc": loc,
      }
    Try(
      body~,
      catch_~,
      catch_all~,
      try_else~,
      has_try~,
      legacy_else~,
      try_loc~,
      catch_loc~,
      else_loc~,
      loc~
    ) =>
      {
        "type": "Expr::Try",
        "body": body,
        "catch_": list_to_json(catch_),
        "catch_all": catch_all,
        "try_else": option_to_json(try_else.map(list_to_json)),
        "has_try": has_try,
        "legacy_else": legacy_else,
        "try_loc": try_loc,
        "catch_loc": catch_loc,
        "else_loc": else_loc,
        "loc": loc,
      }
    TryOperator(body~, kind~, try_loc~, loc~) =>
      {
        "type": "Expr::TryOperator",
        "kind": kind,
        "body": body,
        "try_loc": try_loc,
        "loc": loc,
      }
    Map(elems~, loc~) =>
      { "type": "Expr::Map", "elems": list_to_json(elems), "loc": loc }
    Group(expr~, group~, loc~) =>
      { "type": "Expr::Group", "expr": expr, "group": group, "loc": loc }
    StaticAssert(asserts~, body~) =>
      {
        "type": "Expr::StaticAssert",
        "asserts": list_to_json(asserts),
        "body": body,
      }
  }
}

///|
// Impl - The top-level implementation/declaration type
pub impl ToJson for Impl with to_json(self) {
  match self {
    TopExpr(expr~, is_main~, local_types~, loc~) =>
      {
        "type": "Impl::TopExpr",
        "expr": expr,
        "is_main": is_main,
        "local_types": list_to_json(local_types),
        "loc": loc,
      }
    TopTest(expr~, name~, params~, local_types~, loc~, attrs~, doc~) =>
      {
        "type": "Impl::TopTest",
        "expr": expr,
        "name": option_to_json(
          name.map(fn(tuple) {
            let (s, l) = tuple
            ([s, l] : Json)
          }),
        ),
        "params": option_to_json(params.map(list_to_json)),
        "local_types": list_to_json(local_types),
        "loc": loc,
        "attrs": list_to_json(attrs),
        "doc": doc,
      }
    TopTypeDef(type_decl) => { "type": "Impl::TopTypeDef", "0": type_decl }
    TopFuncDef(fun_decl~, decl_body~, loc~) =>
      {
        "type": "Impl::TopFuncDef",
        "fun_decl": fun_decl,
        "decl_body": decl_body,
        "loc": loc,
      }
    TopFuncAlias(pkg~, type_name~, targets~, vis~, attrs~, is_list~, doc~, loc~) =>
      {
        "type": "Impl::TopFuncAlias",
        "pkg": option_to_json(pkg),
        "type_name": option_to_json(type_name),
        "targets": list_to_json(targets),
        "vis": vis,
        "attrs": list_to_json(attrs),
        "is_list": is_list,
        "doc": doc,
        "loc": loc,
      }
    TopLetDef(binder~, ty~, expr~, vis~, is_constant~, loc~, attrs~, doc~) =>
      {
        "type": "Impl::TopLetDef",
        "binder": binder,
        "ty": option_to_json(ty),
        "expr": expr,
        "vis": vis,
        "is_constant": is_constant,
        "loc": loc,
        "attrs": list_to_json(attrs),
        "doc": doc,
      }
    TopTrait(trait_decl) =>
      { "type": "Impl::TopTrait", "trait_decl": trait_decl }
    TopBatchTypeAlias(pkg~, targets~, vis~, loc~, attrs~, is_list~, doc~) =>
      {
        "type": "Impl::TopBatchTypeAlias",
        "pkg": option_to_json(pkg),
        "targets": list_to_json(targets),
        "vis": vis,
        "loc": loc,
        "attrs": list_to_json(attrs),
        "is_list": is_list,
        "doc": doc,
      }
    TopBatchTraitAlias(pkg~, targets~, vis~, loc~, attrs~, is_list~, doc~) =>
      {
        "type": "Impl::TopBatchTraitAlias",
        "pkg": option_to_json(pkg),
        "targets": list_to_json(targets),
        "vis": vis,
        "loc": loc,
        "attrs": list_to_json(attrs),
        "is_list": is_list,
        "doc": doc,
      }
    TopImpl(
      self_ty~,
      trait_~,
      method_name~,
      has_error~,
      quantifiers~,
      params~,
      ret_ty~,
      err_ty~,
      body~,
      vis~,
      loc~,
      attrs~,
      doc~
    ) =>
      {
        "type": "Impl::TopImpl",
        "self_ty": option_to_json(self_ty),
        "trait_": trait_,
        "method_name": method_name,
        "has_error": option_to_json(has_error),
        "quantifiers": list_to_json(quantifiers),
        "params": list_to_json(params),
        "ret_ty": option_to_json(ret_ty),
        "err_ty": err_ty,
        "body": body,
        "vis": vis,
        "loc": loc,
        "attrs": list_to_json(attrs),
        "doc": doc,
      }
    TopView(
      quantifiers~,
      source_ty~,
      view_constrs~,
      body~,
      vis~,
      loc~,
      attrs~,
      doc~
    ) =>
      {
        "type": "Impl::TopView",
        "quantifiers": list_to_json(quantifiers),
        "source_ty": source_ty,
        "view_constrs": list_to_json(view_constrs),
        "body": list_to_json(body),
        "vis": vis,
        "loc": loc,
        "attrs": list_to_json(attrs),
        "doc": doc,
      }
    TopImplRelation(self_ty~, trait_~, quantifiers~, vis~, attrs~, doc~, loc~) =>
      {
        "type": "Impl::TopImplRelation",
        "self_ty": self_ty,
        "trait_": trait_,
        "quantifiers": list_to_json(quantifiers),
        "vis": vis,
        "attrs": list_to_json(attrs),
        "loc": loc,
        "doc": doc,
      }
  }
}

///|
pub impl ToJson for TryOperatorKind with to_json(self) {
  match self {
    Question => { "type": "TryOperatorKind::Question" }
    Exclamation => { "type": "TryOperatorKind::Exclamation" }
  }
}
