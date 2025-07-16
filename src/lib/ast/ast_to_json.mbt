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
// ErrorType
pub impl ToJson for ErrorType with to_json(self) {
  match self {
    ErrorType(ty~, is_old_syntax~) =>
      {
        "type": "ErrorType",
        "ty": ty.to_json(),
        "is_old_syntax": is_old_syntax,
      }
    DefaultErrorType(loc, is_old_syntax~) =>
      {
        "type": "DefaultErrorType",
        "0": loc.to_json(),
        "is_old_syntax": is_old_syntax,
      }
    NoErrorType => { "type": "NoErrorType" }
    MaybeError(ty, is_old_syntax~) =>
      {
        "type": "MaybeError",
        "0": ty.to_json(),
        "is_old_syntax": is_old_syntax,
      }
  }
}

///|
pub impl ToJson for Type with to_json(self) {
  match self {
    TAny(loc) => { "type": "TAny", "0": loc.to_json() }
    TArrow(args~, res~, err~, is_async~, loc~) =>
      {
        "type": "TArrow",
        "args": list_to_json(args),
        "res": res.to_json(),
        "err": err.to_json(),
        "is_async": is_async,
        "loc": loc.to_json(),
      }
    TTuple(types, loc~) =>
      { "type": "TTuple", "0": list_to_json(types), "loc": loc.to_json() }
    TName(constr_id, type_args, loc~) =>
      {
        "type": "TName",
        "0": constr_id.to_json(),
        "1": list_to_json(type_args),
        "loc": loc.to_json(),
      }
    TOption(inner_type, loc~, question_loc~) =>
      {
        "type": "TOption",
        "0": inner_type.to_json(),
        "loc": loc.to_json(),
        "question_loc": question_loc.to_json(),
      }
    TObject(constr_id) => { "type": "TObject", "0": constr_id.to_json() }
  }
}

///|
pub impl ToJson for ConstrParam with to_json(self) {
  {
    "type": "ConstrParam",
    "type_": self.type_.to_json(),
    "mut_": self.mut_,
    "label": option_to_json(self.label),
  }
}

///|
pub impl ToJson for ConstrDecl with to_json(self) {
  {
    "type": "ConstrDecl",
    "name": self.name.to_json(),
    "args": option_to_json(self.args.map(list_to_json)),
    "tag": option_to_json(self.tag.map(tuple_to_json)),
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for ExceptionDecl with to_json(self) {
  match self {
    NoPayload => { "type": "NoPayload" }
    SinglePayload(ty) => { "type": "SinglePayload", "0": ty.to_json() }
    EnumPayload(constrs) =>
      { "type": "EnumPayload", "0": list_to_json(constrs) }
  }
}

///|
pub impl ToJson for FieldName with to_json(self) {
  { "type": "FieldName", "name": self.name, "loc": self.loc.to_json() }
}

///|
pub impl ToJson for FieldDecl with to_json(self) {
  {
    "type": "FieldDecl",
    "name": self.name.to_json(),
    "type_": self.type_.to_json(),
    "mut_": self.mut_,
    "vis": self.vis.to_json(),
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for TypeDesc with to_json(self) {
  match self {
    Abstract => { "type": "Abstract" }
    Extern => { "type": "Extern" }
    Newtype(ty) => { "type": "Newtype", "0": ty.to_json() }
    Error(exception_decl) => { "type": "Error", "0": exception_decl.to_json() }
    Variant(constrs) => { "type": "Variant", "0": list_to_json(constrs) }
    Record(fields) => { "type": "Record", "0": list_to_json(fields) }
    Alias(ty) => { "type": "Alias", "0": ty.to_json() }
  }
}

///|
pub impl ToJson for Hole with to_json(self) {
  match self {
    Synthesized => { "type": "Synthesized" }
    Incomplete => { "type": "Incomplete" }
    Todo => { "type": "Todo" }
  }
}

///|
pub impl ToJson for ArgumentKind with to_json(self) {
  match self {
    Positional => { "type": "Positional" }
    Labelled(label) => { "type": "Labelled", "0": label.to_json() }
    LabelledPun(label) => { "type": "LabelledPun", "0": label.to_json() }
    LabelledOption(label~, question_loc~) =>
      {
        "type": "LabelledOption",
        "label": label.to_json(),
        "question_loc": question_loc.to_json(),
      }
    LabelledOptionPun(label~, question_loc~) =>
      {
        "type": "LabelledOptionPun",
        "label": label.to_json(),
        "question_loc": question_loc.to_json(),
      }
  }
}

///|
pub impl ToJson for FnKind with to_json(self) {
  match self {
    Lambda => { "type": "Lambda" }
    Matrix => { "type": "Matrix" }
    Arrow => { "type": "Arrow" }
  }
}

///|
pub impl ToJson for Group with to_json(self) {
  match self {
    GroupBrace => { "type": "GroupBrace" }
    GroupParen => { "type": "GroupParen" }
  }
}

///|
pub impl ToJson for TrailingMark with to_json(self) {
  match self {
    TrailingComma => { "type": "TrailingComma" }
    TrailingSemi => { "type": "TrailingSemi" }
    TrailingNone => { "type": "TrailingNone" }
  }
}

///|
pub impl ToJson for ApplyAttr with to_json(self) {
  match self {
    NoAttr => { "type": "NoAttr" }
    Exclamation => { "type": "Exclamation" }
    Question => { "type": "Question" }
  }
}

///|
pub impl ToJson for TypeVarConstraint with to_json(self) {
  {
    "type": "TypeVarConstraint",
    "trait_": self.trait_.to_json(),
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for TypeVarBinder with to_json(self) {
  {
    "type": "TypeVarBinder",
    "name": self.name,
    "constraints": list_to_json(self.constraints),
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for TypeDeclBinder with to_json(self) {
  {
    "type": "TypeDeclBinder",
    "name": option_to_json(self.name),
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for Binder with to_json(self) {
  { "type": "Binder", "name": self.name, "loc": self.loc.to_json() }
}

///|
pub impl ToJson for Var with to_json(self) {
  { "type": "Var", "name": self.name.to_json(), "loc": self.loc.to_json() }
}

///|
pub impl ToJson for ConstructorExtraInfo with to_json(self) {
  match self {
    TypeName(type_name) => { "type": "TypeName", "0": type_name.to_json() }
    Package(pkg) => { "type": "Package", "0": pkg }
    NoExtraInfo => { "type": "NoExtraInfo" }
  }
}

///|
pub impl ToJson for Constructor with to_json(self) {
  {
    "type": "Constructor",
    "name": self.name.to_json(),
    "extra_info": self.extra_info.to_json(),
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for Accessor with to_json(self) {
  match self {
    Label(label) => { "type": "Label", "0": label.to_json() }
    Index(tuple_index~, loc~) =>
      { "type": "Index", "tuple_index": tuple_index, "loc": loc.to_json() }
    Newtype(loc~) => { "type": "Newtype", "loc": loc.to_json() }
  }
}

///|
pub impl ToJson for AliasTarget with to_json(self) {
  {
    "type": "AliasTarget",
    "binder": self.binder.to_json(),
    "target": option_to_json(self.target),
  }
}

///|
pub impl ToJson for Argument with to_json(self) {
  {
    "type": "Argument",
    "value": self.value.to_json(),
    "kind": self.kind.to_json(),
  }
}

///|
pub impl ToJson for Parameter with to_json(self) {
  match self {
    DiscardPositional(ty~, loc~) =>
      {
        "type": "DiscardPositional",
        "ty": option_to_json(ty),
        "loc": loc.to_json(),
      }
    Positional(binder~, ty~) =>
      {
        "type": "Positional",
        "binder": binder.to_json(),
        "ty": option_to_json(ty),
      }
    Labelled(binder~, ty~) =>
      {
        "type": "Labelled",
        "binder": binder.to_json(),
        "ty": option_to_json(ty),
      }
    Optional(binder~, default~, ty~) =>
      {
        "type": "Optional",
        "binder": binder.to_json(),
        "default": default.to_json(),
        "ty": option_to_json(ty),
      }
    QuestionOptional(binder~, ty~) =>
      {
        "type": "QuestionOptional",
        "binder": binder.to_json(),
        "ty": option_to_json(ty),
      }
  }
}

///|
pub impl ToJson for Case with to_json(self) {
  {
    "type": "Case",
    "pattern": self.pattern.to_json(),
    "guard_": option_to_json(self.guard_),
    "body": self.body.to_json(),
  }
}

///|
pub impl ToJson for MultiArgCase with to_json(self) {
  {
    "type": "MultiArgCase",
    "patterns": list_to_json(self.patterns),
    "guard_": option_to_json(self.guard_),
    "body": self.body.to_json(),
  }
}

///|
pub impl ToJson for SpreadableElem with to_json(self) {
  match self {
    ElemRegular(expr) => { "type": "ElemRegular", "0": expr.to_json() }
    ElemSpread(expr~, loc~) =>
      { "type": "ElemSpread", "expr": expr.to_json(), "loc": loc.to_json() }
  }
}

///|
pub impl ToJson for MapExprElem with to_json(self) {
  {
    "type": "MapExprElem",
    "key": self.key.to_json(),
    "expr": self.expr.to_json(),
    "key_loc": self.key_loc.to_json(),
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for StaticAssertion with to_json(self) {
  {
    "type": "StaticAssertion",
    "assert_type": self.assert_type.to_json(),
    "assert_trait": self.assert_trait.to_json(),
    "assert_loc": self.assert_loc.to_json(),
    "assert_msg": self.assert_msg,
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
        "type": "Lambda",
        "parameters": list_to_json(parameters),
        "params_loc": params_loc.to_json(),
        "body": body.to_json(),
        "return_type": option_to_json(return_type),
        "error_type": error_type.to_json(),
        "kind": kind.to_json(),
        "has_error": option_to_json(has_error),
        "is_async": is_async,
        "loc": loc.to_json(),
      }
    Match(cases~, has_error~, is_async~, fn_loc~, loc~) =>
      {
        "type": "Match",
        "cases": list_to_json(cases),
        "has_error": option_to_json(has_error),
        "is_async": is_async,
        "fn_loc": fn_loc.to_json(),
        "loc": loc.to_json(),
      }
  }
}

///|
pub impl ToJson for FieldDef with to_json(self) {
  {
    "type": "FieldDef",
    "label": self.label.to_json(),
    "expr": self.expr.to_json(),
    "is_pun": self.is_pun,
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for InterpElem with to_json(self) {
  match self {
    InterpLit(repr~, loc~) =>
      { "type": "InterpLit", "repr": repr, "loc": loc.to_json() }
    InterpExpr(expr~, loc~) =>
      { "type": "InterpExpr", "expr": expr.to_json(), "loc": loc.to_json() }
    InterpSource(source) =>
      { "type": "InterpSource", "source": source.to_json() }
  }
}

///|
pub impl ToJson for MultilineStringElem with to_json(self) {
  match self {
    MultilineString(s) => { "type": "MultilineString", "0": s }
    MultilineInterp(elems) =>
      { "type": "MultilineInterp", "0": list_to_json(elems) }
  }
}

///|
pub impl ToJson for DotDotBinder with to_json(self) {
  match self {
    Underscore => { "type": "Underscore" }
    NoBinder => { "type": "NoBinder" }
    BinderAs(binder) => { "type": "BinderAs", "0": binder.to_json() }
    Binder_(binder) => { "type": "Binder_", "0": binder.to_json() }
  }
}

///|
pub impl ToJson for ArrayPattern with to_json(self) {
  match self {
    Pattern(pattern) => { "type": "Pattern", "0": pattern.to_json() }
    StringSpread(s) => { "type": "StringSpread", "0": s }
    BytesSpread(s) => { "type": "BytesSpread", "0": s }
    ConstSpread(binder~, pkg~, loc~) =>
      {
        "type": "ConstSpread",
        "binder": binder.to_json(),
        "pkg": option_to_json(pkg),
        "loc": loc.to_json(),
      }
  }
}

///|
pub impl ToJson for ArrayPatterns with to_json(self) {
  match self {
    Closed(patterns) => { "type": "Closed", "0": list_to_json(patterns) }
    Open(start_patterns, end_patterns, dot_dot_binder) =>
      {
        "type": "Open",
        "0": list_to_json(start_patterns),
        "1": list_to_json(end_patterns),
        "2": dot_dot_binder.to_json(),
      }
  }
}

///|
pub impl ToJson for FieldPat with to_json(self) {
  {
    "type": "FieldPat",
    "label": self.label.to_json(),
    "pattern": self.pattern.to_json(),
    "is_pun": self.is_pun,
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for ConstrPatArg with to_json(self) {
  {
    "type": "ConstrPatArg",
    "pat": self.pat.to_json(),
    "kind": self.kind.to_json(),
  }
}

///|
pub impl ToJson for MapPatElem with to_json(self) {
  {
    "type": "MapPatElem",
    "key": self.key.to_json(),
    "pat": self.pat.to_json(),
    "match_absent": self.match_absent,
    "key_loc": self.key_loc.to_json(),
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for Pattern with to_json(self) {
  match self {
    Alias(pat~, alias_~, loc~) =>
      {
        "type": "Alias",
        "pat": pat.to_json(),
        "alias_": alias_.to_json(),
        "loc": loc.to_json(),
      }
    Any(loc~) => { "type": "Any", "loc": loc.to_json() }
    Array_(pats~, loc~) =>
      { "type": "Array_", "pats": pats.to_json(), "loc": loc.to_json() }
    Constant(c~, loc~) =>
      { "type": "Constant", "c": c.to_json(), "loc": loc.to_json() }
    Constraint(pat~, ty~, loc~) =>
      {
        "type": "Constraint",
        "pat": pat.to_json(),
        "ty": ty.to_json(),
        "loc": loc.to_json(),
      }
    Constr(constr~, args~, is_open~, loc~) =>
      {
        "type": "Constr",
        "constr": constr.to_json(),
        "args": option_to_json(args.map(list_to_json)),
        "is_open": is_open,
        "loc": loc.to_json(),
      }
    Or(pat1~, pat2~, loc~) =>
      {
        "type": "Or",
        "pat1": pat1.to_json(),
        "pat2": pat2.to_json(),
        "loc": loc.to_json(),
      }
    Tuple(pats~, loc~) =>
      { "type": "Tuple", "pats": list_to_json(pats), "loc": loc.to_json() }
    Var(binder) => { "type": "Var", "0": binder.to_json() }
    Record(fields~, is_closed~, loc~) =>
      {
        "type": "Record",
        "fields": list_to_json(fields),
        "is_closed": is_closed,
        "loc": loc.to_json(),
      }
    Map(elems~, is_closed~, loc~) =>
      {
        "type": "Map",
        "elems": list_to_json(elems),
        "is_closed": is_closed,
        "loc": loc.to_json(),
      }
    Range(lhs~, rhs~, inclusive~, loc~) =>
      {
        "type": "Range",
        "lhs": lhs.to_json(),
        "rhs": rhs.to_json(),
        "inclusive": inclusive,
        "loc": loc.to_json(),
      }
  }
}

///|
pub impl ToJson for LocalTypeDecl with to_json(self) {
  {
    "type": "LocalTypeDecl",
    "tycon": self.tycon,
    "tycon_loc": self.tycon_loc.to_json(),
    "components": self.components.to_json(),
    "deriving": list_to_json(self.deriving),
  }
}

///|
pub impl ToJson for DerivingDirective with to_json(self) {
  {
    "type": "DerivingDirective",
    "type_name": self.type_name.to_json(),
    "args": list_to_json(self.args),
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for TypeDecl with to_json(self) {
  {
    "type": "TypeDecl",
    "tycon": self.tycon,
    "tycon_loc": self.tycon_loc.to_json(),
    "params": list_to_json(self.params),
    "components": self.components.to_json(),
    "attrs": list_to_json(self.attrs),
    "doc": self.doc.to_json(),
    "type_vis": self.type_vis.to_json(),
    "deriving": list_to_json(self.deriving),
    "loc": self.loc.to_json(),
    "deprecated_type_bang": self.deprecated_type_bang,
  }
}

///|
pub impl ToJson for FuncStubs with to_json(self) {
  match self {
    Import(module_name~, func_name~) =>
      { "type": "Import", "module_name": module_name, "func_name": func_name }
    Embedded(language~, code~) =>
      {
        "type": "Embedded",
        "language": option_to_json(language),
        "code": code.to_json(),
      }
  }
}

///|
pub impl ToJson for EmbeddedCode with to_json(self) {
  match self {
    CodeString(s) => { "type": "CodeString", "0": s }
    CodeMultilineString(lines) =>
      { "type": "CodeMultilineString", "0": list_to_json(lines) }
  }
}

///|
pub impl ToJson for DeclBody with to_json(self) {
  match self {
    DeclBody(local_types~, expr~) =>
      {
        "type": "DeclBody",
        "local_types": list_to_json(local_types),
        "expr": expr.to_json(),
      }
    DeclStubs(stubs) => { "type": "DeclStubs", "0": stubs.to_json() }
  }
}

///|
pub impl ToJson for FunDecl with to_json(self) {
  {
    "type": "FunDecl",
    "type_name": option_to_json(self.type_name),
    "name": self.name.to_json(),
    "has_error": option_to_json(self.has_error),
    "is_async": self.is_async,
    "decl_params": option_to_json(self.decl_params.map(list_to_json)),
    "params_loc": self.params_loc.to_json(),
    "quantifiers": list_to_json(self.quantifiers),
    "return_type": option_to_json(self.return_type),
    "error_type": self.error_type.to_json(),
    "vis": self.vis.to_json(),
    "attrs": list_to_json(self.attrs),
    "doc": self.doc.to_json(),
  }
}

///|
pub impl ToJson for TraitMethodDecl with to_json(self) {
  {
    "type": "TraitMethodDecl",
    "name": self.name.to_json(),
    "has_error": option_to_json(self.has_error),
    "is_async": self.is_async,
    "quantifiers": list_to_json(self.quantifiers),
    "params": list_to_json(self.params),
    "return_type": option_to_json(self.return_type),
    "error_type": self.error_type.to_json(),
    "has_default": option_to_json(self.has_default),
    "loc": self.loc.to_json(),
  }
}

///|
pub impl ToJson for TraitDecl with to_json(self) {
  {
    "type": "TraitDecl",
    "trait_name": self.trait_name.to_json(),
    "trait_supers": list_to_json(self.trait_supers),
    "trait_methods": list_to_json(self.trait_methods),
    "trait_vis": self.trait_vis.to_json(),
    "trait_loc": self.trait_loc.to_json(),
    "trait_attrs": list_to_json(self.trait_attrs),
    "trait_doc": self.trait_doc.to_json(),
  }
}

///|
// Expr - This is the most complex type with many variants
pub impl ToJson for Expr with to_json(self) {
  match self {
    Apply(func~, args~, attr~, loc~) =>
      {
        "type": "Apply",
        "func": func.to_json(),
        "args": list_to_json(args),
        "attr": attr.to_json(),
        "loc": loc.to_json(),
      }
    Infix(op~, lhs~, rhs~, loc~) =>
      {
        "type": "Infix",
        "op": op.to_json(),
        "lhs": lhs.to_json(),
        "rhs": rhs.to_json(),
        "loc": loc.to_json(),
      }
    Unary(op~, expr~, loc~) =>
      {
        "type": "Unary",
        "op": op.to_json(),
        "expr": expr.to_json(),
        "loc": loc.to_json(),
      }
    Array_(exprs~, loc~) =>
      { "type": "Array_", "exprs": list_to_json(exprs), "loc": loc.to_json() }
    ArraySpread(elems~, loc~) =>
      {
        "type": "ArraySpread",
        "elems": list_to_json(elems),
        "loc": loc.to_json(),
      }
    ArrayGet(array~, index~, loc~) =>
      {
        "type": "ArrayGet",
        "array": array.to_json(),
        "index": index.to_json(),
        "loc": loc.to_json(),
      }
    ArrayGetSlice(array~, start_index~, end_index~, index_loc~, loc~) =>
      {
        "type": "ArrayGetSlice",
        "array": array.to_json(),
        "start_index": option_to_json(start_index),
        "end_index": option_to_json(end_index),
        "index_loc": index_loc.to_json(),
        "loc": loc.to_json(),
      }
    ArraySet(array~, index~, value~, loc~) =>
      {
        "type": "ArraySet",
        "array": array.to_json(),
        "index": index.to_json(),
        "value": value.to_json(),
        "loc": loc.to_json(),
      }
    ArrayAugmentedSet(op~, array~, index~, value~, loc~) =>
      {
        "type": "ArrayAugmentedSet",
        "op": op.to_json(),
        "array": array.to_json(),
        "index": index.to_json(),
        "value": value.to_json(),
        "loc": loc.to_json(),
      }
    Constant(c~, loc~) =>
      { "type": "Constant", "c": c.to_json(), "loc": loc.to_json() }
    MultilineString(elems~, loc~) =>
      {
        "type": "MultilineString",
        "elems": list_to_json(elems),
        "loc": loc.to_json(),
      }
    Interp(elems~, loc~) =>
      { "type": "Interp", "elems": list_to_json(elems), "loc": loc.to_json() }
    Constraint(expr~, ty~, loc~) =>
      {
        "type": "Constraint",
        "expr": expr.to_json(),
        "ty": ty.to_json(),
        "loc": loc.to_json(),
      }
    Constr(constr~, loc~) =>
      { "type": "Constr", "constr": constr.to_json(), "loc": loc.to_json() }
    While(loop_cond~, loop_body~, while_else~, label~, loc~) =>
      {
        "type": "While",
        "loop_cond": loop_cond.to_json(),
        "loop_body": loop_body.to_json(),
        "while_else": option_to_json(while_else),
        "label": option_to_json(label),
        "loc": loc.to_json(),
      }
    Function_(func~, loc~) =>
      { "type": "Function_", "func": func.to_json(), "loc": loc.to_json() }
    Ident(id~, loc~) =>
      { "type": "Ident", "id": id.to_json(), "loc": loc.to_json() }
    If(cond~, ifso~, ifnot~, loc~) =>
      {
        "type": "If",
        "cond": cond.to_json(),
        "ifso": ifso.to_json(),
        "ifnot": option_to_json(ifnot),
        "loc": loc.to_json(),
      }
    Guard(cond~, otherwise~, body~, loc~) =>
      {
        "type": "Guard",
        "cond": cond.to_json(),
        "otherwise": option_to_json(otherwise),
        "body": body.to_json(),
        "loc": loc.to_json(),
      }
    Is(expr~, pat~, loc~) =>
      {
        "type": "Is",
        "expr": expr.to_json(),
        "pat": pat.to_json(),
        "loc": loc.to_json(),
      }
    LetFn(name~, func~, body~, loc~) =>
      {
        "type": "LetFn",
        "name": name.to_json(),
        "func": func.to_json(),
        "body": body.to_json(),
        "loc": loc.to_json(),
      }
    LetRec(bindings~, body~, loc~) =>
      {
        "type": "LetRec",
        "bindings": list_tuple_to_json(bindings),
        "body": body.to_json(),
        "loc": loc.to_json(),
      }
    LetAnd(bindings~, body~, loc~) =>
      {
        "type": "LetAnd",
        "bindings": letand_bindings_to_json(bindings),
        "body": body.to_json(),
        "loc": loc.to_json(),
      }
    Let(pattern~, expr~, body~, loc~) =>
      {
        "type": "Let",
        "pattern": pattern.to_json(),
        "expr": expr.to_json(),
        "body": body.to_json(),
        "loc": loc.to_json(),
      }
    Sequence(exprs~, last_expr~, loc~) =>
      {
        "type": "Sequence",
        "exprs": list_to_json(exprs),
        "last_expr": last_expr.to_json(),
        "loc": loc.to_json(),
      }
    Tuple(exprs~, loc~) =>
      { "type": "Tuple", "exprs": list_to_json(exprs), "loc": loc.to_json() }
    Record(type_name~, fields~, trailing~, loc~) =>
      {
        "type": "Record",
        "type_name": option_to_json(type_name),
        "fields": list_to_json(fields),
        "trailing": trailing.to_json(),
        "loc": loc.to_json(),
      }
    RecordUpdate(type_name~, record~, fields~, loc~) =>
      {
        "type": "RecordUpdate",
        "type_name": option_to_json(type_name),
        "record": record.to_json(),
        "fields": list_to_json(fields),
        "loc": loc.to_json(),
      }
    Field(record~, accessor~, loc~) =>
      {
        "type": "Field",
        "record": record.to_json(),
        "accessor": accessor.to_json(),
        "loc": loc.to_json(),
      }
    Method(type_name~, method_name~, loc~) =>
      {
        "type": "Method",
        "type_name": type_name.to_json(),
        "method_name": method_name.to_json(),
        "loc": loc.to_json(),
      }
    DotApply(self~, method_name~, args~, return_self~, attr~, loc~) =>
      {
        "type": "DotApply",
        "self": self.to_json(),
        "method_name": method_name.to_json(),
        "args": list_to_json(args),
        "return_self": return_self,
        "attr": attr.to_json(),
        "loc": loc.to_json(),
      }
    As(expr~, trait_~, loc~) =>
      {
        "type": "As",
        "expr": expr.to_json(),
        "trait_": trait_.to_json(),
        "loc": loc.to_json(),
      }
    Mutate(record~, accessor~, field~, augmented_by~, loc~) =>
      {
        "type": "Mutate",
        "record": record.to_json(),
        "accessor": accessor.to_json(),
        "field": field.to_json(),
        "augmented_by": option_to_json(augmented_by),
        "loc": loc.to_json(),
      }
    Match(expr~, cases~, match_loc~, using_~, loc~) =>
      {
        "type": "Match",
        "expr": expr.to_json(),
        "cases": list_to_json(cases),
        "match_loc": match_loc.to_json(),
        "using_": option_to_json(using_),
        "loc": loc.to_json(),
      }
    LetMut(binder~, ty~, expr~, body~, loc~) =>
      {
        "type": "LetMut",
        "binder": binder.to_json(),
        "ty": option_to_json(ty),
        "expr": expr.to_json(),
        "body": body.to_json(),
        "loc": loc.to_json(),
      }
    Pipe(lhs~, rhs~, loc~) =>
      {
        "type": "Pipe",
        "lhs": lhs.to_json(),
        "rhs": rhs.to_json(),
        "loc": loc.to_json(),
      }
    Assign(var_~, expr~, augmented_by~, loc~) =>
      {
        "type": "Assign",
        "var_": var_.to_json(),
        "expr": expr.to_json(),
        "augmented_by": option_to_json(augmented_by),
        "loc": loc.to_json(),
      }
    Hole_(loc~, kind~) =>
      { "type": "Hole_", "loc": loc.to_json(), "kind": kind.to_json() }
    Return(return_value~, loc~) =>
      {
        "type": "Return",
        "return_value": option_to_json(return_value),
        "loc": loc.to_json(),
      }
    Raise(err_value~, loc~) =>
      {
        "type": "Raise",
        "err_value": err_value.to_json(),
        "loc": loc.to_json(),
      }
    Unit_(loc~, faked~) =>
      { "type": "Unit_", "loc": loc.to_json(), "faked": faked }
    Break(arg~, label~, loc~) =>
      {
        "type": "Break",
        "arg": option_to_json(arg),
        "label": option_to_json(label),
        "loc": loc.to_json(),
      }
    Continue(args~, label~, loc~) =>
      {
        "type": "Continue",
        "args": list_to_json(args),
        "label": option_to_json(label),
        "loc": loc.to_json(),
      }
    Loop(args~, body~, label~, loop_loc~, loc~) =>
      {
        "type": "Loop",
        "args": list_to_json(args),
        "body": list_to_json(body),
        "label": option_to_json(label),
        "loop_loc": loop_loc.to_json(),
        "loc": loc.to_json(),
      }
    For(binders~, condition~, continue_block~, body~, for_else~, label~, loc~) =>
      {
        "type": "For",
        "binders": list_tuple_to_json(binders),
        "condition": option_to_json(condition),
        "continue_block": list_tuple_to_json(continue_block),
        "body": body.to_json(),
        "for_else": option_to_json(for_else),
        "label": option_to_json(label),
        "loc": loc.to_json(),
      }
    ForEach(binders~, expr~, body~, else_block~, label~, loc~) =>
      {
        "type": "ForEach",
        "binders": list_to_json(binders.map(option_to_json)),
        "expr": expr.to_json(),
        "body": body.to_json(),
        "else_block": option_to_json(else_block),
        "label": option_to_json(label),
        "loc": loc.to_json(),
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
        "type": "Try",
        "body": body.to_json(),
        "catch_": list_to_json(catch_),
        "catch_all": catch_all,
        "try_else": option_to_json(try_else.map(list_to_json)),
        "has_try": has_try,
        "legacy_else": legacy_else,
        "try_loc": try_loc.to_json(),
        "catch_loc": catch_loc.to_json(),
        "else_loc": else_loc.to_json(),
        "loc": loc.to_json(),
      }
    TryQuestion(body~, try_loc~, loc~) =>
      {
        "type": "TryQuestion",
        "body": body.to_json(),
        "try_loc": try_loc.to_json(),
        "loc": loc.to_json(),
      }
    Map(elems~, loc~) =>
      { "type": "Map", "elems": list_to_json(elems), "loc": loc.to_json() }
    Group(expr~, group~, loc~) =>
      {
        "type": "Group",
        "expr": expr.to_json(),
        "group": group.to_json(),
        "loc": loc.to_json(),
      }
    StaticAssert(asserts~, body~) =>
      {
        "type": "StaticAssert",
        "asserts": list_to_json(asserts),
        "body": body.to_json(),
      }
  }
}

///|
// Impl - The top-level implementation/declaration type
pub impl ToJson for Impl with to_json(self) {
  match self {
    TopExpr(expr~, is_main~, local_types~, loc~) =>
      {
        "type": "TopExpr",
        "expr": expr.to_json(),
        "is_main": is_main,
        "local_types": list_to_json(local_types),
        "loc": loc.to_json(),
      }
    TopTest(expr~, name~, params~, local_types~, loc~, attrs~, doc~) =>
      {
        "type": "TopTest",
        "expr": expr.to_json(),
        "name": option_to_json(
          name.map(fn(tuple) {
            let (s, l) = tuple
            Json::array([s.to_json(), l.to_json()])
          }),
        ),
        "params": option_to_json(params.map(list_to_json)),
        "local_types": list_to_json(local_types),
        "loc": loc.to_json(),
        "attrs": list_to_json(attrs),
        "doc": doc.to_json(),
      }
    TopTypeDef(type_decl) => { "type": "TopTypeDef", "0": type_decl.to_json() }
    TopFuncDef(fun_decl~, decl_body~, loc~) =>
      {
        "type": "TopFuncDef",
        "fun_decl": fun_decl.to_json(),
        "decl_body": decl_body.to_json(),
        "loc": loc.to_json(),
      }
    TopFuncAlias(pkg~, type_name~, targets~, vis~, attrs~, is_list~, doc~, loc~) =>
      {
        "type": "TopFuncAlias",
        "pkg": option_to_json(pkg),
        "type_name": option_to_json(type_name),
        "targets": list_to_json(targets),
        "vis": vis.to_json(),
        "attrs": list_to_json(attrs),
        "is_list": is_list,
        "doc": doc.to_json(),
        "loc": loc.to_json(),
      }
    TopLetDef(binder~, ty~, expr~, vis~, is_constant~, loc~, attrs~, doc~) =>
      {
        "type": "TopLetDef",
        "binder": binder.to_json(),
        "ty": option_to_json(ty),
        "expr": expr.to_json(),
        "vis": vis.to_json(),
        "is_constant": is_constant,
        "loc": loc.to_json(),
        "attrs": list_to_json(attrs),
        "doc": doc.to_json(),
      }
    TopTrait(trait_decl) =>
      { "type": "TopTrait", "trait_decl": trait_decl.to_json() }
    TopLegacyTypeAlias(binder~, target~, vis~, deriving~, loc~, attrs~, doc~) =>
      {
        "type": "TopLegacyTypeAlias",
        "binder": binder.to_json(),
        "target": target.to_json(),
        "vis": vis.to_json(),
        "deriving": list_to_json(deriving),
        "loc": loc.to_json(),
        "attrs": list_to_json(attrs),
        "doc": doc.to_json(),
      }
    TopBatchTypeAlias(pkg~, targets~, vis~, loc~, attrs~, is_list~, doc~) =>
      {
        "type": "TopBatchTypeAlias",
        "pkg": option_to_json(pkg),
        "targets": list_to_json(targets),
        "vis": vis.to_json(),
        "loc": loc.to_json(),
        "attrs": list_to_json(attrs),
        "is_list": is_list,
        "doc": doc.to_json(),
      }
    TopBatchTraitAlias(
      pkg~,
      targets~,
      vis~,
      loc~,
      attrs~,
      is_list~,
      is_old_syntax~,
      doc~
    ) =>
      {
        "type": "TopBatchTraitAlias",
        "pkg": option_to_json(pkg),
        "targets": list_to_json(targets),
        "vis": vis.to_json(),
        "loc": loc.to_json(),
        "attrs": list_to_json(attrs),
        "is_list": is_list,
        "is_old_syntax": is_old_syntax,
        "doc": doc.to_json(),
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
        "type": "TopImpl",
        "self_ty": option_to_json(self_ty),
        "trait_": trait_.to_json(),
        "method_name": method_name.to_json(),
        "has_error": option_to_json(has_error),
        "quantifiers": list_to_json(quantifiers),
        "params": list_to_json(params),
        "ret_ty": option_to_json(ret_ty),
        "err_ty": err_ty.to_json(),
        "body": body.to_json(),
        "vis": vis.to_json(),
        "loc": loc.to_json(),
        "attrs": list_to_json(attrs),
        "doc": doc.to_json(),
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
        "type": "TopView",
        "quantifiers": list_to_json(quantifiers),
        "source_ty": source_ty.to_json(),
        "view_constrs": list_to_json(view_constrs),
        "body": list_to_json(body),
        "vis": vis.to_json(),
        "loc": loc.to_json(),
        "attrs": list_to_json(attrs),
        "doc": doc.to_json(),
      }
    TopImplRelation(self_ty~, trait_~, quantifiers~, vis~, attrs~, doc~, loc~) =>
      {
        "type": "TopImplRelation",
        "self_ty": self_ty.to_json(),
        "trait_": trait_.to_json(),
        "quantifiers": list_to_json(quantifiers),
        "vis": vis.to_json(),
        "attrs": list_to_json(attrs),
        "loc": loc.to_json(),
        "doc": doc.to_json(),
      }
  }
}
