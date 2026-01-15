///|
/// A simple pretty printer for MoonBit AST.
/// This module provides functions to convert AST nodes back to MoonBit source code.

///|
/// Print parsed code back to a string.
/// This is a convenience function that takes the result of parse_string directly.
/// Returns None if there are parse errors.
pub fn print_code(parsed : (@syntax.Impls, Array[@basic.Report])) -> String? {
  let (impls, errors) = parsed
  if errors.length() > 0 {
    return None
  }
  Some(print_impls(impls))
}

///|
/// Print a list of top-level implementations to a string.
pub fn print_impls(impls : @syntax.Impls) -> String {
  let buf = StringBuilder::new()
  for impl_ in impls {
    print_impl(buf, impl_)
    buf.write_char('\n')
  }
  buf.to_string()
}

///|
/// Print a single top-level implementation.
fn print_impl(buf : StringBuilder, impl_ : @syntax.Impl) -> Unit {
  match impl_ {
    TopFuncDef(fun_decl~, decl_body~, ..) => {
      print_doc_string(buf, fun_decl.doc)
      print_attributes(buf, fun_decl.attrs)
      print_fun_decl(buf, fun_decl)
      print_decl_body(buf, decl_body)
    }
    TopTypeDef(type_decl) => {
      print_doc_string(buf, type_decl.doc)
      print_attributes(buf, type_decl.attrs)
      print_type_decl(buf, type_decl)
    }
    TopLetDef(binder~, ty~, expr~, vis~, is_constant~, attrs~, doc~, ..) => {
      print_doc_string(buf, doc)
      print_attributes(buf, attrs)
      print_visibility(buf, vis)
      if is_constant {
        buf.write_string("const ")
      } else {
        buf.write_string("let ")
      }
      buf.write_string(binder.name)
      match ty {
        Some(t) => {
          buf.write_string(" : ")
          print_type(buf, t)
        }
        None => ()
      }
      buf.write_string(" = ")
      print_expr(buf, expr)
    }
    TopTest(expr~, name~, attrs~, doc~, ..) => {
      print_doc_string(buf, doc)
      print_attributes(buf, attrs)
      buf.write_string("test ")
      match name {
        Some((lit, _)) => {
          buf.write_char('"')
          buf.write_string(lit)
          buf.write_char('"')
          buf.write_char(' ')
        }
        None => ()
      }
      buf.write_string("{\n")
      print_expr(buf, expr)
      buf.write_string("\n}")
    }
    TopTrait(trait_decl) => {
      print_doc_string(buf, trait_decl.doc)
      print_attributes(buf, trait_decl.attrs)
      print_trait_decl(buf, trait_decl)
    }
    TopImpl(
      self_ty~,
      trait_~,
      method_name~,
      quantifiers~,
      params~,
      ret_ty~,
      body~,
      attrs~,
      doc~,
      ..
    ) => {
      print_doc_string(buf, doc)
      print_attributes(buf, attrs)
      buf.write_string("impl ")
      print_quantifiers(buf, quantifiers)
      print_type_name(buf, trait_)
      match self_ty {
        Some(t) => {
          buf.write_string(" for ")
          print_type(buf, t)
        }
        None => ()
      }
      buf.write_string(" with ")
      buf.write_string(method_name.name)
      buf.write_char('(')
      print_parameters(buf, params)
      buf.write_char(')')
      match ret_ty {
        Some(t) => {
          buf.write_string(" -> ")
          print_type(buf, t)
        }
        None => ()
      }
      buf.write_char(' ')
      print_decl_body(buf, body)
    }
    TopExpr(expr~, ..) => print_expr(buf, expr)
    TopFuncAlias(pkg~, type_name~, targets~, vis~, attrs~, is_list~, doc~, ..) => {
      print_doc_string(buf, doc)
      print_attributes(buf, attrs)
      print_visibility(buf, vis)
      buf.write_string("fnalias ")
      match pkg {
        Some(p) => {
          buf.write_char('@')
          buf.write_string(p.name)
        }
        None => ()
      }
      match type_name {
        Some(tn) => {
          if pkg is Some(_) {
            buf.write_char('.')
          }
          buf.write_string(tn.name)
          buf.write_string("::")
        }
        None => if pkg is Some(_) { buf.write_char('.') }
      }
      print_alias_targets(buf, targets, is_list)
    }
    TopBatchTypeAlias(pkg~, targets~, vis~, attrs~, is_list~, doc~, ..) => {
      print_doc_string(buf, doc)
      print_attributes(buf, attrs)
      print_visibility(buf, vis)
      buf.write_string("typealias ")
      match pkg {
        Some(p) => {
          buf.write_char('@')
          buf.write_string(p.name)
          buf.write_char('.')
        }
        None => ()
      }
      print_alias_targets(buf, targets, is_list)
    }
    TopBatchTraitAlias(pkg~, targets~, vis~, attrs~, is_list~, doc~, ..) => {
      print_doc_string(buf, doc)
      print_attributes(buf, attrs)
      print_visibility(buf, vis)
      buf.write_string("traitalias ")
      match pkg {
        Some(p) => {
          buf.write_char('@')
          buf.write_string(p.name)
          buf.write_char('.')
        }
        None => ()
      }
      print_alias_targets(buf, targets, is_list)
    }
    TopView(
      quantifiers~,
      source_ty~,
      view_type_name~,
      view_constrs~,
      view_func_name~,
      parameters~,
      body~,
      vis~,
      attrs~,
      doc~,
      ..
    ) => {
      print_doc_string(buf, doc)
      print_attributes(buf, attrs)
      print_visibility(buf, vis)
      buf.write_string("enumview ")
      print_quantifiers(buf, quantifiers)
      buf.write_string(view_type_name)
      buf.write_string(" {\n")
      for constr in view_constrs {
        buf.write_string("  ")
        buf.write_string(constr.name.name)
        match constr.args {
          Some(args) if not(args.is_empty()) => {
            buf.write_char('(')
            print_constr_params(buf, args)
            buf.write_char(')')
          }
          _ => ()
        }
        buf.write_char('\n')
      }
      buf.write_string("} for ")
      print_type(buf, source_ty)
      buf.write_string(" with ")
      buf.write_string(view_func_name.name)
      buf.write_char('(')
      print_parameters(buf, parameters)
      buf.write_string(") {\n")
      print_expr(buf, body)
      buf.write_string("\n}")
    }
    TopImplRelation(self_ty~, trait_~, quantifiers~, vis~, attrs~, doc~, ..) => {
      print_doc_string(buf, doc)
      print_attributes(buf, attrs)
      print_visibility(buf, vis)
      buf.write_string("impl ")
      print_quantifiers(buf, quantifiers)
      print_type_name(buf, trait_)
      buf.write_string(" for ")
      print_type(buf, self_ty)
    }
    TopUsing(pkg~, names~, vis~, attrs~, doc~, ..) => {
      print_doc_string(buf, doc)
      print_attributes(buf, attrs)
      print_visibility(buf, vis)
      buf.write_string("using @")
      buf.write_string(pkg.name)
      buf.write_string(" {\n")
      for pair in names {
        let (target, kind) = pair
        buf.write_string("  ")
        match kind {
          Type => buf.write_string("type ")
          Trait => buf.write_string("trait ")
          Value => ()
        }
        buf.write_string(target.binder.name)
        match target.target {
          Some(t) => {
            buf.write_string(" as ")
            buf.write_string(t.name)
          }
          None => ()
        }
        buf.write_char('\n')
      }
      buf.write_char('}')
    }
  }
}

///|
fn print_alias_targets(
  buf : StringBuilder,
  targets : @list.List[@syntax.AliasTarget],
  is_list : Bool,
) -> Unit {
  if is_list {
    buf.write_char('(')
  }
  let mut first = true
  for target in targets {
    if not(first) {
      buf.write_string(", ")
    }
    first = false
    // target = original type name, binder = new alias name
    // Syntax: typealias OriginalName as AliasName
    match target.target {
      Some(t) => {
        buf.write_string(t.name)
        buf.write_string(" as ")
        buf.write_string(target.binder.name)
      }
      None =>
        // No alias, just the name
        buf.write_string(target.binder.name)
    }
  }
  if is_list {
    buf.write_char(')')
  }
}

///|
fn print_fun_decl(buf : StringBuilder, decl : @syntax.FunDecl) -> Unit {
  print_visibility(buf, decl.vis)
  buf.write_string("fn ")
  match decl.type_name {
    Some(tn) => {
      print_type_name(buf, tn)
      buf.write_string("::")
    }
    None => ()
  }
  buf.write_string(decl.name.name)
  print_quantifiers(buf, decl.quantifiers)
  match decl.decl_params {
    Some(params) => {
      buf.write_char('(')
      print_parameters(buf, params)
      buf.write_char(')')
    }
    None => buf.write_string("()")
  }
  match decl.return_type {
    Some(t) => {
      buf.write_string(" -> ")
      print_type(buf, t)
    }
    None => ()
  }
  print_error_type(buf, decl.error_type)
  buf.write_char(' ')
}

///|
fn print_decl_body(
  buf : StringBuilder,
  body : @syntax.DeclBody,
  indent? : Int = 0,
) -> Unit {
  match body {
    DeclBody(expr~, ..) => {
      buf.write_string("{\n")
      print_indent(buf, indent + 1)
      print_expr(buf, expr, indent=indent + 1)
      buf.write_char('\n')
      print_indent(buf, indent)
      buf.write_char('}')
    }
    DeclStubs(stubs) =>
      match stubs {
        Import(module_name~, func_name~, language~) => {
          buf.write_string("= \"")
          match language {
            Some(lang) => {
              buf.write_string(lang)
              buf.write_string("\" \"")
            }
            None => ()
          }
          buf.write_string(module_name)
          buf.write_string("\" \"")
          buf.write_string(func_name)
          buf.write_char('"')
        }
        Embedded(language~, code~) => {
          buf.write_string("= ")
          match language {
            Some(lang) => {
              buf.write_char('"')
              buf.write_string(lang)
              buf.write_string("\" ")
            }
            None => ()
          }
          match code {
            CodeString(s) => {
              buf.write_char('"')
              buf.write_string(s)
              buf.write_char('"')
            }
            CodeMultilineString(lines) =>
              for line in lines {
                buf.write_string("#| ")
                buf.write_string(line)
                buf.write_char('\n')
              }
          }
        }
      }
  }
}

///|
fn print_type_decl(buf : StringBuilder, decl : @syntax.TypeDecl) -> Unit {
  print_visibility(buf, decl.type_vis)
  match decl.components {
    Variant(constrs) => {
      buf.write_string("enum ")
      buf.write_string(decl.tycon)
      print_type_params(buf, decl.params)
      buf.write_string(" {\n")
      for constr in constrs {
        buf.write_string("  ")
        buf.write_string(constr.name.name)
        match constr.args {
          Some(args) if not(args.is_empty()) => {
            buf.write_char('(')
            print_constr_params(buf, args)
            buf.write_char(')')
          }
          _ => ()
        }
        buf.write_char('\n')
      }
      buf.write_char('}')
    }
    Record(fields) => {
      buf.write_string("struct ")
      buf.write_string(decl.tycon)
      print_type_params(buf, decl.params)
      buf.write_string(" {\n")
      for field in fields {
        buf.write_string("  ")
        print_visibility(buf, field.vis)
        if field.mut_ {
          buf.write_string("mut ")
        }
        buf.write_string(field.name.label)
        buf.write_string(" : ")
        print_type(buf, field.ty)
        buf.write_char('\n')
      }
      buf.write_char('}')
    }
    Alias(ty) => {
      buf.write_string("typealias ")
      buf.write_string(decl.tycon)
      print_type_params(buf, decl.params)
      buf.write_string(" = ")
      print_type(buf, ty)
    }
    Newtype(ty) => {
      buf.write_string("type ")
      buf.write_string(decl.tycon)
      print_type_params(buf, decl.params)
      buf.write_char(' ')
      print_type(buf, ty)
    }
    Abstract => {
      buf.write_string("type ")
      buf.write_string(decl.tycon)
      print_type_params(buf, decl.params)
    }
    Extern => {
      buf.write_string("extern type ")
      buf.write_string(decl.tycon)
      print_type_params(buf, decl.params)
    }
    TupleStruct(tys) => {
      buf.write_string("struct ")
      buf.write_string(decl.tycon)
      print_type_params(buf, decl.params)
      buf.write_char('(')
      print_type_list(buf, tys)
      buf.write_char(')')
    }
    Error(ex_decl) => {
      buf.write_string("suberror ")
      buf.write_string(decl.tycon)
      match ex_decl {
        NoPayload => ()
        SinglePayload(ty) => {
          buf.write_char(' ')
          print_type(buf, ty)
        }
        EnumPayload(constrs) => {
          buf.write_string(" {\n")
          for constr in constrs {
            buf.write_string("  ")
            buf.write_string(constr.name.name)
            match constr.args {
              Some(args) if not(args.is_empty()) => {
                buf.write_char('(')
                print_constr_params(buf, args)
                buf.write_char(')')
              }
              _ => ()
            }
            buf.write_char('\n')
          }
          buf.write_char('}')
        }
      }
    }
  }
  if not(decl.deriving.is_empty()) {
    buf.write_string(" derive(")
    let mut first = true
    for d in decl.deriving {
      if not(first) {
        buf.write_string(", ")
      }
      first = false
      print_type_name(buf, d.type_name)
    }
    buf.write_char(')')
  }
}

///|
fn print_trait_decl(buf : StringBuilder, decl : @syntax.TraitDecl) -> Unit {
  print_visibility(buf, decl.vis)
  buf.write_string("trait ")
  buf.write_string(decl.name.name)
  if not(decl.supers.is_empty()) {
    buf.write_string(" : ")
    let mut first = true
    for super_ in decl.supers {
      if not(first) {
        buf.write_string(" + ")
      }
      first = false
      print_long_ident(buf, super_.trait_)
    }
  }
  buf.write_string(" {\n")
  for m in decl.methods {
    buf.write_string("  ")
    buf.write_string(m.name.name)
    buf.write_char('(')
    print_trait_method_params(buf, m.params)
    buf.write_char(')')
    match m.return_type {
      Some(t) => {
        buf.write_string(" -> ")
        print_type(buf, t)
      }
      None => ()
    }
    buf.write_char('\n')
  }
  buf.write_char('}')
}

///|
fn print_visibility(buf : StringBuilder, vis : @syntax.Visibility) -> Unit {
  match vis {
    Default => ()
    Pub(attr~, ..) => {
      buf.write_string("pub")
      match attr {
        Some(a) => {
          buf.write_char('(')
          buf.write_string(a)
          buf.write_char(')')
        }
        None => ()
      }
      buf.write_char(' ')
    }
    Priv(..) => buf.write_string("priv ")
  }
}

///|
fn print_quantifiers(
  buf : StringBuilder,
  quantifiers : @list.List[@syntax.TypeVarBinder],
) -> Unit {
  if not(quantifiers.is_empty()) {
    buf.write_char('[')
    let mut first = true
    for q in quantifiers {
      if not(first) {
        buf.write_string(", ")
      }
      first = false
      buf.write_string(q.name)
      if not(q.constraints.is_empty()) {
        buf.write_string(" : ")
        let mut first_c = true
        for c in q.constraints {
          if not(first_c) {
            buf.write_string(" + ")
          }
          first_c = false
          print_long_ident(buf, c.trait_)
        }
      }
    }
    buf.write_char(']')
  }
}

///|
fn print_type_params(
  buf : StringBuilder,
  params : @list.List[@syntax.TypeDeclBinder],
) -> Unit {
  if not(params.is_empty()) {
    buf.write_char('[')
    let mut first = true
    for p in params {
      if not(first) {
        buf.write_string(", ")
      }
      first = false
      match p.name {
        Some(name) => buf.write_string(name)
        None => buf.write_char('_')
      }
    }
    buf.write_char(']')
  }
}

///|
fn print_constr_params(
  buf : StringBuilder,
  params : @list.List[@syntax.ConstrParam],
) -> Unit {
  let mut first = true
  for p in params {
    if not(first) {
      buf.write_string(", ")
    }
    first = false
    match p.label {
      Some(label) => {
        buf.write_string(label.name)
        buf.write_string("~ : ")
      }
      None => ()
    }
    if p.mut_ {
      buf.write_string("mut ")
    }
    print_type(buf, p.ty)
  }
}

///|
/// Print parameters for trait method signatures.
/// In trait methods, DiscardPositional outputs just the type (e.g., Self)
fn print_trait_method_params(
  buf : StringBuilder,
  params : @list.List[@syntax.Parameter],
) -> Unit {
  let mut first = true
  for p in params {
    if not(first) {
      buf.write_string(", ")
    }
    first = false
    match p {
      Positional(binder~, ty~) => {
        buf.write_string(binder.name)
        match ty {
          Some(t) => {
            buf.write_string(" : ")
            print_type(buf, t)
          }
          None => ()
        }
      }
      DiscardPositional(ty~, ..) =>
        // In trait methods, output just the type (e.g., Self)
        match ty {
          Some(t) => print_type(buf, t)
          None => buf.write_char('_')
        }
      Labelled(binder~, ty~) | Optional(binder~, ty~, ..) => {
        buf.write_string(binder.name)
        match ty {
          Some(t) => {
            buf.write_string(" : ")
            print_type(buf, t)
          }
          None => ()
        }
      }
      _ => ()
    }
  }
}

///|
fn print_parameters(
  buf : StringBuilder,
  params : @list.List[@syntax.Parameter],
) -> Unit {
  let mut first = true
  for p in params {
    if not(first) {
      buf.write_string(", ")
    }
    first = false
    match p {
      Positional(binder~, ty~) => {
        buf.write_string(binder.name)
        match ty {
          Some(t) => {
            buf.write_string(" : ")
            print_type(buf, t)
          }
          None => ()
        }
      }
      DiscardPositional(ty~, ..) => {
        buf.write_char('_')
        match ty {
          Some(t) => {
            buf.write_string(" : ")
            print_type(buf, t)
          }
          None => ()
        }
      }
      Labelled(binder~, ty~) => {
        buf.write_string(binder.name)
        buf.write_char('~')
        match ty {
          Some(t) => {
            buf.write_string(" : ")
            print_type(buf, t)
          }
          None => ()
        }
      }
      Optional(binder~, default~, ty~) => {
        buf.write_string(binder.name)
        buf.write_char('~')
        match ty {
          Some(t) => {
            buf.write_string(" : ")
            print_type(buf, t)
          }
          None => ()
        }
        buf.write_string(" = ")
        print_expr(buf, default)
      }
      QuestionOptional(binder~, ty~) => {
        buf.write_string(binder.name)
        buf.write_char('?')
        match ty {
          Some(t) => {
            buf.write_string(" : ")
            print_type(buf, t)
          }
          None => ()
        }
      }
    }
  }
}

///|
fn print_error_type(buf : StringBuilder, err_ty : @syntax.ErrorType) -> Unit {
  match err_ty {
    NoErrorType => ()
    ErrorType(ty~) => {
      buf.write_char('!')
      print_type(buf, ty)
    }
    DefaultErrorType(..) => buf.write_char('!')
    Noraise(..) => ()
    MaybeError(ty~) => {
      buf.write_char('?')
      print_type(buf, ty)
    }
  }
}

///|
/// Print doc string (/// comments)
fn print_doc_string(buf : StringBuilder, doc : @syntax.DocString) -> Unit {
  for line in doc.content {
    buf.write_string("///")
    buf.write_string(line)
    buf.write_char('\n')
  }
}

///|
/// Print attributes (#name or #name(...))
fn print_attributes(
  buf : StringBuilder,
  attrs : @list.List[@attribute.Attribute],
) -> Unit {
  for attr in attrs {
    buf.write_string(attr.raw)
    buf.write_char('\n')
  }
}
