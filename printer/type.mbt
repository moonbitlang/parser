///|
/// Print a type expression.
pub fn print_type(buf : StringBuilder, ty : @syntax.Type) -> Unit {
  match ty {
    Any(..) => buf.write_char('_')
    Arrow(args~, res~, err~, is_async~, ..) => {
      match is_async {
        Some(_) => buf.write_string("async ")
        None => ()
      }
      buf.write_char('(')
      print_type_list(buf, args)
      buf.write_string(") -> ")
      print_type(buf, res)
      match err {
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
    Tuple(tys~, ..) => {
      buf.write_char('(')
      print_type_list(buf, tys)
      buf.write_char(')')
    }
    Name(constr_id~, tys~, ..) => {
      print_constr_id(buf, constr_id)
      if not(tys.is_empty()) {
        buf.write_char('[')
        print_type_list(buf, tys)
        buf.write_char(']')
      }
    }
    Option(ty~, ..) => {
      print_type(buf, ty)
      buf.write_char('?')
    }
    Object(constr_id) => {
      buf.write_char('&')
      print_constr_id(buf, constr_id)
    }
  }
}

///|
fn print_type_list(buf : StringBuilder, tys : @list.List[@syntax.Type]) -> Unit {
  let mut first = true
  for ty in tys {
    if not(first) {
      buf.write_string(", ")
    }
    first = false
    print_type(buf, ty)
  }
}

///|
fn print_constr_id(buf : StringBuilder, id : @syntax.ConstrId) -> Unit {
  print_long_ident(buf, id.id)
}

///|
fn print_long_ident(buf : StringBuilder, id : @syntax.LongIdent) -> Unit {
  match id {
    Ident(name~) => buf.write_string(name)
    Dot(pkg~, id~) => {
      buf.write_char('@')
      buf.write_string(pkg)
      buf.write_char('.')
      buf.write_string(id)
    }
  }
}

///|
fn print_type_name(buf : StringBuilder, tn : @syntax.TypeName) -> Unit {
  if tn.is_object {
    buf.write_char('&')
  }
  print_long_ident(buf, tn.name)
}
