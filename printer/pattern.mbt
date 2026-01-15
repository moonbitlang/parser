///|
/// Print a pattern.
pub fn print_pattern(buf : StringBuilder, pat : @syntax.Pattern) -> Unit {
  match pat {
    Alias(pat~, alias_~, ..) => {
      print_pattern(buf, pat)
      buf.write_string(" as ")
      buf.write_string(alias_.name)
    }
    Any(..) => buf.write_char('_')
    Array(pats~, ..) => {
      buf.write_char('[')
      print_array_patterns(buf, pats)
      buf.write_char(']')
    }
    Constant(c~, ..) => print_constant(buf, c)
    Constraint(pat~, ty~, ..) => {
      buf.write_char('(')
      print_pattern(buf, pat)
      buf.write_string(" : ")
      print_type(buf, ty)
      buf.write_char(')')
    }
    Constr(constr~, args~, is_open~, ..) => {
      print_constructor(buf, constr)
      match args {
        Some(args) if not(args.is_empty()) => {
          buf.write_char('(')
          print_constr_pat_args(buf, args)
          if is_open {
            buf.write_string(", ..")
          }
          buf.write_char(')')
        }
        _ => if is_open { buf.write_string("(..)") }
      }
    }
    Or(pat1~, pat2~, ..) => {
      print_pattern(buf, pat1)
      buf.write_string(" | ")
      print_pattern(buf, pat2)
    }
    Tuple(pats~, ..) => {
      buf.write_char('(')
      let mut first = true
      for p in pats {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        print_pattern(buf, p)
      }
      buf.write_char(')')
    }
    Var(binder) => buf.write_string(binder.name)
    Record(fields~, is_closed~, ..) => {
      buf.write_string("{ ")
      let mut first = true
      for f in fields {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        if f.is_pun {
          buf.write_string(f.label.name)
        } else {
          buf.write_string(f.label.name)
          buf.write_string(": ")
          print_pattern(buf, f.pattern)
        }
      }
      if not(is_closed) {
        if not(fields.is_empty()) {
          buf.write_string(", ")
        }
        buf.write_string("..")
      }
      buf.write_string(" }")
    }
    Map(elems~, is_closed~, ..) => {
      buf.write_string("{ ")
      let mut first = true
      for e in elems {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        print_constant(buf, e.key)
        buf.write_string(": ")
        if e.match_absent {
          buf.write_char('?')
        }
        print_pattern(buf, e.pat)
      }
      if not(is_closed) {
        if not(elems.is_empty()) {
          buf.write_string(", ")
        }
        buf.write_string("..")
      }
      buf.write_string(" }")
    }
    Range(lhs~, rhs~, kind~, ..) => {
      print_pattern(buf, lhs)
      match kind {
        Inclusive | InclusiveMissingEqual => buf.write_string("..=")
        Exclusive => buf.write_string("..<")
      }
      print_pattern(buf, rhs)
    }
    SpecialConstr(binder~, args~, ..) => {
      buf.write_string(binder.name)
      buf.write_char('(')
      print_constr_pat_args(buf, args)
      buf.write_char(')')
    }
  }
}

///|
fn print_array_patterns(
  buf : StringBuilder,
  pats : @syntax.ArrayPatterns,
) -> Unit {
  match pats {
    Closed(pats) => {
      let mut first = true
      for p in pats {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        print_array_pattern(buf, p)
      }
    }
    Open(before, after, binder) => {
      let mut first = true
      for p in before {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        print_array_pattern(buf, p)
      }
      if not(first) {
        buf.write_string(", ")
      }
      match binder {
        Underscore => buf.write_string(".._")
        NoBinder => buf.write_string("..")
        BinderAs(b) => {
          buf.write_string(".. as ")
          buf.write_string(b.name)
        }
        Binder(b) => {
          buf.write_string("..")
          buf.write_string(b.name)
        }
      }
      for p in after {
        buf.write_string(", ")
        print_array_pattern(buf, p)
      }
    }
  }
}

///|
fn print_array_pattern(buf : StringBuilder, p : @syntax.ArrayPattern) -> Unit {
  match p {
    Pattern(pat) => print_pattern(buf, pat)
    StringSpread(str~, ..) => {
      buf.write_string("..\"")
      buf.write_string(str)
      buf.write_char('"')
    }
    BytesSpread(bytes~, ..) => {
      buf.write_string("..b\"")
      buf.write_string(bytes)
      buf.write_char('"')
    }
    ConstSpread(binder~, pkg~, ..) => {
      buf.write_string("..")
      match pkg {
        Some(p) => {
          buf.write_char('@')
          buf.write_string(p)
          buf.write_char('.')
        }
        None => ()
      }
      buf.write_string(binder.name)
    }
  }
}

///|
fn print_constr_pat_args(
  buf : StringBuilder,
  args : @list.List[@syntax.ConstrPatArg],
) -> Unit {
  let mut first = true
  for arg in args {
    if not(first) {
      buf.write_string(", ")
    }
    first = false
    match arg.kind {
      Positional => ()
      Labelled(label) => {
        buf.write_string(label.name)
        buf.write_string("=")
      }
      LabelledPun(label) => {
        buf.write_string(label.name)
        buf.write_char('~')
        continue // skip pattern printing for pun
      }
      LabelledOption(label~, ..) => {
        buf.write_string(label.name)
        buf.write_string("?=")
      }
      LabelledOptionPun(label~, ..) => {
        buf.write_string(label.name)
        buf.write_char('?')
        continue // skip pattern printing for pun
      }
    }
    print_pattern(buf, arg.pat)
  }
}

///|
fn print_constructor(buf : StringBuilder, c : @syntax.Constructor) -> Unit {
  match c.extra_info {
    TypeName(tn) => {
      print_type_name(buf, tn)
      buf.write_string("::")
    }
    Package(pkg) => {
      buf.write_char('@')
      buf.write_string(pkg)
      buf.write_string("::")
    }
    NoExtraInfo => ()
  }
  buf.write_string(c.name.name)
}
