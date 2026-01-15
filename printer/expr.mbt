///|
/// Write indentation (tabs)
fn print_indent(buf : StringBuilder, indent : Int) -> Unit {
  for _ in 0..<indent {
    buf.write_char('\t')
  }
}

///|
/// Get operator name from LongIdent
fn long_ident_to_string(id : @syntax.LongIdent) -> String {
  match id {
    Ident(name~) => name
    Dot(pkg~, id~) => "@" + pkg + "." + id
  }
}

///|
/// Get operator precedence (higher = binds tighter)
fn get_precedence(op : String) -> Int {
  // MoonBit operator precedence (based on common conventions)
  match op {
    "||" => 1
    "&&" => 2
    "==" | "!=" | "<" | ">" | "<=" | ">=" => 3
    "|" => 4
    "^" => 5
    "&" => 6
    "<<" | ">>" => 7
    "+" | "-" => 8
    "*" | "/" | "%" => 9
    _ => 10 // custom operators, highest precedence
  }
}

///|
/// Check if expression needs parentheses in a binary context
fn needs_parens(expr : @syntax.Expr, parent_prec : Int) -> Bool {
  match expr {
    Infix(op~, ..) =>
      get_precedence(long_ident_to_string(op.name)) < parent_prec
    _ => false
  }
}

///|
/// Print expression with optional parentheses based on precedence
fn print_expr_with_prec(
  buf : StringBuilder,
  expr : @syntax.Expr,
  parent_prec : Int,
  indent : Int,
) -> Unit {
  if needs_parens(expr, parent_prec) {
    buf.write_char('(')
    print_expr(buf, expr, indent~)
    buf.write_char(')')
  } else {
    print_expr(buf, expr, indent~)
  }
}

///|
/// Print an expression.
pub fn print_expr(
  buf : StringBuilder,
  expr : @syntax.Expr,
  indent? : Int = 0,
) -> Unit {
  match expr {
    // Literals
    Constant(c~, ..) => print_constant(buf, c)
    Unit(..) => buf.write_string("()")

    // Identifiers
    Ident(id~, ..) => print_var(buf, id)
    Constr(constr~, ..) => print_constructor(buf, constr)

    // Collections
    Array(exprs~, ..) => {
      buf.write_char('[')
      let mut first = true
      for e in exprs {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        print_expr(buf, e)
      }
      buf.write_char(']')
    }
    ArraySpread(elems~, ..) => {
      buf.write_char('[')
      let mut first = true
      for e in elems {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        match e {
          Regular(expr) => print_expr(buf, expr)
          Spread(expr~, ..) => {
            buf.write_string("..")
            print_expr(buf, expr)
          }
        }
      }
      buf.write_char(']')
    }
    Tuple(exprs~, ..) => {
      buf.write_char('(')
      let mut first = true
      for e in exprs {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        print_expr(buf, e)
      }
      buf.write_char(')')
    }

    // Operators
    Apply(func~, args~, attr~, ..) => {
      print_expr(buf, func)
      buf.write_char('(')
      print_arguments(buf, args)
      buf.write_char(')')
      match attr {
        NoAttr => ()
        Exclamation => buf.write_char('!')
        Question => buf.write_char('?')
      }
    }
    Infix(op~, lhs~, rhs~, ..) => {
      let prec = get_precedence(long_ident_to_string(op.name))
      print_expr_with_prec(buf, lhs, prec, indent)
      buf.write_char(' ')
      print_var(buf, op)
      buf.write_char(' ')
      print_expr_with_prec(buf, rhs, prec + 1, indent) // right associative needs +1
    }
    Unary(op~, expr~, ..) => {
      print_var(buf, op)
      print_expr(buf, expr)
    }
    Pipe(lhs~, rhs~, ..) => {
      print_expr(buf, lhs)
      buf.write_string(" |> ")
      print_expr(buf, rhs)
    }

    // Array operations
    ArrayGet(array~, index~, ..) => {
      print_expr(buf, array)
      buf.write_char('[')
      print_expr(buf, index)
      buf.write_char(']')
    }
    ArrayGetSlice(array~, start_index~, end_index~, ..) => {
      print_expr(buf, array)
      buf.write_char('[')
      match start_index {
        Some(e) => print_expr(buf, e)
        None => ()
      }
      buf.write_char(':')
      match end_index {
        Some(e) => print_expr(buf, e)
        None => ()
      }
      buf.write_char(']')
    }
    ArraySet(array~, index~, value~, ..) => {
      print_expr(buf, array)
      buf.write_char('[')
      print_expr(buf, index)
      buf.write_string("] = ")
      print_expr(buf, value)
    }
    ArrayAugmentedSet(op~, array~, index~, value~, ..) => {
      print_expr(buf, array)
      buf.write_char('[')
      print_expr(buf, index)
      buf.write_string("] ")
      print_var(buf, op)
      buf.write_string("= ")
      print_expr(buf, value)
    }

    // Control flow
    If(cond~, ifso~, ifnot~, ..) => {
      buf.write_string("if ")
      print_expr(buf, cond)
      buf.write_string(" {\n")
      print_expr(buf, ifso)
      buf.write_string("\n}")
      match ifnot {
        Some(e) => {
          buf.write_string(" else {\n")
          print_expr(buf, e)
          buf.write_string("\n}")
        }
        None => ()
      }
    }
    Match(expr~, cases~, ..) => {
      buf.write_string("match ")
      print_expr(buf, expr, indent~)
      buf.write_string(" {\n")
      for c in cases {
        print_indent(buf, indent + 1)
        print_pattern(buf, c.pattern)
        match c.guard_ {
          Some(g) => {
            buf.write_string(" if ")
            print_expr(buf, g, indent~)
          }
          None => ()
        }
        buf.write_string(" => ")
        print_expr(buf, c.body, indent=indent + 1)
        buf.write_char('\n')
      }
      print_indent(buf, indent)
      buf.write_char('}')
    }
    While(loop_cond~, loop_body~, while_else~, label~, ..) => {
      match label {
        Some(l) => {
          buf.write_string(l.name)
          buf.write_string("~ ")
        }
        None => ()
      }
      buf.write_string("while ")
      print_expr(buf, loop_cond, indent~)
      buf.write_string(" {\n")
      print_indent(buf, indent + 1)
      print_expr(buf, loop_body, indent=indent + 1)
      buf.write_char('\n')
      print_indent(buf, indent)
      buf.write_char('}')
      match while_else {
        Some(e) => {
          buf.write_string(" else {\n")
          print_indent(buf, indent + 1)
          print_expr(buf, e, indent=indent + 1)
          buf.write_char('\n')
          print_indent(buf, indent)
          buf.write_char('}')
        }
        None => ()
      }
    }
    For(binders~, condition~, continue_block~, body~, for_else~, label~, ..) => {
      match label {
        Some(l) => {
          buf.write_string(l.name)
          buf.write_string("~ ")
        }
        None => ()
      }
      buf.write_string("for ")
      let mut first = true
      for b in binders {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        buf.write_string(b.0.name)
        buf.write_string(" = ")
        print_expr(buf, b.1, indent~)
      }
      match condition {
        Some(c) => {
          buf.write_string("; ")
          print_expr(buf, c, indent~)
        }
        None => ()
      }
      if not(continue_block.is_empty()) {
        buf.write_string("; ")
        let mut first_c = true
        for b in continue_block {
          if not(first_c) {
            buf.write_string(", ")
          }
          first_c = false
          buf.write_string(b.0.name)
          buf.write_string(" = ")
          print_expr(buf, b.1, indent~)
        }
      }
      buf.write_string(" {\n")
      print_indent(buf, indent + 1)
      print_expr(buf, body, indent=indent + 1)
      buf.write_char('\n')
      print_indent(buf, indent)
      buf.write_char('}')
      match for_else {
        Some(e) => {
          buf.write_string(" else {\n")
          print_indent(buf, indent + 1)
          print_expr(buf, e, indent=indent + 1)
          buf.write_char('\n')
          print_indent(buf, indent)
          buf.write_char('}')
        }
        None => ()
      }
    }
    ForEach(binders~, expr~, body~, else_block~, label~, ..) => {
      match label {
        Some(l) => {
          buf.write_string(l.name)
          buf.write_string("~ ")
        }
        None => ()
      }
      buf.write_string("for ")
      let mut first = true
      for b in binders {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        match b {
          Some(binder) => buf.write_string(binder.name)
          None => buf.write_char('_')
        }
      }
      buf.write_string(" in ")
      print_expr(buf, expr, indent~)
      buf.write_string(" {\n")
      print_indent(buf, indent + 1)
      print_expr(buf, body, indent=indent + 1)
      buf.write_char('\n')
      print_indent(buf, indent)
      buf.write_char('}')
      match else_block {
        Some(e) => {
          buf.write_string(" else {\n")
          print_indent(buf, indent + 1)
          print_expr(buf, e, indent=indent + 1)
          buf.write_char('\n')
          print_indent(buf, indent)
          buf.write_char('}')
        }
        None => ()
      }
    }
    Loop(arg~, body~, label~, ..) => {
      match label {
        Some(l) => {
          buf.write_string(l.name)
          buf.write_string("~ ")
        }
        None => ()
      }
      buf.write_string("loop ")
      print_expr(buf, arg, indent~)
      buf.write_string(" {\n")
      for c in body {
        print_indent(buf, indent + 1)
        print_pattern(buf, c.pattern)
        match c.guard_ {
          Some(g) => {
            buf.write_string(" if ")
            print_expr(buf, g, indent~)
          }
          None => ()
        }
        buf.write_string(" => ")
        print_expr(buf, c.body, indent=indent + 1)
        buf.write_char('\n')
      }
      print_indent(buf, indent)
      buf.write_char('}')
    }

    // Functions
    Function(func~, ..) =>
      match func.kind {
        Lambda => {
          buf.write_string("fn(")
          print_parameters(buf, func.parameters)
          buf.write_char(')')
          match func.return_type {
            Some(t) => {
              buf.write_string(" -> ")
              print_type(buf, t)
            }
            None => ()
          }
          buf.write_string(" {\n")
          print_indent(buf, indent + 1)
          print_expr(buf, func.body, indent=indent + 1)
          buf.write_char('\n')
          print_indent(buf, indent)
          buf.write_char('}')
        }
        Arrow => {
          buf.write_char('(')
          print_parameters(buf, func.parameters)
          buf.write_string(") => ")
          print_expr(buf, func.body, indent~)
        }
      }

    // Let bindings
    Let(pattern~, expr~, body~, ..) => {
      buf.write_string("let ")
      print_pattern(buf, pattern)
      buf.write_string(" = ")
      print_expr(buf, expr, indent~)
      buf.write_char('\n')
      print_indent(buf, indent)
      print_expr(buf, body, indent~)
    }
    LetMut(binder~, ty~, expr~, body~, ..) => {
      buf.write_string("let mut ")
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
      buf.write_char('\n')
      print_expr(buf, body)
    }
    LetFn(name~, func~, body~, ..) => {
      buf.write_string("fn ")
      buf.write_string(name.name)
      buf.write_char('(')
      print_parameters(buf, func.parameters)
      buf.write_char(')')
      match func.return_type {
        Some(t) => {
          buf.write_string(" -> ")
          print_type(buf, t)
        }
        None => ()
      }
      buf.write_string(" {\n")
      print_expr(buf, func.body)
      buf.write_string("\n}\n")
      print_expr(buf, body)
    }
    LetRec(bindings~, body~, ..) => {
      for b in bindings {
        buf.write_string("fn ")
        buf.write_string(b.0.name)
        buf.write_char('(')
        print_parameters(buf, b.1.parameters)
        buf.write_char(')')
        match b.1.return_type {
          Some(t) => {
            buf.write_string(" -> ")
            print_type(buf, t)
          }
          None => ()
        }
        buf.write_string(" {\n")
        print_expr(buf, b.1.body)
        buf.write_string("\n}\n")
      }
      print_expr(buf, body)
    }
    LetAnd(bindings~, body~, ..) => {
      let mut first = true
      for b in bindings {
        if first {
          buf.write_string("let ")
          first = false
        } else {
          buf.write_string("and ")
        }
        buf.write_string(b.0.name)
        match b.1 {
          Some(t) => {
            buf.write_string(" : ")
            print_type(buf, t)
          }
          None => ()
        }
        buf.write_string(" = fn(")
        print_parameters(buf, b.2.parameters)
        buf.write_string(") {\n")
        print_expr(buf, b.2.body)
        buf.write_string("\n}\n")
      }
      print_expr(buf, body)
    }

    // Sequences
    Sequence(exprs~, last_expr~, ..) => {
      for e in exprs {
        print_expr(buf, e)
        buf.write_char('\n')
      }
      print_expr(buf, last_expr)
    }

    // Records
    Record(type_name~, fields~, ..) => {
      match type_name {
        Some(tn) => {
          print_type_name(buf, tn)
          buf.write_string("::")
        }
        None => ()
      }
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
          print_expr(buf, f.expr)
        }
      }
      buf.write_string(" }")
    }
    RecordUpdate(type_name~, record~, fields~, ..) => {
      match type_name {
        Some(tn) => {
          print_type_name(buf, tn)
          buf.write_string("::")
        }
        None => ()
      }
      buf.write_string("{ ..")
      print_expr(buf, record)
      for f in fields {
        buf.write_string(", ")
        if f.is_pun {
          buf.write_string(f.label.name)
        } else {
          buf.write_string(f.label.name)
          buf.write_string(": ")
          print_expr(buf, f.expr)
        }
      }
      buf.write_string(" }")
    }
    Field(record~, accessor~, ..) => {
      print_expr(buf, record)
      buf.write_char('.')
      match accessor {
        Label(label) => buf.write_string(label.name)
        Index(tuple_index~, ..) => buf.write_string(tuple_index.to_string())
        Newtype(..) => buf.write_char('_')
      }
    }
    Method(type_name~, method_name~, ..) => {
      print_type_name(buf, type_name)
      buf.write_string("::")
      buf.write_string(method_name.name)
    }
    DotApply(self~, method_name~, args~, return_self~, attr~, ..) => {
      print_expr(buf, self)
      buf.write_char('.')
      buf.write_string(method_name.name)
      buf.write_char('(')
      print_arguments(buf, args)
      buf.write_char(')')
      match attr {
        NoAttr => ()
        Exclamation => buf.write_char('!')
        Question => buf.write_char('?')
      }
      if return_self {
        buf.write_char('.')
      }
    }

    // Mutation and assignment
    Mutate(record~, accessor~, field~, augmented_by~, ..) => {
      print_expr(buf, record)
      buf.write_char('.')
      match accessor {
        Label(label) => buf.write_string(label.name)
        Index(tuple_index~, ..) => buf.write_string(tuple_index.to_string())
        Newtype(..) => buf.write_char('_')
      }
      match augmented_by {
        Some(op) => {
          buf.write_char(' ')
          print_var(buf, op)
          buf.write_string("= ")
        }
        None => buf.write_string(" = ")
      }
      print_expr(buf, field)
    }
    Assign(var_~, expr~, augmented_by~, ..) => {
      print_var(buf, var_)
      match augmented_by {
        Some(op) => {
          buf.write_char(' ')
          print_var(buf, op)
          buf.write_string("= ")
        }
        None => buf.write_string(" = ")
      }
      print_expr(buf, expr)
    }

    // Type constraints
    Constraint(expr~, ty~, ..) => {
      buf.write_char('(')
      print_expr(buf, expr)
      buf.write_string(" : ")
      print_type(buf, ty)
      buf.write_char(')')
    }
    As(expr~, trait_~, ..) => {
      print_expr(buf, expr)
      buf.write_string(" as ")
      print_type_name(buf, trait_)
    }
    Is(expr~, pat~, ..) => {
      print_expr(buf, expr)
      buf.write_string(" is ")
      print_pattern(buf, pat)
    }

    // Control flow keywords
    Return(return_value~, ..) => {
      buf.write_string("return")
      match return_value {
        Some(e) => {
          buf.write_char(' ')
          print_expr(buf, e)
        }
        None => ()
      }
    }
    Raise(err_value~, ..) => {
      buf.write_string("raise ")
      print_expr(buf, err_value)
    }
    Break(arg~, label~, ..) => {
      buf.write_string("break")
      match label {
        Some(l) => {
          buf.write_char(' ')
          buf.write_string(l.name)
        }
        None => ()
      }
      match arg {
        Some(e) => {
          buf.write_char(' ')
          print_expr(buf, e)
        }
        None => ()
      }
    }
    Continue(args~, label~, ..) => {
      buf.write_string("continue")
      match label {
        Some(l) => {
          buf.write_char(' ')
          buf.write_string(l.name)
        }
        None => ()
      }
      if not(args.is_empty()) {
        buf.write_char(' ')
        let mut first = true
        for e in args {
          if not(first) {
            buf.write_string(", ")
          }
          first = false
          print_expr(buf, e)
        }
      }
    }

    // Error handling
    Try(body~, catch_~, try_else~, has_try~, ..) => {
      if has_try {
        buf.write_string("try {\n")
        print_expr(buf, body)
        buf.write_string("\n}")
      } else {
        print_expr(buf, body)
      }
      if not(catch_.is_empty()) {
        buf.write_string(" catch {\n")
        for c in catch_ {
          buf.write_string("  ")
          print_pattern(buf, c.pattern)
          buf.write_string(" => ")
          print_expr(buf, c.body)
          buf.write_char('\n')
        }
        buf.write_char('}')
      }
      match try_else {
        Some(cases) => {
          buf.write_string(" else {\n")
          for c in cases {
            buf.write_string("  ")
            print_pattern(buf, c.pattern)
            buf.write_string(" => ")
            print_expr(buf, c.body)
            buf.write_char('\n')
          }
          buf.write_char('}')
        }
        None => ()
      }
    }
    TryOperator(body~, kind~, ..) => {
      match kind {
        Question => buf.write_string("try? ")
        Exclamation => buf.write_string("try! ")
      }
      print_expr(buf, body)
    }

    // Guard
    Guard(cond~, otherwise~, body~, ..) => {
      buf.write_string("guard ")
      print_expr(buf, cond)
      match otherwise {
        Some(e) => {
          buf.write_string(" else {\n")
          print_expr(buf, e)
          buf.write_string("\n}")
        }
        None => ()
      }
      buf.write_char('\n')
      print_expr(buf, body)
    }

    // Defer
    Defer(expr~, body~, ..) => {
      buf.write_string("defer ")
      print_expr(buf, expr)
      buf.write_char('\n')
      print_expr(buf, body)
    }

    // Hole
    Hole(kind~, ..) =>
      match kind {
        Synthesized => buf.write_char('_')
        Incomplete => buf.write_string("...")
        Todo => buf.write_string("todo")
      }

    // Map
    Map(elems~, ..) => {
      buf.write_string("{ ")
      let mut first = true
      for e in elems {
        if not(first) {
          buf.write_string(", ")
        }
        first = false
        print_constant(buf, e.key)
        buf.write_string(": ")
        print_expr(buf, e.expr)
      }
      buf.write_string(" }")
    }

    // Group
    Group(expr~, group~, ..) =>
      match group {
        Paren => {
          buf.write_char('(')
          print_expr(buf, expr)
          buf.write_char(')')
        }
        Brace => {
          buf.write_string("{\n")
          print_expr(buf, expr)
          buf.write_string("\n}")
        }
      }

    // String interpolation
    Interp(elems~, ..) => {
      buf.write_char('"')
      for e in elems {
        match e {
          Literal(repr~, ..) => buf.write_string(repr)
          Expr(expr~, ..) => {
            buf.write_string("\\{")
            print_expr(buf, expr)
            buf.write_char('}')
          }
          Source(_) => ()
        }
      }
      buf.write_char('"')
    }
    MultilineString(elems~, ..) =>
      for e in elems {
        match e {
          String(s) => {
            buf.write_string("#| ")
            buf.write_string(s)
            buf.write_char('\n')
          }
          Interp(interp_elems) => {
            buf.write_string("#| ")
            for ie in interp_elems {
              match ie {
                Literal(repr~, ..) => buf.write_string(repr)
                Expr(expr~, ..) => {
                  buf.write_string("\\{")
                  print_expr(buf, expr)
                  buf.write_char('}')
                }
                Source(_) => ()
              }
            }
            buf.write_char('\n')
          }
        }
      }

    // LexMatch
    LexMatch(strategy~, expr~, cases~, ..) => {
      buf.write_string("lexmatch ")
      match strategy {
        Some(s) => {
          buf.write_string("using ")
          buf.write_string(s.name)
          buf.write_char(' ')
        }
        None => ()
      }
      print_expr(buf, expr)
      buf.write_string(" {\n")
      for c in cases {
        buf.write_string("  ")
        print_lex_top_patterns(buf, c.pat)
        buf.write_string(" => ")
        print_expr(buf, c.body)
        buf.write_char('\n')
      }
      buf.write_char('}')
    }
    IsLexMatch(expr~, strategy~, pat~, ..) => {
      print_expr(buf, expr)
      buf.write_string(" lexmatch")
      match strategy {
        Some(s) => {
          buf.write_char('?')
          buf.write_char(' ')
          buf.write_string(s.name)
        }
        None => buf.write_char('?')
      }
      buf.write_char(' ')
      print_lex_top_patterns(buf, pat)
    }
    // StaticAssert is compiler-generated, just print the body
    StaticAssert(body~, ..) => print_expr(buf, body)
  }
}

///|
fn print_var(buf : StringBuilder, v : @syntax.Var) -> Unit {
  print_long_ident(buf, v.name)
}

///|
fn print_arguments(
  buf : StringBuilder,
  args : @list.List[@syntax.Argument],
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
        continue // skip value printing for pun
      }
      LabelledOption(label~, ..) => {
        buf.write_string(label.name)
        buf.write_string("?=")
      }
      LabelledOptionPun(label~, ..) => {
        buf.write_string(label.name)
        buf.write_char('?')
        continue // skip value printing for pun
      }
    }
    print_expr(buf, arg.value)
  }
}

///|
fn print_lex_top_patterns(
  buf : StringBuilder,
  pats : @list.List[@syntax.LexTopPattern],
) -> Unit {
  let mut first = true
  for p in pats {
    if not(first) {
      buf.write_char(' ')
    }
    first = false
    print_lex_top_pattern(buf, p)
  }
}

///|
fn print_lex_top_pattern(
  buf : StringBuilder,
  pat : @syntax.LexTopPattern,
) -> Unit {
  match pat {
    Pattern(lex_pat) => print_lex_pattern(buf, lex_pat)
    Binder(b) => buf.write_string(b.name)
    Wildcard(..) => buf.write_char('_')
  }
}

///|
fn print_lex_pattern(buf : StringBuilder, pat : @syntax.LexPattern) -> Unit {
  match pat {
    Regex(lit~, ..) => {
      buf.write_char('/')
      buf.write_string(lit)
      buf.write_char('/')
    }
    RegexInterp(elems~, ..) => {
      buf.write_char('/')
      for e in elems {
        match e {
          Literal(repr~, ..) => buf.write_string(repr)
          Expr(expr~, ..) => {
            buf.write_string("\\{")
            print_expr(buf, expr)
            buf.write_char('}')
          }
          Source(_) => ()
        }
      }
      buf.write_char('/')
    }
    Alias(pat~, binder~, ..) => {
      print_lex_pattern(buf, pat)
      buf.write_string(" as ")
      buf.write_string(binder.name)
    }
    Sequence(pats~, ..) => {
      let mut first = true
      for p in pats {
        if not(first) {
          buf.write_char(' ')
        }
        first = false
        print_lex_pattern(buf, p)
      }
    }
  }
}
