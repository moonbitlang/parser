///|
priv enum OperandPosition {
  LeftOperand
  RightOperand
} derive(Eq)

///|
priv enum InfixCtx {
  InfixOperand(op~ : String, pos~ : OperandPosition)
  Other
}

///|
fn precedence_of_operator(op : String) -> Int {
  match op {
    "||" => 1
    "&&" => 2
    "|" => 3
    "^" => 4
    "&" => 5
    ">" | "<" | "==" | "!=" | "<=" | ">=" => 300
    "<<" | ">>" => 400
    "+" | "-" => 500
    "*" | "/" | "%" => 600
    "..<" | "..=" => 900
    _ => panic()
  }
}

///|
priv enum Assoc {
  Left
  Right
} derive(Eq)

///|
fn assoc_of_operator(op : String) -> Assoc {
  match op {
    "||" | "&&" => Right
    "|" | "^" | "&" => Left
    ">" | "<" | "==" | "!=" | "<=" | ">=" => Left
    "<<" | ">>" => Left
    "+" | "-" => Left
    "*" | "/" | "%" => Left
    "..=" | "..<" => Left
    _ => panic()
  }
}

///|
/// Context classify the formatting context of an expression, e.g., what kind 
/// of expression it expects and the position of the expression in the code.
/// These contexts represent the expressions that can be parsed by 
/// 
/// specific grammar rules in moonbitlang/parser/yacc_parser/parser.mbty:
/// 
/// | context         |  rules in menhir parser      |
/// | --------------- | ---------------------------- |
/// | Simple          | simple_expr                  |
/// | Prefix          | prefix_expr                  |
/// | Range           | range_expr                   |
/// | Infix           | infix_expr                   |
/// | Pipeline        | pipe_expr                    |
/// | Expression      | expr                         |
/// | ExprStatement   | expr_statement               |
/// | Statement       | statement                    |
/// | Any             | all rules above              |
/// 
/// **Ideally, the relationship above and the context passed to Fmt::expr should 
///   faithfully reflect the grammar in the Menhir parser.**
/// 
/// Given code `e1 * e2`, the context of e2 is `Infix (Rhs "*")`, this means:
/// 
///  - if e2 is an expression that cannot parsed by `infix_expr`, it should be 
///    wrapped in braces.
/// 
///  - if e2 is a infix expression, and the operator precedence is lower than `*`, 
///    it should be wrapped in parentheses.
/// 
priv enum Context {
  Stmt
  ExprStmt
  Expr
  Pipeline
  Infix(InfixCtx)
  Range
  Prefix
  Simple
}

///|
/// `expr_kind` classifies the kind of expression
priv enum ExprKind {
  SimpleKind
  PrefixKind
  PostfixKind(String)
  InfixKind(String)
  PipeKind
  /// loop, for, foreach, while, try, if, simple_try, arrow_fn
  ControlFlowAndArrowFnKind
  /// local let, letrec, let mut, local fn, guard 
  ExprStmtKind
  StmtKind
}

///|
fn @syntax.Expr::classify(expr : Self) -> ExprKind {
  match expr {
    Group(_) => panic()
    Apply(_)
    | Array(_)
    | ArraySpread(_)
    | ListComprehension(_)
    | ArrayGet(_)
    | Constant(_)
    | MultilineString(_)
    | Interp(_)
    | BytesInterp(_)
    | Constraint(_)
    | Constr(_)
    | Ident(_)
    | Tuple(_)
    | Record(_)
    | RecordUpdate(_)
    | Field(_)
    | ArrayGetSlice(_)
    | Method(_)
    | Map(_)
    | Hole(kind=Synthesized | Incomplete, ..)
    | Unit(_)
    | Function(func={ kind: Lambda, .. }, ..)
    | DotApply(_) => SimpleKind
    Unary(_) => PrefixKind
    As(_) => PostfixKind("as")
    Is(_) => PostfixKind("is")
    RegexMatch(_) => PostfixKind("=~")
    Infix(op~, ..) =>
      match op.name {
        Ident(name~) => InfixKind(name)
        _ => panic()
      }
    Pipe(_) | RevPipe(_) => PipeKind
    TemplateWriting(is_conditional~, ..) =>
      PostfixKind(if is_conditional { "<?" } else { "<+" })
    Quantifier(_) => PrefixKind
    Implies(_) => PostfixKind("→")
    Match(_)
    | LexMatch(_)
    | LexScan(_)
    | If(_)
    | For(_)
    | ForEach(_)
    | While(_)
    | Try(_)
    | TryOperator(_)
    | Function(func={ kind: Arrow, .. }, ..) => ControlFlowAndArrowFnKind
    Mutate(_)
    | Assign(_)
    | ArraySet(_)
    | ArrayAugmentedSet(_)
    | Return(_)
    | Break(_)
    | Continue(_)
    | Raise(_)
    | ProofAssert(_)
    | ProofLet(_)
    | Hole(kind=Todo, ..) => ExprStmtKind
    Guard(_)
    | Defer(_)
    | LetFn(_)
    | LetAnd(_)
    | Let(_)
    | Sequence(_)
    | LetMut(_)
    | StaticAssert(_) => StmtKind
  }
}

///|
/// Consider a sequence of mixed infix expression `e1 op1 e2 op2 e3`,
fn is_ambiguous(op1 : String, op2 : String) -> Bool {
  ignore((op1, op2))
  false //TODO
}

///|
priv enum ParenResult {
  GroupedParens
  NeedBraces
  NeedParens
  NoParen
}

///|
/// Infer whether this kind of expression needs to be wrapped with `()` or `{}` 
/// in this context.
fn infer_parens(context : Context, kind : ExprKind) -> ParenResult {
  let braces = NeedBraces
  let parens = NeedParens
  match (context, kind) {
    (Stmt, _) => NoParen
    (ExprStmt, StmtKind) => braces
    (
      ExprStmt,
      ExprStmtKind
      | PipeKind
      | ControlFlowAndArrowFnKind
      | InfixKind(_)
      | PostfixKind(_)
      | PrefixKind
      | SimpleKind,
    ) => NoParen
    (Expr, StmtKind | ExprStmtKind) => braces
    (
      Expr,
      ControlFlowAndArrowFnKind
      | PipeKind
      | InfixKind(_)
      | PostfixKind(_)
      | PrefixKind
      | SimpleKind,
    ) => NoParen
    (Pipeline, StmtKind | ExprStmtKind) => braces
    (Pipeline, ControlFlowAndArrowFnKind) => parens
    (Pipeline, InfixKind(_) | PostfixKind(_)) => NoParen
    (Pipeline, PipeKind | PrefixKind | SimpleKind) => NoParen
    (Infix(_), StmtKind | ExprStmtKind) => braces
    (Infix(_), ControlFlowAndArrowFnKind | PipeKind) => parens
    (Infix(ctx), InfixKind(inner_op)) =>
      match ctx {
        Other => NoParen
        InfixOperand(op=outer_op, pos~) => {
          let outer_prec = precedence_of_operator(outer_op)
          let outer_assoc = assoc_of_operator(outer_op)
          let inner_prec = precedence_of_operator(inner_op)
          let keep_user_parens = outer_prec > inner_prec ||
            (
              outer_prec == inner_prec &&
              (
                (pos == LeftOperand && outer_assoc == Right) ||
                (pos == RightOperand && outer_assoc == Left)
              )
            )
          if keep_user_parens || is_ambiguous(outer_op, inner_op) {
            GroupedParens
          } else {
            NoParen
          }
        }
      }
    (Infix(ctx), PostfixKind(op2)) =>
      match ctx {
        Other => NoParen
        InfixOperand(op=op1, ..) =>
          if is_ambiguous(op1, op2) {
            parens
          } else {
            NoParen
          }
      }
    (Infix(_), PrefixKind | SimpleKind) => NoParen
    (Range, StmtKind | ExprStmtKind) => braces
    (Range, ControlFlowAndArrowFnKind | PipeKind | InfixKind(_) | PostfixKind(_)
    ) => parens
    (Range, PrefixKind | SimpleKind) => NoParen
    (Prefix | Simple, StmtKind | ExprStmtKind) => braces
    (
      Prefix
      | Simple,
      ControlFlowAndArrowFnKind
      | PipeKind
      | InfixKind(_)
      | PostfixKind(_),
    ) => parens
    (Prefix | Simple, PrefixKind) => parens
    (Prefix | Simple, SimpleKind) => NoParen
  }
}
