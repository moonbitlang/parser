# AST Loc 字段改造成 CST 的可执行计划

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 在不新增 CST 树、不改变 AST 语义层级的前提下，给现有 `syntax/ast.mbt` 节点补齐被 parser 语义动作丢弃的关键字、括号、逗号、分号、冒号、箭头、等号等 token 位置。

**Architecture:** `syntax/ast.mbt` 仍是唯一公开 AST；新增字段只保存 token 位置，不引入 token 子节点。`yacc_parser/parser.mbty`、`mbti_parser/mbti_parser.mbty`、`handrolled_parser/parser.mbt` 同步填充字段，`syntax/map_visitor.mbt`、`syntax/iter_visitor.mbt`、`syntax/utils.mbt`、`syntax/util/compact.mbt` 同步重建和默认值；`syntax/ast_json_repr.mbt` 只做 AST 形状适配，输出 JSON 格式必须保持不变。

**Tech Stack:** MoonBit, MoonYacc (`*.mbty` pre-build), handrolled parser, `moon check`, `moon test`, `moon info`.

---

## Scope

- 保持现有 enum variant/struct 的语义子节点不变；只给现有节点追加 `Location`、`Location?`、`@list.List[Location]` 字段。
- 不手写或人工修改生成文件：不要编辑 `yacc_parser/parser.mbt`、`mbti_parser/mbti_parser.mbt`、`attribute/parser.mbt`。通过对应 `moon.pkg` 的 pre-build 重新生成。
- 不把注释和空白纳入本次改造；只保存 lexer 产生的关键字、标点和复合 token 的位置。
- `.mbti` 路径只填充它复用的 `syntax` AST/别名节点；`mbti_ast.TypeDefinition`、`TypeSig`、`FuncSig`、`TraitSig`、`AliasSig` 等 `.mbti` 专有容器不在本计划内新增 loc 字段。
- `Location?` 仅表示源代码中 token 真正可选；parser 合成节点使用 `@syntax.no_location`，不要用 `None` 混淆“语法不存在”和“合成但需要占位”。
- `comma_locs` / `semi_locs` 保存源代码中实际出现的分隔符；有 trailing comma/semi 时，列表长度等于元素数量，否则通常等于元素数量减一。`SEMI(true)` 对应真实 `;`，应进入 `semi_locs`；ASI 插入的 `SEMI(false)` 不进入 `semi_locs`，statement-derived 节点的 `semi_loc : Location?` 在这种情况下为 `None`。
- 字段按 vertical slice 分批添加：一个可提交批次必须同步更新 AST、helpers、visitors、`ast_json_repr` 兼容适配、parser 和测试后再进入下一批，避免一次性改完整 AST 后让中间测试长期不可执行。Task 1-5 是 Task 6 的准备阶段，不作为可提交边界；执行时不要在 `moon check` 失败状态停下或提交。
- 任何 caller 需要精确 block `{}` 位置时，parser 不能只传 `block_expr : Expr`；需要用 parser-local record/tuple 携带 `expr/lbrace_loc/rbrace_loc`，再由 caller 写入对应 AST 字段。
- `syntax/ast_json_repr.mbt` 输出的 JSON schema、字段名、字段顺序和既有字段含义必须保持不变；新增 loc 字段不得出现在 JSON 中。现有 JSON snapshot/断言不得因本 AST loc 改造更新，包括 `test/sync_test/__snapshot__/*.json`、`syntax/*_test.mbt`、`handrolled_parser/parse_expr_test.mbt` 和 `README.mbt.md` 中的 JSON 期望。若这些测试出现 JSON diff，优先修正 `syntax/ast_json_repr.mbt` 或测试构造方式以继续匹配旧输出；禁止用 `moon test test/sync_test --update` 或 `moon run test/sync_test/generator/generator.mbt` 接受这类变化。

## Files

- Modify: `syntax/ast.mbt` - 追加字段，调整 enum variant/struct 签名。
- Modify: `syntax/utils.mbt` - `loc()`、构造 helper、desugar 默认 token loc。
- Modify: `syntax/map_visitor.mbt` - visitor trait 签名、base visitor 重建逻辑、默认转发。
- Modify: `syntax/iter_visitor.mbt` - visitor trait 签名、base visitor 遍历逻辑、默认转发。
- Modify: `syntax/util/compact.mbt` - `Statement` 到 `Expr::Let`/`LetMut`/`LetFn`/`LetAnd`/`Defer`/`Guard`/`Sequence`/合成 `Unit` 的 token loc 传递。
- Modify: `syntax/ast_json_repr.mbt` - 适配新增 AST 字段但保持现有 JSON 输出格式不变；新增 loc 字段不输出。
- Modify: `yacc_parser/parser.mbty` - 从 `$loc($n)`、`$sloc` 捕获新增 token loc。
- Modify: `mbti_parser/mbti_parser.mbty` - 更新复用 `syntax` AST 的构造点。
- Modify: `handrolled_parser/parser.mbt` - 手写 parser 填充相同 loc，保持与 MoonYacc JSON 一致。
- Modify: `handrolled_parser/parse_expr_test.mbt` - 更新 AST 构造适配；JSON 快照断言必须保持旧格式，不因新增 loc 字段更新。
- Modify: `fmt/internal/format/syntax2doc.mbt` - 更新 AST 模式匹配/构造点以适配新增字段。
- Modify: `fmt/internal/format/attach_docstring.mbt`, `fmt/internal/format/remove_group.mbt`, `fmt/internal/comment/mapper_visitor.mbt`, `top.mbt` - 更新受 AST variant 形状影响的遍历/注释附着逻辑。
- Modify: `syntax/ast_test.mbt`, `syntax/ast_wbtest.mbt`, `syntax/map_visitor_wbtest.mbt` - 更新手写 AST 构造和 visitor 断言；JSON 断言必须保持旧格式。
- Modify: `test/sync_test/loc_regression_test.mbt` - 增加 token loc 精确断言和 Handrolled/MoonYacc JSON parity；JSON parity 必须基于旧格式。
- Modify: `README.mbt.md` - 仅更新 AST 构造适配；公开示例里的 JSON 断言必须保持旧格式。
- Create: `test/mbti_parser_test/loc_test.mbt` - 只覆盖 `.mbti` 中复用 `syntax` AST 的字段；不要在本计划中给 `mbti_ast.TypeDefinition` 等 `.mbti` 专有容器加 loc 字段。
- Generated/updated by command only: `yacc_parser/parser.mbt`, `yacc_parser/parser.mbt.map.json`, `mbti_parser/mbti_parser.mbt`, `mbti_parser/mbti_parser.mbt.map.json`, `pkg.generated.mbti`, `syntax/pkg.generated.mbti`, `syntax/util/pkg.generated.mbti`, `syntax/util/util.mbti`, package-level `pkg.generated.mbti` files. 修改 `.mbty` 后运行 `moon check`/`moon test` 会立即触发 pre-build 生成 parser 输出；不要等到 Task 11 才预期这些文件变化。`test/sync_test/__snapshot__/*.json`、`test/sync_test/parser_test.mbt`、`test/sync_test/lexer_test.mbt` 在本计划中必须保持不变。

## Existing Location Fields To Keep

这些字段已经存在，不要重复添加；实施时只需要继续传递，不要为了本次 loc 改造改变 JSON 输出。

- `Visibility::Pub.loc`, `Visibility::Priv.loc`
- `Type::Option.question_loc`
- `ErrorType::DefaultErrorType.loc`, `ErrorType::Noraise.loc`
- `ArgumentKind::LabelledOption.question_loc`, `ArgumentKind::LabelledOptionPun.question_loc`
- `Func.is_async`, `FunDecl.is_async`, `TraitMethodDecl.is_async`, `Impl::TopExpr.is_async`, `Impl::TopTest.is_async`
- `Expr::Match.match_loc`
- `Expr::Try.try_loc`, `Expr::Try.catch_loc`, `Expr::Try.else_loc`（`else_loc` 保存 `noraise` 关键字位置；没有 `noraise` 时继续使用 `@syntax.no_location`）
- `Expr::TryOperator.try_loc`
- `Expr::ArrayGetSlice.index_loc`
- `ListComprehensionKind::For.for_loc`
- `TraitMethodDecl.has_default`
- `Impl::TopView.view_type_loc`
- 所有已有 `loc`、`*_loc`、identifier/binder/label/name loc 字段

## Field Naming Rules

- 定界符：`lparen_loc` / `rparen_loc`, `lbrace_loc` / `rbrace_loc`, `lbracket_loc` / `rbracket_loc`。
- 分隔符：`comma_locs`, `semi_locs`, `plus_locs`。
- 关键字：`if_loc`, `else_loc`, `for_loc`, `in_loc`, `where_loc`, `fn_loc`, `impl_loc` 等。
- 二元标点：`colon_loc`, `coloncolon_loc`, `equal_loc`, `arrow_loc`, `fat_arrow_loc`, `pipe_loc`, `rev_pipe_loc`, `lt_plus_loc`。
- post-label token：`post_label_loc` 保存完整 `name~` 复合 token 位置；只有真实 `name~:` loop label 才使用 `label_colon_loc`。
- 复合 lexer token 不拆字符级位置；例如 `DOT_LIDENT`、`DOT_UIDENT`、`POST_LABEL`、`TRY_QUESTION`、`TRY_EXCLAMATION` 保留该 token 的 loc。需要同时保留 package token 时，增加 `pkg_loc` / `dot_id_loc`。

## Complete Field Additions

以下是本次需要新增的完整字段清单。未列出的类型不新增字段。执行时不要在 Task 1 一次性全部加入；按 Task 6-9 的覆盖批次分批加入并保持每批可编译/可测试。

### Qualified Names And Reusable Nodes

- `LongIdent::Dot`: add `pkg_loc : Location`, `dot_id_loc : Location`
- `TypeName`: add `object_amp_loc : Location?`
- `Type::Arrow`: add `lparen_loc : Location`, `rparen_loc : Location`, `arrow_loc : Location`, `comma_locs : @list.List[Location]`
- `Type::Tuple`: add `lparen_loc : Location`, `rparen_loc : Location`, `comma_locs : @list.List[Location]`
- `Type::Name`: add `type_args_lbracket_loc : Location?`, `type_args_rbracket_loc : Location?`, `type_arg_comma_locs : @list.List[Location]`
- `Type::Object`: change to named fields and add `amp_loc : Location`, `loc : Location`
- `ErrorType::ErrorType`: add `raise_loc : Location`, `loc : Location`
- `ErrorType::MaybeError`: add `raise_loc : Location`, `question_loc : Location`, `loc : Location`
- `ConstrParam`: add `mut_loc : Location?`, `post_label_loc : Location?`, `colon_loc : Location?`
- `ConstrDecl`: add `args_lparen_loc : Location?`, `args_rparen_loc : Location?`, `arg_comma_locs : @list.List[Location]`, `tag_equal_loc : Location?`
- `ExceptionDecl::EnumPayload`: add `lbrace_loc : Location`, `rbrace_loc : Location`, `semi_locs : @list.List[Location]`
- `FieldDecl`: add `mut_loc : Location?`, `colon_loc : Location`
- `TypeDesc::ExtensibleEnum`: add `lbrace_loc : Location`, `rbrace_loc : Location`, `semi_locs : @list.List[Location]`
- `TypeDesc::Variant`: add `lbrace_loc : Location`, `rbrace_loc : Location`, `semi_locs : @list.List[Location]`
- `TypeDesc::ExtendEnum`: add `plus_equal_loc : Location`, `lbrace_loc : Location`, `rbrace_loc : Location`, `semi_locs : @list.List[Location]`
- `TypeDesc::Record`: add `lbrace_loc : Location`, `rbrace_loc : Location`, `semi_locs : @list.List[Location]`
- `TypeDesc::TupleStruct`: add `lparen_loc : Location`, `rparen_loc : Location`, `comma_locs : @list.List[Location]`
- `TypeDesc::Alias`: add `equal_loc : Location`
- `ArgumentKind::Labelled`: add `equal_loc : Location`
- `ArgumentKind::LabelledOption`: add `equal_loc : Location`
- `TypeVarBinder`: add `colon_loc : Location?`, `plus_locs : @list.List[Location]`
- `ConstructorExtraInfo::TypeName`: add `coloncolon_loc : Location`
- `ConstructorExtraInfo::TypeNameWithConstrPackage`: add `coloncolon_loc : Location`, `pkg_loc : Location`, `dot_constr_loc : Location`
- `ConstructorExtraInfo::Package`: add `pkg_loc : Location`, `dot_constr_loc : Location`
- `AliasTarget`: add `as_loc : Location?`
- `Parameter::DiscardPositional`: add `colon_loc : Location?`
- `Parameter::Positional`: add `colon_loc : Location?`
- `Parameter::Labelled`: add `post_label_loc : Location?`, `colon_loc : Location?`
- `Parameter::Optional`: add `post_label_loc : Location?`, `question_loc : Location?`, `colon_loc : Location?`, `equal_loc : Location`
- `Parameter::QuestionOptional`: add `question_loc : Location`, `colon_loc : Location?`
- `Case`: add `if_loc : Location?`, `fat_arrow_loc : Location?`, `ellipsis_loc : Location?`
- `MapExprElem`: add `colon_loc : Location`
- `Func`: add `fn_loc : Location?`, `lparen_loc : Location?`, `rparen_loc : Location?`, `comma_locs : @list.List[Location]`, `return_arrow_loc : Location?`, `fat_arrow_loc : Location?`, `body_lbrace_loc : Location?`, `body_rbrace_loc : Location?`
- `FieldDef`: add `colon_loc : Location?`
- `SpreadableElem::Spread`: add `dotdot_loc : Location`
- `LexPattern::Alias`: add `as_loc : Location`
- `LexPattern::Sequence`: add `semi_locs : @list.List[Location]`
- `LexCase`: add `if_loc : Location?`, `fat_arrow_loc : Location?`, `ellipsis_loc : Location?`
- `RegexPattern::Sequence`: add `plus_loc : Location`
- `RegexPattern::Alternation`: add `bar_loc : Location`
- `RegexPattern::Alias`: add `as_loc : Location`
- `ListComprehensionKind::Foreach`: add `for_loc : Location`, `in_loc : Location`, `binder_comma_locs : @list.List[Location]`, `semi_locs : @list.List[Location]`, `init_comma_locs : @list.List[Location]`, `init_equal_locs : @list.List[Location]`, `continue_comma_locs : @list.List[Location]`, `continue_equal_locs : @list.List[Location]`
- `ListComprehensionKind::For`: add `binder_comma_locs : @list.List[Location]`, `semi_locs : @list.List[Location]`, `binder_equal_locs : @list.List[Location]`, `continue_comma_locs : @list.List[Location]`, `continue_equal_locs : @list.List[Location]`

### Expressions

- `Expr::Apply`: add `lparen_loc : Location`, `rparen_loc : Location`, `comma_locs : @list.List[Location]`
- `Expr::Array`: add `lbracket_loc : Location`, `rbracket_loc : Location`, `comma_locs : @list.List[Location]`
- `Expr::ArraySpread`: add `lbracket_loc : Location`, `rbracket_loc : Location`, `comma_locs : @list.List[Location]`
- `Expr::ListComprehension`: add `lbracket_loc : Location`, `rbracket_loc : Location`, `if_loc : Location?`, `fat_arrow_loc : Location`
- `Expr::ArrayGet`: add `lbracket_loc : Location`, `rbracket_loc : Location`
- `Expr::ArrayGetSlice`: add `lbracket_loc : Location`, `colon_loc : Location`, `rbracket_loc : Location`
- `Expr::ArraySet`: add `lbracket_loc : Location`, `rbracket_loc : Location`, `equal_loc : Location`
- `Expr::ArrayAugmentedSet`: add `lbracket_loc : Location`, `rbracket_loc : Location`, `augmented_assign_loc : Location`
- `Expr::Constraint`: add `lparen_loc : Location?`, `colon_loc : Location`, `rparen_loc : Location?`
- `Expr::While`: add `label_colon_loc : Location?`, `while_loc : Location`, `body_lbrace_loc : Location`, `body_rbrace_loc : Location`, `nobreak_loc : Location?`, `nobreak_lbrace_loc : Location?`, `nobreak_rbrace_loc : Location?`
- `Expr::If`: add `if_loc : Location`, `then_lbrace_loc : Location`, `then_rbrace_loc : Location`, `else_loc : Location?`, `else_lbrace_loc : Location?`, `else_rbrace_loc : Location?`
- `Expr::Is`: add `is_loc : Location`
- `Expr::IsLexMatch`: add `lexmatch_question_loc : Location`, `with_loc : Location?`
- `Expr::RegexMatch`: add `eq_tilde_loc : Location`, `rhs_lparen_loc : Location?`, `rhs_rparen_loc : Location?`, `binding_comma_locs : @list.List[Location]`, `binding_equal_locs : @list.List[Location]`
- `Expr::Guard`: add `guard_loc : Location`, `else_loc : Location?`, `else_lbrace_loc : Location?`, `else_rbrace_loc : Location?`, `semi_loc : Location?`
- `Expr::Defer`: add `defer_loc : Location`, `semi_loc : Location?`
- `Expr::LetFn`: add `semi_loc : Location?`
- `Expr::LetAnd`: add `letrec_loc : Location`, `and_locs : @list.List[Location]`, `equal_locs : @list.List[Location]`, `annot_colon_locs : @list.List[Location]`, `semi_loc : Location?`
- `Expr::Let`: add `let_loc : Location`, `annot_colon_loc : Location?`, `equal_loc : Location`, `semi_loc : Location?`
- `Expr::Sequence`: add `semi_locs : @list.List[Location]`
- `Expr::Tuple`: add `lparen_loc : Location`, `rparen_loc : Location`, `comma_locs : @list.List[Location]`
- `Expr::Record`: add `coloncolon_loc : Location?`, `lbrace_loc : Location`, `rbrace_loc : Location`, `comma_locs : @list.List[Location]`, `semi_locs : @list.List[Location]`
- `Expr::RecordUpdate`: add `coloncolon_loc : Location?`, `lbrace_loc : Location`, `dotdot_loc : Location`, `rbrace_loc : Location`, `comma_locs : @list.List[Location]`
- `Expr::Method`: add `coloncolon_loc : Location`
- `Expr::DotApply`: add `dot_loc : Location`, `lparen_loc : Location`, `rparen_loc : Location`, `comma_locs : @list.List[Location]`
- `Expr::As`: add `as_loc : Location`
- `Expr::Mutate`: add `equal_loc : Location?`, `augmented_assign_loc : Location?`
- `Expr::Match`: add `lbrace_loc : Location`, `rbrace_loc : Location`, `semi_locs : @list.List[Location]`
- `Expr::LexMatch`: add `with_loc : Location?`, `lbrace_loc : Location`, `rbrace_loc : Location`, `semi_locs : @list.List[Location]`
- `Expr::LetMut`: add `let_loc : Location`, `mut_loc : Location`, `annot_colon_loc : Location?`, `equal_loc : Location`, `semi_loc : Location?`
- `Expr::Pipe`: add `pipe_loc : Location`
- `Expr::RevPipe`: add `rev_pipe_loc : Location`
- `Expr::Assign`: add `equal_loc : Location?`, `augmented_assign_loc : Location?`
- `Expr::Return`: add `return_loc : Location`
- `Expr::Raise`: add `raise_loc : Location`
- `Expr::Quantifier`: add `quantifier_loc : Location`, `colon_loc : Location`, `comma_loc : Location`
- `Expr::Implies`: add `implies_loc : Location`
- `Expr::ProofAssert`: add `proof_assert_loc : Location`
- `Expr::ProofLet`: add `proof_let_loc : Location`, `equal_loc : Location`
- `Expr::Unit`: add `lparen_loc : Location`, `rparen_loc : Location`
- `Expr::Break`: add `break_loc : Location`, `post_label_loc : Location?`
- `Expr::Continue`: add `continue_loc : Location`, `post_label_loc : Location?`, `comma_locs : @list.List[Location]`
- `Expr::For`: add `label_colon_loc : Location?`, `for_loc : Location`, `binder_comma_locs : @list.List[Location]`, `binder_equal_locs : @list.List[Location]`, `semi_locs : @list.List[Location]`, `continue_comma_locs : @list.List[Location]`, `continue_equal_locs : @list.List[Location]`, `body_lbrace_loc : Location`, `body_rbrace_loc : Location`, `nobreak_loc : Location?`, `nobreak_lbrace_loc : Location?`, `nobreak_rbrace_loc : Location?`
- `Expr::ForEach`: add `label_colon_loc : Location?`, `for_loc : Location`, `binder_comma_locs : @list.List[Location]`, `in_loc : Location`, `semi_locs : @list.List[Location]`, `init_comma_locs : @list.List[Location]`, `init_equal_locs : @list.List[Location]`, `continue_comma_locs : @list.List[Location]`, `continue_equal_locs : @list.List[Location]`, `body_lbrace_loc : Location`, `body_rbrace_loc : Location`, `nobreak_loc : Location?`, `nobreak_lbrace_loc : Location?`, `nobreak_rbrace_loc : Location?`
- `Expr::Try`: add `catch_lbrace_loc : Location`, `catch_rbrace_loc : Location`, `catch_semi_locs : @list.List[Location]`, `noraise_lbrace_loc : Location?`, `noraise_rbrace_loc : Location?`, `noraise_semi_locs : @list.List[Location]`
- `Expr::Map`: add `lbrace_loc : Location`, `rbrace_loc : Location`, `comma_locs : @list.List[Location]`
- `Expr::Group`: add `left_delim_loc : Location`, `right_delim_loc : Location`
- `Expr::TemplateWriting`: add `lt_plus_loc : Location`

### Patterns

- `DotDotBinder::Underscore`: add `dotdot_loc : Location`, `underscore_loc : Location`
- `DotDotBinder::NoBinder`: add `dotdot_loc : Location`
- `DotDotBinder::BinderAs`: add `dotdot_loc : Location`, `as_loc : Location`
- `DotDotBinder::Binder`: add `dotdot_loc : Location`
- `ArrayPattern::StringSpread`: add `dotdot_loc : Location`
- `ArrayPattern::BytesSpread`: add `dotdot_loc : Location`
- `ArrayPattern::ConstSpread`: add `dotdot_loc : Location`, `pkg_loc : Location?`, `dot_uident_loc : Location?`
- `ArrayPatterns::Closed`: add `comma_locs : @list.List[Location]`
- `ArrayPatterns::Open`: add `left_comma_locs : @list.List[Location]`, `dotdot_comma_loc : Location?`, `right_comma_locs : @list.List[Location]`
- `WhereClause`: add `where_loc : Location`, `lbrace_loc : Location`, `rbrace_loc : Location`, `comma_locs : @list.List[Location]`
- `FieldPat`: add `colon_loc : Location?`
- `ConstrPatArg`: add `equal_loc : Location?`
- `MapPatElem`: add `question_loc : Location?`, `colon_loc : Location`
- `Pattern::Alias`: add `as_loc : Location`
- `Pattern::Array`: add `lbracket_loc : Location`, `rbracket_loc : Location`
- `Pattern::Constraint`: add `lparen_loc : Location?`, `colon_loc : Location`, `rparen_loc : Location?`
- `Pattern::Constr`: add `args_lparen_loc : Location?`, `args_rparen_loc : Location?`, `arg_comma_locs : @list.List[Location]`, `open_dotdot_loc : Location?`
- `Pattern::Or`: add `bar_loc : Location`
- `Pattern::Tuple`: add `lparen_loc : Location`, `rparen_loc : Location`, `comma_locs : @list.List[Location]`
- `Pattern::Record`: add `lbrace_loc : Location`, `rbrace_loc : Location`, `comma_locs : @list.List[Location]`, `dotdot_loc : Location?`
- `Pattern::Map`: add `lbrace_loc : Location`, `rbrace_loc : Location`, `comma_locs : @list.List[Location]`, `dotdot_loc : Location?`
- `Pattern::Range`: add `range_op_loc : Location`
- `Pattern::SpecialConstr`: add `lparen_loc : Location`, `rparen_loc : Location`, `comma_locs : @list.List[Location]`

### Top-Level Declarations, Bodies, And Stubs

- `DerivingDirective`: add `args_lparen_loc : Location?`, `args_rparen_loc : Location?`, `arg_comma_locs : @list.List[Location]`
- `TypeDecl`: add `declare_loc : Location?`, `keyword_loc : Location`, `params_lbracket_loc : Location?`, `params_rbracket_loc : Location?`, `param_comma_locs : @list.List[Location]`, `derive_loc : Location?`, `derive_lparen_loc : Location?`, `derive_rparen_loc : Location?`, `derive_comma_locs : @list.List[Location]`
- `FuncStubs::Import`: add `module_name_loc : Location`, `func_name_loc : Location`, `language_loc : Location?`
- `FuncStubs::Embedded`: add `language_loc : Location?`, `code_loc : Location`
- `EmbeddedCode::CodeString`: add `loc : Location`
- `EmbeddedCode::CodeMultilineString`: add `string_locs : @list.List[Location]`
- `DeclBody::DeclBody`: add `lbrace_loc : Location?`, `rbrace_loc : Location?`
- `DeclBody::DeclStubs`: add `equal_loc : Location`
- `FunDecl`: add `declare_loc : Location?`, `extern_loc : Location?`, `fn_loc : Location`, `coloncolon_loc : Location?`, `type_params_lbracket_loc : Location?`, `type_params_rbracket_loc : Location?`, `type_param_comma_locs : @list.List[Location]`, `params_lparen_loc : Location?`, `params_rparen_loc : Location?`, `param_comma_locs : @list.List[Location]`, `return_arrow_loc : Location?`
- `TraitMethodDecl`: add `fn_loc : Location?`, `type_params_lbracket_loc : Location?`, `type_params_rbracket_loc : Location?`, `type_param_comma_locs : @list.List[Location]`, `params_lparen_loc : Location`, `params_rparen_loc : Location`, `param_comma_locs : @list.List[Location]`, `return_arrow_loc : Location?`, `default_equal_loc : Location?`
- `TraitDecl`: add `declare_loc : Location?`, `trait_loc : Location`, `colon_loc : Location?`, `super_plus_locs : @list.List[Location]`, `lbrace_loc : Location`, `rbrace_loc : Location`, `semi_locs : @list.List[Location]`
- `Impl::TopTest`: add `test_loc : Location`, `params_lparen_loc : Location?`, `params_rparen_loc : Location?`, `param_comma_locs : @list.List[Location]`, `body_lbrace_loc : Location`, `body_rbrace_loc : Location`
- `Impl::TopFuncDef`: add `equal_loc : Location?`
- `Impl::TopLetDef`: add `declare_loc : Location?`, `let_or_const_loc : Location`, `annot_colon_loc : Location?`, `equal_loc : Location`
- `Impl::TopImpl`: add `impl_loc : Location`, `type_params_lbracket_loc : Location?`, `type_params_rbracket_loc : Location?`, `type_param_comma_locs : @list.List[Location]`, `for_loc : Location?`, `with_loc : Location`, `fn_loc : Location?`, `params_lparen_loc : Location`, `params_rparen_loc : Location`, `param_comma_locs : @list.List[Location]`, `return_arrow_loc : Location?`
- `Impl::TopView`: add `enumview_loc : Location`, `type_params_lbracket_loc : Location?`, `type_params_rbracket_loc : Location?`, `type_param_comma_locs : @list.List[Location]`, `lbrace_loc : Location`, `rbrace_loc : Location`, `constructor_semi_locs : @list.List[Location]`, `for_loc : Location`, `with_loc : Location`, `params_lparen_loc : Location`, `params_rparen_loc : Location`, `param_comma_locs : @list.List[Location]`, `body_lbrace_loc : Location`, `body_rbrace_loc : Location`
- `Impl::TopImplRelation`: add `declare_loc : Location?`, `impl_loc : Location`, `type_params_lbracket_loc : Location?`, `type_params_rbracket_loc : Location?`, `type_param_comma_locs : @list.List[Location]`, `for_loc : Location`
- `Impl::TopUsing`: add `using_loc : Location`, `lbrace_loc : Location`, `rbrace_loc : Location`, `comma_locs : @list.List[Location]`
- `UsingKind::Type`: change to `Type(loc : Location)`
- `UsingKind::Trait`: change to `Trait(loc : Location)`

### `syntax/util` Statement Payloads

这些不是公开 `syntax/ast.mbt` AST 节点，但它们是 parser 到 `Expr` 的中间层；如果不携带 token loc，Task 7 的 statement-derived `Expr` 字段无法可靠填充。

- `Statement::StmtExpr`: change to `StmtExpr(expr~ : Expr, semi_loc~ : Location?)`
- `Statement::StmtLet`: add `let_loc : Location`, `annot_colon_loc : Location?`, `equal_loc : Location`, `semi_loc : Location?`
- `Statement::StmtGuard`: add `guard_loc : Location`, `else_loc : Location?`, `else_lbrace_loc : Location?`, `else_rbrace_loc : Location?`, `semi_loc : Location?`
- `Statement::StmtLetmut`: add `let_loc : Location`, `mut_loc : Location`, `annot_colon_loc : Location?`, `equal_loc : Location`, `semi_loc : Location?`
- `Statement::StmtFunc`: add `semi_loc : Location?`; local function keyword/params/body delimiter locs live on the nested `Func`.
- `Statement::StmtLetand`: add `letrec_loc : Location`, `and_locs : @list.List[Location]`, `equal_locs : @list.List[Location]`, `annot_colon_locs : @list.List[Location]`, `semi_loc : Location?`
- `Statement::StmtDefer`: add `defer_loc : Location`, `semi_loc : Location?`

## Implementation Plan

### Task 1: Add Vertical-Slice AST Fields

**Files:**
- Modify: `syntax/ast.mbt`

- [ ] Add only the fields needed for the first vertical slice: `Expr::If.if_loc/then_lbrace_loc/then_rbrace_loc/else_loc/else_lbrace_loc/else_rbrace_loc`, `Func.fn_loc/lparen_loc/rparen_loc/comma_locs/return_arrow_loc/fat_arrow_loc/body_lbrace_loc/body_rbrace_loc`, `Parameter::*` token fields, `Expr::Record.coloncolon_loc/lbrace_loc/rbrace_loc/comma_locs/semi_locs`, `FieldDef.colon_loc`, and `Case.if_loc/fat_arrow_loc/ellipsis_loc`.
- [ ] Keep public variant names unchanged.
- [ ] Prefer appending fields after existing semantic fields and before final `loc` only when it improves readability; use named constructors everywhere, so positional compatibility is not required.
- [ ] Run: `moon check syntax`
- [ ] Expected: this preparation step may report constructor errors in consumers that construct the vertical-slice nodes. Continue immediately through Task 6 and do not commit or hand off until `moon check` is green. Do not fix generated `parser.mbt` by hand.

### Task 2: Update Core Helpers And Defaults

**Files:**
- Modify: `syntax/utils.mbt`
- Modify: `syntax/util/compact.mbt`

- [ ] Update only first-slice location/default code: `Expr::loc`, `Parameter::loc`, and helpers that construct changed first-slice nodes such as `make_record_expr`, `make_field_def`, and `make_arrow_fn`.
- [ ] Defer helpers for tuple/array/pattern/type nodes (`make_tuple_expr`, `make_array_expr`, `make_alias_pattern`, `make_constant_pattern`, `make_tuple_pattern`, `make_constr_pattern`, `make_tuple_type`, `make_option_type`, `make_field_pat`, assignment/desugar helpers) to the task where those AST fields are introduced.
- [ ] Do not overhaul `syntax/util/compact.mbt` statement payloads in this preparation task unless the first-slice constructor changes force a compile fix. Full keyword/equal/semicolon propagation for `StmtLet`, `StmtLetmut`, `StmtFunc`, `StmtLetand`, `StmtDefer`, `StmtGuard`, and `StmtExpr`/sequence belongs to Task 7 when the **`syntax/util` Statement Payloads** fields are added.
- [ ] For desugared or synthetic nodes, fill new token fields with `@syntax.no_location` or empty lists. Use `None` only when the source token is genuinely absent.
- [ ] Run: `moon check syntax`
- [ ] Expected: this preparation step may still report parser, visitor, or JSON constructor mismatches. Continue immediately through Task 6 and do not commit or hand off until `moon check` is green.

### Task 3: Update Visitors

**Files:**
- Modify: `syntax/map_visitor.mbt`
- Modify: `syntax/iter_visitor.mbt`

- [ ] Update trait method signatures for every changed struct/variant.
- [ ] In `MapVisitorBase`, preserve token loc fields verbatim while recursively visiting semantic children.
- [ ] In `IterVisitorBase`, ignore token loc fields unless a method already exposes them through a visitor callback.
- [ ] Ensure `visit_Expr` and `visit_Pattern` dispatch cases pass all new named fields.
- [ ] Run: `moon check syntax`
- [ ] Expected: visitor package compiles or remaining errors are JSON/parser-only. Continue immediately through Task 6 and do not commit or hand off until `moon check` is green.

### Task 4: Keep JSON Representation Stable

**Files:**
- Modify: `syntax/ast_json_repr.mbt`

- [ ] Update pattern matches and destructuring in `syntax/ast_json_repr.mbt` for the changed AST shapes.
- [ ] Do not add JSON children for new loc fields. Do not expose previously ignored loc-like fields as part of this plan.
- [ ] Keep emitted JSON field names, field order, and value shapes unchanged for existing fixtures. Run a focused snapshot diff before and after each vertical slice; any JSON diff caused only by new AST loc fields is a bug in `syntax/ast_json_repr.mbt`.
- [ ] Run: `moon check syntax`
- [ ] Expected: JSON code compiles once parser constructors are updated, while serialized output remains compatible with existing snapshots. Continue immediately through Task 6 and do not commit or hand off until `moon check` is green.

### Task 5: Add Loc-Carrying List Helpers In Grammars

**Files:**
- Modify: `yacc_parser/parser.mbty`
- Modify: `mbti_parser/mbti_parser.mbty`

- [ ] Add loc-aware list helper nonterminals next to current `list_commas` / `list_semis` helpers in `yacc_parser/parser.mbty`:
  - `list_commas_with_locs[T](X : T) -> (List[T], List[Location])`
  - `list_commas_with_trailing_locs[T](X : T) -> (List[T], List[Location], Bool)`
  - `list_semis_with_locs[T](X : T) -> (List[T], List[Location])`
  - `list_semis_with_trailing_locs[T](X : T) -> (List[T], List[Location], Bool)`
  - `binder_equal_expr -> (Binder, Location, Expr)`
  - `binder_equal_infix_expr -> (Binder, Location, Expr)`
  - `label_equal_expr -> (Label, Location, Expr)`
  - `label_equal_pattern -> (Label, Location, Pattern)`
- [ ] Add equivalent helpers to `mbti_parser/mbti_parser.mbty`; it has its own comma helper and also uses `separated_list`, `separated_nonempty_list`, and `delimited` for shared `syntax` nodes such as type params, tuple types, enum constructor params, record fields, and trait method params. Do not migrate derive args or `.mbti`-only tuple-struct/container delimiters in this plan.
- [ ] Keep older helpers until all call sites migrate; delete only when no longer used. For existing `TrailingMark` behavior, use the helper `Bool` result instead of inferring trailing state from list length.
- [ ] Run: `moon check yacc_parser`
- [ ] Run: `moon check mbti_parser`
- [ ] Expected: grammar generation succeeds or reports only call-site type errors to fix in Task 6. The commands may regenerate `yacc_parser/parser.mbt` and `mbti_parser/mbti_parser.mbt`; keep those generated changes, but review them only after the slice compiles.

### Task 6: Implement Vertical Slice

**Files:**
- Modify: `syntax/ast.mbt`
- Modify: `syntax/utils.mbt`
- Modify: `syntax/map_visitor.mbt`
- Modify: `syntax/iter_visitor.mbt`
- Modify: `syntax/ast_json_repr.mbt`
- Modify: `yacc_parser/parser.mbty`
- Modify: `handrolled_parser/parser.mbt`
- Modify: `handrolled_parser/parse_expr_test.mbt`
- Modify: `fmt/internal/format/syntax2doc.mbt`
- Modify: `syntax/ast_test.mbt`
- Modify: `syntax/ast_wbtest.mbt`
- Modify: `syntax/map_visitor_wbtest.mbt`
- Modify: `test/sync_test/loc_regression_test.mbt`
- Modify: `README.mbt.md`

- [ ] Cover `Expr::If`, including `then_lbrace_loc/then_rbrace_loc/else_lbrace_loc/else_rbrace_loc`, `Func`, `Parameter`, `Expr::Record`, `FieldDef`, and `Case`.
- [ ] Introduce a parser-local block payload for MoonYacc and handrolled parser call sites that need brace locations, e.g. `(expr : Expr, lbrace_loc : Location, rbrace_loc : Location)`. Use it for `if` branches and `Func` bodies; keep `Statement::compact_rev` returning plain `Expr`. Do not assert top-level `FunDecl` or `DeclBody` delimiter fields in this slice; those are Task 9 fields.
- [ ] Extend `loc_regression_source` with:

```moonbit
fn sample(x : Int, y : Int) -> Int {
  let f = fn(a : Int, b? : Int = 1) -> Int {
    if a > 0 {
      { a: a, b: b }
    } else {
      b
    }
  }
  f(x, b=y)
}
```

- [ ] Walk into the anonymous `Expr::Function(Func)` assigned to `f` and assert exact locs for `Func.fn_loc`, `Func.lparen_loc/rparen_loc/comma_locs`, `Func.return_arrow_loc`, `Func.body_lbrace_loc/body_rbrace_loc`, and its `Parameter` fields including `Optional.question_loc/colon_loc/equal_loc`.
- [ ] Inside that `Func` body, assert exact locs for `if_loc`, `then_lbrace_loc/then_rbrace_loc`, `else_loc`, `else_lbrace_loc/else_rbrace_loc`, record `lbrace_loc/rbrace_loc/comma_locs/semi_locs`, and field `colon_loc`. Add one small `{ x; }` or equivalent fixture so `Expr::Record.semi_locs` covers the parser's `TrailingMark::Semi` path.
- [ ] Assert Handrolled and MoonYacc JSON equality using the unchanged JSON representation; the equality output must not gain new loc fields.
- [ ] Run: `moon check`
- [ ] Expected: full repo compiles after the vertical slice; if it fails on formatter or AST tests, update the files listed in this task before continuing.
- [ ] Run: `moon test test/sync_test/loc_regression_test.mbt -v`
- [ ] Run: `moon test handrolled_parser -v`
- [ ] Run: `moon test README.mbt.md -v`
- [ ] Expected: full vertical-slice check and targeted tests pass before broad rollout.

### Task 7: Finish Expression Coverage

**Files:**
- Modify: `syntax/ast.mbt`
- Modify: `syntax/utils.mbt`
- Modify: `syntax/util/compact.mbt`
- Modify: `syntax/map_visitor.mbt`
- Modify: `syntax/iter_visitor.mbt`
- Modify: `yacc_parser/parser.mbty`
- Modify: `handrolled_parser/parser.mbt`
- Modify: `handrolled_parser/parse_expr_test.mbt`
- Modify: `syntax/ast_json_repr.mbt`
- Modify: `fmt/internal/format/syntax2doc.mbt`
- Modify: `syntax/ast_test.mbt`
- Modify: `syntax/ast_wbtest.mbt`
- Modify: `syntax/map_visitor_wbtest.mbt`
- Modify: `test/sync_test/loc_regression_test.mbt`
- Modify: `README.mbt.md`

- [ ] Add and fill all remaining expression fields listed under **Expressions**. For statement-derived nodes, update `syntax/util/compact.mbt` first so `Let`, `LetMut`, `LetFn`, `LetAnd`, `Defer`, `Guard`, `Sequence`, and faked `Unit` do not lose keyword, equal, annotation-colon, and explicit semicolon locs. `semi_loc` is `Some(loc)` only for source `SEMI(true)`; it is `None` for ASI `SEMI(false)` or a final statement without an explicit semicolon.
- [ ] In MoonYacc and handrolled statement-list helpers, carry the terminator loc with the preceding `Statement` before calling `compact_rev`; do not infer `semi_loc` from statement span or child expression range. Explicit `SEMI(true)` contributes to the previous statement's `semi_loc` and to `Expr::Sequence.semi_locs`; ASI `SEMI(false)` contributes no loc.
- [ ] Also fill `ListComprehensionKind::Foreach` and `ListComprehensionKind::For` fields from **Qualified Names And Reusable Nodes** in this expression batch.
- [ ] Reuse the parser-local block payload from Task 6 for `while`, `for`, `foreach`, `nobreak`, and `guard else` blocks so their `{}` locations are not reconstructed from child expression ranges.
- [ ] Use `post_label_loc` for `break label~` / `continue label~`; reserve `label_colon_loc` for loop labels parsed from `label~:`.
- [ ] Add loc regression cases for apply, array/spread `..`, tuple, map, match, lexmatch, try/noraise, while/nobreak, guard else, for, foreach, list comprehension, assignment, mutate, pipe/revpipe, group, array slice, regex match, and template writing. Include both an explicit `;` and an ASI newline case to verify `semi_locs` / `semi_loc` policy.
- [ ] Run: `moon check`
- [ ] Run: `moon test test/sync_test/loc_regression_test.mbt -v`
- [ ] Run: `moon test test/sync_test --filter parse_test_try -v`
- [ ] Run: `moon test handrolled_parser -v`
- [ ] Run: `moon test README.mbt.md -v`
- [ ] Expected: loc regression passes and targeted parser snapshots still parse with unchanged JSON output. Do not update imported sync snapshots at any point for this AST loc change; if targeted parser snapshots fail because JSON shape changed, fix `syntax/ast_json_repr.mbt`.

### Task 8: Finish Type And Pattern Coverage

**Files:**
- Modify: `syntax/ast.mbt`
- Modify: `syntax/utils.mbt`
- Modify: `syntax/map_visitor.mbt`
- Modify: `syntax/iter_visitor.mbt`
- Modify: `yacc_parser/parser.mbty`
- Modify: `mbti_parser/mbti_parser.mbty`
- Modify: `handrolled_parser/parser.mbt`
- Modify: `handrolled_parser/parse_expr_test.mbt`
- Modify: `syntax/ast_json_repr.mbt`
- Modify: `fmt/internal/format/syntax2doc.mbt`
- Modify: `syntax/ast_test.mbt`
- Modify: `syntax/ast_wbtest.mbt`
- Modify: `syntax/map_visitor_wbtest.mbt`
- Modify: `test/sync_test/loc_regression_test.mbt`
- Create: `test/mbti_parser_test/loc_test.mbt`
- Modify: `README.mbt.md`

- [ ] Fill all fields listed under **Qualified Names And Reusable Nodes** that are type-expression, pattern, argument, constructor-reference, lex/regex, and `.mbti`-shared declaration-component related. This includes `ConstrParam`, `ConstrDecl`, `ExceptionDecl::EnumPayload`, and `FieldDecl` because `mbti_parser` constructs those `syntax` nodes directly. Leave `ListComprehensionKind::*` to Task 7, and leave source-only top-level declaration containers such as `TypeDesc`, `DerivingDirective`, `TypeDecl`, `FunDecl`, `TraitDecl`, and `Impl::*` to Task 9.
- [ ] Fill all fields listed under **Patterns**.
- [ ] In `mbti_parser/mbti_parser.mbty`, replace relevant `separated_list`, `separated_nonempty_list`, and `delimited` uses only where the resulting values are `syntax` AST nodes or aliases to them: function type arrows, type argument brackets, tuple types, constructor params (`ConstrParam` / `ConstrDecl`), record fields (`FieldDecl`), and trait/function method params (`Parameter`). Do not add loc fields to `mbti_ast.TypeDefinition`, `TypeSig`, `FuncSig`, `TraitSig`, or `AliasSig` in this plan.
- [ ] Add `.mbti` fixtures for function type arrows, type arguments, tuple type values, enum constructor args, labelled constructor params, record field colon/mut tokens, and trait method params. Do not assert tuple-struct parentheses, enum braces, derive parentheses, or other `.mbti`-only container delimiter locs unless a separate follow-up extends `mbti_ast`.
- [ ] Run: `moon check`
- [ ] Run: `moon test test/mbti_parser_test -v`
- [ ] Run: `moon test test/sync_test/loc_regression_test.mbt -v`
- [ ] Run: `moon test README.mbt.md -v`
- [ ] Expected: `.mbti` parser and source parser both expose the same token loc shape for shared `syntax` nodes.

### Task 9: Finish Top-Level Declarations

**Files:**
- Modify: `syntax/ast.mbt`
- Modify: `syntax/utils.mbt`
- Modify: `syntax/map_visitor.mbt`
- Modify: `syntax/iter_visitor.mbt`
- Modify: `yacc_parser/parser.mbty`
- Modify: `mbti_parser/mbti_parser.mbty`
- Modify: `handrolled_parser/parser.mbt`
- Modify: `handrolled_parser/parse_expr_test.mbt`
- Modify: `syntax/ast_json_repr.mbt`
- Modify: `fmt/internal/format/syntax2doc.mbt`
- Modify: `syntax/ast_test.mbt`
- Modify: `syntax/ast_wbtest.mbt`
- Modify: `syntax/map_visitor_wbtest.mbt`
- Modify: `test/sync_test/loc_regression_test.mbt`
- Modify: `test/mbti_parser_test/loc_test.mbt`
- Modify: `README.mbt.md`

- [ ] Fill all fields listed under **Top-Level Declarations, Bodies, And Stubs**.
- [ ] Fill source-only declaration-component fields from **Qualified Names And Reusable Nodes** that were intentionally left out of Task 8: `TypeDesc`, `DerivingDirective`, and declaration-level `TypeVarBinder` delimiter lists.
- [ ] Use `Impl::TopLetDef.equal_loc : Location`; current source grammar always parses top-level `let`/`const` through `val_header "=" expr`.
- [ ] Cover `type`, `suberror`, `struct`, `enum`, `extenum`, type alias syntax, `derive`, `let`, `const`, `fn`, `extern fn`, `declare`, `trait`, `impl`, `enumview`, `using`, and `test`, including `Impl::TopTest.body_lbrace_loc/body_rbrace_loc`, `DeclBody::DeclBody.lbrace_loc/rbrace_loc`, `UsingKind::Type(loc)`, and `UsingKind::Trait(loc)`. Update `.mbti` using-alias parsing for the `UsingKind` shape change without adding loc fields to `.mbti`-only containers.
- [ ] Preserve string token locs in `FuncStubs` and `EmbeddedCode`.
- [ ] Run: `moon check`
- [ ] Run: `moon test test/sync_test/loc_regression_test.mbt -v`
- [ ] Run: `moon test test/mbti_parser_test -v`
- [ ] Run: `moon test README.mbt.md -v`
- [ ] Expected: all new top-level token loc fields are stored in AST and covered by direct loc assertions, while JSON output stays unchanged and Handrolled/MoonYacc source parser paths agree.

### Task 10: Update Downstream AST Consumers

**Files:**
- Modify: `fmt/internal/format/syntax2doc.mbt`
- Modify: `fmt/internal/format/context.mbt` if pattern matching stops compiling.
- Modify: `fmt/internal/format/attach_docstring.mbt`
- Modify: `fmt/internal/format/remove_group.mbt`
- Modify: `fmt/internal/comment/mapper_visitor.mbt`
- Modify: `top.mbt`
- Modify: `syntax/ast_test.mbt`
- Modify: `syntax/ast_wbtest.mbt`
- Modify: `syntax/map_visitor_wbtest.mbt`
- Modify: `handrolled_parser/parse_expr_test.mbt`
- Modify: `README.mbt.md`
- Must stay unchanged: `test/sync_test/__snapshot__/*.json`, `test/sync_test/parser_test.mbt`, `test/sync_test/lexer_test.mbt`, and existing JSON expectations in local tests/docs.

- [ ] Update every handwritten AST constructor to pass the new loc fields, using explicit dummy locs in tests where source token locations are irrelevant.
- [ ] Update formatter pattern matches to ignore token loc fields with `..` when formatting does not need them.
- [ ] Run `rg -n 'visit_Expr_|visit_Pattern|Expr::(Group|Array|Tuple|Record|Match|Let|LetMut|LetFn|LetAnd|Defer|Guard|Sequence)|Pattern::|Top(FuncDef|LetDef|Trait|View|Impl|Using|Test)|TypeDesc|DeclBody|FuncStubs|EmbeddedCode|UsingKind|Stmt(Let|Letmut|Func|Letand|Defer|Guard|Expr)|LongIdent|TypeName|Parameter' top.mbt fmt/internal/format fmt/internal/comment syntax test handrolled_parser README.mbt.md` and update every handwritten pattern or constructor that still assumes the old AST shape, including visitor override signatures such as `visit_Expr_Group`.
- [ ] Do not update JSON snapshot expectations in `syntax/ast_test.mbt`, `syntax/ast_wbtest.mbt`, `handrolled_parser/parse_expr_test.mbt`, or `README.mbt.md` for new loc fields. If they fail because serialized JSON changed, fix `syntax/ast_json_repr.mbt` so the old expected JSON remains valid.
- [ ] Do not hand-edit or regenerate imported `test/sync_test/__snapshot__/*.json`. Do not run `moon test test/sync_test --update` or `moon run test/sync_test/generator/generator.mbt` for this plan.
- [ ] Run: `moon check`
- [ ] Run: `moon test syntax`
- [ ] Run: `moon test handrolled_parser -v`
- [ ] Run: `moon test README.mbt.md -v`
- [ ] Expected: non-parser consumers compile, local JSON tests pass with unchanged expected output, and imported sync snapshots remain untouched before generated parser/interface files are reviewed.

### Task 11: Regenerate Parsers And Public Interfaces

**Files:**
- Generated: `yacc_parser/parser.mbt`
- Generated: `yacc_parser/parser.mbt.map.json`
- Generated: `mbti_parser/mbti_parser.mbt`
- Generated: `mbti_parser/mbti_parser.mbt.map.json`
- Generated: `syntax/pkg.generated.mbti`, `syntax/util/pkg.generated.mbti`, `syntax/util/util.mbti`, root `pkg.generated.mbti`, and package-level `pkg.generated.mbti`

- [ ] Run: `moon check`
- [ ] Confirm parser outputs and source maps were regenerated by pre-build from `.mbty` changes. They may already have changed during Tasks 5-9; do not manually edit `yacc_parser/parser.mbt`, `yacc_parser/parser.mbt.map.json`, `mbti_parser/mbti_parser.mbt`, or `mbti_parser/mbti_parser.mbt.map.json`.
- [ ] Run: `moon info`
- [ ] Review generated `pkg.generated.mbti` and `syntax/util/util.mbti` diffs. Expected: public API expands with the planned loc fields, including `syntax/util/Statement` payload changes; no unrelated API changes.

### Task 12: Full Verification

**Files:**
- No new edits unless failures identify missing fields.

- [ ] Run: `moon fmt`
- [ ] Run: `moon test syntax`
- [ ] Run: `moon test handrolled_parser -v`
- [ ] Run: `moon test README.mbt.md -v`
- [ ] Run: `moon test fmt/internal/format -v`
- [ ] Run: `moon test fmt/internal/testsuite/style_test -v`
- [ ] Run: `moon test fmt/internal/testsuite/comment_test -v`
- [ ] Run: `moon test test/sync_test`
- [ ] Run: `moon test test/manual_test`
- [ ] Run: `moon test test/mbti_parser_test`
- [ ] Run: `moon check --target all`
- [ ] Run: `moon info`
- [ ] Expected: all tests pass, generated parser files are derived from `.mbty`, public API diff contains only the planned loc-field additions, and existing JSON snapshots/expectations remain unchanged.

## Implementation Notes

- Parser helper migration should be incremental. Start with loc-aware comma/semi list helpers, then migrate grammar call sites in small batches.
- `block_expr -> Expr` is insufficient for CST-style delimiter locs. Add a parser-local block payload (for example `BlockExpr::{ expr, lbrace_loc, rbrace_loc }` in generated parser actions, or a tuple if record syntax is awkward) and keep a compatibility helper that returns just `expr` for call sites that do not need delimiters.
- In the handrolled parser, extend `parse_block_expr` or add a sibling helper that returns both the compacted expression and `{}` token locations; use the sibling helper at `if`, `while`, `for`, `foreach`, `guard else`, `test`, and `Func` body construction sites.
- Existing `match_header -> Expr` currently makes it too easy to lose the `match` keyword and `{` loc; replace header rules with tuple records that carry keyword and delimiter locations.
- Existing `catch_keyword -> Location` and `else_keyword -> Location` combine keyword and `{`; split them into `catch_loc`, `catch_lbrace_loc` and existing `else_loc` (the `noraise` keyword loc), `noraise_lbrace_loc`.
- `Case.fat_arrow_loc` and `LexCase.fat_arrow_loc` are optional because `...` cases have no `=>`; store the ellipsis token in `ellipsis_loc` instead.
- For handrolled parser parity, prefer extending token-consuming helper return values to include the consumed token loc instead of reconstructing locs from surrounding child nodes.
- `syntax/ast_json_repr.mbt` must keep emitted JSON compatible with existing snapshots: no new loc children, no renames, no field-order churn, and no snapshot regeneration for this loc-field migration.
