# Syntax AST 向位置增强型 CST 的改造方案

## Summary

将 `syntax` 包里的现有 AST 直接升级为“位置增强型 CST”表示：保留现有节点名、语义字段和旧 `loc` / `*_loc` 字段的原有含义，再补充精确的 keyword、delimiter 和关键语法符号位置，使 parser 产物能表达 `if/else/try/match/fn` 以及 `{ } ( ) [ ] :: =>` 等语法壳信息。

`syntax/ast_json_repr.mbt` 继续只输出当前 JSON 中已有的信息。所有新增位置字段都只参与内部表示，不进入 JSON，因此现有 JSON 输出和测试期望保持不变。

这次改造允许 `syntax` 包公开数据结构和 visitor 签名发生 breaking change，不做兼容层。

## Implementation Changes

### 1. 数据模型原则

- 在 `syntax/ast.mbt` 上直接扩展现有 AST，而不是额外再造一套并行 CST 类型。
- 旧字段保留原语义，不重解释：
  - `loc`
  - `params_loc`
  - `match_loc`
  - `loop_loc`
  - `pat_loc`
  - `index_loc`
  - `view_type_loc`
  - `question_loc`
  - `key_loc`
- 新增字段只表达更细粒度的语法位置，统一采用“最小拥有者”原则：
  - 调用括号属于 `Expr::Apply` / `Expr::DotApply`，不属于 `Argument`
  - `match/loop/lexmatch/try` 的块花括号属于外层 `Expr`，不属于 `Case` / `LexCase`
  - `record/map/tuple/array` 的外层分隔符属于容器节点，不属于字段/元素节点
  - 已有 leaf 位置不重复存，比如 `Label`、`Binder`、`ConstrName`、`FieldName`、`TypeName`、`Var`、`Accessor`、`TypeVarBinder.name_loc`、`MapExprElem.key_loc`、`MapPatElem.key_loc`、`ArgumentKind.question_loc`、`Type::Option.question_loc`
- 新增字段命名约定：
  - keyword 位置：`if_loc`、`else_loc`、`fn_loc`、`let_loc`、`mut_loc`、`for_loc`、`in_loc`、`while_loc`、`guard_loc`、`defer_loc`、`return_loc`、`break_loc`、`continue_loc`、`match_keyword_loc`、`lexmatch_loc`、`with_loc`、`try_keyword_loc`、`catch_keyword_loc`、`noraise_loc`、`impl_loc`、`trait_loc`、`struct_loc`、`enum_loc`、`type_loc`、`suberror_loc`、`enumview_loc`、`using_loc`、`test_loc`、`declare_loc`、`extern_loc`
  - 成对分隔符：`lparen_loc`、`rparen_loc`、`lbrace_loc`、`rbrace_loc`、`lbracket_loc`、`rbracket_loc`
  - 一节点存在多组分隔符时，加语义前缀，如 `params_lparen_loc`、`params_rparen_loc`、`body_lbrace_loc`、`body_rbrace_loc`
  - 其他关键符号：`fat_arrow_loc`、`thin_arrow_loc`、`double_colon_loc`、`equal_loc`、`colon_loc`、`pipe_loc`、`attr_loc`
- v1 不追求完整 token stream 级 lossless 表示，不为所有逗号、分号、每个 `and` 单独建位置列表。

### 2. AST 节点扩展范围

- 表达式节点：
  - `Apply` / `DotApply`：补调用 `(` / `)` 和 `attr_loc`
  - `Array` / `ArraySpread` / `ArrayGet` / `ArrayGetSlice`：补 `[` / `]`，`ArrayGetSlice` 再补 `colon_loc`
  - `Tuple` / `Group` / `Constraint`：补外层 `(` / `)`，`Constraint` 补 `colon_loc`
  - `Record` / `RecordUpdate` / `Map`：补 `{` / `}`；若有 `TypeName::`，补 `double_colon_loc`；`RecordUpdate` 再补 `spread_loc`
  - `If` / `While` / `For` / `ForEach` / `Loop` / `Match` / `LexMatch` / `Try`：补各自 keyword 位置和 block `{` / `}`；`ForEach` 补 `in_loc`；`LexMatch` 补 `with_loc?`；`Try` 补 `catch` / `noraise` 块位置
  - `Function` 依赖 `Func`
  - `Func`：补 `fn_loc?`、`fat_arrow_loc?`、参数 `(` / `)`、类型参数 `[` / `]`
  - `Let` / `LetMut` / `Guard` / `Defer` / `Return` / `Break` / `Continue` / `Pipe` / `Assign` / `Mutate` / `Is` / `As`：补 keyword 或 operator 位置
  - `LetAnd`：只补 `letrec_loc`，不在 v1 为每个 `and` 单独建 loc
- 模式和类型：
  - `Pattern::Tuple` / `Record` / `Map` / `Array` / `Constraint` / `Range` / `Constr`：补必要的外层 delimiter 和 operator loc
  - `ArrayPatterns::Open` 不为内部 separator 单独建 loc，依赖父节点语义
  - `Type::Arrow`：补参数 `(` / `)` 和 `thin_arrow_loc`
  - `Type::Name`：补类型实参 `[` / `]`
  - `Type::Tuple`：补 `(` / `)`
- 顶层和声明：
  - `TypeDecl` / `LocalTypeDecl`：补声明 keyword 位置、类型参数 `[` / `]`
  - `TypeDesc::Record` / `Variant` / `TupleStruct` / `Alias` / `Error`：补 `{}` / `()` / `=`
  - `FunDecl` / `TraitMethodDecl` / `TraitDecl`：补 `fn/trait`、类型参数 `[` / `]`、参数 `(` / `)`、trait body `{}` 
  - `Impl::TopLetDef` / `TopFuncDef` / `TopTrait` / `TopImpl` / `TopView` / `TopImplRelation` / `TopUsing` / `TopTest`：补各自顶层 keyword 和必要分隔符
  - `TopView` 继续保留旧 `view_type_loc` / `params_loc`，额外补精确 token loc

### 3. Parser 改造边界

- 只改 `.mbty` 源文件和手写 `.mbt` 文件，不直接阅读或手改由 `.mbty` 生成的 parser `.mbt`
- 主要改动文件：
  - `yacc_parser/parser.mbty`
  - `syntax/ast.mbt`
  - `syntax/utils.mbt`
  - `syntax/util/compact.mbt`
  - `syntax/map_visitor.mbt`
  - `syntax/iter_visitor.mbt`
  - `syntax/ast_json_repr.mbt`
- parser 规约动作里优先直接保存 `$loc(token)` / `$sloc` 得到的精确位置，不从最终整体 `loc` 反推
- 对 `block_expr` 和 statement compact 路径，必须在压平前保留语法壳位置：
  - `StmtLet`
  - `StmtGuard`
  - `StmtLetmut`
  - `StmtFunc`
  - `StmtLetand`
  - `StmtDefer`
- `syntax/util/compact.mbt` 负责把这些中间 statement 持有的细粒度位置转移到最终 `Expr`，避免 `Statement::compact_rev` 后信息丢失
- `syntax/utils.mbt` 里所有构造 helper、`loc_of_expression`、`Parameter::loc` 等辅助函数都要同步更新，但旧 coarse `loc` 结果不能变化

### 4. JSON 兼容性

- `syntax/ast_json_repr.mbt` 继续保持当前 JSON 结构不变
- 所有新增 loc 字段在 `json_repr` 中都显式忽略
- 现有已经“内部有 loc，但 JSON 不输出”的模式继续保留，尤其是：
  - `Type::Option.question_loc`
  - `ArgumentKind.question_loc`
  - `Expr::ArrayGetSlice.index_loc`
  - `Expr::Match.match_loc`
  - `Expr::LexMatch.match_loc`
  - `Expr::Loop.loop_loc`
  - `Expr::Try.try_loc`
  - `Expr::Try.catch_loc`
  - `Expr::Try.else_loc`
  - `Impl::TopView.view_type_loc`
- 目标是：升级后的 AST/CST 更丰富，但 `syntax/ast_json_repr.mbt` 输出的 JSON 完全不变

### 5. Visitor 与调用点

- `syntax/map_visitor.mbt` 和 `syntax/iter_visitor.mbt` 的 trait 签名与默认实现全部升级，纳入新增字段
- 仓库内所有直接构造这些 AST 节点的测试和 helper 都同步更新参数
- 不保留旧 visitor 接口，也不做 deprecated shim

## 解析层的具体实现策略

### 1. 直接从 `.mbty` 捕获已有 token loc

`yacc_parser/parser.mbty` 里已经大量使用 `$loc(...)` 和 `$sloc`，这次改造优先把这些位置直接挂到 AST/CST 上，不重新推导。

优先改造的规约类别：

- 函数和方法：
  - `fun_header`
  - `declare_fun_header`
  - `extern_fun_header`
  - `trait_method_decl`
  - `anony_fn`
  - `arrow_fn_expr`
- 控制流：
  - `if_expr`
  - `while_expr`
  - `for_expr`
  - `foreach_expr`
  - `match_expr`
  - `lexmatch_expr`
  - `try_expr`
  - `simple_try_expr`
- 复合字面量与调用：
  - `simple_expr`
  - `tuple_expr`
  - `argument`
  - `record_defn`
  - `map_expr_elem`
  - `pattern`
  - `simple_pattern`
  - `type_`
  - `simple_type`

### 2. 不能依赖事后推导的位置

以下位置必须在 parser 动作阶段明确保存，不能指望后续通过已有节点推回：

- `if` / `else` / `catch` / `noraise` / `with` / `in` 这类 keyword loc
- 调用括号 `(` / `)` 和 block 花括号 `{` / `}`
- `TypeName::` 中的 `::`
- `=>` / `->`
- `RecordUpdate` 中 `..`
- `Constraint` / slice / labelled field 里的 `:`

### 3. compact 路径要保留的信息

`syntax/util/compact.mbt` 当前会把 statement 列表压平为最终表达式，v1 需要确保这些信息不会丢：

- `let` keyword loc
- `mut` keyword loc
- `guard` keyword loc
- `defer` keyword loc
- local `fn` keyword loc
- `letrec` keyword loc

如果某个最终 `Expr` 节点本身没有宿主字段，就先在 `Statement` 侧扩展，再在 compact 时转移到对应 `Expr`。

## Public API Changes

- `syntax/ast.mbt` 中多个公开 struct / enum variant 将新增精确位置字段
- 旧 `loc` 与旧 `*_loc` 字段保留，语义不变
- `MapVisitor` / `IterVisitor` 的相关 `visit_*` 方法签名会同步扩展
- `syntax/ast_json_repr.mbt` 和 `ToJson for Expr/Impl` 输出保持稳定，不新增 JSON key

## Test Plan

- 现有 AST JSON 测试全部通过，JSON 文本不变
- 现有基线 `moon check` 必须保持通过
- 新增 parser 驱动测试，至少覆盖：
  - `if/else`
  - `while ... else/nobreak`
  - `try/catch/noraise`
  - `match/lexmatch/with`
  - `fn` / arrow fn / trait method / impl method 的参数括号和类型参数位置
  - `record` / `record update` / `tuple` / `array get/slice` / `apply` / `dot apply` 的 delimiter 位置
- 新增 compact 回归测试，确认 statement 压平后新增 loc 不丢
- 新增 coarse loc 回归测试，确认以下行为不变：
  - `Expr::loc()`
  - `Type::loc()`
  - `Pattern::loc()`
  - `Parameter::loc()`
  - 既有 `params_loc`
  - 既有 `match_loc`
  - 既有 `loop_loc`
  - 既有 `pat_loc`
  - 既有 `index_loc`

建议验证命令：

- `moon check`
- `moon test syntax`
- `moon test yacc_parser`

## Assumptions

- 允许 `syntax` 公开 API breaking change
- 本次目标是在现有 AST 形状上直接做 CST 化，而不是构建完整 token stream 级 parser tree
- v1 重点补 keyword、成对 delimiter 和少量关键语法符号位置，不覆盖所有 separator
- `.mbty` 是 parser 的真实源文件，生成的 `.mbt` 由现有 `moon.pkg` 规则自动刷新，不手改生成文件
