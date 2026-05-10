# 在 AST 中保留 CST Token 位置信息

## 概述

本 PR 将 AST 从“主要表达语义结构”扩展为“同时保留关键 CST token 位置”的结构。新增字段大多是可选的 `Location?` 或 `List[Location]`，用于记录括号、花括号、方括号、逗号、分号、关键字、操作符等语法 token 的精确位置，为 formatter、source mapping、IDE 精准定位以及保源码结构重写提供基础。

现有语义 JSON 表示保持稳定：`syntax/ast_json_repr.mbt` 已更新为忽略这些只服务于 CST 的字段，因此下游使用 JSON AST 的场景不会看到语义结构变化。

## 主要变更

- `basic/loc.mbt`: 新增 `Location::first_char`、`Location::last_char`、`Location::trim_first_and_last_char`，用于从较大的源码范围中切出单字符 token 的位置。
- `syntax/ast.mbt`: 为类型、表达式、模式、声明和顶层 impl 节点增加 CST location 字段。
- `handrolled_parser` 和 `yacc_parser`: 两套 MoonBit parser 都开始填充这些新字段，包括分隔符列表和 trailing delimiter 的位置。
- `mbti_parser`: 适配新的 AST 构造器，并在 `.mbti` 解析中尽量保留可用 token 位置信息。
- `syntax/iter_visitor.mbt`、`syntax/map_visitor.mbt`、`syntax/utils.mbt`、`syntax/util/compact.mbt`: 更新 visitor、构造辅助函数和 statement compact 逻辑，确保新增 CST 字段在遍历、重写和语句折叠过程中被保留。
- `test/sync_test/cst_loc_test.mbt`: 新增一致性测试，先比较 handrolled parser 和 MoonYacc parser 的语义 JSON 输出，再直接断言新增 CST location 字段。

## AST 与语法对应关系

### 类型与错误标注

- `Type::Arrow`
  - 新增字段：`lparen_loc`、`rparen_loc`、`comma_locs`、`trailing_comma_loc`、`thin_arrow_loc`。
  - 对应语法：函数类型，例如 `(A, B,) -> C`、`async (A) -> B`。
  - 这些字段记录参数列表括号、逗号、trailing comma 以及 `->`。

- `Type::Tuple`
  - 新增字段：`lparen_loc`、`rparen_loc`、`comma_locs`、`trailing_comma_loc`。
  - 对应语法：元组类型，例如 `(A, B)`、`(A, B,)`。

- `Type::Name`
  - 新增字段：`lbracket_loc`、`rbracket_loc`、`comma_locs`、`trailing_comma_loc`。
  - 对应语法：类型实参，例如 `Result[T, E,]`。

- `Type::Object`
  - 从 `Object(ConstrId)` 改为 `Object(constr_id, amp_loc)`。
  - 对应语法：object type，例如 `&Thing`；`amp_loc` 记录 `&`。

- `ErrorType::MaybeError`
  - 新增 `question_loc`。
  - 对应语法：`raise ?`；`question_loc` 记录 `?`。

### 参数、实参与类型变量

- `Parameter`
  - positional、labelled、discarded、optional、question-optional 参数都新增 `colon_loc`。
  - optional 参数额外新增 `question_loc` 和 `equal_loc`。
  - 对应语法：`_ : T`、`x : T`、`label~ : T`、`label? : T`、`label? : T = default`。

- `Argument` 和 `ConstrPatArg`
  - 新增 `equal_loc`。
  - 对应语法：带标签实参或构造器模式实参，例如 `label=value`。

- `ConstrParam`
  - 新增 `loc`、`mut_loc`、`colon_loc`。
  - 对应语法：枚举构造器 payload，例如 `mut Int`、`label~ : String`。

- `TypeVarBinder`
  - 新增 `colon_loc` 和 `plus_locs`。
  - 对应语法：类型参数约束，例如 `[T : Show + Eq]`。

### 表达式

- `Expr::Apply` 和 `Expr::DotApply`
  - 新增 `bang_loc`、`question_loc`、`lparen_loc`、`rparen_loc`、`comma_locs`、`trailing_comma_loc`。
  - 对应语法：`f!(x)`、`f?(x)`、`f(a, b,)`、`recv.call!(arg)`、`recv.call?(arg)`。

- `Expr::Tuple`、`Expr::Array`、`Expr::ArraySpread`、`Expr::Map`、`Expr::Group`
  - 新增 `(`、`)`、`[`、`]`、`{`、`}`、逗号和 trailing comma 等 delimiter 字段。
  - 对应语法：`(a, b)`、`(a, b,)`、`[a, ..b, c,]`、`{ "k": v }`、`(expr)`、`{ expr }`。

- `Expr::Record` 和 `Expr::RecordUpdate`
  - 新增 `lbrace_loc`、`rbrace_loc`、`comma_locs`、`trailing_comma_loc`，record update 额外记录 `dotdot_loc`。
  - `FieldDef` 新增 `colon_loc`。
  - 对应语法：`{ x: y, z, }`、`{ ..record, x: y }`。

- `Expr::Constant`
  - 新增 `uplus_loc` 和 `uminus_loc`。
  - 对应语法：带符号数字字面量，例如 `+1`、`-1`；同时保留 compact constant 表示。

- `Expr::If`
  - 新增 `if_loc`、`else_loc`、`if_lbrace_loc`、`if_rbrace_loc`、`else_lbrace_loc`、`else_rbrace_loc`。
  - 对应语法：`if cond { a } else if other { b } else { c }`。

- `Expr::While`
  - 新增 `while_loc`、循环体花括号位置、`nobreak_loc` 和 nobreak block 花括号位置。
  - 对应语法：`while cond { body } nobreak { fallback }`。

- `Expr::For` 和 `Expr::ForEach`
  - 新增关键字、delimiter 和分隔符位置，包括 `for_loc`、`in_loc`、body braces、`nobreak` 相关位置、binder commas 和 header semicolons。
  - 对应语法：`for i = init; cond; i = next { body } nobreak { fail }`、`for k, v in data { body }`。

- `WhereClause`
  - 新增 `where_loc`、`lbrace_loc`、`rbrace_loc`、`comma_locs`、`trailing_comma_loc`。
  - 对应语法：`where { key: value, other: pun, }`。

- `Expr::Match` 和 `Expr::LexMatch`
  - 新增关键字和 block 花括号位置。
  - `Case` 和 `LexCase` 新增 `guard_keyword_loc` 和 `fat_arrow_loc`。
  - 对应语法：`match v { p if g => e }`、`lexmatch value with longest { "a" => a }`。

- `Expr::Is` 和 `Expr::IsLexMatch`
  - 新增 `is_loc`、`lexmatch_loc`、`question_loc`、`with_loc`。
  - 对应语法：`value is Some(_)`、`subject lexmatch? "a"`、`subject lexmatch? "a" with longest`。

- `Expr::Try` 和 `Expr::TryOperator`
  - 新增 `try`、`catch`、`catch!`、catch braces、`noraise`、noraise braces、`try?`、`try!` 的位置。
  - 对应语法：`try work catch! { err => bad } noraise { ok => good }`、`try? work`、`try! work`。

- `Expr::Guard`、`Expr::Defer`、`Expr::Let`、`Expr::LetMut`、`Expr::LetFn`、`Expr::LetRec`、`Expr::LetAnd`、`Expr::Sequence`
  - 新增关键字、`=`、`;` 和 sequence semicolon 位置。
  - 对应语法：block statement，例如 `let value = init;`、`let mut value = init;`、`fn inner() { ... };`、`letrec ...;`、`guard ... else { ... };`、`defer cleanup;` 以及表达式序列。

- `Func`
  - 新增 `fn_loc`、函数体花括号位置、函数体内部分号位置和函数体 trailing semicolon 位置。
  - 对应语法：local/top-level 函数体，例如 `fn f() { prep; done; }`。

### 模式

- `Pattern::Array`
  - 新增方括号、逗号、trailing comma 和 `dotdot_loc`。
  - `ArrayPattern::StringSpread`、`BytesSpread`、`ConstSpread` 也新增 `dotdot_loc`。
  - 对应语法：`[head, ..rest]`、`[.. "s"]`、`[..b"s"]`、`[..SomeConst]`。

- `Pattern::Constr`
  - 新增括号、逗号、trailing comma 和 `dotdot_loc`。
  - 对应语法：`Ctor(a, label=b, ..)`。

- `Pattern::Tuple`、`Pattern::Record`、`Pattern::Map`
  - 新增 delimiter、逗号、trailing comma 和 open-pattern `..` 位置。
  - `FieldPat` 新增 `colon_loc`；`MapPatElem` 新增 `question_loc` 和 `colon_loc`。
  - 对应语法：`(a, b)`、`{ field: pat, .. }`、`{ "k"?: v, .. }`。

- `Pattern::Range`
  - 新增 `op_loc`。
  - 对应语法：`1..=3`、`1..<3`。

- `Pattern::Constant`
  - 新增 `uminus_loc`。
  - 对应语法：负数字面量模式。

### 声明与顶层项

- `DeclBody`
  - 新增 body braces、semicolon locations 和 trailing semicolon location。
  - 对应语法：声明体，例如 `{ prep; done; }`。

- `FunDecl`
  - 新增 `declare_loc`、`extern_loc`、`fn_loc`、类型参数括号/逗号、参数括号/逗号、trailing parameter comma 和 `thin_arrow_loc`。
  - 对应语法：`fn [T, U,] f(x : Int, y : Int,) -> Int`、`declare fn [T,] f(...) -> T`、`extern "js" fn foreign(...) -> T`。

- `TraitMethodDecl`
  - 新增类型参数、参数、`->` 和 `=` 位置。
  - 对应语法：`method[T, U,](x : Int, y : Int,) -> Int = _`。

- `TypeDecl` 和 `LocalTypeDecl`
  - 新增 keyword 位置、类型参数 delimiter 和 comma 位置、`=`、record/variant/tuple struct body delimiter，以及 `derive(...)` 的 delimiter 和 comma。
  - 对应语法：`type Alias[T, U,] = Int derive(Eq, Show)`、`struct Pair[T, U,](Int, Bool,) derive(Eq, Show)`、`enum Demo { ... }`。

- `TraitDecl`
  - 新增 `declare_loc`、`trait_loc`、`colon_loc`、`plus_locs` 和 trait body brace 位置。
  - 对应语法：`trait Fancy : Eq + Show { ... }`。

- `Impl::TopExpr`、`TopTest`、`TopTypeDef`、`TopFuncDef`、`TopLetDef`、`TopTrait`、`TopImpl`、`TopView`、`TopImplRelation`、`TopUsing`
  - 新增顶层 `trailing_semi_loc`。
  - 部分 variant 还记录各自声明关键字和 header delimiter：
    - `TopImpl`: `impl`、类型参数、`for`、`with`、方法参数、`->`。
    - `TopView`: `enumview`、类型参数、构造器 braces、`for`、`with`、参数。
    - `TopImplRelation`: `declare`、`impl`、类型参数、`for`。
    - `TopUsing`: `using`、braces、逗号和 trailing comma。

## Parser 实现说明

- handrolled parser 新增 `series_with_follow_info` 和 `surround_series_info`，返回解析元素、分隔符位置列表和可选 trailing delimiter 位置。
- MoonYacc grammar 新增对应的 `*_info` productions，用于逗号分隔列表和分号分隔列表。
- statement compaction 现在会先把 semicolon location 挂到 statement 上，再折叠为 `Let`、`LetMut`、`LetFn`、`LetRec`、`LetAnd`、`Guard`、`Defer` 或 `Sequence`。
- 新增 `set_impl_trailing_semi`，用于在每个顶层项解析后记录顶层 trailing semicolon。

## 验证

已运行：

```bash
moon test test/sync_test/cst_loc_test.mbt
```

结果：

```text
Total tests: 7, passed: 7, failed: 0.
```

测试覆盖 handrolled parser 与 MoonYacc parser 的一致性，验证两者语义 JSON AST 输出保持相等，并直接断言新增 CST token location 字段。
