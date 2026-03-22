# handrolled_parser vs yacc_parser 行为对比报告

结论：有不一致。除了错误恢复之外，当前至少还存在 `loc` 级别的不一致。

## 已确认的不一致

### 1. Brace group 的位置不一致

当 `"{ ... }"` 被解析成 `Expr::Group(group=Brace)` 时，handrolled parser 的 group `loc` 会算错，甚至可能出现反向区间。

最小复现：

- `test/sync_test/__snapshot__/pipeline_test_global_let.mbt`

在这个用例里，`let global: Int = { ... }` 的右侧 block 被包成 `Expr::Group(group=Brace)`。

- handrolled: `1:19-1:18`
- yacc: `1:19-4:2`

根因：

- `handrolled_parser/parser.mbt` 在调用 `parse_block_expr()` 之前，就先用 `loc_start_with(spos)` 生成了外层 group 的 `loc`
- `yacc_parser/parser.mbty` 则直接使用整条产生式的 span

对应代码位置：

- `handrolled_parser/parser.mbt:4252`
- `handrolled_parser/parser.mbt:4262`
- `yacc_parser/parser.mbty:1687`

### 2. 无参数函数的 `params_loc` 不一致

无参数 `fn` 的 `params_loc` 在两边不一致，这会继续影响：

- `FunDecl::QuantifierList`
- `vis`
- `attrs`

最小复现同样是：

- `test/sync_test/__snapshot__/pipeline_test_global_let.mbt`

其中：

```mbt
fn init {
  let _ = global
}
```

对 `fn init {`：

- handrolled 的 `params_loc` 是 `6:9-6:8`
- yacc 的 `params_loc` 是 `6:8-6:8`

根因：

- handrolled 在没有参数时，`param_loc_start` 取的是下一个 token `{` 的起点
- 但结束位置仍然停在函数名 `init` 的末尾
- yacc 这里使用的是 `option(parameters)` 的空 span

对应代码位置：

- `handrolled_parser/parser.mbt:436`
- `handrolled_parser/parser.mbt:446`
- `yacc_parser/parser.mbty:325`
- `yacc_parser/parser.mbty:336`
- `syntax/ast_json_repr.mbt:904`

### 3. 两边 package API 的 token 契约不一致

如果不是通过顶层 `parse_string`，而是直接调用 package 级 API，两边对于输入 token 流的约定并不一致。

- `handrolled_parser.parse/parse_expr` 会在内部跳过 `NEWLINE` 和 `COMMENT`
- `yacc_parser` 依赖调用者事先过滤掉 `NEWLINE` 和 `COMMENT`

因此直接把 raw lexer tokens 传给两边时，它们并不等价。

对应代码位置：

- `handrolled_parser/core.mbt:61`
- `yacc_parser/parser.mbty:28`
- `top.mbt:118`

## 关于现有 sync_test

现有 `test/sync_test` 我已运行，结果是：

- 总计 2091 个用例
- 全部通过

但这只能证明“结构 AST 一致”，不能证明“位置信息一致”。

原因是：

- `json_repr()` 是否输出 `loc` 取决于 `show_loc`
- 默认配置下 `show_loc = Hidden`
- 因此 sync_test 默认比较时，`loc` 被序列化为 `null`

对应代码位置：

- `basic/config.mbt:9`
- `basic/loc.mbt:109`
- `test/sync_test/helper_test.mbt:25`

## 最终结论

当前可以确认：

1. 在成功解析路径上，结构 AST 基本一致，现有同步测试全部通过。
2. 但位置信息并不完全一致。
3. 已确认的差异至少包括：
   - brace group 的 `loc`
   - 无参数函数的 `params_loc` 及其派生位置
   - 直接调用底层 parser API 时，对 `NEWLINE/COMMENT` 的输入契约差异
