# `untyped_cst` Notes 行为等价性审查报告

- 审查日期：2026-07-17
- 审查范围：`untyped_cst/README.mbt.md` 的 `Note` 一节
- 对照实现：`handrolled_parser`
- 结论：**当前 `untyped_cst` 不符合 Note 声明的行为等价要求**

## 1. 审查标准

[`untyped_cst/README.mbt.md`](untyped_cst/README.mbt.md#L33) 要求：

1. 对任何被 `handrolled_parser` 拒绝的输入，CST parser 必须产生完全相同的 reports。
2. 对任何被 `handrolled_parser` 无报告接受的输入，CST parser 也必须无报告接受，并且 CST 转换得到的 AST 必须与 handrolled AST 相等。

本次审查按以下内容进行精确比较：

- report 数量、顺序、位置和消息；
- handrolled 与 CST 的接受/拒绝结果；
- 成功路径的 CST lowering reports；
- 成功路径的 AST JSON（包括位置数据）；
- CST 根节点还原的源码文本。

## 2. 主要问题

### P1：残缺表达式被零诊断接受

最小 Structure 示例：

```mbt
fn f() { let x =; }
```

handrolled parser 返回：

```text
Unexpected token `;`, you may expect simple expression.
```

CST parser 返回空 reports。

在 50 个截断 Expression 输入中发现 33 个不一致：

- 28 个被 handrolled 拒绝、却被 CST 零诊断接受；
- 5 个双方都报告错误，但 reports 不同。

被静默接受的代表输入包括：

```text
x +
x |>
x ..<
x is
x =~
-
!
raise
defer
(
[
{
f(
match x
while x {
fn(x)
```

截断矩阵位于 [`untyped_cst/ast_equiv_test.mbt`](untyped_cst/ast_equiv_test.mbt#L534)。

实现中存在一类重复模式：解析到运算符后，如果 RHS/operand 是空节点，只是不把它加入 children，却没有生成 missing node 或 report。例如：

- [`parse_expr_binary_prec`](untyped_cst/parse.mbt#L3306)；
- [`parse_expr_range`](untyped_cst/parse.mbt#L4053)；
- [`parse_expr_prefix`](untyped_cst/parse.mbt#L4080)。

在全量 snapshot 中，另有 37 个 handrolled 拒绝的文件被 CST 零诊断接受。

### P2：双方都拒绝时，reports 仍大面积不一致

对 1192 个可在单次聚合测试中完成比较的 snapshot，结果如下：

| 分类 | 数量 |
|---|---:|
| handrolled 无报告接受，且 CST reports/lowering/AST 全部一致 | 1004 |
| handrolled 报告错误，但 CST 零诊断接受 | 37 |
| CST 产生更多 reports | 84 |
| CST 产生更少但非零的 reports | 27 |
| report 数量相同，仅位置不同 | 1 |
| report 数量相同，仅消息不同 | 3 |
| report 数量相同，消息和位置都不同 | 17 |
| handrolled 错误路径完全等价 | 19 |

也就是说，在聚合测试覆盖的 188 个 handrolled 错误输入中，有 169 个不满足 Note。

#### 示例：小写错误类型

输入：

```mbt
fn f() raise foo {}
```

handrolled 产生一个精确报告：

```text
Unexpected lowercase identifier foo, expected uppercase identifier in error type.
```

CST 产生两个不同报告：

```text
Unexpected token id (lowercase start), you may expect type.
invalid error type
```

[`parse_error_annotation`](untyped_cst/parse.mbt#L10407) 先保留通用 type parser 的错误，又在无法识别错误类型时追加 `invalid error type`，因此报告数量和内容都与 handrolled 不同。

#### 示例：enum 使用逗号分隔

输入：

```mbt
enum Foo{Bar,Baz} //no trailing newline
```

双方都拒绝，但报告消息不同：

- handrolled：`Expecting a newline or ';' here ...`
- CST：`Unexpected token ',', you may expect ';', '}'.`

详细回归位于 [`untyped_cst/ast_equiv_test.mbt`](untyped_cst/ast_equiv_test.mbt#L731)。

## 3. 成功路径结果

在本次覆盖范围内，成功路径表现良好：

- 1004 个 handrolled 无报告接受的 snapshot 全部满足：
  - CST reports 为空；
  - CST lowering reports 为空；
  - CST 根节点可还原完整源码；
  - CST AST 与 handrolled AST 相等。
- 由 12 个 atom、19 个运算符和 18 个定向表达式组成的 2754 个 Expression case 没有发现新的行为或 AST 不一致，测试见 [`untyped_cst/ast_equiv_test.mbt`](untyped_cst/ast_equiv_test.mbt#L492)。
- 本次没有发现 handrolled 成功但 CST 报错、lowering 报错或 AST 不同的样例。

这支持 Note 的第二条在已检查语料上的成立，但有限语料不能构成对所有可能输入的形式化证明。

## 4. 测试语料与限制

共发现 1197 个 `test/sync_test/__snapshot__/*.mbt` 文件：

- 1192 个进入聚合差分测试；
- 4 个错误输入由独立恢复回归覆盖，不进入聚合差分统计；
- `pipeline_test_functional_loop4.mbt` 会使 handrolled parser 自身在 `pop_sync` 中抛错，因此该输入没有可用的 reference reports/AST，无法用于判定 CST 是否等价。

聚合测试会比较所有 snapshot，而不是只停在第一个错误，并按接受性、报告数量、消息和位置分类。实现见 [`handrolled_cst_snapshot_issue`](untyped_cst/ast_equiv_test.mbt#L559)。

## 5. 新增测试与验证状态

原有测试基线：

```text
moon test untyped_cst
Total tests: 91, passed: 91, failed: 0.
```

当前验证结果：

```text
moon test untyped_cst
Total tests: 101, passed: 96, failed: 5.
```

当前 5 个失败均来自审查测试，分别用于稳定复现：

- 截断表达式差分；
- 缺失 `let` 初始值的漏报；
- 小写 error type 的 report 数量/消息差异；
- enum 逗号在 EOF 场景下的消息差异；
- 全量 snapshot 差分汇总。

其他验证：

```text
moon check untyped_cst --warn-list +73
0 errors

moon info untyped_cst
公开接口无变化
```

## 6. 复现命令

```bash
# 全部新增审查测试
moon test untyped_cst/ast_equiv_test.mbt -v

# 成功路径表达式矩阵，应通过
moon test untyped_cst/ast_equiv_test.mbt \
  --filter '*expression operator matrix*' -v

# 截断表达式差分，应报告 33 个不一致
moon test untyped_cst/ast_equiv_test.mbt \
  --filter '*truncated expression matrix*' -v

# 全量 snapshot 差分，应报告 169 个不一致
moon test untyped_cst/ast_equiv_test.mbt \
  --filter '*equivalence across all parser snapshots*' -v

```

## 7. 建议修复顺序

1. 引入统一的 required-expression 恢复辅助函数，类似现有 `required_type_node`，在 RHS、prefix operand、range operand、参数和分隔结构缺失时生成 missing node 与报告。
2. 对照 handrolled 的精确恢复点，消除重复报告、漏报和恢复边界差异。
3. 保留全量 snapshot 差分作为回归门禁，逐类修复 169 个报告不一致，避免只为单个样例打补丁。
4. 最后重新运行成功路径 AST 矩阵，确认错误恢复修改没有改变合法输入的 AST。
