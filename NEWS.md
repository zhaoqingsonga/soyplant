# soyplant 更新日志

本文件记录各版本的重要变更。版本号遵循语义化版本（SemVer）：
主版本号 = 不兼容的 API 变更，次版本号 = 向后兼容的新功能，修订号 = 缺陷修复。

---

## 1.1.0（2026-10-30）

本次版本的核心是**修复一个静默产出错误报告的缺陷**。
该缺陷不报错、不崩溃，2026-04-08 至 2026-10-30 期间产出的所有相关报告都需要重新核对，详见下方「重要」一节。

### 重要：`calculate_MuChan_increase_with_multiple_methods()` 的「较临近对照」结果此前是错的

**受影响的两列**：`JiaoLinJinDuiZhaoZengChan`（较临近对照增产%）、`JiaoLinJinDuiZhaoWeiCi`（较临近对照位次）。

**未受影响的两列**：`JiaoPingJunDuiZhaoZengChan`（较平均对照增产%）、`JiaoPingJunDuiZhaoWeiCi`（较平均对照位次）——
这两列走全局对照均值，不经过出问题的代码路径。

**缺陷成因**：2026-04-08 的向量化重写把 for 循环版改写成矩阵版，其中

```r
control_avg_near <- rowMeans(matrix(control_mu, nrow = length(non_ck_idx)))
```

存在配对错位。`closest_2_idx` 是 2×N 矩阵（行 1 = 最近对照，行 2 = 第二近对照），
对它做索引会按列主序降维，`control_mu` 的顺序变成
`[材料1对照1, 材料1对照2, 材料2对照1, 材料2对照2, ...]`；
再用 `matrix(..., nrow = N)` 按列主序切一次，会把 A 材料的对照 1 与 B 材料的对照 2 配成一对，
增产率与位次因此系统性错位。

**修复方式**：改为按 `nrow = 2` 重建 2×N 矩阵，此时每一列才是一份材料自己的两个对照：

```r
control_avg_near <- colMeans(matrix(control_mu, nrow = 2))
```

**算法口径未变**——仍然是「取离该材料最近的两个对照产量均值」作基准。
2024-12-28 初版至 2025-12-31 的 for 循环实现即为此算法且是正确的，
问题仅在实现层，本次修复让代码回到与算法一致的行为。

**真实数据实测的偏差**（修复前 vs 修复后）：

| 场景 | 偏差 > 1pp 的材料 | 最大偏差 |
|---|---|---|
| N-5.2 宿州点（122 份） | 116 份 | 31.2 pp |
| N-1.3 核心亲本（300 份） | 280 份 | 18.5 pp |
| N-4.2 初级产比（373 份） | 363 份 | 17.8 pp |

**需要采取的行动**：若持有 2026-04-08 至 2026-10-30 期间由本包生成的报告，
其中「较临近对照增产%」与「较临近对照位次」两列应视为无效，需重新计算。

**验证**：新增回归测试 `tests/testthat/test-yield-near-control-pairing.R`，
以朴素 for 循环为参考实现，对间隔 5 / 随机 / 间隔 3 三种布局各做 60 轮随机对照（共 180 轮），
另含 2 个可手算的最小用例与 1 个单对照退化用例。
本地另做 600 轮随机对照，修复版与参考实现 600/600 一致。

### 新增

- `planting()` 支持**每个地点使用不同的对照**：`ck` 可传入命名列表
  `list("石家庄" = "冀豆12", "德州" = c("冀豆17", "鲁豆1号"))`，
  每个地点独立插入对照、独立编号。
- `planting()` 支持**每个地点使用不同的 stageid 前缀**：`s_prefix` 可传入命名列表
  `list("石家庄" = "SJZ", "德州" = "DZ")`。
- `planting()` 新增参数 `first_as_ck`：控制首记录是否作为对照插入。
- `planting()` 新增参数 `startN`：起始编号。

### 修复

- `planting()` 在 `ck` 全为空字符串时不会生成 `is_ck` 列，现已补齐，避免后续取值出错。
- `stageid` 的匹配改用 `substring(stageid)` 计算，正确处理带前缀的 stageid。

### 文档与工程

- 修正 `man/midu.Rd` 的 Rd 语法警告（`\details` 下裸用 `\item`，改为 `\describe{}` 包裹）。
  `R CMD INSTALL` 由此从 3 个 warning 变为零警告。
- 补齐 `tests/testthat.R` 标准入口。此前该文件不存在，`R CMD check` 从未真正执行过任何测试。
- 新增 `tests/testthat/setup.R`，解决 `pkgload::load_all()` 下 `object 'field' not found`
  导致 planting 测试全部报错的问题（`R CMD INSTALL` 会把 `data/*.txt` 转成 lazy-load 数据集，
  而 `load_all()` 不会，故两条加载路径下 `field` 的可见性不同）。
- 修正 `man/planting.Rd` 与 `man/insert_ck_rows.Rd` 的过期内容。
- 补上 `man/calculate_MuChan_increase_with_multiple_methods.Rd`——此前该函数用的是
  错误的小写名 `calculate_muchan_...` 的文档，实际处于无文档状态。
- `DESCRIPTION` 补充 `Suggests: testthat` 与 `Config/testthat/edition: 3`。

### 质量

- `R CMD check` 状态由基线的 `2 ERRORs, 9 WARNINGs, 3 NOTEs` 改善为 `1 ERROR, 8 WARNINGs, 4 NOTEs`。
- 测试共 220 项，在 `test_dir()`、`devtools::test()`、`testthat::test_local()`
  与 `R CMD check` 内部四种方式下均全部通过。
- 剩余 1 个 ERROR 来自 `add_fields_to_table` 的示例需要连接真实 SQLite 文件，与本次改动无关。

---

## 1.0.0（2024-09-07）

首个正式版本。提供从杂交组合、群体、单株、株行、初级产比、高级产比到田间种植的
全流程数据管理与试验设计功能。
