# is_ck 全部为 1 的 Bug 分析报告

## 问题现象

群众记录本生成后，数据库中 `is_ck` 字段全部为 1，而预期应该是：
- 原始材料行：`is_ck = 0`
- 对照行：`is_ck = 1`

## 问题影响

三个模块的田试记录表中，`is_ck` 值的含义：
- `is_ck = 0` 表示材料行（非对照）
- `is_ck = 1` 表示对照行

当 `is_ck` 全为 1 时，无法区分哪些是材料行，哪些是对照行。

## 问题定位

### 1. 调试输出

在 `mod_population.R` 的 `planting()` 调用后添加调试输出：

```r
print(paste0("=== location ", i, " planting is_ck ==="))
print(table(planted_loc$is_ck, useNA = "ifany"))
```

发现 `planting()` 函数返回的 `is_ck` 就已经是全 1，说明问题在 `planting()` 函数内部。

### 2. 代码追踪

`planting()` 函数的调用链：

```
planting()
  └─ addrpckfixed() [ckfixed = TRUE 时]
       ├─ split() 分组
       ├─ insert_ck_rows() 插入对照行
       ├─ do.call(rbind, df_list) 合并
       └─ df_list$is_ck <- ifelse(is.na(df_list$is_ck), 0L, 1L)  ← 问题在这里
```

## 根本原因

### `insert_ck_rows()` 函数（第68-111行）

`insert_ck_rows()` 正确设置了 `is_ck`：
- 原始材料行：`is_ck_vec[seq_len(ng)] <- 0L`（第93行）
- 插入的对照行：`is_ck_vec[nres] <- 1L`（第94行）

### `addrpckfixed()` 函数（第26-88行）

第45行有错误代码：
```r
df_list$is_ck <- ifelse(is.na(df_list$is_ck), 0L, 1L)
```

同样的问题也存在于 `addrpck()` 函数（第100-172行）中。

### Bug 分析

`ifelse(is.na(df_list$is_ck), 0L, 1L)` 的逻辑：

| 原始 is_ck 值 | is.na() 结果 | ifelse 返回值 | 问题 |
|--------------|-------------|--------------|------|
| 0            | FALSE       | 1L           | **错误！应该是 0** |
| 1            | FALSE       | 1L           | 正确 |

所以：
- 所有原始材料行的 `is_ck` 从 0 被错误地改成了 1
- 对照行的 `is_ck` 保持为 1

## 修复方案

删除 `addrpckfixed()` 和 `addrpck()` 中这两行错误代码：

```r
# 需要删除的代码
df_list$is_ck <- ifelse(is.na(df_list$is_ck), 0L, 1L)
```

因为 `insert_ck_rows()` 已经正确设置了 `is_ck`，不需要再次赋值。

### 修复后的代码

```r
df_list <- split(mym, patten)
df_list <- insert_ck_rows(df_list, ck)
df_list <- do.call(rbind, df_list)
# insert_ck_rows 已正确设置 is_ck：原始行=0，对照行=1，无需再次赋值
```

## 修复文件

- `E:\FangCloudSync\R_WD360\Project\soyplant\R\planting.R`
  - 第42-45行：`addrpckfixed()` 函数
  - 第118-121行：`addrpck()` 函数

## 验证方法

```r
# 重新安装包
devtools::install("E:/FangCloudSync/R_WD360/Project/soyplant")

# 测试
library(soyplant)
mydata <- data.frame(
  id = paste0("g", 1:5),
  stageid = paste0("N25F2P", 1:5),
  name = paste0("材料", 1:5)
)

result <- planting(mydata, ck = "对照1", interval = 3, s_prefix = "GC",
                   rp = 1, place = "安徽宿州", ckfixed = TRUE, digits = 3)

# 验证 is_ck 分布
table(result$is_ck)
# 预期：0 出现5次（材料行），1 出现1次（对照行）
```

## 经验教训

1. **不要重复赋值已正确设置的值**：`insert_ck_rows()` 已经正确设置了 `is_ck`，不应该再次覆盖。

2. **`ifelse(is.na(x), a, b)` 的陷阱**：当 x 是 0 或 1 这样的非 NA 值时，`is.na(x)` 返回 FALSE，会返回 b。如果意图是"将 NA 设为 0"，应该用 `ifelse(is.na(x), 0L, x)` 而不是 `ifelse(is.na(x), 0L, 1L)`。

3. **调试技巧**：通过在关键步骤添加 `print()` 或 `table()` 输出，可以快速定位问题发生的环节。
