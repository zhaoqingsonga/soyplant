# 回归测试：calculate_MuChan_increase_with_multiple_methods 的「较临近对照」配对
#
# 背景（2026-04-08 引入的 bug）：
#   旧实现是 for 循环，逐个材料取「离它最近的两个对照」求均值做基准。
#   2026-04-08 被改成矩阵向量化版本，其中
#       control_avg_near <- rowMeans(matrix(control_mu, nrow = length(non_ck_idx)))
#   存在配对错位：closest_2_idx（2 x N 矩阵）做索引时按列主序降维，
#   control_mu 的顺序变成 [材料1对照1, 材料1对照2, 材料2对照1, 材料2对照2, ...]，
#   再用 matrix(..., nrow = N) 按列主序切一次，会把 A 材料的对照1
#   与 B 材料的对照2 配成一对，导致增产率与位次系统性错位。
#   该 bug 不报错、不崩溃，会静默产出错误报告。
#
# 本测试用「参考实现」（朴素 for 循环，语义直白无歧义）作为基准，
# 对三种典型布局各做多轮随机对照，把该行为钉死，防止再被"优化"掉。

library(testthat)

# ── 参考实现：朴素 for 循环，每个材料自己算自己的两个最近对照 ──────────
#
# ⚠️ 必须与被测函数保持同一前置处理：被测函数会先调用 remove_ck_outliers()
#    把离群的对照产量置为 NA（IQR 规则），再参与计算。
#    参考实现若不剔除离群对照，两者会在「恰好发生离群剔除」的轮次不一致。
reference_near_increase <- function(df) {
  df <- remove_ck_outliers(df)
  control_indices <- which(df$is_ck == 1 & !is.na(df$MuChan))
  control_avg <- mean(df$MuChan[control_indices])
  out <- rep(NA_real_, nrow(df))
  for (i in seq_len(nrow(df))) {
    if (df$is_ck[i] == 1) next
    sorted_indices <- order(abs(control_indices - i))
    closest <- control_indices[sorted_indices[1:2]]
    base <- mean(df$MuChan[closest])
    out[i] <- (df$MuChan[i] - base) / base * 100
  }
  out
}

# ── 布局构造 ─────────────────────────────────────────────────────────────
layout_every_5 <- function(n) {
  v <- rep(0, n); v[seq(1, n, by = 5)] <- 1; v
}
layout_every_3 <- function(n) {
  v <- rep(0, n); v[seq(1, n, by = 3)] <- 1; v
}
layout_random  <- function(n) sample(rep(c(0, 1), length.out = n))

test_that("较临近对照增产率：与朴素 for 循环参考实现一致（间隔5）", {
  set.seed(20261030)
  for (trial in seq_len(60)) {
    n <- sample(40:150, 1)
    df <- data.frame(
      is_ck = layout_every_5(n),
      MuChan = round(runif(n, 60, 260), 2)
    )
    skip_if(sum(df$is_ck) < 4)
    got <- calculate_MuChan_increase_with_multiple_methods(df)$JiaoLinJinDuiZhaoZengChan
    exp <- reference_near_increase(df)
    idx <- !is.na(exp)
    expect_equal(got[idx], exp[idx], tolerance = 1e-9,
                 info = sprintf("间隔5布局第 %d 轮不一致", trial))
  }
})

test_that("较临近对照增产率：与朴素 for 循环参考实现一致（随机布局）", {
  set.seed(20261031)
  for (trial in seq_len(60)) {
    n <- sample(40:150, 1)
    df <- data.frame(
      is_ck = layout_random(n),
      MuChan = round(runif(n, 60, 260), 2)
    )
    skip_if(sum(df$is_ck) < 4)
    got <- calculate_MuChan_increase_with_multiple_methods(df)$JiaoLinJinDuiZhaoZengChan
    exp <- reference_near_increase(df)
    idx <- !is.na(exp)
    expect_equal(got[idx], exp[idx], tolerance = 1e-9,
                 info = sprintf("随机布局第 %d 轮不一致", trial))
  }
})

test_that("较临近对照增产率：与朴素 for 循环参考实现一致（间隔3）", {
  set.seed(20261032)
  for (trial in seq_len(60)) {
    n <- sample(40:150, 1)
    df <- data.frame(
      is_ck = layout_every_3(n),
      MuChan = round(runif(n, 60, 260), 2)
    )
    skip_if(sum(df$is_ck) < 4)
    got <- calculate_MuChan_increase_with_multiple_methods(df)$JiaoLinJinDuiZhaoZengChan
    exp <- reference_near_increase(df)
    idx <- !is.na(exp)
    expect_equal(got[idx], exp[idx], tolerance = 1e-9,
                 info = sprintf("间隔3布局第 %d 轮不一致", trial))
  }
})

test_that("可手算的最小用例：最近两对照的均值作基准", {
  # 对照在第 1、3 行，材料在第 2、4 行；产量刻意设成可心算的值
  df <- data.frame(
    is_ck = c(1, 0, 1, 0),
    MuChan = c(100, 150, 200, 50)
  )
  res <- calculate_MuChan_increase_with_multiple_methods(df)
  # 材料1(第2行)：最近两对照 = 第1行(100)、第3行(200) → 基准150 → (150-150)/150 = 0
  expect_equal(res$JiaoLinJinDuiZhaoZengChan[2], 0, tolerance = 1e-9)
  # 材料2(第4行)：最近两对照 = 第3行(200)、第1行(100) → 基准150 → (50-150)/150 = -66.67
  expect_equal(res$JiaoLinJinDuiZhaoZengChan[4], (50 - 150) / 150 * 100, tolerance = 1e-9)
  # 对照行必须是 NA
  expect_true(is.na(res$JiaoLinJinDuiZhaoZengChan[1]))
  expect_true(is.na(res$JiaoLinJinDuiZhaoZengChan[3]))
})

test_that("单个对照时退化为与该对照比较", {
  df <- data.frame(
    is_ck = c(1, 0, 0),
    MuChan = c(200, 250, 100)
  )
  res <- calculate_MuChan_increase_with_multiple_methods(df)
  expect_equal(res$JiaoLinJinDuiZhaoZengChan[2], 25, tolerance = 1e-9)
  expect_equal(res$JiaoLinJinDuiZhaoZengChan[3], -50, tolerance = 1e-9)
})

test_that("较平均对照增产率不受本次修复影响", {
  set.seed(20261033)
  df <- data.frame(
    is_ck = layout_random(80),
    MuChan = round(runif(80, 60, 260), 2)
  )
  res <- calculate_MuChan_increase_with_multiple_methods(df)
  ci <- which(df$is_ck == 1)
  ctrl_avg <- mean(df$MuChan[ci])
  mat <- which(df$is_ck == 0)
  exp <- (df$MuChan[mat] - ctrl_avg) / ctrl_avg * 100
  expect_equal(res$JiaoPingJunDuiZhaoZengChan[mat], exp, tolerance = 1e-9)
})
