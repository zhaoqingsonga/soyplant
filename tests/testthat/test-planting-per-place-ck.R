# TDD: 不同地点插入不同对照 — per-place ck support
# RED phase: these tests should fail until implementation is complete

library(testthat)
library(dplyr)

# 加载包函数和全局数据
devtools::load_all("E:/FangCloudSync/R_WD360/Project/soyplant", quiet = TRUE)
field <- read.table(
  "E:/FangCloudSync/R_WD360/Project/soyplant/data/field.txt",
  header = TRUE, stringsAsFactors = FALSE
)
stage_prefix <- read.table(
  "E:/FangCloudSync/R_WD360/Project/soyplant/data/stage_prefix.txt",
  header = TRUE, stringsAsFactors = FALSE
)

# ── Helper: 构造最小测试数据 ──────────────────────────────────────────────
make_test_primary <- function(n = 9) {
  data.frame(
    id   = paste0("id_", seq_len(n)),
    stageid = paste0("24GC", sprintf("%03d", seq_len(n))),
    name = paste0("品种", LETTERS[seq_len(n)]),
    stringsAsFactors = FALSE
  )
}

# ═══════════════════════════════════════════════════════════════════════════
# 已有功能测试
# ═══════════════════════════════════════════════════════════════════════════

test_that("per-place ck: 两个地点各用不同对照", {
  myp <- make_test_primary(6)
  ck_pp <- list(
    "石家庄" = "冀豆12",
    "德州"   = c("冀豆17", "鲁豆1号")
  )

  res <- planting(myp,
    ck = ck_pp, place = c("石家庄", "德州"),
    interval = 2, rp = 1, digits = 3,
    treatment = c(""), rows = 6
  )

  # — 石家庄只有"冀豆12"作为对照 —
  sjz <- res[res$place == "石家庄", ]
  sjz_ck <- sjz[sjz$is_ck == 1, ]
  expect_true(all(sjz_ck$name == "冀豆12"))
  expect_false(any(sjz_ck$name == "鲁豆1号"))

  # — 德州有"冀豆17"和"鲁豆1号" —
  dz <- res[res$place == "德州", ]
  dz_ck <- dz[dz$is_ck == 1, ]
  expect_true(all(dz_ck$name %in% c("冀豆17", "鲁豆1号")))
  expect_true(any(dz_ck$name == "冀豆17"))
  expect_true(any(dz_ck$name == "鲁豆1号"))
})

test_that("per-place ck: 某地点无对照（空字符串）", {
  myp <- make_test_primary(6)
  ck_pp <- list(
    "石家庄" = c(""),
    "德州"   = "冀豆17"
  )

  res <- planting(myp,
    ck = ck_pp, place = c("石家庄", "德州"),
    interval = 2, rp = 1, digits = 3,
    treatment = c(""), rows = 6
  )

  # 石家庄：is_ck 全为 0（无对照插入）
  sjz <- res[res$place == "石家庄", ]
  expect_true(all(sjz$is_ck == 0))

  # 德州：有对照
  dz <- res[res$place == "德州", ]
  expect_true(any(dz$is_ck == 1))
})

test_that("per-place ck: 向后兼容 — 普通向量 ck 行为不变", {
  myp <- make_test_primary(6)

  res <- planting(myp,
    ck = c("冀豆12", "冀豆17"),
    place = c("石家庄", "德州"),
    interval = 2, rp = 1, digits = 3,
    treatment = c(""), rows = 6
  )

  # 两地对照相同
  sjz_ck <- res[res$place == "石家庄" & res$is_ck == 1, ]
  dz_ck  <- res[res$place == "德州"   & res$is_ck == 1, ]
  expect_true(all(sjz_ck$name %in% c("冀豆12", "冀豆17")))
  expect_true(all(dz_ck$name  %in% c("冀豆12", "冀豆17")))
})

test_that("per-place ck: rp > 1 重复种植时正确", {
  myp <- make_test_primary(6)
  ck_pp <- list(
    "石家庄" = "冀豆12",
    "德州"   = "冀豆17"
  )

  res <- planting(myp,
    ck = ck_pp, place = c("石家庄", "德州"),
    interval = 2, rp = 2, digits = 3,
    treatment = c(""), rows = 6
  )

  # 每地每重复的对照品种一致
  sjz <- res[res$place == "石家庄", ]
  for (rp_i in unique(sjz$rp)) {
    sjz_rp <- sjz[sjz$rp == rp_i & sjz$is_ck == 1, ]
    expect_true(all(sjz_rp$name == "冀豆12"))
  }

  dz <- res[res$place == "德州", ]
  for (rp_i in unique(dz$rp)) {
    dz_rp <- dz[dz$rp == rp_i & dz$is_ck == 1, ]
    expect_true(all(dz_rp$name == "冀豆17"))
  }
})

test_that("per-place ck: ckfixed = FALSE 随机模式也支持", {
  myp <- make_test_primary(9)
  ck_pp <- list(
    "石家庄" = "冀豆12",
    "德州"   = "齐黄34"
  )

  res <- planting(myp,
    ck = ck_pp, place = c("石家庄", "德州"),
    interval = 3, rp = 2, digits = 3,
    ckfixed = FALSE,
    treatment = c(""), rows = 6
  )

  sjz <- res[res$place == "石家庄", ]
  sjz_ck <- sjz[sjz$is_ck == 1, ]
  expect_true(all(sjz_ck$name == "冀豆12"))

  dz <- res[res$place == "德州", ]
  dz_ck <- dz[dz$is_ck == 1, ]
  expect_true(all(dz_ck$name == "齐黄34"))
})

test_that("per-place ck: first_as_ck = TRUE 模式", {
  myp <- make_test_primary(4)
  ck_pp <- list(
    "石家庄" = "冀豆12",
    "德州"   = "冀豆17"
  )

  res <- planting(myp,
    ck = ck_pp, place = c("石家庄", "德州"),
    interval = 2, rp = 1, digits = 3,
    treatment = c(""), rows = 6,
    first_as_ck = TRUE
  )

  # 石家庄第一行是冀豆12
  sjz <- res[res$place == "石家庄", ]
  expect_equal(sjz$name[1], "冀豆12")

  # 德州第一行是冀豆17
  dz <- res[res$place == "德州", ]
  expect_equal(dz$name[1], "冀豆17")
})

test_that("per-place ck: 单个地点 named list 也能工作", {
  myp <- make_test_primary(6)
  ck_pp <- list("石家庄" = "冀豆12")

  res <- planting(myp,
    ck = ck_pp, place = c("石家庄"),
    interval = 2, rp = 1, digits = 3,
    treatment = c(""), rows = 6
  )

  expect_true(all(res$place == "石家庄"))
  ck_rows <- res[res$is_ck == 1, ]
  expect_true(all(ck_rows$name == "冀豆12"))
})

test_that("per-place ck: 多地对照品种完全不重叠", {
  myp <- make_test_primary(6)
  ck_pp <- list(
    "石家庄" = "冀豆12",
    "德州"   = "齐黄34",
    "济南"   = "鲁豆1号"
  )

  res <- planting(myp,
    ck = ck_pp, place = c("石家庄", "德州", "济南"),
    interval = 2, rp = 1, digits = 3,
    treatment = c(""), rows = 6
  )

  for (p in names(ck_pp)) {
    p_data <- res[res$place == p & res$is_ck == 1, ]
    expected_ck <- ck_pp[[p]]
    if (length(expected_ck) > 1 || expected_ck != "") {
      expect_true(all(p_data$name %in% expected_ck),
        info = paste("Place", p, "should only have its own ck"))
    }
  }

  # 确保无交叉污染
  sjz <- res[res$place == "石家庄" & res$is_ck == 1, ]
  expect_false(any(sjz$name == "齐黄34"))
  expect_false(any(sjz$name == "鲁豆1号"))
})

# ═══════════════════════════════════════════════════════════════════════════
# RED 测试 — 跨地点重复排列一致性（当前应失败）
# ═══════════════════════════════════════════════════════════════════════════

test_that("per-place ckfixed=TRUE: rp>1 各地非对照材料排列一致", {
  myp <- make_test_primary(9)
  ck_pp <- list("石家庄" = "冀豆12", "德州" = "冀豆17")

  suppressWarnings(set.seed(42))
  res <- planting(myp,
    ck = ck_pp, interval = 3, rp = 3, digits = 3,
    ckfixed = TRUE, treatment = c(""), rows = 6
  )

  # 排除对照行：各地同一重复的 non-ck 材料排列应完全相同
  for (rpi in 2:3) {
    sjz_order <- res$name[res$place == "石家庄" & res$rp == rpi & res$is_ck == 0]
    dz_order  <- res$name[res$place == "德州"   & res$rp == rpi & res$is_ck == 0]
    expect_equal(as.character(sjz_order), as.character(dz_order),
      label = paste("rp =", rpi, "石家庄 vs 德州"))
  }
})

test_that("per-place ckfixed=FALSE: rp>1 各地非对照材料排列一致", {
  myp <- make_test_primary(9)
  ck_pp <- list("石家庄" = "冀豆12", "德州" = "齐黄34")

  suppressWarnings(set.seed(42))
  res <- planting(myp,
    ck = ck_pp, interval = 3, rp = 3, digits = 3,
    ckfixed = FALSE, treatment = c(""), rows = 6
  )

  for (rpi in 2:3) {
    sjz_order <- res$name[res$place == "石家庄" & res$rp == rpi & res$is_ck == 0]
    dz_order  <- res$name[res$place == "德州"   & res$rp == rpi & res$is_ck == 0]
    expect_equal(as.character(sjz_order), as.character(dz_order),
      label = paste("rp =", rpi, "石家庄 vs 德州"))
  }
})

test_that("per-place ckfixed=TRUE: rp=1 各地非对照材料排列一致（本来就是顺序的）", {
  myp <- make_test_primary(6)
  ck_pp <- list("石家庄" = "冀豆12", "德州" = "冀豆17", "济南" = "齐黄34")

  res <- planting(myp,
    ck = ck_pp, interval = 2, rp = 2, digits = 3,
    ckfixed = TRUE, treatment = c(""), rows = 6
  )

  # rp=1 本来就是顺序的，各地自然一致（regression check）
  sjz_r1 <- res$name[res$place == "石家庄" & res$rp == 1 & res$is_ck == 0]
  dz_r1  <- res$name[res$place == "德州"   & res$rp == 1 & res$is_ck == 0]
  expect_equal(as.character(sjz_r1), as.character(dz_r1))
})

# ═══════════════════════════════════════════════════════════════════════════
# RED 测试 — per-place s_prefix 不同地点不同前缀（当前应失败）
# ═══════════════════════════════════════════════════════════════════════════

test_that("per-place s_prefix: 不同地点用不同 stageid 前缀", {
  myp <- make_test_primary(6)
  ck_pp <- list("石家庄" = "冀豆12", "德州" = "冀豆17")
  sp_pp <- list("石家庄" = "SJZ", "德州" = "DZ")

  res <- planting(myp,
    ck = ck_pp, s_prefix = sp_pp,
    interval = 2, rp = 2, digits = 3,
    ckfixed = TRUE, treatment = c(""), rows = 6
  )

  # 石家庄的 stageid 以 SJZ 开头
  sjz_stageid <- unique(res$stageid[res$place == "石家庄"])
  expect_true(all(grepl("^SJZ", sjz_stageid)),
    info = paste("石家庄 stageid 应以 SJZ 开头:", head(sjz_stageid, 3)))

  # 德州的 stageid 以 DZ 开头
  dz_stageid <- unique(res$stageid[res$place == "德州"])
  expect_true(all(grepl("^DZ", dz_stageid)),
    info = paste("德州 stageid 应以 DZ 开头:", head(dz_stageid, 3)))
})

test_that("per-place s_prefix: 向后兼容 — 普通字符串 s_prefix 不变", {
  myp <- make_test_primary(6)
  ck_pp <- list("石家庄" = "冀豆12", "德州" = "冀豆17")

  res <- planting(myp,
    ck = ck_pp, s_prefix = "GC",
    interval = 2, rp = 2, digits = 3,
    ckfixed = TRUE, treatment = c(""), rows = 6
  )

  # 所有 stageid 以 GC 开头
  all_stageid <- unique(res$stageid)
  expect_true(all(grepl("^GC", all_stageid)))
})

test_that("per-place s_prefix: 与普通 ck 向量一起使用也不影响", {
  myp <- make_test_primary(6)

  # 普通 ck + 普通 s_prefix：完全不受影响
  res <- planting(myp,
    ck = c("冀豆12"), s_prefix = "TS",
    place = c("石家庄"), interval = 2, rp = 1, digits = 3,
    treatment = c(""), rows = 6
  )

  expect_true(all(grepl("^TS", res$stageid)))
})
