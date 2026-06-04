# 验证: 新代码 vs 原始代码, 普通 ck 路径完全一致
suppressPackageStartupMessages({library(dplyr); library(stringr)})
source("R/info_id.R")
source("R/utils.R")
source("R/planting.R")
field <- read.table("data/field.txt", header=TRUE, stringsAsFactors=FALSE)

mp <- data.frame(
  id=paste0("id_",1:6), stageid=NA,
  name=paste0("品种", LETTERS[1:6]),
  stringsAsFactors=FALSE
)

# 模拟原始代码的行为
run_old_way <- function(my_primary, ck, interval, s_prefix, rp, digits, startN, first_as_ck,
                        treatment, place, restartfid, rows, ckfixed=TRUE) {
  result <- if (ckfixed) {
    my_primary |>
      addrpckfixed(ck, interval, s_prefix, rp, digits, startN, first_as_ck) |>
      addtreatment(treatment) |>
      addplace_addfieldid_addrows(place, restartfid, rows)
  } else {
    my_primary |>
      addrpck(ck, interval, s_prefix, rp, digits, startN, first_as_ck) |>
      addtreatment(treatment) |>
      addplace_addfieldid_addrows(place, restartfid, rows)
  }
  result <- align_to_field_schema(result, table_pattern = "planting")
  result
}

set.seed(123)
old <- run_old_way(mp, ck=c("冀豆12","冀豆17"), interval=2, s_prefix="GC",
                   rp=2, digits=3, startN=1, first_as_ck=FALSE,
                   treatment=c(""), place=c("石家庄","德州"),
                   restartfid=FALSE, rows=6, ckfixed=TRUE)

set.seed(123)
new <- planting(mp, ck=c("冀豆12","冀豆17"), interval=2, s_prefix="GC",
                rp=2, digits=3, startN=1, first_as_ck=FALSE,
                treatment=c(""), place=c("石家庄","德州"),
                restartfid=FALSE, rows=6, ckfixed=TRUE)

# 逐列对比
identical_cols <- sapply(names(old), function(cn) identical(old[[cn]], new[[cn]]))
cat("=== 列级对比 (identical) ===\n")
for (cn in names(old)) {
  cat(sprintf("  %-20s: %s\n", cn, if(identical_cols[[cn]]) "OK" else "DIFFERENT!"))
}
cat(sprintf("\n  => %d/%d 列完全一致\n", sum(identical_cols), length(identical_cols)))

# 确认不是引用同一个对象
cat(sprintf("\n  同一对象引用: %s\n", identical(old, new)))

# 再测 ckfixed=FALSE
set.seed(456)
old2 <- run_old_way(mp, ck=c("冀豆12"), interval=3, s_prefix="GC",
                    rp=3, digits=4, startN=1, first_as_ck=FALSE,
                    treatment=c("高密"), place=c("石家庄"),
                    restartfid=TRUE, rows=6, ckfixed=FALSE)

set.seed(456)
new2 <- planting(mp, ck=c("冀豆12"), interval=3, s_prefix="GC",
                 rp=3, digits=4, startN=1, first_as_ck=FALSE,
                 treatment=c("高密"), place=c("石家庄"),
                 restartfid=TRUE, rows=6, ckfixed=FALSE)

identical2 <- sapply(names(old2), function(cn) identical(old2[[cn]], new2[[cn]]))
cat("\n=== ckfixed=FALSE 列级对比 ===\n")
for (cn in names(old2)) {
  cat(sprintf("  %-20s: %s\n", cn, if(identical2[[cn]]) "OK" else "DIFFERENT!"))
}
cat(sprintf("\n  => %d/%d 列完全一致\n\n", sum(identical2), length(identical2)))

# ── 验证前缀机制：generate_stageid 调用链 ──
cat("=== 前缀机制验证 ===\n")
cat("generate_stageid 默认 char:", formals(generate_stageid)$char, "\n")
cat("planting 默认 s_prefix:", formals(planting)$s_prefix, "\n")
cat("addrpckfixed 默认 s_prefix:", formals(addrpckfixed)$s_prefix, "\n")
cat("\n→ planting 传入 s_prefix='GC' → addrpckfixed → generate_stageid(char='GC')\n")

# 验证不同前缀下的 code 提取
test_sp <- c("GC", "SJZ", "DZ", "P24")
for (sp in test_sp) {
  sid <- generate_stageid(start_num=1, end_num=3, char=sp, digit_length=3)
  code <- as.numeric(substring(sid, nchar(sp)+1))
  cat(sprintf("  s_prefix='%s': stageid=%s → code=%s  %s\n",
    sp, paste(sid,collapse=","), paste(code,collapse=","),
    if(all(code==1:3)) "✓" else "ERROR"))
}
cat("\nDONE\n")
