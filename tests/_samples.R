suppressPackageStartupMessages({library(dplyr); library(stringr)})
source("R/info_id.R"); source("R/utils.R"); source("R/planting.R")
field <- read.table("data/field.txt", header=TRUE, stringsAsFactors=FALSE)

mp <- data.frame(
  id=NA, stageid=NA,
  name=c("冀豆12×齐黄34", "鲁豆1号×冀豆17", "齐黄34×黑农48", "冀豆12×鲁豆1号"),
  stringsAsFactors=FALSE
)

sink("tests/testthat/_output.txt")

# ── 样例1：统一对照（原有方式，不应变）──
cat("====== 样例1: 统一对照 ck=c('冀豆12','冀豆17') s_prefix='GC' ======\n")
set.seed(1)
r1 <- planting(mp, ck=c("冀豆12","冀豆17"), interval=3, s_prefix="GC", rp=2,
               digits=3, rows=6, treatment=c(""), place=c("石家庄","德州"))
print(r1[, c("place","rp","stageid","code","name","is_ck","fieldid")], row.names=FALSE)

# ── 样例2：per-place ck ──
cat("\n====== 样例2: per-place ck ======\n")
cat("石家庄→冀豆12, 德州→冀豆17+鲁豆1号\n")
set.seed(1)
r2 <- planting(mp, ck=list("石家庄"="冀豆12", "德州"=c("冀豆17","鲁豆1号")),
               interval=3, s_prefix="GC", rp=2, digits=3, rows=6, treatment=c(""))
print(r2[, c("place","rp","stageid","code","name","is_ck","fieldid")], row.names=FALSE)

# ── 样例3：per-place ck + per-place s_prefix ──
cat("\n====== 样例3: per-place ck + per-place s_prefix ======\n")
cat("石家庄→SJZ前缀, 德州→DZ前缀\n")
set.seed(1)
r3 <- planting(mp, ck=list("石家庄"="冀豆12", "德州"="冀豆17"),
               s_prefix=list("石家庄"="SJZ", "德州"="DZ"),
               interval=3, rp=2, digits=3, rows=6, treatment=c(""))
print(r3[, c("place","rp","stageid","code","name","is_ck","fieldid")], row.names=FALSE)

# ── 样例4：per-place ck + restartfid=TRUE ──
cat("\n====== 样例4: per-place ck, restartfid=TRUE ======\n")
set.seed(1)
r4 <- planting(mp, ck=list("石家庄"="冀豆12", "德州"="齐黄34"),
               interval=3, rp=2, digits=3, rows=6, restartfid=TRUE, treatment=c(""))
print(r4[, c("place","rp","stageid","code","name","is_ck","fieldid")], row.names=FALSE)

# ── 样例5：first_as_ck=TRUE ──
cat("\n====== 样例5: per-place ck, first_as_ck=TRUE ======\n")
set.seed(1)
r5 <- planting(mp, ck=list("石家庄"="冀豆12", "德州"="冀豆17"),
               interval=3, rp=1, digits=3, rows=6, first_as_ck=TRUE, treatment=c(""))
print(r5[, c("place","rp","stageid","code","name","is_ck","fieldid")], row.names=FALSE)

sink()
cat("DONE\n")
