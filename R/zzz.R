# ── 包的依赖与导出声明 ──────────────────────────────────────────────────
#
# 本文件集中声明 NAMESPACE 的内容。roxygen2 会据此生成 NAMESPACE，
# 不要再手工编辑 NAMESPACE —— 手工改动会被下一次 roxygenise 覆盖。
#
# R CMD check 要求：包代码里用到的所有外部函数都必须在 NAMESPACE 中声明，
# 否则报 "no visible global function definition"。
#
# 导出策略沿用本包 1.0.0 起的做法：exportPattern("^[[:alpha:]]+") 导出
# 命名空间里所有以字母开头的对象（点开头的内部辅助函数不受影响）。
# 这样既保持了对既有调用方的兼容，也避免把 . 前缀的内部函数暴露出去。
#
#' @rawNamespace exportPattern("^[[:alpha:]]+")
#' @import stringr dplyr ggplot2 reshape2 openxlsx purrr DBI RSQLite readxl tidyr
#' @importFrom stats aggregate quantile sd setNames
#' @importFrom utils read.table write.table
NULL
