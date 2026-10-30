# ── 保证 field（字段 schema 表）在任何加载方式下都可用 ────────────────────
#
# align_to_field_schema() 的默认参数是 field_df = field，取值时按 R 的常规
# 作用域链从函数所在环境（包命名空间）往外找：
#
#   命名空间 → imports → base → R_GlobalEnv → 已挂载的 package:soyplant → ...
#
#   R CMD INSTALL：data/field.txt 被转成 lazy-load 数据集，挂到 package:soyplant
#                  上 → 上面这条链能查到 field ✅
#   pkgload::load_all()：不做 .txt → lazy-load 的转换，链上任何一环都没有
#                  field → planting() 报 "object 'field' not found" ❌
#
# 所以这里用「按同样规则查一遍」来判定，而不是简单地看命名空间里有没有——
# 命名空间本身两种情况下都没有 field，真正的差别在链尾的已挂载环境。
#
# 查不到时把 data/field.txt 读进 R_GlobalEnv：它在作用域链中位于
# package:soyplant 之前，两种情形下都能被查到。
if (!requireNamespace("soyplant", quietly = TRUE)) {
  stop("soyplant 包未加载，无法准备测试所需的 field 表。")
}

ns <- asNamespace("soyplant")
field_available <- local({
  # 复刻默认参数的查找规则：函数环境 → 父环境链
  env <- environment(get("align_to_field_schema", envir = ns))
  exists("field", envir = env, inherits = TRUE)
})

if (!field_available) {
  field_txt <- testthat::test_path("..", "..", "data", "field.txt")
  if (!file.exists(field_txt)) {
    stop("找不到 data/field.txt（已尝试：", field_txt, "）")
  }
  assign(
    "field",
    utils::read.table(field_txt, header = TRUE, stringsAsFactors = FALSE),
    envir = globalenv()
  )
}
