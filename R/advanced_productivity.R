#' @deprecated 请使用 `get_primary()` 替代，此函数仅作向后兼容保留。
#'
#' 处理晋级到高级产比阶段的数据
#'
#' @param my_primary 数据框，包含晋级前阶段信息
#' @param start_num 整数，起始编号，默认为1
#' @param next_stage 字符，下一阶段名称，同时也是输入行的过滤条件，默认为"高级产比"
#' @param target_stage 字符，晋级后的阶段名称，默认为"多点鉴定"
#' @return 数据框，处理后的晋级数据
#' @export
get_advanced <- function(my_primary,
                         start_num = 1,
                         next_stage = "高级产比",
                         target_stage = "多点鉴定") {
  # 选择 next_stage 为指定值的行
  my_advanced <- subset(my_primary, next_stage == next_stage)

  # 生成唯一ID
  my_advanced$id <- generate_id(start_num, end_num = nrow(my_advanced) + start_num - 1)

  # 记录来源名称和前fieldid,前stageid
  my_advanced$source <- my_advanced$name
  my_advanced$former_fieldid <- my_advanced$fieldid
  my_advanced$former_stageid <- my_advanced$stageid

  # 处理名称，小于9代时追加世代号
  my_advanced$name <- increment_generation_name(my_advanced$name, my_advanced$f)

  # 更新字段
  my_advanced$stage <- next_stage
  my_advanced$next_stage <- target_stage
  my_advanced$f <- my_advanced$f + 1
  my_advanced$sele <- 5
  my_advanced$process <- paste(my_advanced$process, my_advanced$id, sep = "/")
  my_advanced$path <- paste(my_advanced$path, 0, sep = "-")

  # 移除行名
  rownames(my_advanced) <- NULL

  # 对齐到field模式
  my_advanced <- align_to_field_schema(my_advanced, table_pattern = "combination")
}
