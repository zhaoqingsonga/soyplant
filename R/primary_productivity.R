

#' 处理株行晋级到下一阶段的数据
#'从杂交组合中或群体中升级群体,数据框中必有字段：name,next_stage,f
#'
#' @param my_line 数据框，包含株行信息
#' @param start_num 整数，起始编号，默认为1
#' @param next_stage 字符，下一阶段名称，同时也是输入行的过滤条件，默认为"初级产比"
#' @param target_stage 字符，晋级后的阶段名称，默认为"高级产比"
#' @return 数据框，处理后的晋级数据
#' @export
get_primary <- function(my_line,
                        start_num = 1,
                        next_stage = "初级产比",
                        target_stage = "高级产比") {
  # 选择 next_stage 为指定值的行
  my_primary <- subset(my_line, next_stage == next_stage)

  if (nrow(my_primary) == 0) {
    stop("get_primary: 没有找到 next_stage='", next_stage, "' 的行，请检查输入数据")
  }

  # 生成唯一ID
  my_primary$id <- generate_id(start_num, end_num = nrow(my_primary) + start_num - 1)

  # 记录来源名称,和前fieldid和前stageid
  my_primary$source <- my_primary$name
  my_primary$former_fieldid <- my_primary$fieldid
  my_primary$former_stageid <- my_primary$stageid

  # 处理名称，小于9代时追加世代号
  my_primary$name <- increment_generation_name(my_primary$name, my_primary$f)

  # 更新字段
  my_primary$stage <- next_stage
  my_primary$next_stage <- target_stage
  my_primary$f <- my_primary$f + 1
  my_primary$sele <- 5
  my_primary$process <- paste(my_primary$process, my_primary$id, sep = "/")
  my_primary$path <- paste(my_primary$path, 0, sep = "-")

  # 移除行名
  rownames(my_primary) <- NULL

  # 对齐到field模式
  my_primary <- align_to_field_schema(my_primary, table_pattern = "combination")
}





