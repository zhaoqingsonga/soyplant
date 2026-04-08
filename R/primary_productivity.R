

#' 处理株行进入初级产比的数据
#'从杂交组合中或群体中升级群体,数据框中必有字段：name,next_stage,f
#'
#' @param my_line 数据框，包含株行信息
#' @param start_num 整数，起始编号，默认为1
#' @return 数据框，处理后的初级产比数据
#' @export
get_primary <- function(my_line, start_num = 1) {
  # 选择 next_stage 为 "初级产比" 的行
  my_primary <- subset(my_line, next_stage == "初级产比")

  # 生成唯一ID
  my_primary$id <- generate_id(start_num, end_num = nrow(my_primary) + start_num - 1)

  # 记录来源名称,和前fieldid和前stageid
  my_primary$source <- my_primary$name
  my_primary$former_fieldid <- my_primary$fieldid
  my_primary$former_stageid <- my_primary$stageid

  # 处理名称，小于9代时追加世代号
  my_primary$name <- increment_generation_name(my_primary$name, my_primary$f)

  # 更新字段
  my_primary$stage <- "初级产比"
  my_primary$next_stage <- "高级产比"
  my_primary$f <- my_primary$f + 1
  my_primary$sele <- 5
  my_primary$process <- paste(my_primary$process, my_primary$id, sep = "/")
  my_primary$path <- paste(my_primary$path, 0, sep = "-")

  # 移除行名
  rownames(my_primary) <- NULL

  # 对齐到field模式
  align_to_field_schema(my_primary, table_pattern = "combination")
}





