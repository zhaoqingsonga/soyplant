# get_advanced <- function(my_primary,start_num=1) {
#   my_advanced <- subset(my_primary, my_primary$next_stage == "高级产比")
#   #处理各列
#   my_advanced$id <- generate_id(start_num,  end_num = nrow(my_advanced)+start_num-1)
#   my_advanced$source <- my_advanced$name
#   #处理名称小于9代时处理，大于9代时不再追加
#   my_advanced$name <-
#     ifelse(my_advanced$f<9,paste(my_advanced$name, ":", (my_advanced$f + 1),  sep = ""),my_advanced$name)
#
#
#   my_advanced$stage <- "高级产比"
#   my_advanced$next_stage <- "多点鉴定"
#   my_advanced$f <- my_advanced$f + 1
#   my_advanced$sele <- 5
#   my_advanced$process <-
#     paste(my_advanced$process, my_advanced$id, sep = "/")
#   my_advanced$path <- paste(my_advanced$path, 0, sep = "-")
#   #
#   rownames(my_advanced) <- NULL
#   return(my_advanced)
# }


#' 处理初级产比进入高级产比的数据
#'
#' 只有初级产比进入高级产比时，世代不增加
#'
#' @param my_primary 数据框，包含初级产比信息
#' @param start_num 整数，起始编号，默认为1
#' @return 数据框，处理后的高级产比数据
#' @export
get_advanced <- function(my_primary, start_num = 1) {
  # 选择 next_stage 为 "高级产比" 的行
  my_advanced <- subset(my_primary, next_stage == "高级产比")

  # 生成唯一ID
  my_advanced$id <- generate_id(start_num, end_num = nrow(my_advanced) + start_num - 1)

  # 记录来源名称
  my_advanced$source <- my_advanced$name

  # 处理名称，小于9代时处理，大于9代时不再追加
  my_advanced$name <- ifelse(my_advanced$f < 9,
                             paste(my_advanced$name, ":", (my_advanced$f + 1), sep = ""),
                             my_advanced$name)

  # 更新字段
  my_advanced$stage <- "高级产比"
  my_advanced$next_stage <- "多点鉴定"
  my_advanced$f <- my_advanced$f + 1
  my_advanced$sele <- 5
  my_advanced$process <- paste(my_advanced$process, my_advanced$id, sep = "/")
  my_advanced$path <- paste(my_advanced$path, 0, sep = "-")

  # 移除行名
  rownames(my_advanced) <- NULL

  return(my_advanced)
}
