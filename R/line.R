# #只有单株进入株行，单株进入株行时，世代不增加
# get_line <- function(my_plant,start_num=1) {
#   my_line <- subset(my_plant, my_plant$next_stage == "株行")
#   #处理各列
#   my_line$id <- generate_id(start_num, end_num = nrow(my_line)+start_num-1)
#
#   #my_line$source <- my_line$name
#   my_line$stage <- "株行"
#   my_line$next_stage <- "初级产比"
#   my_line$f <- my_line$f + 1
#   my_line$sele <- 5
#   my_line$process <- paste(my_line$process, my_line$id, sep = "/")
#   #
#   rownames(my_line) <- NULL
#   return(my_line)
# }

#' 处理单株进入株行的数据
#'
#' 只有单株进入株行时，世代不增加
#'
#' @param my_plant 数据框，必需含有name,next_stage,f字段
#' @param start_num 整数，起始编号，默认为1
#' @return 数据框，处理后的株行数据
#' @export
get_line <- function(my_plant, start_num = 1) {
  # 选择 next_stage 为 "株行" 的行
  my_line <- subset(my_plant, next_stage == "株行")
  #
  if(is.null(my_line$path)) my_line$path<-my_line$name

  # 生成唯一ID
  my_line$id <- generate_id(start_num, end_num = nrow(my_line) + start_num - 1)

  # 更新字段

  my_line$stage <- "株行"
  my_line$next_stage <- "初级产比"
  my_line$f <- my_line$f + 1
  my_line$sele <- 5
  my_line$process <- paste(my_line$process, my_line$id, sep = "/")

  # 移除行名
  rownames(my_line) <- NULL
  field<-subset(field,grepl("combination", table, ignore.case = TRUE))
  #如果生成表中没有field中所包含的字段则补全
  # 补齐缺失的字段
  for (col in as.character(field$name)) {
    if (!col %in% names(my_line)) {
      my_line[[col]] <- NA
    }
  }




  return(my_line[as.character(field$name)])
}
