

# get_primary <- function(my_line,
#                         start_num = 1) {
#   my_primary <- subset(my_line, my_line$next_stage == "初级产比")
#   #处理各列
#   my_primary$id <-
#     generate_id(start_num,
#            end_num = nrow(my_primary) +start_num - 1)
#
#   my_primary$source <- my_primary$name
#   #处理名称,小于9代时处理，大于9代时不再追加
#   my_primary$name <-
#     ifelse(my_primary$f<9,paste(my_primary$name, ":", (my_primary$f + 1),  sep = ""),my_primary$name)
#   #
#   my_primary$stage <- "初级产比"
#   my_primary$next_stage <- "高级产比"
#   my_primary$f <- my_primary$f + 1
#   my_primary$sele <- 5
#   my_primary$process <-
#     paste(my_primary$process, my_primary$id, sep = "/")
#   my_primary$path <- paste(my_primary$path, 0, sep = "-")
#   #
#   rownames(my_primary) <- NULL
#   return(my_primary)
# }


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

  # 记录来源名称,和前fieldid
  my_primary$source <- my_primary$name
  my_primary$former_fieldid <- my_primary$fieldid
  # 处理名称，小于9代时处理，大于9代时不再追加
  my_primary$name <- ifelse(!is.null(my_primary$f)&my_primary$f < 9,
                            paste(my_primary$name, ":", (my_primary$f + 1), sep = ""),
                            my_primary$name)

  # 更新字段
  my_primary$stage <- "初级产比"
  my_primary$next_stage <- "高级产比"
  my_primary$f <- my_primary$f + 1
  my_primary$sele <- 5
  my_primary$process <- paste(my_primary$process, my_primary$id, sep = "/")
  my_primary$path <- paste(my_primary$path, 0, sep = "-")

  # 移除行名
  rownames(my_primary) <- NULL
  field<-subset(field,grepl("combination", table, ignore.case = TRUE))
  #如果生成表中没有field中所包含的字段则补全
  # 补齐缺失的字段
  for (col in as.character(field$name)) {
    if (!col %in% names(my_primary)) {
      my_primary[[col]] <- NA
    }
  }

  return(my_primary[as.character(field$name)])
}





