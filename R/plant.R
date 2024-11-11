# get_plant <- function(my_pop,start_num=1) {
#   my_pop<-subset(my_pop,my_pop$sele>0)
#   new_df <- data.frame()
#   # 循环遍历每一行，并按给定的次数复制
#   for (i in 1:nrow(my_pop)) {
#     # 使用rbind函数将复制的行添加到新的数据框中
#     pop_name <- my_pop[i, ]$name
#     path_name <- my_pop[i, ]$path
#     source_name <- my_pop[i, ]$name#要考虑一下有没有必要
#     for (j in 1:my_pop$sele[i]) {
#       my_pop[i, ]$name <- paste(pop_name, j, sep = "-")
#       my_pop[i, ]$path <- paste(path_name, j, sep = "-")
#       my_pop[i, ]$source <- source_name
#       new_df <- rbind(new_df, my_pop[i, ])
#     }
#   }
#   # 重置行索引
#   rownames(new_df) <- NULL
#   #处理各列
#   new_df$id <- generate_id(start_num, end_num = nrow(new_df)+start_num-1)
#   new_df$stage <- "单株"
#
#   new_df$next_stage <- "株行"
#   new_df$process <- paste(new_df$process, new_df$id, sep = "/")
#   new_df$sele<-NA
#   return(new_df)
# }

# my_plant <- get_plant(my_pop)
# my_plant



#' 升级单株
#'
#' 从群体数据中升级单株,会增加必需字段。
#' @param my_pop 数据框，必须包括name和sele和f(用于升级株行用)两个字段，sele为选择单株数,可过滤非数字
#' @param start_num 起始编号，默认为 1
#' @return 升级为单株的记录数据框
#@examples
#get_plant(my_pop)

get_plant <- function(my_pop, start_num = 1) {
  #如果sele中有非数字部分则去除掉

  clean_and_convert_to_numbers <- function(vector) {
    # 使用正则表达式检查元素是否仅包含数字
    cleaned_vector <- ifelse(grepl("^[0-9]+$", vector), as.numeric(vector), NA)
    return(cleaned_vector)
  }
  my_pop$sele<-clean_and_convert_to_numbers(my_pop$sele)

  my_pop <- subset(my_pop, sele > 0)
  if(is.null(my_pop$path)) my_pop$path<-my_pop$name
  new_df <- do.call(rbind, lapply(1:nrow(my_pop), function(i) {
    pop_name <- my_pop$name[i]
    path_name <- my_pop$path[i]
    source_name <- my_pop$name[i]
    do.call(rbind, lapply(1:my_pop$sele[i], function(j) {
      temp <- my_pop[i, ]
      temp$name <- paste(pop_name, j, sep = "-")
      temp$path <- paste(path_name, j, sep = "-")
      temp$source <- source_name
      temp
    }))
  }))

  new_df$id <- generate_id(start_num, end_num = nrow(new_df) + start_num - 1)
  new_df$stage <- "单株"
  new_df$next_stage <- "株行"
  new_df$process <- paste(new_df$process, new_df$id, sep = "/")
  new_df$sele <- NA
  rownames(new_df) <- NULL
  field<-subset(field,grepl("combination", table, ignore.case = TRUE))
  #如果生成表中没有field中所包含的字段则补全
  # 补齐缺失的字段
  for (col in as.character(field$name)) {
    if (!col %in% names(new_df)) {
      new_df[[col]] <- NA
    }
  }
  return(new_df[as.character(field$name)])
}

# 示例用法：
# my_plant <- get_plant(my_pop)
# print(my_plant)

