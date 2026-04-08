
#' 升级单株
#'
#' 从群体数据中升级单株,会增加必需字段。
#' @param my_pop 数据框，必须包括name和sele和f(用于升级株行用)两个字段，sele为选择单株数,可过滤非数字
#' @param start_num 起始编号，默认为 1
#' @return 升级为单株的记录数据框
#@examples
#get_plant(my_pop)

get_plant <- function(my_pop, start_num = 1) {
  # 如果sele中有非数字部分则去除掉
  clean_and_convert_to_numbers <- function(vector) {
    cleaned_vector <- ifelse(grepl("^[0-9]+$", vector), as.numeric(vector), NA)
    return(cleaned_vector)
  }
  my_pop$sele <- clean_and_convert_to_numbers(my_pop$sele)
  my_pop <- subset(my_pop, !is.na(sele) & sele > 0)

  if (is.null(my_pop$path)) my_pop$path <- my_pop$name

  # 记录原始字段用于后续更新
  original_names <- my_pop$name
  original_paths <- my_pop$path
  original_fieldid <- my_pop$fieldid
  original_stageid <- my_pop$stageid

  # 使用 expand_rows 进行行扩展
  new_df <- expand_rows(my_pop, "sele", "seq_id")

  # 获取每行对应的原始数据行索引（expand_rows内部使用的索引）
  orig_idx <- rep(seq_len(nrow(my_pop)), my_pop$sele)

  # 更新 name, path, source
  new_df$name <- paste(original_names[orig_idx], new_df$seq_id, sep = "-")
  new_df$path <- paste(original_paths[orig_idx], new_df$seq_id, sep = "-")
  new_df$source <- original_names[orig_idx]
  new_df$former_fieldid <- original_fieldid[orig_idx]
  new_df$former_stageid <- original_stageid[orig_idx]
  new_df$seq_id <- NULL  # 移除辅助列

  new_df$id <- generate_id(start_num, end_num = nrow(new_df) + start_num - 1)
  new_df$stage <- "单株"
  new_df$next_stage <- "株行"
  new_df$process <- paste(new_df$process, new_df$id, sep = "/")
  new_df$sele <- NA
  rownames(new_df) <- NULL

  # 将fieldid和stageid置空
  new_df$fieldid <- NA
  new_df$stageid <- NA

  # 对齐到field模式
  align_to_field_schema(new_df, table_pattern = "combination")
}

# 示例用法：
# my_plant <- get_plant(my_pop)
# print(my_plant)

