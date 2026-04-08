#' 增加名称中的世代数字
#'
#' 当名称末尾已有冒号分隔的世代号时，替换它；否则追加新的世代号。
#'
#' @param names 字符向量，材料名称
#' @param f_vals 数值向量，对应的世代数
#' @param max_gen 整数，最大世代数，超过则不再追加，默认为9
#'
#' @return 字符向量，处理后的名称
#'
#' @examples
#' increment_generation_name(c("冀豆12:3", "冀豆15"), c(3, 4))
#'
#' @export
increment_generation_name <- function(names, f_vals, max_gen = 9) {
  mask <- !is.na(f_vals) & f_vals < max_gen
  has_suffix <- grepl("(:\\d+)$", names, perl = TRUE)

  result <- names
  # 处理已有冒号后缀的情况：替换
  idx_replace <- mask & has_suffix
  result[idx_replace] <- sub(
    "(:\\d+)$",
    paste0(":", f_vals[idx_replace] + 1),
    names[idx_replace],
    perl = TRUE
  )
  # 处理没有冒号后缀的情况：追加
  idx_append <- mask & !has_suffix
  result[idx_append] <- paste0(
    names[idx_append], ":", f_vals[idx_append] + 1
  )

  result
}


#' 对齐数据框字段到field模式
#'
#' 根据field表定义，补齐缺失的字段并按field顺序排列列。
#'
#' @param df 数据框，要对齐的数据
#' @param field_df field数据框，默认使用全局field变量
#' @param table_pattern 字符向量，匹配table列的正则表达式
#'
#' @return 按field顺序排列的数据框
align_to_field_schema <- function(df, field_df = field, table_pattern) {
  field_subset <- subset(field_df, grepl(table_pattern, table, ignore.case = TRUE))
  for (col in as.character(field_subset$name)) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df[as.character(field_subset$name)]
}


#' 向数据框中插入对照行
#'
#' 在每个分组中插入对照行（NA行），用于田间试验设计。
#'
#' @param df 数据框，分组后的数据
#' @param ck 字符向量，对照名称
#'
#' @return 在每个分组末尾追加对照行的数据框
insert_ck_rows <- function(df, ck) {
  lapply(df, function(sub_df) {
    n_insert <- length(ck)
    if (n_insert == 0) return(sub_df)

    # 预先构建所有要插入的行，最后一次性合并
    insert_rows <- lapply(ck, function(iname) {
      mdf <- sub_df[1, , drop = FALSE]
      mdf[] <- NA
      if ("id" %in% names(mdf)) mdf$id <- NA
      if ("stageid" %in% names(mdf)) mdf$stageid <- NA
      if ("name" %in% names(mdf)) mdf$name <- iname
      mdf
    })

    do.call(rbind, c(list(sub_df), insert_rows))
  })
}
