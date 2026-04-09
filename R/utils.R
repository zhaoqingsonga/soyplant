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
  # 保留 field schema 列 + 额外列
  extra_cols <- setdiff(names(df), field_subset$name)
  df[c(as.character(field_subset$name), extra_cols)]
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
  # df 必须是 list（由 split() 产生），传入单个 data.frame 时自动包装
  if (is.data.frame(df)) df <- list(df)
  if (!is.list(df)) return(df)

  n_insert <- length(ck)
  # 零对照或全空字符串对照：直接返回
  if (n_insert == 0 || all(nchar(ck) == 0)) return(df)

  lapply(seq_along(df), function(i) {
    sub_df <- df[[i]]
    ng <- nrow(sub_df)
    if (ng == 0) return(sub_df)

    # 按组轮换选择对应的 ck（循环）
    this_ck <- ck[(i %% n_insert) + 1]
    nres <- ng + 1

    # 预分配：先复制 sub_df 行，再将最后1行覆盖为 NA
    res <- sub_df[seq_len(nres), , drop = FALSE]
    res[seq_len(ng), ] <- sub_df
    res[nres, ] <- NA

    # is_ck：对照行设为1
    is_ck_vec <- integer(nres)
    is_ck_vec[seq_len(ng)] <- 0L
    is_ck_vec[nres] <- 1L
    res$is_ck <- is_ck_vec

    # name 列：该组的 ck 品种名
    if ("name" %in% names(res)) {
      name_vec <- character(nres)
      name_vec[seq_len(ng)] <- as.character(sub_df$name)
      name_vec[nres] <- this_ck
      res$name <- name_vec
    }

    rownames(res) <- NULL

    # 统一列顺序，is_ck 放最后
    all_cols <- c(setdiff(names(res), "is_ck"), "is_ck")
    res[, all_cols, drop = FALSE]
  })
}
