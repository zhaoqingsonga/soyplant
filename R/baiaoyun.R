

#' 替换数据框表头名称的函数
#'
#' 该函数依据给定的别名对应表，对原数据框的表头名称进行替换操作。如果原表头名称在别名对应表中有对应的别名，则使用别名替换；
#' 若原表头名称不存在对应别名，则将原表头转换为合法的 `R` 语言字符型变量名形式作为新表头名称。同时，函数会对别名对应表中的因子类型数据进行处理，确保映射关系基于字符型数据。
#'
#' @param original_df 原数据框，需要替换表头名称的数据框对象，类型应为 `data.frame`。
#' @param alias_df 别名对应表，是一个 `data.frame`，包含两列，第一列是原表头名称，第二列是对应的别名，两列的数据类型可能为多种情况，但函数会对其进行处理确保映射关系正确建立。
#'
#' @return 返回表头名称已替换后的原数据框对象，类型仍为 `data.frame`，其列名已根据别名对应表完成相应的更新。
#'
#' @examples
#' # 构造原数据框示例
#' original_data <- data.frame(
#'     col1 = 1:3,
#'     col2 = c("a", "b", "c")
#' )
#'
#' # 构造别名对应表示例
#' alias_table <- data.frame(
#'     original_header = factor(c("col1", "col3")),
#'     alias = factor(c("new_col1", "new_col3"))
#' )
#'
#' # 调用函数进行表头替换
#' result <- replace_headers(original_data, alias_table)
#' print(result)
#'
replace_headers <- function(original_df, alias_df) {
  # 构建映射：使用命名向量处理因子类型
  header_mapping <- setNames(as.character(alias_df[[2]]), as.character(alias_df[[1]]))

  # 向量化替换：直接在命名向量中查找
  orig_names <- names(original_df)
  new_names <- ifelse(orig_names %in% names(header_mapping),
                      header_mapping[orig_names],
                      make.names(orig_names, unique = TRUE))
  names(original_df) <- new_names
  return(original_df)
}




#' 根据唯一编号和fieldid字段更新数据框
#'
#' 该函数通过匹配第一个数据框（`reference_df`）中的“唯一编号”字段与第二个数据框（`target_df`）中的“fieldid”字段，
#' 将`reference_df`中对应行的相关列值更新到`target_df`中相应列上，并且在更新时会判断数据类型，若更新内容为数字则以数字类型存储。
#'
#' @param reference_df 包含参考信息的数据框，其中必须包含名为“唯一编号”的列，此列用于与`target_df`中的“fieldid”列进行匹配。
#' @param target_df 要更新的数据框，必须包含名为“fieldid”的列，此列用于与`reference_df`中的“唯一编号”列进行匹配。
#'
#' @return 更新后的`target_df`数据框，其中相应列已按照匹配规则更新了值，数字类型的数据会以数字类型存储。
#'
#' @examples
#' # 示例数据框（你可以替换为真实的数据框）
#' df1 <- data.frame(
#'     唯一编号 = c(1, 2, 3),
#'     value1 = c("a", "b", "c"),
#'     value2 = c(10, 20, 30)
#' )
#' df2 <- data.frame(
#'     fieldid = c(1, 2, 3),
#'     col1 = c("x", "y", "z"),
#'     col2 = c(5, 6, 7)
#' )
#' updated_df <- update_dataframe_by_fieldid(df1, df2)
#' print(updated_df)
#'
update_dataframe_by_fieldid <- function(reference_df, target_df) {
  # 检查reference_df中是否存在'唯一编号'列
  if (!("唯一编号" %in% names(reference_df))) {
    stop("reference_df does not have the '唯一编号' column.")
  }
  # 检查target_df中是否存在'fieldid'列
  if (!("fieldid" %in% names(target_df))) {
    stop("target_df does not have the 'fieldid' column.")
  }

  # 获取reference_df中除'唯一编号'外用于更新的列名（可动态获取不确定数量的列）
  update_cols_reference_df <- setdiff(names(reference_df), "唯一编号")
  # 获取target_df中原本就有的列名（去除'fieldid'字段）
  target_cols <- setdiff(names(target_df), "fieldid")

  # 找出在update_cols_reference_df中同时也在target_cols里且字段名完全一致的列名
  cols_to_update <- intersect(update_cols_reference_df, target_cols)

  # 向量化查找：建立 ID -> reference_df行索引 的映射
  ref_id_map <- setNames(seq_len(nrow(reference_df)), as.character(reference_df[["唯一编号"]]))
  target_ids <- as.character(target_df[["fieldid"]])
  row_match <- ref_id_map[target_ids]

  # 批量更新每个列
  for (col_name in cols_to_update) {
    if (col_name %in% names(reference_df) && col_name %in% names(target_df)) {
      valid_match <- !is.na(row_match)
      ref_vals <- reference_df[[col_name]][row_match[valid_match]]
      if (is.numeric(reference_df[[col_name]])) {
        target_df[valid_match, col_name] <- as.numeric(ref_vals)
      } else {
        target_df[valid_match, col_name] <- ref_vals
      }
    }
  }

  return(target_df)
}

#' 将数据框中仅含数字或空值的列转换为数字类型的函数
#'
#' 此函数接受一个数据框作为输入，遍历其每一列，对于那些元素仅包含数字、`NA` 值或空字符串的列，
#' 将其转换为数字类型（`numeric`），方便后续进行数值相关的计算与分析。
#'
#' @param input_df 输入的数据框，需要对其中符合条件的列进行类型转换的数据框对象，类型应为 `data.frame`。
#'
#' @return 返回处理后的新数据框，原数据框中仅含数字或空值的列已被转换为数字类型，其他列保持不变，类型仍为 `data.frame`。
#'
#' @examples
#' # 构造示例数据框，包含不同情况的列
#' df <- data.frame(
#'     col1 = c("1", "2", "3"),
#'     col2 = c("a", "b", "c"),
#'     col3 = c(NA, "", "4"),
#'     col4 = c("5", "6x", "7")
#' )
#'
#' # 调用函数进行类型转换
#' result_df <- convert_string_numbers_to_numeric(input_df = df)
#' print(result_df)
#'
convert_string_numbers_to_numeric <- function(input_df) {
  # 向量化版本：遍历每一列
  for (col_name in names(input_df)) {
    # 只处理字符型列
    if (is.character(input_df[[col_name]])) {
      # 提取非空非NA的元素
      vals <- input_df[[col_name]]
      non_na_mask <- !is.na(vals) & vals != ""
      # 判断是否所有非空元素都能转换为数字
      if (any(non_na_mask)) {
        non_empty <- vals[non_na_mask]
        can_convert <- !is.na(suppressWarnings(as.numeric(non_empty)))
        if (all(can_convert)) {
          input_df[[col_name]] <- as.numeric(vals)
        }
      }
    }
  }
  return(input_df)
}





#' 更新百奥云数据集中的性状信息
#'
#' 根据提供的更新数据集 `updf` 更新主数据集 `df` 中的性状信息。
#'
#' @param df 主数据集，包含需要更新的性状信息。必须包含唯一编号列和性状列。
#' @param updf 更新数据集，包含需要更新的性状信息以及唯一编号。列名应包含字段ID和性状信息。
#' @param overwrite 逻辑值，是否覆盖主数据集中数据，TRUE覆盖，FALSE不覆盖。
#'
#' @return 更新后的主数据集 `df`。
#' @details
#' 函数的工作流程如下：
#' \itemize{
#'   \item 找到主数据集中唯一编号的列。
#'   \item 遍历更新数据集中的每个性状，找到对应的中文名称，并在主数据集中找到对应列。
#'   \item 根据唯一编号，将更新数据集中的性状信息更新到主数据集中。
#' }
#'
#' @examples
#' df <- data.frame("唯一编号" = c(1, 2, 3), "性状1" = c(NA, 2, 3))
#' updf <- data.frame("fieldid" = c(1, 2), "性状1" = c(10, NA))
#' updated_df <- update_baiaoyun(df, updf)
#' print(updated_df)
update_baiaoyun_old <- function(df, updf,overwrite=TRUE) {
  # 找到主数据集中唯一编号的列
  col.ID<-which(df=="唯一编号",arr.ind = TRUE)[2]

  if (length(col.ID) == 0) {
    stop("主数据集 df 必须包含 '唯一编号' 列")
  }

  # 获取更新数据集中需要更新的性状列
  update_traits <- setdiff(colnames(updf), "fieldid")

  # 遍历每个需要更新的性状
  for (trait in update_traits) {
    #要更新的性状中不含NA值
    updf_i<-subset(updf,!is.na(updf[,trait]))
    # 获取更新数据集中的性状列索引
    col.trait_updf_i <- which(colnames(updf_i) == trait)

    # 将性状名称翻译为中文名称
    #只能有一个返回值
    trait_ZH <- as.character(baiaoyun_traits$性状名称TRAIT_NAME[baiaoyun_traits$名称缩写ABBR_NAME == trait&!is.na(baiaoyun_traits$名称缩写ABBR_NAME)])

    # 找到主数据集中对应的性状列索引
    col.trait_df<-which(df==trait_ZH,arr.ind = TRUE)[2]
    # 找不到则下一个循环（不能更新df中没有的字段）
    if(is.na(col.trait_df))  next
    # 根据唯一编号更新主数据集
    update_value<-NA
    for (i in seq_len(nrow(updf_i))) {
      field_id <- updf_i$fieldid[i]
      update_value <- updf_i[i, col.trait_updf_i]
      row_idx <- which(df[, col.ID] == field_id)
      if (length(row_idx) > 0) {
        #处理备注信息,备注信息为添加不覆盖
        if(trait%in%c("tianjianbeizhu","zhizhubeizhu","zilibeizhu")){
          #判断如果为NA则不进行合并粘贴
          if(is.na(df[row_idx, col.trait_df])){
            df[row_idx, col.trait_df] <- paste(update_value,",",Sys.Date(),".",sep="")
          }
          else{
            df[row_idx, col.trait_df] <- paste(df[row_idx, col.trait_df],update_value,",",Sys.Date(),".",sep="")
          }

          next
        }

        #是否覆盖
        if(overwrite){
          #覆盖
          df[row_idx, col.trait_df] <- update_value
        }else{
          #不覆盖
          if(is.na(df[row_idx, col.trait_df]))  df[row_idx, col.trait_df] <- update_value
        }

      }
    }
  }

  return(df)
}



update_baiaoyun <- function(df, updf, overwrite = TRUE) {
  # 确保更新的字段在df中全有
  updfnames <- names(updf)[-1]
  if (!all(updfnames %in% df[2, 8:ncol(df)])) {
    needadd <- updfnames[!updfnames %in% df[2, 8:ncol(df)]]
    return(paste("error,在下载的文件中添加", needadd, sep = ""))
  }
  # 只保留有更新的字段
  df <- cbind(df[1:7], df[df[2, ] %in% updfnames])

  # 找到主数据集中唯一编号的列
  col.ID <- which(df == "唯一编号", arr.ind = TRUE)[2]

  if (length(col.ID) == 0) {
    stop("主数据集 df 必须包含 '唯一编号' 列")
  }

  # 创建fieldid到行索引的映射（避免循环内O(n)查找）
  id_to_row_idx <- setNames(seq_len(nrow(df)), df[, col.ID])

  # 获取更新数据集中需要更新的性状列
  update_traits <- setdiff(colnames(updf), "fieldid")

  # 遍历每个需要更新的性状
  for (trait in update_traits) {
    # 要更新的性状中不含NA值
    updf_i <- subset(updf, !is.na(updf[, trait]))
    # 获取更新数据集中的性状列索引
    col.trait_updf_i <- which(colnames(updf_i) == trait)
    # 找到主数据集中对应的性状列索引
    col.trait_df <- which(df == trait, arr.ind = TRUE)[2]
    # 找不到则下一个循环（不能更新df中没有的字段）
    if (is.na(col.trait_df)) next

    # 备注性状（添加不覆盖）
    is_memo_trait <- trait %in% c("T080", "T093", "T101")

    # 批量处理：获取所有需要更新的fieldid
    field_ids <- updf_i$fieldid
    # 查找对应的行索引（向量化的lookup）
    row_indices <- id_to_row_idx[as.character(field_ids)]
    valid_idx <- !is.na(row_indices)

    if (any(valid_idx)) {
      if (is_memo_trait) {
        # 备注信息：追加不覆盖
        for (j in which(valid_idx)) {
          row_idx <- row_indices[j]
          update_value <- updf_i[j, col.trait_updf_i]
          if (is.na(df[row_idx, col.trait_df])) {
            df[row_idx, col.trait_df] <- paste(update_value, ",", Sys.Date(), ".", sep = "")
          } else {
            df[row_idx, col.trait_df] <- paste(df[row_idx, col.trait_df], update_value, ",", Sys.Date(), ".", sep = "")
          }
        }
      } else {
        if (overwrite) {
          # 覆盖模式：向量化赋值
          df[row_indices[valid_idx], col.trait_df] <- updf_i[valid_idx, col.trait_updf_i]
        } else {
          # 不覆盖模式：只更新NA值，向量化
          target_rows <- row_indices[valid_idx]
          na_mask <- is.na(df[target_rows, col.trait_df])
          df[target_rows[na_mask], col.trait_df] <- updf_i[valid_idx, col.trait_updf_i][na_mask]
        }
      }
    }
  }

  return(df)
}









