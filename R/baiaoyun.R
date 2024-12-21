

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
  # 创建一个空的字符向量用于存储新的表头名称
  new_headers <- character(length = ncol(original_df))
  # 构建一个从原表头名称到别名的映射列表，在此处判断并处理因子类型，确保为字符型映射
  header_mapping <- setNames(lapply(alias_df[, 2], function(x) {
    if (is.factor(x)) {
      as.character(x)
    } else {
      x
    }
  }), lapply(alias_df[, 1], function(x) {
    if (is.factor(x)) {
      as.character(x)
    } else {
      x
    }
  }))
  # 遍历原数据框的表头
  for (i in seq_along(names(original_df))) {
    header <- names(original_df)[i]
    # 根据精确的原表头名称查找对应的别名，如果找到则使用别名，否则使用原表头转换为合法的字符型变量名形式
    if (header %in% names(header_mapping)) {
      new_headers[i] <- header_mapping[header]
    } else {
      new_headers[i] <- make.names(header, unique = TRUE)
    }
  }
  # 使用新的表头名称重新命名原数据框的列
  names(original_df) <- new_headers
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

  # 获取target_df和reference_df的行数
  n_reference <- nrow(reference_df)
  n_target <- nrow(target_df)

  # 循环遍历target_df的每一行
  for (i in 1:n_target) {
    # 获取target_df当前行的fieldid值
    current_id <- target_df[i, "fieldid"]
    # 在reference_df中查找匹配的'唯一编号'行
    matching_rows <- which(reference_df[, "唯一编号"] == current_id)
    if (length(matching_rows) > 0) {
      # 如果找到匹配行，遍历要更新的列，判断参考数据框对应列的数据类型，若是数字则以数字类型更新
      for (col_name in cols_to_update) {
        if (col_name %in% names(reference_df) && col_name %in% names(target_df)) {
          if (is.numeric(reference_df[[col_name]])) {
            target_df[i, col_name] <- as.numeric(reference_df[matching_rows[1], col_name])
          } else {
            target_df[i, col_name] <- reference_df[matching_rows[1], col_name]
          }
        }
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
  # 获取输入数据框的列数
  num_cols <- ncol(input_df)
  # 遍历每一列
  for (col_index in 1:num_cols) {
    # 获取当前列名
    col_name = names(input_df)[col_index]
    # 判断当前列的数据类型是否为字符型
    if (is.character(input_df[[col_name]])) {
      # 提取当前列非空值的元素
      non_na_elements <- input_df[[col_name]][!is.na(input_df[[col_name]])]
      # 去除非空值元素中的空字符串（如果有）
      non_empty_elements <- non_na_elements[non_na_elements!= ""]
      # 判断去除空值和空字符串后剩余的元素是否都能转换为数字
      if (all(sapply(non_empty_elements, function(x) {
        suppressWarnings(!is.na(as.numeric(x)))
      }))) {
        # 如果都能转换为数字，则将原列转换为数字类型
        input_df[[col_name]] <- as.numeric(input_df[[col_name]])
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



update_baiaoyun <- function(df, updf,overwrite=TRUE) {
  #确保更新的字段在df中全有
  updfnames<-names(updf)[-1]
  if(!all(updfnames%in%df[2,8:ncol(df)])){
    needadd<- updfnames[!updfnames%in%df[2,8:ncol(df)]]
    return(paste("error,在下载的文件中添加",needadd,sep=""))
  }
  #只保留有更新的字段
  df<-cbind(df[1:7],df[df[2,]%in%updfnames])

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
    # 找到主数据集中对应的性状列索引
    col.trait_df<-which(df==trait,arr.ind = TRUE)[2]
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
        if(trait%in%c("T080","T093","T101")){
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









