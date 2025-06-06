###
#
#' 杂交组合
#'
#' 根据父母本配制杂交组合
#' @param ma 向量，母本列表.
#' @param pa 向量，父本列表
#' @param memo 字符串， 备注
#' @return 两组亲本所配制的组合
#' @examples
#' combination(ma=c("冀豆12", "冀豆17"),pa=c("中联豆6001", "中联豆6024", "中联豆6033"),memo = "转基因")
#' combination(ma=c("冀豆12", "冀豆17"),pa=c("中联豆6001", "中联豆6024", "中联豆6033"),memo = NA)
combination <- function(ma = c("JD12", "JD17"),
                        pa = c("ZLD6001", "ZLD6024", "ZLD6033"),
                        memo = NA) {
  all_combinations <- expand.grid(ma, pa)
  names(all_combinations) <- c("ma", "pa")
  all_combinations$mapa <-
   combine_slash_connector(all_combinations$ma, all_combinations$pa, sep = "/")
   #paste(all_combinations$ma, all_combinations$pa, sep = "/")
  all_combinations$memo <- memo
  return(all_combinations)
}

#'文件中杂交组合
#' @param mydata 数据框，里面包括3列，顺序为为母本，父本和备注，其它列无要求
#' @param only 逻辑值，是否去除重复组合
#' @param order 是否配制的组合进行排序
#' @return 返回从文件中配制的杂交合组
combination_from_dataframe <- function(mydata,
                                 only = TRUE,
                                 order = FALSE) {
  # library(openxlsx)
  # mydata <- read.xlsx(filename, 1, colNames = TRUE)

  mylist <- list()
  for (i in 1:nrow(mydata)) {
    mylist[[i]] <-
      combination(ma = mydata[i, 1],
                  pa = mydata[i, 2],
                  memo = mydata[i, 3])
  }
  #extra_params<-list(prefix = prefix, only = only,order = order)
  #re_v<-do.call(combi_bind, c(mylist,extra_params))
  re_v <- do.call(rbind, mylist)
  if (only)
    re_v <- re_v[!duplicated(re_v$mapa),]
  if (order) {
    re_v <- re_v[order(re_v$mapa),]
  }
  rownames(re_v) <- NULL
  return(re_v)
}


#' 合并母本和父本字符串，用重复的连接符连接
#'
#' 此函数用于将母本和父本字符串对逐一合并。连接符（默认为 "/"）的重复次数，
#' 是两者中出现最多的连续连接符数量加一。例如：
#' `"冀豆12/冀豆15//冀豆16"` 和 `"冀豆21/冀豆23"` 中最多连续为 `//`，
#' 则合并结果使用 `///` 连接。
#'
#' @param mother_vec 字符向量，表示母本名称。
#' @param father_vec 字符向量，表示父本名称。
#' @param sep 字符串，用作连接符。默认值为 `"/"`，可以设定为其他字符（如 `"-"`）。
#'
#' @return 字符向量，每个元素是母本和父本通过计算后的连接符拼接的结果。
#'
#' @examples
#' combine_slash_connector("冀豆12/冀豆17//冀豆19", "2232/788")
#' # 返回: "冀豆12/冀豆17//冀豆19///2232/788"
#'
#' combine_slash_connector("A-B--C", "D-E", sep = "-")
#' # 返回: "A-B--C---D-E"
#'
#' @export
combine_slash_connector <- function(mother_vec, father_vec, sep = "/") {
  # 验证连接符为单个非空字符
  if (!is.character(sep) || length(sep) != 1 || nchar(sep) == 0) {
    stop("`sep` 必须是一个非空的单个字符字符串。")
  }

  # 辅助函数：统计某字符串中连续 sep 的最大长度
  count_max_sep <- function(text) {
    pattern <- paste0("(", sep, "+)")
    matches <- gregexpr(pattern, text, perl = TRUE)
    matched <- regmatches(text, matches)
    if (length(matched[[1]]) == 0) return(0)
    max(nchar(matched[[1]]))
  }

  # 对母本与父本字符串对逐一处理
  result <- mapply(function(mom, dad) {
    max_mom <- count_max_sep(mom)
    max_dad <- count_max_sep(dad)

    connector <- strrep(sep, max(max_mom, max_dad) + 1)
    paste0(mom, connector, dad)
  }, mother_vec, father_vec, USE.NAMES = FALSE)

  return(result)
}





#' 获得组合列表
#'
#' 根据父母本获得组合列表
#' @param mydata 数据框，里面包括3列，顺序为为母本，父本和备注，其它列无要求
#' @param prefix 组合前缀
#' @param startN 起始编号
#' @param only 逻辑值 是否去重
#' @param order 逻辑值，是否按亲本排序
#' @return 返回杂交表

get_combination <- function(mydata,
                            prefix = "ZJ",
                            startN = 1,
                            only = TRUE,
                            order = FALSE)
{
  mapa <- combination_from_dataframe(mydata)
  my_len <- length(mapa$mapa)
  user <- get_computer_nodename()
  #
  name_path <-
   generate_stageid(start_num = startN,end_num = my_len + startN - 1,char = prefix,digit_length = 3 )#合并时注意，要重新生成
  name <- paste(name_path, "F0", sep = "")#合并时注意，要重新生成

  id <- generate_id(start_num = 1, end_num = my_len)
  f <- rep(0, my_len)
  re_v <- data.frame(
    fieldid=NA,
    id = id,
    user = rep(user, my_len),
    stageid = NA,
    name = name,
    f = f
  )
  re_v <- cbind(re_v, mapa)
  re_v$stage <- "杂交"
  re_v$next_stage <- "群体"
  re_v$process <- id#合并时要重新生成
  re_v$path <- name_path#合并时要重新生成
  re_v$sele<-0
  re_v$source<-NA
  re_v$former_fieldid<-NA
  field<-subset(field,grepl("combination", table, ignore.case = TRUE))
  #如果生成表中没有field中所包含的字段则补全
  # 补齐缺失的字段
  for (col in as.character(field$name)) {
    if (!col %in% names(re_v)) {
      re_v[[col]] <- NA
    }
  }

  re_v<-re_v[as.character(field$name)]
  return(re_v)
}
##





#'  获得组合矩阵
#'
#' @param my_combi data.frame, 里面要有三列数据:分别为 ma, pa, name
#' @return 返回组合矩阵
#' @examples
#' combination_matrix(data.frame(ma=c("JD12","JD17","JD32"),pa=c("ZLD6001","ZLD6003","ZLD6024"),name=c("ZJ001","ZJ002","ZJ003")))

#组合矩阵
combination_matrix <- function(my_combi) {
  ma <- my_combi$ma[!duplicated(my_combi$ma)]
  pa <- my_combi$pa[!duplicated(my_combi$pa)]
  mapamatri <-
    matrix(rep(NA, (length(ma) * length(pa))),
           nrow = length(ma),
           ncol = length(pa),
           byrow = TRUE)
  rownames(mapamatri) <- ma
  colnames(mapamatri) <- pa

  for (i in 1:nrow(my_combi)) {
    if (is.na(mapamatri[my_combi$ma[i], my_combi$pa[i]])) {
      mapamatri[my_combi$ma[i], my_combi$pa[i]] <- my_combi$name[i]
    } else{
      mapamatri[my_combi$ma[i], my_combi$pa[i]] <-
        paste(mapamatri[my_combi$ma[i], my_combi$pa[i]], my_combi$name[i], sep =
                "/")
    }
  }
  mapamatri <- as.data.frame(mapamatri)
  mapamatri <- cbind(data.frame(母本 = ma), mapamatri)
  rownames(mapamatri) <- NULL
  return(mapamatri)
}



#' 将矩阵格式转换为配对数据框
#'
#' 该函数处理包含遗传育种信息的方阵数据，提取亲本特征并生成配对组合。
#' 适用于处理包含"mapa"标记的遗传分析矩阵，将其转换为便于分析的配对数据框。
#'
#' @param data_frame_input 必需，输入的方阵数据框，必须包含"mapa"单元格
#' @param feature_cols 可选，数值向量，指定要提取的特征列索引。默认为NULL，表示提取所有可用特征
#' @param mylag 可选，字符值，用于识别有效配对的标记值，默认为"1"
#'
#' @return 返回一个数据框，包含三列：ma（母本名称）、pa（父本名称）和memo（母本和父本特征组合）
#'
#' @details
#' 函数执行以下主要步骤：
#' 1. 验证输入是否为方阵数据框
#' 2. 定位"mapa"单元格并验证其位置
#' 3. 提取并验证母本和父本名称
#' 4. 填充特征值（确保矩阵对称性）
#' 5. 基于指定的标记值(mylag)生成配对组合
#'
#' @examples
#' # 假设data是符合要求的方阵数据框
#' # pairs_data <- convert_to_pairs(data)
#' # pairs_data <- convert_to_pairs(data, feature_cols = c(1, 3, 5))
#'
#' @export
#' @importFrom stats setNames
convert_to_pairs <- function(data_frame_input, feature_cols = NULL,mylag="1") {
  # 基本校验
  if (!is.data.frame(data_frame_input))
    stop("输入必须是一个数据框")
  if (nrow(data_frame_input) != ncol(data_frame_input))
    stop("输入数据框必须为方阵")

  # 定位mapa单元格
  mapa_pos <- which(data_frame_input == "mapa", arr.ind = TRUE)
  if (nrow(mapa_pos) == 0) stop("未找到mapa单元格")
  if (nrow(mapa_pos) > 1) stop("找到多个mapa单元格")
  row_mapa <- mapa_pos[1, "row"]
  col_mapa <- mapa_pos[1, "col"]

  # 校验对称性
  if (row_mapa != col_mapa)
    stop("mapa必须位于对角线位置")

  # 处理feature_cols参数
  if (is.null(feature_cols)) {
    mother_feature_indices <- 1:(col_mapa - 1)
    father_feature_indices <- 1:(row_mapa - 1)
  } else {
    if (!is.numeric(feature_cols))
      stop("feature_cols必须是数值向量")
    feature_cols <- as.integer(feature_cols)
    if (any(feature_cols < 1 | feature_cols > (col_mapa - 1)))
      stop("feature_cols中的索引超出母本特征列范围（1到", col_mapa - 1, ")")
    if (any(feature_cols < 1 | feature_cols > (row_mapa - 1)))
      stop("feature_cols中的索引超出父本特征行范围（1到", row_mapa - 1, ")")
    mother_feature_indices <- feature_cols
    father_feature_indices <- feature_cols
  }

  # 获取母本和父本名称
  mother_names <- data_frame_input[(row_mapa+1):nrow(data_frame_input), col_mapa]
  father_names <- unlist(data_frame_input[row_mapa, (col_mapa+1):ncol(data_frame_input)])

  # 校验名称一致性
  if (!identical(as.vector(mother_names), as.vector(father_names)))
    stop("母本与父本名称不一致")

  # 特征填充（对称处理）
  for (j in (col_mapa+1):ncol(data_frame_input)) {
    mother_row <- row_mapa + (j - col_mapa)
    features <- data_frame_input[mother_row, 1:(col_mapa-1)]
    data_frame_input[1:(row_mapa-1), j] <- t(features)
  }

  # 构建结果
  result <- list()
  n_mother <- length(mother_names)

  for (i in 1:n_mother) {
    curr_row <- row_mapa + i
    mother <- mother_names[i]
    m_features <- paste(data_frame_input[curr_row, mother_feature_indices], collapse = ";")

    for (j in 1:n_mother) {
      curr_col <- col_mapa + j
      if (!is.na(data_frame_input[curr_row, curr_col]) && data_frame_input[curr_row, curr_col] == mylag) {
        father <- father_names[j]
        p_features <- paste(data_frame_input[father_feature_indices, curr_col], collapse = ";")
        memo <- paste(m_features, p_features, sep = "+")
        result[[length(result)+1]] <- data.frame(ma=mother, pa=father, memo=memo)
      }
    }
  }

  if (length(result) == 0) return(data.frame(ma=character(), pa=character(), memo=character()))
  re_data <- do.call(rbind, result)
  rownames(re_data) <- NULL
  return(re_data)
}

#' 组合数据框中的记录（pair_id、行号置前，字段交错）
#'
#' @description
#' 将数据框中记录两两组合，生成所有 `(i, j)` 和 `(j, i)` 组合。字段加上 `ma_`、`pa_` 前缀，
#' 并将字段按顺序排列为：pair_id、ma_row、pa_row、ma_x1、pa_x1、ma_x2、pa_x2、...
#'
#' @param df 一个包含至少两条记录的数据框。
#'
#' @return 返回字段顺序交错的数据框。
#'
#' @export
combine_records <- function(df) {
  n <- nrow(df)
  if (n < 2) {
    stop("数据框必须至少包含两条记录。")
  }

  base_pairs <- utils::combn(n, 2)

  all_pairs <- do.call(rbind, lapply(seq_len(ncol(base_pairs)), function(k) {
    i <- base_pairs[1, k]
    j <- base_pairs[2, k]
    data.frame(
      i = c(i, j),
      j = c(j, i),
      pair_id = rep(k, 2),
      stringsAsFactors = FALSE
    )
  }))

  combined <- purrr::pmap_dfr(
    all_pairs,
    function(i, j, pair_id) {
      ma <- df[i, , drop = FALSE]
      pa <- df[j, , drop = FALSE]

      names(ma) <- paste0("ma_", names(ma))
      names(pa) <- paste0("pa_", names(pa))

      result <- dplyr::bind_cols(
        ma,
        pa,
        ma_row = i,
        pa_row = j,
        pair_id = pair_id
      )

      # 调整字段顺序：pair_id, ma_row, pa_row, 然后交错的 ma_x / pa_x
      result <- dplyr::relocate(result, pair_id, .before = dplyr::everything()) |>
        dplyr::relocate(ma_row, pa_row, .after = pair_id)

      # 获取所有字段名
      all_names <- names(result)

      # 获取交错变量名
      ma_vars <- all_names[grepl("^ma_", all_names)]
      pa_vars <- all_names[grepl("^pa_", all_names)]

      base_names <- intersect(
        sub("^ma_", "", ma_vars),
        sub("^pa_", "", pa_vars)
      ) |>
        sort()

      # 交错组合
      reordered <- unlist(purrr::map(base_names, function(name) {
        c(paste0("ma_", name), paste0("pa_", name))
      }))

      final_order <- c("pair_id", "ma_row", "pa_row", reordered)
      dplyr::select(result, dplyr::all_of(final_order))
    }
  )

  dplyr::arrange(combined, pair_id)
}



#' 向组合数据框中添加矩阵内容
#'
#' 根据给定的正方形矩阵 `mat`，将非空内容添加到组合数据框 `combined_df` 中。
#' 对于矩阵中的每个非空元素 `(i, j)`，在结果中添加对应的行，并可选地添加其“反交”对称元素 `(j, i)`。
#'
#' @param combined_df 数据框，必须包含 `ma_row` 和 `pa_row` 两个字段，用于与矩阵行列对应。
#' @param mat 正方形矩阵，元素为字符串，代表内容。
#' @param add_symmetric 逻辑值，是否添加“反交”对称内容，默认为 `TRUE`。
#'
#' @return 返回一个数据框，在原有 `combined_df` 基础上新增 `content` 字段，并调整字段顺序。
#'
#' @details
#' - 函数会遍历矩阵所有元素，对非空且非NA的元素生成对应的内容行。
#' - 如果 `add_symmetric = TRUE`，则还会添加对称位置的“反交”内容，字段内容为原内容加上字符串“反交”。
#' - 最终合并生成的新数据框中，字段顺序会自动调整，方便后续使用。
#'
#' @examples
#' combined_df <- data.frame(
#'   pair_id = 1:4,
#'   ma_row = c(1,1,2,2),
#'   pa_row = c(2,3,1,3)
#' )
#' mat <- matrix(c("A", NA, "B", NA, "C", "D", NA, NA, "E"), nrow=3)
#' add_matrix_content(combined_df, mat)
#'
#' @import dplyr
#' @importFrom purrr map
#' @export
add_matrix_content <- function(combined_df, mat, add_symmetric = TRUE) {
  if (!is.matrix(mat)) stop("mat 必须是一个矩阵")
  if (nrow(mat) != ncol(mat)) stop("mat 必须是正方形矩阵")

  n <- nrow(mat)

  entries <- list()

  # 收集所有非空值，并生成反交
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      val <- mat[i, j]
      if (!is.na(val) && nzchar(val)) {
        # (i, j): 原始值
        entries[[length(entries) + 1]] <- list(
          ma_row = i, pa_row = j, content = val
        )

        # (j, i): 反交值
        if (add_symmetric && i != j) {
          entries[[length(entries) + 1]] <- list(
            ma_row = j, pa_row = i, content = paste0(val, "反交")
          )
        }
      }
    }
  }

  # 构造成 content_df
  content_df <- dplyr::bind_rows(entries)

  # 合并
  df <- dplyr::left_join(combined_df, content_df, by = c("ma_row", "pa_row"))

  # 字段顺序处理
  all_names <- names(df)
  ma_vars <- all_names[grepl("^ma_", all_names)]
  pa_vars <- all_names[grepl("^pa_", all_names)]

  base_names <- intersect(sub("^ma_", "", ma_vars), sub("^pa_", "", pa_vars)) |>
    sort()

  reordered <- unlist(purrr::map(base_names, \(name) c(paste0("ma_", name), paste0("pa_", name))))
  final_order <- c("pair_id", "ma_row", "pa_row", "content", reordered)
  remaining <- setdiff(names(df), final_order)

  df[, c(final_order, remaining)]
}




#' 母本使用统计（可选是否包含空内容）
#'
#' @description
#' 对组合数据框按母本名称（`ma_名称`）进行统计，计算每个亲本作为母本参与的组合次数，
#' 并列出对应使用过的父本（`pa_名称`）列表，同时返回该母本对应的其它字段信息（如 ID、蛋白、株高等）。
#'
#' 默认情况下仅统计 `content` 不为空、且不包含“反交”的正交组合。可通过参数控制是否包括空内容。
#'
#' @param df 包含 `ma_名称`, `pa_名称`, `content` 和母本相关信息字段的数据框。
#' @param include_empty_content 合理布尔值，是否包括 `content` 为空或缺失的记录，默认值为 `FALSE`。
#'
#' @return 数据框，每行为一个母本，包含其组合次数、所用父本列表及母本自身信息。
#'
#' @export

summarize_by_mother_name <- function(df, include_empty_content = FALSE) {
  if (!all(c("ma_名称", "pa_名称", "content") %in% names(df))) {
    stop("数据框必须包含 ma_名称、pa_名称 和 content 字段")
  }

  valid <- df

  if (!include_empty_content) {
    valid <- valid |>
      dplyr::filter(
        !is.na(content),
        nzchar(content),
        !grepl("反交", content)
      )
  }

  summary_tbl <- valid |>
    dplyr::group_by(ma_名称) |>
    dplyr::summarise(
      n_combinations = dplyr::n(),
      used_fathers = paste(unique(pa_名称), collapse = ", ")
    )

  mother_info <- valid |>
    dplyr::select(dplyr::starts_with("ma_")) |>
    dplyr::distinct(ma_名称, .keep_all = TRUE)

  dplyr::left_join(summary_tbl, mother_info, by = "ma_名称") |>
    dplyr::arrange(desc(n_combinations))
}




#' 父本使用统计（可选是否包含空内容）
#'
#' @description
#' 对组合数据框按父本名称（`pa_名称`）进行统计，计算每个亲本作为父本参与的组合次数，
#' 并列出对应使用过的母本（`ma_名称`）列表，同时返回该父本对应的其它字段信息（如 ID、蛋白、株高等）。
#'
#' 默认情况下仅统计 `content` 不为空、且不包含“反交”的正交组合。可通过参数控制是否包括空内容。
#'
#' @param df 包含 `pa_名称`, `ma_名称`, `content` 和父本相关信息字段的数据框。
#' @param include_empty_content 合理布尔值，是否包括 `content` 为空或缺失的记录，默认值为 `FALSE`。
#'
#' @return 数据框，每行为一个父本，包含其组合次数、所用母本列表及父本自身信息。
#'
#' @export

summarize_by_father_name <- function(df, include_empty_content = FALSE) {
  if (!all(c("ma_名称", "pa_名称", "content") %in% names(df))) {
    stop("数据框必须包含 ma_名称、pa_名称 和 content 字段")
  }

  valid <- df

  if (!include_empty_content) {
    valid <- valid |>
      dplyr::filter(
        !is.na(content),
        nzchar(content),
        !grepl("反交", content)
      )
  }

  summary_tbl <- valid |>
    dplyr::group_by(pa_名称) |>
    dplyr::summarise(
      n_combinations = dplyr::n(),
      used_mothers = paste(unique(ma_名称), collapse = ", ")
    )

  father_info <- valid |>
    dplyr::select(dplyr::starts_with("pa_")) |>
    dplyr::distinct(pa_名称, .keep_all = TRUE)

  dplyr::left_join(summary_tbl, father_info, by = "pa_名称") |>
    dplyr::arrange(desc(n_combinations))
}




#' 合并母本与父本使用统计
#'
#' @description
#' 将 `summarize_by_mother_name()` 和 `summarize_by_father_name()` 的统计结果合并为一个综合表格，
#' 展示每个亲本在母本和父本两个方向的使用情况，包括组合次数、使用材料列表及亲本本身的各类字段。
#'
#' 当一个亲本只在某一方向（母本或父本）出现时，也会保留记录。
#'
#' 默认情况下仅统计 `content` 不为空、且不包含“反交”的正交组合。可通过参数控制是否包括空内容。
#'
#' @param df 含有 `ma_名称`, `pa_名称`, `content` 及其它亲本信息的组合数据框。
#' @param include_empty_content 合理布尔值，是否包括 `content` 为空或缺失的记录，默认值为 `FALSE`。
#'
#' @return 数据框，字段包括：
#' \describe{
#'   \item{parent_name}{统一的亲本名称字段}
#'   \item{n_as_mother}{作为母本的组合次数}
#'   \item{used_fathers}{搭配的父本列表}
#'   \item{n_as_father}{作为父本的组合次数}
#'   \item{used_mothers}{搭配的母本列表}
#'   \item{其它字段}{如 ID、蛋白、株高等亲本本身信息}
#' }
#'
#' @export

merge_mother_father_stats <- function(df, include_empty_content = FALSE) {
  mother_stats <- summarize_by_mother_name(df, include_empty_content) |>
    dplyr::rename(
      parent_name = ma_名称,
      n_as_mother = n_combinations,
      used_fathers = used_fathers
    )

  father_stats <- summarize_by_father_name(df, include_empty_content) |>
    dplyr::rename(
      parent_name = pa_名称,
      n_as_father = n_combinations,
      used_mothers = used_mothers
    )

  merged <- dplyr::full_join(mother_stats, father_stats, by = "parent_name")

  info_cols <- grep("^ma_|^pa_", names(merged), value = TRUE)
  ma_only <- grep("^ma_", info_cols, value = TRUE)
  pa_only <- grep("^pa_", info_cols, value = TRUE)

  for (col in sub("^ma_", "", ma_only)) {
    ma_col <- paste0("ma_", col)
    pa_col <- paste0("pa_", col)
    merged[[col]] <- dplyr::coalesce(merged[[ma_col]], merged[[pa_col]])
  }

  merged |>
    dplyr::select(-all_of(c(ma_only, pa_only))) |>
    dplyr::relocate(parent_name, n_as_mother, used_fathers, n_as_father, used_mothers)
}


#' 配置杂交组合（支持筛选且自动避免自交）
#'
#' @description
#' 随机配置 n 条杂交组合，支持通过亲本“名称”和性状条件进行筛选。
#' 默认排除母本和父本名称相同的组合（避免自交）。
#'
#' @param df 组合数据框，包含 pair_id、ma_row、pa_row、ma_名称、pa_名称、content 等字段
#' @param n 欲配置的组合数量
#' @param mother_names 可选，母本名称列表
#' @param father_names 可选，父本名称列表
#' @param mother_filter 可选，母本性状筛选表达式（如 quote(ma_蛋白 > 12)）
#' @param father_filter 可选，父本性状筛选表达式（如 quote(pa_株高 < 150)）
#'
#' @return 返回一个列表：
#' \describe{
#'   \item{plan}{配置成功的 n 条正交组合}
#'   \item{updated_df}{更新后的完整组合数据框（含 content 修改）}
#' }
#'
#' @export
generate_cross_plan <- function(df,
                                n,
                                mother_names = NULL,
                                father_names = NULL,
                                mother_filter = NULL,
                                father_filter = NULL) {
  stopifnot(all(c("pair_id", "ma_名称", "pa_名称", "content") %in% names(df)))

  available <- df |>
    dplyr::filter(is.na(content) | !nzchar(content))

  # 避免自交：母本 ≠ 父本
  available <- available |>
    dplyr::filter(ma_名称 != pa_名称)

  if (!is.null(mother_names)) {
    available <- available |>
      dplyr::filter(ma_名称 %in% mother_names)
  }

  if (!is.null(father_names)) {
    available <- available |>
      dplyr::filter(pa_名称 %in% father_names)
  }

  if (!is.null(mother_filter)) {
    available <- available |>
      dplyr::filter(!!mother_filter)
  }

  if (!is.null(father_filter)) {
    available <- available |>
      dplyr::filter(!!father_filter)
  }

  available <- available |>
    dplyr::distinct(pair_id, .keep_all = TRUE)

  if (nrow(available) < n) {
    stop("筛选后的组合数量不足，最多可配置 ", nrow(available), " 条")
  }

  selected <- dplyr::slice_sample(available, n = n)
  today <- format(Sys.Date(), "%Y-%m-%d")

  direct <- df |>
    dplyr::inner_join(selected |> dplyr::select(pair_id, ma_row, pa_row),
                      by = c("pair_id", "ma_row", "pa_row")) |>
    dplyr::mutate(content = today)

  inverse <- df |>
    dplyr::filter(pair_id %in% selected$pair_id) |>
    dplyr::anti_join(direct |> dplyr::select(pair_id, ma_row, pa_row),
                     by = c("pair_id", "ma_row", "pa_row")) |>
    dplyr::mutate(content = paste0(today, "反交"))

  untouched <- df |>
    dplyr::filter(!(pair_id %in% selected$pair_id))

  updated_df <- dplyr::bind_rows(direct, inverse, untouched) |>
    dplyr::arrange(pair_id)

  list(
    plan = direct,
    updated_df = updated_df
  )
}


