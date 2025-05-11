###
#sdfsf
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
   paste(all_combinations$ma, all_combinations$pa, sep = "/")
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



#' 将数据框转换为配对数据
#'
#' 该函数的作用是把特定格式的数据框转换为配对数据，配对数据包含母本、父本及其特征信息。
#'
#' @param data_frame_input 输入的数据框，必须是方阵且包含 "mapa" 单元格。
#' @param feature_cols 可选的数值向量，用于指定要提取的特征列索引。若为 NULL，则提取所有可用特征。
#'
#' @return 一个数据框，包含三列："ma"（母本名称）、"pa"（父本名称）和 "memo"（母本和父本特征信息）。
#'         若未找到符合条件的配对，返回一个空的数据框。
#'
#' @examples
#' # 假设 df 是符合要求的数据框
#' # result <- convert_to_pairs(df)
#'
#' @export
#'
#' @importFrom stats na.omit
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


