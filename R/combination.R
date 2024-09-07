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
  field<-subset(field,grepl("combination", table, ignore.case = TRUE))
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

#filename="E:\\FangCloudSync\\★大豆试验设计及总结\\2024年试验设计及总结\\2024杂交圃准备2024-03-29.xlsx"
#my_combi<-get_combination(filename,prefix = "ZJ24")

#write.table(my_combi,"E:\\FangCloudSync\\R_WD360\\Project\\soyplant\\data\\my_combi.txt")

#' 转换二维表为母本与父本配对的数据框
#'
#' 此函数将输入的二维表（数据框）转换为一个数据框，其中包括母本（第一列）和父本（其余列）之间的配对关系。结果数据框包含三列：母本 (`ma`)、父本 (`pa`) 和备注 (`memo`)，其中备注列的值为 `NA`。
#'
#' @param data_frame_input 一个数据框，第一列为母本，其余列为父本，内容为0或1，表示母本与父本的配对关系。
#' @return 返回一个数据框，包括三列：
#' \item{ma}{母本的名称}
#' \item{pa}{父本的名称}
#' \item{memo}{备注列，所有值为NA}
#' @examples
#' # 创建一个示例数据框
#' example_df <- data.frame(
#'   Mother = c("Mother1", "Mother2", "Mother3"),
#'   Father1 = c(1, 0, 1),
#'   Father2 = c(0, 1, 0),
#'   Father3 = c(1, 0, 0),
#'   Father4 = c(0, 1, 0),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 使用函数转换示例数据框
#' result <- convert_to_pairs(example_df)
#' print(result)
convert_to_pairs <- function(data_frame_input) {
  # 检查输入是否为数据框
  if (!is.data.frame(data_frame_input)) {
    stop("输入必须是一个数据框")
  }

  # 确保数据框的列数大于1
  if (ncol(data_frame_input) < 2) {
    stop("数据框必须至少包含两列")
  }

  # 获取母本和父本的名称
  mother_names <- data_frame_input[[1]]
  father_names <- colnames(data_frame_input)[-1]  # 排除第一列的母本名称

  # 创建一个空的数据框来存储结果
  result_list <- list()

  # 遍历每一行数据
  for (i in 1:nrow(data_frame_input)) {
    # 获取当前母本名称
    mother <- mother_names[i]

    # 找到当前母本对应的所有父本
    father_indices <- which(data_frame_input[i, -1] == 1)

    # 获取对应的父本名称
    fathers <- father_names[father_indices]

    # 将母本和父本配对添加到结果列表中，并且设置"memo"列为 NA
    result_list <- append(result_list, lapply(fathers, function(father) data.frame(ma = mother, pa = father, memo = NA, stringsAsFactors = FALSE)))
  }

  # 合并结果列表为一个数据框
  result_df <- do.call(rbind, result_list)

  return(result_df)
}





