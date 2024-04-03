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
#'
#' @param filename 文件名，里面包括3列，顺序为为母本，父本和备注，其它列无要求，文件格式为xlsx
#' @param prefix 组合的前缀
#' @param only 逻辑值，是否去除重复组合
#' @param order 是否配制的组合进行排序
#' @return 返回从文件中配制的杂交合组
combination_fromfile <- function(filename,
                                 only = TRUE,
                                 order = FALSE) {
  library(openxlsx)
  mydata <- read.xlsx(filename, 1, colNames = TRUE)

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
#' @param filename 文件名，里面包括3列，顺序为为母本，父本和备注，其它列无要求，文件格式为xlsx
#' @param prefix 组合前缀
#' @param startN 起始编号
#' @param only 逻辑值 是否去重
#' @param order 逻辑值，是否按亲本排序
#' @return 返回杂交表

get_combination <- function(filename,
                            prefix = "ZJ",
                            startN = 1,
                            only = TRUE,
                            order = FALSE)
{
  mapa <- combination_fromfile(filename)
  my_len <- length(mapa$mapa)
  user <- get_computer_nodename()
  #
  name_path <-
    get_prefix_linename(prefix = prefix,
                        n1 = startN,
                        n2 = my_len + startN - 1)#合并时注意，要重新生成
  name <- paste(name_path, "F0", sep = "")#合并时注意，要重新生成

  id <- get_ID(1, my_len)
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
