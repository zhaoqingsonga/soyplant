
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
combination <- function(ma = c("冀豆12", "冀豆17"),
                        pa = c("中联豆6001", "中联豆6024", "中联豆6033"),
                        memo = NA) {
  all_combinations <- expand.grid(ma, pa)
  names(all_combinations) <- c("ma", "pa")
  all_combinations$mapa <-
    paste(all_combinations$ma, all_combinations$pa, sep = "/")
  all_combinations$memo = memo
  return(all_combinations)
}


#' get ID prefix
#'
#' @param name description
#'
#'
#'
ID_prefix <- function() {
  onlyID <- format(Sys.time(), "%Y%m%d%H%M%S")
  return(onlyID)
}
#
ID_suffix5 <- function(n1 = 1, n2 = 99999) {
  id1 <- paste("0000", 1:9, sep = "")
  id2 <- paste("000", 10:99, sep = "")
  id3 <- paste("00", 100:999, sep = "")
  id4 <- paste("0", 1000:9999, sep = "")
  id5 <- 10000:99999
  alln <- c(id1, id2, id3, id4, id5)
  if (is.numeric(n1) & is.numeric(n2) & n2 >= n1 & n1 >= 1 &
      n2 <= 99999)
  {
    re_v <- alln[n1:n2]
    return(re_v)
  } else{
    return(NA)
  }
}
#
ID_suffix4 <- function(n1 = 1, n2 = 9999) {
  id1 <- paste("000", 1:9, sep = "")
  id2 <- paste("00", 10:99, sep = "")
  id3 <- paste("0", 100:999, sep = "")
  id4 <- 1000:9999
  alln <- c(id1, id2, id3, id4)
  if (is.numeric(n1) &
      is.numeric(n2) & n2 >= n1 & n1 >= 1 & n2 <= 9999)
  {
    re_v <- alln[n1:n2]
    return(re_v)
  } else{
    return(NA)
  }
}

get_ID <- function(n1 = 1, n2 = 6) {
  my_id_prefix <- ID_prefix()
  my_id_sufix <- ID_suffix5(n1, n2)
  re_v <- paste(my_id_prefix, my_id_sufix, sep = "")
  return(re_v)
}

ID_suffix3 <- function(n1 = 1, n2 = 999) {
  id1 <- paste("00", 1:9, sep = "")
  id2 <- paste("0", 10:99, sep = "")
  id3 <- 100:999
  alln <- c(id1, id2, id3)
  if (is.numeric(n1) &
      is.numeric(n2) & n2 >= n1 & n1 >= 1 & n2 <= 999)
  {
    re_v <- alln[n1:n2]
    return(re_v)
  } else{
    return(NA)
  }
}




#paste(ID_prefix(),ID_suffix(1,9),sep="")
#获得电脑nodename
get_computer_nodename <- function() {
  return(Sys.info()["nodename"])
}

#获得prefix_linename
get_prefix_linename <- function(prefix = "ZJ",
                                n1 = 1,
                                n2 = 6,
                                digits = 3) {
  if (digits == 3) {
    re_v <- paste(prefix, ID_suffix3(n1, n2), sep = "")
  }
  else if (digits == 4) {
    re_v <- paste(prefix, ID_suffix4(n1, n2), sep = "")
  }
  else{
    re_v <- paste(prefix, ID_suffix3(n1, n2), sep = "")
  }
  return(re_v)
}
#添加名称


# get_combination_list<-function(
    #     mylist=list(
#     com1=list(ma=c("冀豆12","冀豆17"),
#               pa=c("中联豆6001","中联豆6024","中联豆6033"),
#               memo="high protein"),
#     com2=list(ma=c("冀豆15","冀豆20"),
#               pa=c("中联豆6001","中联豆6024","中联豆6033")
#               )
#     ),
#     prefix="ZJ",
#     startN=1,
#     only=FALSE
# ){
#   mapa<-data.frame()
#   for(i in 1:length(mylist)){
#    if(length(mylist[[i]]$memo)==0) {
#      mapa<-rbind(mapa,combination(mylist[[i]]$ma,mylist[[i]]$pa))
#      }
#     else{
#       mapa<-rbind(mapa,combination(mylist[[i]]$ma,mylist[[i]]$pa,mylist[[i]]$memo))
#         }
#     }
#
#   if(only){mapa<-mapa[!duplicated(mapa$mapa),]}
#   my_len<-length(mapa$mapa)
#   user<-get_computer_nodename()
#   name<-get_prefix_linename(prefix=prefix,n1=startN,n2=my_len+startN-1)
#   id<-get_ID(1,my_len)
#   f<-rep(0,my_len)
#   re_v<-data.frame(
#     id=id,
#     user=rep(user,my_len),
#     name=name,
#     f=f
#   )
#   re_v<-cbind(re_v,mapa)
#   re_v$stage<-"杂交"
#   re_v$next_stage<-"群体"
#   re_v$process<-id
#   re_v$path<-re_v$name
#   return(re_v)
# }


#' 获得组合列表
#'
#' 根据父母本获得组合列表
#' @param ma 向量，母本列表
#' @param pa 向量，父本列表
#' @param memo 字符串， 备注
#' @param prefix 组合前缀
#' @param startN 起始编号
#' @return 返回杂交列表
#' @examples
#' get_combination(ma=c("JD12", "JD17"),pa=c("ZLD6001", "ZLD6024", "ZLD6033"),memo = "转基因",prefix="ZJ",startN=1)
#' get_combination(ma=c("JD12", "JD17"),pa=c("ZLD6001", "ZLD6024", "ZLD6033"),prefix="ZJ",startN=101)
get_combination <- function(ma = c("JD12", "JD17"),
                            pa = c("ZJD6001", "ZJD6024"),
                            memo = NA,
                            prefix = "ZJ",
                            startN = 1)
{
  mapa <- combination(ma, pa)
  mapa$memo = memo
  my_len <- length(mapa$mapa)
  user <- get_computer_nodename()
  stageid <-
    get_prefix_linename(prefix = prefix,
                        n1 = startN,
                        n2 = my_len + startN - 1)#合并时注意，要重新生成
  name <- paste(stageid, "F0", sep = "")#合并时注意，要重新生成

  id <- get_ID(1, my_len)
  f <- rep(0, my_len)
  re_v <- data.frame(
    id = id,
    user = rep(user, my_len),
    stageid = stageid,
    name = name,
    f = f
  )
  re_v <- cbind(re_v, mapa)
  re_v$stage <- "杂交"
  re_v$next_stage <- "群体"
  re_v$process <- id
  re_v$path <- re_v$stageid#合并时要重新生成
  return(re_v)
}
##
combi_bind <- function(...,
                       prefix = "ZJ",
                       only = TRUE,
                       order = FALSE) {
  # 在函数内部，你可以通过...来访问不定参数
  arg <- list(...)
  re_v <- do.call(rbind, arg)
  if (only)
    re_v <- re_v[!duplicated(re_v$mapa),]
  if (order) {
    re_v <- re_v[order(re_v$mapa),]
  }
  #合并这两个字段时重命名
  re_v$stageid <- get_prefix_linename(prefix, n2 = nrow(re_v))
  re_v$name <- paste(re_v$stageid, "F0", sep = "")
  re_v$path <- re_v$stageid
  rownames(re_v)<-NULL
  return(re_v)
}

#'配置杂交组合
#'
#' @param filename 文件名，里面包括3列，分别为母本，父本和备注，其它列无要求，文件格式为xlsx.
#' @param prefix 组合的前缀。
get_combination_fromfile<-function(filename,prefix = "ZJ24"){
  library(openxlsx)
  mydata<-read.xlsx(filename,1)
  mylist<-list()
  for(i in 1:nrow(mydata)){
    mylist[[i]]<-get_combination(ma=mydata$母本[i],pa=mydata$父本[i],memo=mydata$备注[i])
  }
  extra_params<-list(prefix = prefix, only = TRUE,order = FALSE)
  do.call(combi_bind, c(mylist,extra_params))
}


#'  获得组合矩阵
#'
#' @param my_combi data.frame, 里面要有三列数据:分别为 ma, pa, stageid

# my_combi <- get_combination(prefix = "ZJ24")
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
      mapamatri[my_combi$ma[i], my_combi$pa[i]] <- my_combi$stageid[i]
    } else{
      mapamatri[my_combi$ma[i], my_combi$pa[i]] <-
        paste(mapamatri[my_combi$ma[i], my_combi$pa[i]], my_combi$stageid[i], sep =
                "/")
    }
  }
  mapamatri <- as.data.frame(mapamatri)
  mapamatri <- cbind(data.frame(母本 = ma), mapamatri)
  rownames(mapamatri)<-NULL
  return(mapamatri)
}

