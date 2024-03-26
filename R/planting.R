#' 添加编码重复对照（按间隔插入）
#'
#'
#' @param my_plant 数据框，数据框中要包含id,stageid,name三列
#' @param ck 向量，对照名称,可一个对照或多个对照
#' @param interval 插入对照的材料间隔数
#' @param s_prefix 材料前缀
#' @param rp 种植材料重复数，第一重复顺序，其它重复随机，并排两列材料不相同
#' @return 返回插入对照和重复的数据框，给材料进行了编号


addrpckfixed <- function(my_plant,
                             ck = "JD12",
                             interval = 3,
                             s_prefix="GC",
                             rp = 2)

{
  mym <- my_primary[c("id", "stageid", "name")]

  #加对照(如果有对照且interval>=1)
  if (interval > 0 & length(ck) > 0) {
    patten <- rep(1:ceiling(nrow(mym) / interval),
                  each = interval,
                  length.out = nrow(mym))
    df_list <- split(mym, patten)

    # 定义追加函数
    add_row <-
      function(df, myck = ck) {
        for (iname in myck) {
          mdf <- data.frame(id = NA,
                            stageid = NA,
                            name = iname)#与前面要对应
          df <- rbind(df, mdf)
        }
        return(df)
      }

    df_list <- lapply(df_list, add_row)
    df_list <- do.call(rbind, df_list)

  }else{
    df_list<-mym
  }

  #加stageid，默认第一次重复
  df_list$code <- 1:nrow(df_list)
  df_list$stageid <- get_prefix_linename(prefix =s_prefix,n1 = 1,n2 = nrow(df_list),digits = 3)
  df_list$rp <- 1
  re_v = df_list
  rownames(re_v) <- NULL

  #如果rp>1增加重复(固定)
  if (rp > 1) {
    multirp <- list()
    for (rpi in 1:(rp - 1)) {
      df_list2 <- df_list
      rownames(df_list2) <- NULL
      #非对照code
      excode <- df_list$code[!df_list$name %in% ck]
      #对照code
      ckcode <- df_list$code[df_list$name %in% ck]
      #删非对照，这个地方想了好长时间
      df_list2[excode,] <- NA
      #随机，这个地方想了好长时间
      df_list2[excode,] <- df_list[sample(excode),]
      df_list2$rp <- rpi + 1
      multirp[[rpi]] <- df_list2
    }
    multirp <- do.call(rbind, multirp)
    re_v <- rbind(df_list, multirp)
    rownames(re_v) <- NULL
  }

  return(re_v)
}

#' 添加编码重复对照（对照随机）
#'
#'
#'
#' @param my_plant 数据框，数据框中要包含id,stageid,name三列
#' @param ck 向量，对照名称
#' @param interval 插入对照的材料间隔数
#' @param s_prefix 材料前缀
#' @param rp 种植材料重复数，第一重复顺序，其它重复随机，并排两列材料不相同
#' @return 返回插入对照和重复的数据框，给材料进行了编号，对照随机

#ck随机,相邻两个重复，并排时不会相同
addrpck <- function(my_plant,
                        ck = c("冀豆12", "冀豆17"),
                        interval = 3,
                       s_prefix="GC",
                        rp = 3)

{
  mym <- my_primary[c("id", "stageid", "name")]

  #加对照(如果有对照且interval>=1)
  if (interval > 0 & length(ck) > 0) {
    patten <- rep(1:ceiling(nrow(mym) / interval),
                  each = interval,
                  length.out = nrow(mym))
    df_list <- split(mym, patten)

    # 定义追加函数
    add_row <-
      function(df, myck = ck) {
        for (iname in myck) {
          mdf <- data.frame(id = NA,
                            stageid = NA,
                            name = iname)#与前面要对应
          df <- rbind(df, mdf)
        }
        return(df)
      }

    df_list <- lapply(df_list, add_row)
    df_list <- do.call(rbind, df_list)

  }else{
    df_list<-mym
  }

  #加stageid，默认第一次重复
  df_list$code <- 1:nrow(df_list)
  df_list$stageid <- get_prefix_linename(prefix =s_prefix,n1 = 1,n2 = nrow(df_list),digits = 3)
  df_list$rp <- 1
  re_v = df_list
  rownames(re_v) <- NULL

  #增加重复(随机)
  if (rp > 1) {
    multirp <- list()
    curr <- df_list#用于首个
    for (rpi in 1:(rp - 1)) {
      df_list2 <- df_list

      #随机一次
      df_list2 <- df_list[sample(df_list$code),]
      #直到全不一样，并排两个重复不相同
      outj <- 0
      while (any(curr$code == df_list2$code)) {
        df_list2 <- df_list[sample(df_list$code),]
        outj <- outj + 1
        if (outj >= 20) {
          break
        }
      }

      df_list2$rp <- rpi + 1
      multirp[[rpi]] <- df_list2
      curr <- df_list2
    }

    multirp <- do.call(rbind, multirp)
    re_v <- rbind(df_list, multirp)
    rownames(re_v) <- NULL

  }

  return(re_v)
}


addplace <- function(my_primary, place = c("石家庄", "德州")) {
  mat <- my_primary
  myd <- data.frame()
  for (iname in place) {
    mat$place = iname
    myd <- rbind(myd, mat)
  }
  return(myd)
}

addfieldid <- function(my_primary) {
  fieldid <- data.frame(fieldid = get_ID(n2 = nrow(my_primary)))
  re_v <- cbind(fieldid, my_primary)
  return(re_v)
}

#' 计划种植试验
#'
#'
#' @param my_primary 数据框，数据框中要包含id,stageid,name三列
#' @param ck 向量，对照名称
#' @param interval 插入对照的材料间隔数
#' @param s_prefix 材料前缀
#' @param rp 种植材料重复数，第一重复顺序，其它重复随机，并排两列材料不相同
#' @param place 向量，试验地点，可多个试验地点
#' @param ckfixed 逻辑值，对照是否固定，是固定则按等材料数插入
#' @return 返回插入对照和重复的数据框，给材料进行了编号，对照随机

#set.seed(100)#随机固定
planting <- function(my_primary,
                     ck = c("冀豆12", "冀豆17"),
                     interval = 3,
                     s_prefix="GC",
                     rp = 2,
                     place = c("石家庄", "德州"),
                     ckfixed = TRUE) {
  library(dplyr)
  #ck固定
  if (ckfixed) {
    my_primary %>%
      addrpckfixed(ck, interval,s_prefix,rp) %>%
      addplace(place) %>%
      addfieldid()
  } else{
    my_primary %>%
      addrpck(ck, interval,s_prefix,rp) %>%
      addplace(place) %>%
      addfieldid()
  }
}

# # 材料在石家庄种3个重复，在德州种2个重复
# library(dplyr)
# set.seed(100)#随机固定
# my_primary%>%planting(ck="JD12",interval=3,rp=3,place=c("石家庄","新乡"),ckfixed = TRUE)
# my_primary%>%planting(ck="JD12",interval=2,rp=2,place="德州",ckfixed=FALSE)
#
