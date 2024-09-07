
#' 行数转行号
#'
#' @param hangshu 向量，各材料种植的行数
#' @return 返回各材料的行号
#' @example
#' number_to_linenumber(1:10)

rows_to_linenumber<-function(number=1:10){
  end_number<-cumsum(number)
  start_number<- end_number-number+1
  paste(start_number,end_number,sep="-")
}


#' 添加编码重复对照（按间隔插入）
#'
#' @param my_primary 数据框，数据框中要包含id,stageid,name三列
#' @param ck 向量，对照名称,可一个对照或多个对照
#' @param interval 插入对照的材料间隔数
#' @param s_prefix 材料前缀
#' @param rp 种植材料重复数，第一重复顺序，其它重复随机，并排两列材料不相同
#' @return 返回插入对照和重复的数据框，给材料进行了编号
addrpckfixed <- function(my_primary,
                         ck = "JD12",
                         interval = 3,
                         s_prefix = "GC",
                         rp = 2,
                         digits = 3)

{
  mym <- my_primary#[c("id", "stageid", "name")]

  #加对照(如果有对照且interval>1)
  if (interval > 0 & length(ck) > 0) {
    patten <- rep(1:ceiling(nrow(mym) / interval),
                  each = interval,
                  length.out = nrow(mym))
    df_list <- split(mym, patten)

    # 定义追加函数
    add_row <-
      function(df, myck = ck) {
        for (iname in myck) {
          mdf <- df
          mdf <- mdf[1, ]
          mdf[1, ] <- NA
          mdf$id = NA
          mdf$stageid = NA
          mdf$name = iname#
          df <- rbind(df, mdf)
        }
        return(df)
      }

    df_list <- lapply(df_list, add_row)
    df_list <- do.call(rbind, df_list)

  } else{
    df_list <- mym
  }

  #第一重复加stageid,rp,code
  df_list$code <- 1:nrow(df_list)#编号
  df_list$stageid <-
    generate_stageid(
      start_num = 1,
      end_num = nrow(df_list),
      char= s_prefix,
      digit_length = digits
    )
  df_list$rp <- 1#重复
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
      df_list2[excode, ] <- NA
      #随机，这个地方想了好长时间
      df_list2[excode, ] <- df_list[sample(excode), ]
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
#' @param my_plant 数据框，数据框中要包含id,stageid,name三列
#' @param ck 向量，对照名称
#' @param interval 插入对照的材料间隔数
#' @param s_prefix 材料前缀
#' @param rp 种植材料重复数，第一重复顺序，其它重复随机，并排两列材料不相同
#' @return 返回插入对照和重复的数据框，给材料进行了编号，对照随机

#ck随机,相邻两个重复，并排时不会相同
addrpck <- function(my_primary,
                    ck = c("冀豆12", "冀豆17"),
                    interval = 3,
                    s_prefix = "GC",
                    rp = 3,
                    digits = 3
                    )

{
  mym <- my_primary#[c("id", "stageid", "name")]

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
          mdf <- df
          mdf <- mdf[1, ]
          mdf[1, ] <- NA

          mdf$id = NA
          mdf$stageid = NA
          mdf$name = iname
          df <- rbind(df, mdf)
        }
        return(df)
      }

    df_list <- lapply(df_list, add_row)
    df_list <- do.call(rbind, df_list)

  } else{
    df_list <- mym
  }

  #第一重复加stageid,rp,code
  df_list$code <- 1:nrow(df_list)#编号
  df_list$stageid <-
    generate_stageid(
      start_num  = 1,
      end_num  = nrow(df_list),
      char = s_prefix,
      digit_length = digits
    )
  df_list$rp <- 1#重复
  re_v = df_list
  rownames(re_v) <- NULL

  #增加重复(随机)
  if (rp > 1) {
    multirp <- list()
    curr <- df_list#用于首个
    for (rpi in 1:(rp - 1)) {
      df_list2 <- df_list

      #随机一次
      df_list2 <- df_list[sample(df_list$code), ]
      #直到全不一样，并排两个重复不相同
      outj <- 0
      while (any(curr$code == df_list2$code)) {
        df_list2 <- df_list[sample(df_list$code), ]
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

addtreatment <- function(my_primary, treatment = c("高密", "低密")) {
  mat <- my_primary
  myd <- data.frame()
  for (iname in treatment) {
    mat$treatment = iname
    myd <- rbind(myd, mat)
  }
  return(myd)
}



addfieldid <- function(my_primary) {
  fieldid <-
    data.frame(fieldid = generate_id(start_num = 1,end_num  = nrow(my_primary),char = "f"))
  re_v <- cbind(fieldid, my_primary)
  return(re_v)
}

#' 确保数据框包含指定字段并调整字段顺序
#'
#' 该函数确保数据框中包含指定的必需字段。如果数据框中缺少某个必需字段，则自动添加并填充为 `NA`。函数还将必需字段调整至数据框的前面，其余字段保持在后面。
#'
#' @param df 数据框，需要检查并调整字段的顺序。
#' @param required_columns 字符串向量，包含所有需要的必需字段名。
#'
#' @return 返回调整后的数据框，其中必需字段在前，其他字段在后。
#'
#' @examples
#' # 定义数据框
#' df <- data.frame(name = c("A", "B"), stageid = c(1, 2))
#'
#' # 定义必需字段
#' required_columns <- c("id", "user", "stageid", "name", "ma", "pa", "mapa", "memo",
#'                       "stage", "next_stage", "f", "sele", "process", "path", "source")
#'
#' # 调用函数确保字段存在并重新排序
#' df <- ensure_and_reorder_columns(df, required_columns)
#'
#' # 查看结果
#' print(df)
#'
#' @export
ensure_and_reorder_columns <- function(df, required_columns) {
  for (col in required_columns) {
    if (!col %in% colnames(df)) {
      df[[col]] <- NA
    }
  }

  all_columns <- colnames(df)
  extra_columns <- setdiff(all_columns, required_columns)
  ordered_columns <- c(required_columns, extra_columns)

  df <- df[, ordered_columns]

  return(df)
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
#' @param treatment 向量，处理，可以多个水平
#' @param ckfixed 逻辑值，对照是否固定，是固定则按等材料数插入
#' @return 返回插入对照和重复的数据框，给材料进行了编号，对照随机

#set.seed(100)#随机固定
planting <- function(my_primary,
                     ck = c("冀豆12", "冀豆17"),
                     interval = 3,
                     s_prefix = "GC",
                     rp = 2,
                     treatment= c(""),
                     place = c("石家庄", "德州"),
                     ckfixed = TRUE,
                     digits = 3,
                     rows=6
                     ) {
  library(dplyr)
  field<-subset(field,grepl("planting", table, ignore.case = TRUE))
  #ck固定
  if (ckfixed) {
    re_v<-my_primary %>%
      addrpckfixed(ck, interval, s_prefix, rp, digits) %>%
      addtreatment(treatment) %>%
      addplace(place) %>%
      addfieldid()
  } else{
    re_v<-my_primary %>%
      addrpck(ck, interval, s_prefix, rp, digits) %>%
      addtreatment(treatment) %>%
      addplace(place) %>%
      addfieldid()
  }
  re_v$rows<-rows
  re_v$line_number<-rows_to_linenumber(re_v$rows)
  #确保必须的列都存在
  re_v<-ensure_and_reorder_columns(re_v,as.character(field$name))
  return(re_v)
}


# # 材料在石家庄种3个重复，在德州种2个重复
# library(dplyr)
# set.seed(100)#随机固定
# my_primary%>%planting(ck="JD12",interval=3,rp=3,place=c("石家庄","新乡"),ckfixed = TRUE)
# my_primary%>%planting(ck="JD12",interval=2,rp=2,place="德州",ckfixed=FALSE)
#
