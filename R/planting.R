

addcoderpckfixed <- function(my_primary,
                             ck = c("冀豆12", "冀豆17"),
                             interval = 3,
                             rp = 5)

{
  mym <- my_primary[c("id", "stageid", "name")]
  #在一定条件下进行，不符合条件则返回原始数据。
  if (interval > 0 & length(ck) > 0) {
    patten <- rep(1:ceiling(nrow(mym) / interval),
                  each = interval,
                  length.out = nrow(mym))
    df_list <- split(mym, patten)
    # 定义追加函数
    add_row <-
      function(df, myck = ck) {
        for (iname in rev(myck)) {
          mdf <- data.frame(id = NA,
                            stageid = iname,
                            name = NA)#与前面要对应
          df <- rbind(df, mdf)
        }
        return(df)
      }
    df_list <- lapply(df_list, add_row)
    df_list <- do.call(rbind, df_list)
    df_list$code <- 1:nrow(df_list)
    df_list$rp <- 1
    re_v = df_list
    rownames(re_v) <- NULL
    #增加重复(随机)
    if (rp > 1) {
      multirp <- list()
      for (rpi in 1:(rp - 1)) {
        df_list2 <- df_list
        #非对照code
        excode <- df_list$code[!df_list$stageid %in% ck]
        #对照code
        ckcode <- df_list$code[df_list$stageid %in% ck]
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
  } else{
    re_v <- mym
  }

  return(re_v)
}

#ck随机,相邻两个重复，并排时不会相同
addcoderpck <- function(my_primary,
                        ck = c("冀豆12", "冀豆17"),
                        interval = 3,
                        rp = 3)

{
  mym <- my_primary[c("id", "stageid", "name")]
  if (interval > 0 & length(ck) > 0) {
    patten <- rep(1:ceiling(nrow(mym) / interval),
                  each = interval,
                  length.out = nrow(mym))
    df_list <- split(mym, patten)
    # 定义追加函数
    add_row <-
      function(df, myck = ck) {
        for (iname in rev(myck)) {
          mdf <- data.frame(id = NA,
                            stageid = iname,
                            name = NA)#与前面要对应
          df <- rbind(df, mdf)
        }
        return(df)
      }
    df_list <- lapply(df_list, add_row)
    df_list <- do.call(rbind, df_list)
    df_list$code <- 1:nrow(df_list)
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
  } else{
    re_v <- mym
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
#set.seed(100)#随机固定
planting <- function(my_primary,
                     ck = c("冀豆12", "冀豆17"),
                     interval = 3,
                     rp = 2,
                     place = c("石家庄", "德州"),
                     ckfixed = TRUE) {
  library(dplyr)
  #ck固定
  if (ckfixed) {
    my_primary %>%
      addcoderpckfixed(ck, interval, rp) %>%
      addplace(place) %>%
      addfieldid()
  } else{
    my_primary %>%
      addcoderpck(ck, interval, rp) %>%
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
