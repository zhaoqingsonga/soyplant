#' 升级群体
#'
#' 从杂交组合中或群体中升级群体,数据框中必有字段：name,next_stage,f，没有的字段会根据群体中必须字段补齐。
#' @param my_combi 数据框，从get_combination或get_population函数所产生数据
#' @return 下阶段值为“群体”记录升级为群体
#@examples
#get_population(my_combi)
#'

get_population <- function(my_combi,start_num=1) {
  my_pop <- subset(my_combi, my_combi$next_stage == "群体")
  #如果name中是任意字符+F或者T+数字则其后数字加1，如果不是则后面直接加 Fn,单株不能进入群体，path字段有bug
  # 使用正则表达式检测名字是否符合“任意字符+F+数字”的模式
  pattern <- ".*[FT][0-9]+$"
  if (all(grepl(pattern, my_pop$name))) {
    name <-
      paste(sapply(my_pop$name, function(x)
        substr(x, 1, nchar(x) - 1)),
        (my_pop$f + 1),
        sep = "")
  } else{
    name <- paste(my_pop$name,"F",(my_pop$f + 1),  sep = "")
  }
  if(length(name)==0) return("No selected population!")
  user <- get_computer_nodename()
  id <- generate_id(start_num, end_num =nrow(my_pop)+start_num-1)
  re_v <- data.frame(
    id = id,
    user = rep(user, length(name)),
    stageid = NA,
    name = name
  )
  re_v$ma = my_pop$ma
  re_v$pa = my_pop$pa
  re_v$mapa = my_pop$mapa
  re_v$stage <- "群体"
  re_v$next_stage <- "群体"
  re_v$f <- my_pop$f + 1
  re_v$sele <- 2
  re_v$process <- paste(my_pop$process, "/", id, sep = "")
  #
  if(is.null(my_pop$path)) my_pop$path<-my_pop$name
  re_v$path <- paste(my_pop$path, 0, sep = "-")
  re_v$source <- my_pop$name
  re_v$memo<-my_pop$memo
  re_v$former_fieldid<-my_pop$fieldid
  field<-subset(field,grepl("combination", table, ignore.case = TRUE))
  #如果生成表中没有field中所包含的字段则补全
  # 补齐缺失的字段
  for (col in as.character(field$name)) {
    if (!col %in% names(re_v)) {
      re_v[[col]] <- NA
    }
  }
  return(re_v[as.character(field$name)])
}

# my_pop <- get_population(my_combi)
# my_pop
#
#
# get_population(my_plant)
# get_population(my_line)
#
#

