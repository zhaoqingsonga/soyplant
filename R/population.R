#' 升级群体
#'
#' 从杂交组合中或群体中升级群体
#' @param my_combi 数据框，从get_combination或get_population函数所产生数据
#' @return 下阶段值为“群体”记录升级为群体
#' @examples
#' get_population(my_combi)
#'

get_population <- function(my_combi,start_num=1) {
  my_pop <- subset(my_combi, my_combi$next_stage == "群体")
  #如果是杂交到群体或群体到群体则F后增加，如果不是株行则后面直接加 Fn,单株不能进入群体，path字段有bug
  if (all(my_pop$stage == "群体")|all(my_pop$stage == "杂交")) {
    name <-
      paste(sapply(my_pop$name, function(x)
        substr(x, 1, nchar(x) - 1)),
        (my_pop$f + 1),
        sep = "")
  } else{
    name <- paste(my_pop$name,"F",(my_pop$f + 1),  sep = "")
  }

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
  re_v$path <- paste(my_pop$path, 0, sep = "-")
  re_v$source <- my_pop$name
  re_v$memo<-my_pop$memo
  return(re_v)
}

# my_pop <- get_population(my_combi)
# my_pop
#
#
# get_population(my_plant)
# get_population(my_line)
#
#

