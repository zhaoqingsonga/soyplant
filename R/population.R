#' 升级群体
#'
#' 从杂交组合中或群体中升级群体
#' @param my_combi 数据框，从get_combination或get_population函数所产生数据
#' @param prefix 字符串，群体名称前缀
#' @return 下阶段值为“群体”记录升级为群体
#' @examples
#' get_population(my_combi,prefix="QT24")
#'
#
#

get_population <- function(my_combi, prefix = "QT24") {
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

  STAGEID <- get_prefix_linename(prefix = prefix,
                                 n1 = 1,
                                 n2 = length(name))
  user <- get_computer_nodename()
  id <- get_ID(1, length(name))
  re_v <- data.frame(
    id = id,
    user = rep(user, length(name)),
    stageid = STAGEID,
    name = name
  )
  re_v$mapa = my_pop$mapa
  re_v$stage <- "群体"
  re_v$next_stage <- "群体"
  re_v$f <- my_pop$f + 1
  re_v$sele <- 2
  re_v$process <- paste(my_pop$process, "/", id, sep = "")
  re_v$path <- paste(my_pop$path, 0, sep = "-")
  re_v$source <- my_pop$stageid
  return(re_v)
}

# my_pop <- get_population(my_combi,prefix="QT25")
# my_pop
#
#
# get_population(my_plant)
# get_population(my_line)
#
#

