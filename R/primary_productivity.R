

get_primary <- function(my_line,
                        n1 = 1,
                        id_prefix = NULL) {
  my_primary <- subset(my_line, my_line$next_stage == "初级产比")
  #处理各列
  my_primary$id <-
    get_ID(n1 = n1,
           n2 = nrow(my_primary) + n1 - 1,
           id_prefix = id_prefix)

  my_primary$source <- my_primary$name
  #处理名称,小于9代时处理，大于9代时不再追加
  my_primary$name <-
    ifelse(my_primary$f<9,paste(my_primary$name, ":", (my_primary$f + 1),  sep = ""),my_primary$name)
  #
  my_primary$stage <- "初级产比"
  my_primary$next_stage <- "高级产比"
  my_primary$f <- my_primary$f + 1
  my_primary$sele <- 5
  my_primary$process <-
    paste(my_primary$process, my_primary$id, sep = "/")
  my_primary$path <- paste(my_primary$path, 0, sep = "-")
  #
  rownames(my_primary) <- NULL
  return(my_primary)
}

# my_primary <- get_primary(my_line)
# my_primary
