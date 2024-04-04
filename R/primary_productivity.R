

get_primary <- function(my_line,
                        start_num = 1) {
  my_primary <- subset(my_line, my_line$next_stage == "初级产比")
  #处理各列
  my_primary$id <-
    generate_id(start_num,
           end_num = nrow(my_primary) +start_num - 1)

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
