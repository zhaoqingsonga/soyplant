get_advanced <- function(my_primary,start_num=1) {
  my_advanced <- subset(my_primary, my_primary$next_stage == "高级产比")
  #处理各列
  my_advanced$id <- generate_id(start_num,  end_num = nrow(my_advanced)+start_num-1)
  my_advanced$source <- my_advanced$name
  #处理名称小于9代时处理，大于9代时不再追加
  my_advanced$name <-
    ifelse(my_advanced$f<9,paste(my_advanced$name, ":", (my_advanced$f + 1),  sep = ""),my_advanced$name)


  my_advanced$stage <- "高级产比"
  my_advanced$next_stage <- "多点鉴定"
  my_advanced$f <- my_advanced$f + 1
  my_advanced$sele <- 5
  my_advanced$process <-
    paste(my_advanced$process, my_advanced$id, sep = "/")
  my_advanced$path <- paste(my_advanced$path, 0, sep = "-")
  #
  rownames(my_advanced) <- NULL
  return(my_advanced)
}

# my_advanced <- get_advanced(my_primary)
#
# my_advanced
