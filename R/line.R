#只有单株进入株行，单株进入株行时，世代不增加
get_line <- function(my_plant,start_num=1) {
  my_line <- subset(my_plant, my_plant$next_stage == "株行")
  #处理各列
  my_line$id <- generate_id(start_num, end_num = nrow(my_line)+start_num-1)

  #my_line$source <- my_line$name
  my_line$stage <- "株行"
  my_line$next_stage <- "初级产比"
  my_line$f <- my_line$f + 1
  my_line$sele <- 5
  my_line$process <- paste(my_line$process, my_line$id, sep = "/")
  #
  rownames(my_line) <- NULL
  return(my_line)
}

# my_line <- get_line(my_plant)
# my_line
