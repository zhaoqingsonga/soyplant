#只有单株进入株行，单株进入株行时，世代不增加
get_line <- function(my_plant, prefix = "ZH") {
  my_line <- subset(my_plant, my_plant$next_stage == "株行")
  #处理各列
  my_line$id <- get_ID(n1 = 1, n2 = nrow(my_line))

  my_line$source <- my_line$stageid

  my_line$stageid <-
    get_prefix_linename(
      prefix = prefix,
      n1 = 1,
      n2 = nrow(my_line),
      digits = 4
    )
  my_line$stage <- "株行"
  my_line$next_stage <- "初级产比"
  my_line$f <- my_line$f + 1
  my_line$sele <- 5
  my_line$process <- paste(my_line$process, my_line$id, sep = "/")
  #
  rownames(my_line) <- NULL
  return(my_line)
}

# my_line <- get_line(my_plant, prefix = "ZH26")
# my_line
