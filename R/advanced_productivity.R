get_advanced <- function(my_primary, prefix = "JD") {
  my_advanced <- subset(my_primary, my_primary$next_stage == "高级产比")
  #处理各列
  my_advanced$id <- get_ID(n1 = 1, n2 = nrow(my_advanced))

  my_advanced$source <- my_advanced$stageid
  #处理名称
  if (!all(my_advanced$stage == "株行")) {
    my_advanced$name <-
      paste(sapply(my_advanced$name, function(x)
        substr(x, 1, nchar(x) - 1)),
        (my_advanced$f + 1),
        sep = "")
  } else{
    my_advanced$name <-
      paste(my_advanced$name, "F", (my_advanced$f + 1),  sep = "")
  }

  my_advanced$stageid <-
    get_prefix_linename(
      prefix = prefix,
      n1 = 1,
      n2 = nrow(my_advanced),
      digits = 3
    )
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

# my_advanced <- get_advanced(my_primary, "JD27")
#
# my_advanced
