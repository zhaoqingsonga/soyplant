get_primary <- function(my_line, prefix = "JD") {
  my_primary <- subset(my_line, my_line$next_stage == "初级产比")
  #处理各列
  my_primary$id <- get_ID(n1 = 1, n2 = nrow(my_primary))

  my_primary$source <- my_primary$stageid
  #处理名称
  if (!all(my_primary$stage == "株行")) {
    my_primary$name <-
      paste(sapply(my_primary$name, function(x)
        substr(x, 1, nchar(x) - 1)),
        (my_primary$f + 1),
        sep = "")
  } else{
    my_primary$name <-
      paste(my_primary$name, "F", (my_primary$f + 1),  sep = "")
  }

  my_primary$stageid <-
    get_prefix_linename(
      prefix = prefix,
      n1 = 1,
      n2 = nrow(my_primary),
      digits = 3
    )
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

# my_primary <- get_primary(my_line, "GC27")
# my_primary
