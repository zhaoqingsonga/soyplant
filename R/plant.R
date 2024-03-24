get_plant <- function(my_pop,prefix="DZ") {
  my_pop<-subset(my_pop,my_pop$sele>0)
  new_df <- data.frame()
  # 循环遍历每一行，并按给定的次数复制
  for (i in 1:nrow(my_pop)) {
    # 使用rbind函数将复制的行添加到新的数据框中
    pop_name <- my_pop[i, ]$name
    path_name <- my_pop[i, ]$path
    source_name <- my_pop[i, ]$stageid
    for (j in 1:my_pop$sele[i]) {
      my_pop[i, ]$name <- paste(pop_name, j, sep = "-")
      my_pop[i, ]$path <- paste(path_name, j, sep = "-")
      my_pop[i, ]$source <- source_name
      new_df <- rbind(new_df, my_pop[i, ])
    }
  }
  # 重置行索引
  rownames(new_df) <- NULL
  #处理各列
  new_df$id <- get_ID(n1 = 1, n2 = nrow(new_df))
  new_df$stageid <-
    get_prefix_linename(
      prefix = prefix,
      n1 = 1,
      n2 = nrow(new_df),
      digits = 4
    )
  new_df$stage <- "单株"

  new_df$next_stage <- "株行"
  new_df$process <- paste(new_df$process, new_df$id, sep = "/")
  new_df$sele<-NA
  return(new_df)
}

# my_plant <- get_plant(my_pop,prefix="DZ25")
# my_plant


