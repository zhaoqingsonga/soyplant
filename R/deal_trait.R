# 加载必要的包
library(dplyr)

# 定义函数
merge_data_frames_by_row <- function(...) {
  # 获取所有传入的参数（数据框）
  data_frames <- list(...)

  # 初始化结果数据框为第一个数据框
  combined_data <- data_frames[[1]]

  # 依次左连接其余的数据框
  for (i in 2:length(data_frames)) {
    combined_data <- left_join(combined_data, data_frames[[i]], by = "id")
  }

  return(combined_data)
}


#' 创建配对数据框
#'
#' 该函数遍历输入数据框中的每个元素，如果元素以 "f" 开头，接着是任意7个字符，然后是5个数字，
#' 则将其存入一个临时变量。如果元素不匹配该模式，则将临时变量和该元素组合，并存入一个新的数据框中。
#'
#' @param df 一个包含元素的单列数据框
#' @return 一个包含两个变量的数据框，一个变量是以 "f" 开头的元素，另一个变量是不以 "f" 开头的元素
#' @examples
#' elements_df <- data.frame(elements = c("fabcdefg12345", "bar", "baz", "fghijklm67890", "buzz", "buzzie", "fnopqrst54321", "buzzbuzz"), stringsAsFactors = FALSE)
#' pairs_df <- create_multiple_pairs_from_df(elements_df)
#' print(pairs_df)
#' @export
create_multiple_pairs_from_df <- function(df) {
  # 确认输入的是data frame并且有一列
  if (!is.data.frame(df) || ncol(df) != 1) {
    stop("输入必须是一个只有一列的data frame")
  }

  # 初始化临时变量和结果数据框
  temp_f <- NULL
  result <- data.frame(f_elements = character(), non_f_elements = character(), stringsAsFactors = FALSE)

  # 正则表达式，匹配f开头，后跟7个任意字符，最后是5位数字
  pattern <- "^f.{7}\\d{5}$"

  # 遍历数据框中的每个元素
  for (element in df[[1]]) {
    if (grepl(pattern, element)) {
      # 如果元素匹配正则表达式，存入临时变量
      temp_f <- element
    } else {
      # 如果元素不匹配，并且临时变量不为空，则将临时变量与当前元素组合
      if (!is.null(temp_f)) {
        result <- rbind(result, data.frame(f_elements = temp_f, non_f_elements = element, stringsAsFactors = FALSE))
      }
    }
  }

  #result<-merge(trmd,qr_trait,all.x=TRUE,by.x="non_f_elements",by.y="traitid")
  #result<-result[,1:4]
  return(result)
}
