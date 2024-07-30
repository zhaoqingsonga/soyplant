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
