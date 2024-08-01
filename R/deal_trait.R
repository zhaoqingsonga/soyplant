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
  result <- data.frame(fieldid = character(), non_fieldid = character(), stringsAsFactors = FALSE)

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
        result <- rbind(result, data.frame(fieldid = temp_f, non_fieldid = element, stringsAsFactors = FALSE))
      }
    }
  }

  return(result)
}

get_clipboard<-function(){
  read.table("clipboard",header=TRUE)
}


# 加载必要的包
library(dplyr)
library(tidyr)

#' 根据 fieldid 和 xingzhuang 组合生成 class 展示矩阵
#'
#' 这个函数接受一个数据框，并生成一个以 fieldid 和 xingzhuang 为行列的宽格式数据框，
#' 其中包含每个组合的 class 值。如果有重复的 (fieldid, xingzhuang) 组合，将保留最后一个记录。
#'
#' @param df 一个数据框，必须包含以下列：fieldid, xingzhuang 和 class。
#' @return 返回一个宽格式的数据框，以 fieldid 和 xingzhuang 为行列，显示 class 值。
#' @examples
#' df <- data.frame(
#'   non_fieldid = c("12_2_Excellent", "20_2_Brown", "20_99_Segregating", "36_1_Lanceolate", "36_2_Ovate", "36_4_Round"),
#'   fieldid = c("f246oa2H01028", "f246oa2H01028", "f246oa2H01028", "f246oa2H01059", "f246oa2H01059", "f246oa2H01059"),
#'   xingzhuang = c("huaqipingjia", "rongmaose", "rongmaose", "yexing", "yexing", "yexing"),
#'   class = c("优", "棕", "分离", "披针", "卵圆", "圆"),
#'   class_C = c("Excellent", "Brown", "Segregating", "Lanceolate", "Ovate", "Round"),
#'   trait_code = c(12, 20, 20, 36, 36, 36),
#'   class_code = c(2, 2, 99, 1, 2, 4),
#'   number = c(19, 51, 52, 88, 89, 91)
#' )
#' create_class_matrix(df)
#' @export
create_class_matrix <- function(df) {
  # 确保输入是数据框
  if (!is.data.frame(df)) stop("输入必须是数据框")

  # 检查是否包含所需的列
  required_cols <- c("fieldid", "xingzhuang", "class")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop("数据框缺少以下必要的列: ", paste(missing_cols, collapse = ", "))
  }

  # 去除重复的 (fieldid, xingzhuang) 组合，保留最后一个记录
  df_unique <- df %>%
    group_by(fieldid, xingzhuang) %>%
    slice_tail(n = 1) %>%
    ungroup()

  # 选择所需的列
  df_selected <- df_unique %>%
    select(fieldid, xingzhuang, class)

  # 将数据转换为宽格式，以 fieldid 和 xingzhuang 为行列，展示 class
  df_wide <- df_selected %>%
    pivot_wider(names_from = xingzhuang, values_from = class, values_fill = list(class = NA))

  return(df_wide)
}










