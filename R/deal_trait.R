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

#
# create_class_matrix <- function(df) {
#   # 确保输入是数据框
#   if (!is.data.frame(df)) stop("输入必须是数据框")
#
#   # 检查是否包含所需的列
#   required_cols <- c("fieldid", "xingzhuang", "class")
#   missing_cols <- setdiff(required_cols, names(df))
#
#   if (length(missing_cols) > 0) {
#     stop("数据框缺少以下必要的列: ", paste(missing_cols, collapse = ", "))
#   }
#
#   # 去除重复的 (fieldid, xingzhuang) 组合，保留最后一个记录
#   df_unique <- df %>%
#     group_by(fieldid, xingzhuang) %>%
#     slice_tail(n = 1) %>%
#     ungroup()
#
#   # 选择所需的列
#   df_selected <- df_unique %>%
#     select(fieldid, xingzhuang, class)
#
#   # 将数据转换为宽格式，以 fieldid 和 xingzhuang 为行列，展示 class
#   df_wide <- df_selected %>%
#     pivot_wider(names_from = xingzhuang, values_from = class, values_fill = list(class = NA))
#
#   return(df_wide)
# }



#' 将数据框转换为特定的宽格式数据框
#'
#' 该函数接受一个数据框作为输入，对其进行一系列的数据处理操作，包括检查列的完整性、去除重复组合以及转换为宽格式，最终返回处理后的宽格式数据框。主要用于根据 `fieldid` 和 `name_code` 列将数据展开，便于后续分析和展示。
#'
#' @param df 输入的数据框，该数据框需要包含 `fieldid`、`name_code` 和 `level_C` 这几个列，用于后续的数据处理和格式转换操作。
#'
#' @return 返回一个宽格式的数据框，行索引为 `fieldid`，列名为 `name_code`，对应的值为 `level_C` 的值，若某 `(fieldid, name_code)` 组合不存在对应的值，则用 `NA` 填充。
#'
#' @examples
#' # 创建一个示例数据框（实际应用中替换为真实数据框）
#' df_example <- data.frame(
#'   fieldid = c(1, 1, 2, 2),
#'   name_code = c("T001", "T002", "T001", "T003"),
#'   level_C = c("A", "B", "C", "D")
#' )
#' result <- create_class_matrix(df_example)
#' print(result)
#'
#' @importFrom dplyr group_by slice_tail ungroup select pivot_wider
#' @importFrom rlang setdiff
#' @export
create_class_matrix <- function(df) {
  # 确保输入是数据框
  if (!is.data.frame(df)) stop("输入必须是数据框")

  # 检查是否包含所需的列
  required_cols <- c("fieldid", "name_code", "level_C")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop("数据框缺少以下必要的列: ", paste(missing_cols, collapse = ", "))
  }

  # 去除重复的 (fieldid, name_code) 组合，保留最后一个记录
  df_unique <- df %>%
    group_by(fieldid, name_code) %>%
    slice_tail(n = 1) %>%
    ungroup()

  # 选择所需的列
  df_selected <- df_unique %>%
    select(fieldid, name_code, level_C)

  # 将数据转换为宽格式，以 为行列，展示
  df_wide <- df_selected %>%
    pivot_wider(names_from = name_code, values_from = level_C, values_fill = list(level_C = NA))
  return(df_wide)
}




#' 读取文件夹中所有 Excel 文件的 planting 表，并处理字段
#'
#' 该函数会读取指定文件夹中的所有 Excel 文件的 `planting` 表，确保表中的字段顺序为
#' "id", "user", "stageid", "name", "ma", "pa", "mapa", "memo", "stage", "next_stage", "f"。
#' 如果缺少某些字段，则补齐这些字段并填充 NA；如果有多余的字段，则删除这些字段。
#'
#' @param folder_path 字符串，表示文件夹的路径。
#'
#' @return 一个数据框，包含所有 Excel 文件的 `planting` 表合并后的结果。
#'
#' @export
read_and_process_planting_tables <- function(folder_path) {
  # 定义所需字段的顺序
  required_columns <- as.character(field$name)

  # 获取文件夹中所有 Excel 文件的文件路径
  file_list <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

  # 初始化一个空的列表，用于存储所有表的数据
  all_tables <- list()

  # 循环遍历每个文件
  for (file in file_list) {
    # 尝试读取文件中的 planting 表
    tryCatch({
      # 读取 planting 表
      df <- read_excel(file, sheet = "planting")

      # 补齐缺失的字段
      for (col in required_columns) {
        if (!col %in% names(df)) {
          df[[col]] <- NA
        }
      }

      # 只保留所需的字段，并按顺序排列
      df <- df[required_columns]

      # 将处理后的表添加到列表中
      all_tables[[length(all_tables) + 1]] <- df

    }, error = function(e) {
      message(paste("Error reading file:", file, "-", e$message))
    })
  }

  # 合并所有表
  final_df <- bind_rows(all_tables)

  return(final_df)
}





