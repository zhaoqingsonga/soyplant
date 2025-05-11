
#' 转换二维表为母本与父本配对的数据框
#'
#' 此函数将输入的二维表（数据框）转换为一个数据框，其中包括母本（第一列）和父本（其余列）之间的配对关系。结果数据框包含三列：母本 (`ma`)、父本 (`pa`) 和备注 (`memo`)，其中备注列的值为 `NA`。
#'
#' @param data_frame_input 一个数据框，第一列为母本，其余列为父本，内容为0或1，表示母本与父本的配对关系。
#' @return 返回一个数据框，包括三列：
#' \item{ma}{母本的名称}
#' \item{pa}{父本的名称}
#' \item{memo}{备注列，所有值为NA}
#' @examples
#' # 创建一个示例数据框
#' example_df <- data.frame(
#'   Mother = c("Mother1", "Mother2", "Mother3"),
#'   Father1 = c(1, 0, 1),
#'   Father2 = c(0, 1, 0),
#'   Father3 = c(1, 0, 0),
#'   Father4 = c(0, 1, 0),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 使用函数转换示例数据框
#' result <- convert_to_pairs(example_df)
#' print(result)
# convert_to_pairs_old <- function(data_frame_input) {
#   # 检查输入是否为数据框
#   if (!is.data.frame(data_frame_input)) {
#     stop("输入必须是一个数据框")
#   }
#
#   # 确保数据框的列数大于1
#   if (ncol(data_frame_input) < 2) {
#     stop("数据框必须至少包含两列")
#   }
#
#   # 获取母本和父本的名称
#   mother_names <- data_frame_input[[1]]
#   father_names <- colnames(data_frame_input)[-1]  # 排除第一列的母本名称
#
#   # 创建一个空的数据框来存储结果
#   result_list <- list()
#
#   # 遍历每一行数据
#   for (i in 1:nrow(data_frame_input)) {
#     # 获取当前母本名称
#     mother <- mother_names[i]
#
#     # 找到当前母本对应的所有父本
#     father_indices <- which(data_frame_input[i, -1] == 1)
#
#     # 获取对应的父本名称
#     fathers <- father_names[father_indices]
#
#     # 将母本和父本配对添加到结果列表中，并且设置"memo"列为 NA
#     result_list <- append(result_list, lapply(fathers, function(father) data.frame(ma = mother, pa = father, memo = NA, stringsAsFactors = FALSE)))
#   }
#
#   # 合并结果列表为一个数据框
#   result_df <- do.call(rbind, result_list)
#
#   return(result_df)
# }

