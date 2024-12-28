#' 移除对照中亩产异常值
#'
#' 该函数从数据框中提取`is_ck`为1的对照组数据，使用IQR（四分位距）方法判断异常值，并将异常值设为NA。
#' 异常值的判定是基于对照组数据的第一四分位数（Q1）和第三四分位数（Q3），异常值定义为低于Q1 - 1.5 * IQR 或 高于Q3 + 1.5 * IQR的值。
#'
#' @param df 数据框，包含两列：`muchan`（产量数据）和`is_ck`（是否为对照组，0为非对照，1为对照）。
#' @return 返回处理后的数据框，其中对照组的异常值被替换为NA。
#' @examples
#' df <- data.frame(
#'   muchan = c(10, 12, 15, 8, 25, 30, 35, 100, 20, 18),
#'   is_ck = c(0, 1, 1, 0, 1, 1, 0, 1, 0, 1)
#' )
#' df_cleaned <- remove_ck_outliers(df)
#' print(df_cleaned)
remove_ck_outliers <- function(df) {
  # 提取is_ck为1的对照组数据
  control_data <- df[df$is_ck == 1, "muchan"]

  # 计算四分位数和IQR
  Q1 <- quantile(control_data, 0.25,na.rm = TRUE)
  Q3 <- quantile(control_data, 0.75,na.rm = TRUE)
  IQR_value <- Q3 - Q1

  # 定义异常值的上下限
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value

  # 将异常值设为NA
  df$muchan[df$is_ck == 1 & (df$muchan < lower_bound | df$muchan > upper_bound)] <- NA

  return(df)
}

#' 计算增产百分比并返回排序结果
#'
#' 该函数根据两种方式计算增产百分比：一种是与最近两个对照点比较，另一种是与所有非空对照的平均值比较。函数还会返回每种方式的增产百分比，并为每种方式计算位次。
#'
#' @param df 数据框，必须包含以下列：
#'   \itemize{
#'     \item muchan: 产量数据
#'     \item is_ck: 是否为对照组，值为1表示对照组，其他值表示实验组
#'   }
#'
#' @return 返回数据框，新增以下列：
#'   \itemize{
#'     \item 较临近对照增产%: 与最近两个对照点的增产百分比
#'     \item 较平均对照增产%: 与所有非空对照的增产百分比
#'     \item 较临近对照位次: 根据增产百分比与最近两个对照点比较的位次
#'     \item 较平均对照位次: 根据增产百分比与所有非空对照比较的位次
#'   }
#'
#' @examples
#' df <- data.frame(
#'   muchan = c(10, 20, 15, 25, 30, 10),
#'   is_ck = c(1, 0, 1, 0, 0, 1)
#' )
#' result <- calculate_muchan_increase_with_multiple_methods(df)
#' print(result)
#'
#' @export
calculate_muchan_increase_with_multiple_methods <- function(df) {
  # 检查是否有至少一个非空的对照组数据
  if (sum(df$is_ck == 1 & !is.na(df$muchan)) == 0) {
    df$jiaolinjinduizhaozengchan<-NA
    df$jiaopingjunduizhaozengchan<-NA
    df$jiaolinjinduizhaoweici<-NA
    df$jiaopingjunduizhaoweici<-NA
    return(df)
  }

  # 移除对照中的异常值
  df <- remove_ck_outliers(df)

  # 获取所有非空对照的索引
  control_indices <- which(df$is_ck == 1 & !is.na(df$muchan))

  # 计算所有非空对照的平均产量
  control_avg <- mean(df$muchan[control_indices])

  # 创建两个向量存储增产百分比：一种是与最近两个对照点比较，另一种是与所有非空对照的平均值比较
  increase_percent_near_controls <- numeric(nrow(df))
  increase_percent_all_controls <- numeric(nrow(df))

  # 遍历数据框
  for (i in 1:nrow(df)) {
    if (df$is_ck[i] == 1) {
      # 如果当前是对照组，跳过计算增产
      increase_percent_near_controls[i] <- NA
      increase_percent_all_controls[i] <- NA
    } else {
      # 获取当前产量
      muchan <- df$muchan[i]

      # 计算与最近两个对照点的增产百分比
      distances <- abs(control_indices - i)
      sorted_indices <- order(distances)
      closest_controls <- control_indices[sorted_indices[1:2]]
      control_avg_near_controls <- mean(df$muchan[closest_controls])
      increase_percent_near_controls[i] <- (muchan - control_avg_near_controls) / control_avg_near_controls * 100

      # 计算与所有非空对照的增产百分比
      increase_percent_all_controls[i] <- (muchan - control_avg) / control_avg * 100
    }
  }

  # 将增产百分比加入数据框
  df$jiaolinjinduizhaozengchan <- as.numeric(increase_percent_near_controls)
  df$jiaopingjunduizhaozengchan <- as.numeric(increase_percent_all_controls)

  # 根据增产百分比排序，并生成位次列（分别计算两种方式的位次）
  df$jiaolinjinduizhaoweici <- rank(-df$jiaolinjinduizhaozengchan, na.last = "keep", ties.method = "min")
  df$jiaopingjunduizhaoweici <- rank(-df$jiaopingjunduizhaozengchan, na.last = "keep", ties.method = "min")

  return(df)
}




#' 判断数据中的异常值
#'
#' 该函数通过四分位数间距法（IQR）或标准差法（SD）来判断数据中的异常值。支持两种方法：IQR法和标准差法。
#'
#' @param muchan 向量，包含需要检测异常值的产量数据，可以包含NA值。
#' @param method 字符串，指定使用的方法。默认为 "IQR"，可以选择 "SD" 来使用标准差法。IQR方法基于四分位数间距，SD方法基于均值和标准差。
#'
#' @return 返回一个逻辑向量，指示哪些数据点是异常值（TRUE表示异常值，FALSE表示正常值）。对于NA值，默认会被判定为异常值。
#'
#' @details
#'   - 如果使用IQR方法，异常值是指低于 `Q1 - 1.5 * IQR` 或高于 `Q3 + 1.5 * IQR` 的值，其中 Q1 和 Q3 分别是第一四分位数和第三四分位数，IQR 是四分位数间距。
#'   - 如果使用标准差法（SD），异常值是指低于 `mean - 3 * SD` 或高于 `mean + 3 * SD` 的值，其中 mean 是均值，SD 是标准差。
#'
#' @examples
#' # 示例：使用IQR方法检测异常值
#' muchan_data <- c(10, 20, 15, 25, 30, 100, 5)
#' outliers_iqr <- find_outliers(muchan_data, method = "IQR")
#' print(outliers_iqr)
#'
#' # 示例：使用标准差方法检测异常值
#' outliers_sd <- find_outliers(muchan_data, method = "SD")
#' print(outliers_sd)
#'
#' @export
find_outliers <- function(muchan, method = "IQR") {
  # 去除NA值，计算有效数据
  valid_data <- muchan[!is.na(muchan)]

  # 使用四分位数间距法（IQR）
  if (method == "IQR") {
    # 计算Q1, Q3, IQR
    q1 <- quantile(valid_data, 0.25)
    q3 <- quantile(valid_data, 0.75)
    iqr <- q3 - q1

    # 计算上下边界
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr

    # 判断哪些是异常值
    outliers <- muchan < lower_bound | muchan > upper_bound | is.na(muchan)

    # 使用标准差法（SD）
  } else if (method == "SD") {
    # 计算均值和标准差
    mean_value <- mean(valid_data)
    sd_value <- sd(valid_data)

    # 计算上下边界
    lower_bound <- mean_value - 3 * sd_value
    upper_bound <- mean_value + 3 * sd_value

    # 判断哪些是异常值
    outliers <- muchan < lower_bound | muchan > upper_bound | is.na(muchan)

  } else {
    stop("无效的方法参数，只能是 'IQR' 或者 'SD'")
  }

  return(outliers)
}

#' 转化为日期的函数
#'
#' 该函数用于将数据框中符合日期格式（"xxxx-xx-xx"）的字符列转换为日期类型。
#' 它会检查每一列是否为字符类型，并确保该列的非空值符合日期格式。
#'
#' @param df 一个数据框，包含需要转换为日期格式的列。
#'
#' @return 返回一个与输入数据框结构相同的新的数据框，其中符合条件的字符列已被转换为日期类型。
#'
#' @examples
#' df <- data.frame(
#'   date_col = c("2020-01-01", "2021-02-02", "2022-03-03"),
#'   num_col = c(1, 2, 3)
#' )
#' df_converted <- convert_columns_to_date(df)
#' print(df_converted)
#'
#' @export
convert_columns_to_date <- function(df) {
  # 遍历所有列，找出需要转换的列
  cols_to_convert <- sapply(df, function(col) {
    # 检查列是否是字符类型
    is.character(col) &&
      # 确认非空值均匹配 "xxxx-xx-xx" 格式
      all(is.na(col) | grepl("^\\d{4}-\\d{2}-\\d{2}$", col))
  })

  # 如果有符合条件的列
  if (any(cols_to_convert)) {
    df[cols_to_convert] <- lapply(df[cols_to_convert], function(col) {
      # 将符合条件的列转换为日期类型
      as.Date(col, format = "%Y-%m-%d")
    })
  }
  return(df)
}

#' 对大豆数据进行汇总
#'
#' 该函数用于对大豆数据进行汇总，包括：
#'  * 筛选出重复次数为1的数据
#'  * 去除需要汇总的数值型列
#'  * 对数值型列进行分组汇总，计算平均值
#'  * 将汇总结果合并回原始数据
#'
#' @param updated 数据框，包含原始大豆数据
#' @param soy_traits 数据框，包含大豆性状信息，用于指定需要汇总的数值型列
#' @return 返回一个数据框，包含汇总后的数据
#'
summarize_data <- function(updated, soy_traits) {
  # 将数据转换为数据框
  updated <- as.data.frame(updated)
  # 获取原始列名
  original_colnames <- colnames(updated)

  # 筛选出重复次数为1的数据
  updatedrp1 <- subset(updated, rp == 1)

  # 获取需要汇总的数值型列名
  numeric_cols <- soy_traits$name_Lib[soy_traits$field_type == "N"]
  numeric_cols <-as.character(numeric_cols)
  #将updated分为两部分
  #不用汇总部分字段
  no_summary_name<-original_colnames[!original_colnames%in%numeric_cols]
  summary_name<-original_colnames[original_colnames%in%numeric_cols]

  # 选择要保留的列,非数字部分
  updatedrp1 <- updatedrp1[no_summary_name]

  # 选择数字列
  numeric_data<- updated[c("code",summary_name)]

  # 分类汇总，计算平均值
  numeric_summary <- numeric_data %>%
    group_by(code) %>%
    summarize_all(~ round(mean(., na.rm = TRUE), 2))

  # 将汇总结果合并回原始数据
  updatedrp1 <- merge(updatedrp1, numeric_summary, by = "code", all.x = TRUE)

  # 按照原始列名顺序返回结果
  return(updatedrp1[original_colnames])
}



#将特定的字段转化为数据类型，columns_to_convert，为向量，为数据类型字段名。
convert_specified_columns_to_numeric <- function(input_df, columns_to_convert) {
  # 遍历数据框的列名
  for (col_name in columns_to_convert) {
    # 检查列是否存在于数据框中
    if (col_name %in% names(input_df)) {
      # 将该列转换为数字类型，忽略无法转换的元素（转为NA）
      input_df[[col_name]] <- as.numeric(input_df[[col_name]])
    }
  }
  return(input_df)
}



