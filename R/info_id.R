
#获得电脑nodename
get_computer_nodename <- function() {
  return(Sys.info()["nodename"])
}

#' Get the next ID from a primary ID
#'
#' This function extracts the current ID and returns the next ID.
#'
#' @param my_primary A data frame containing primary IDs.
#' @return A list containing the next numeric part of the ID and the prefix part of the ID.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' my_primary <- data.frame(ID = c("g24040412094300123", "g24040412094300124"))
#' result <- get_next_id(my_primary)
#' }
#'
#' @export
get_next_id <- function(my_primary) {
  # Extract the last ID from the data frame
  mystr <- my_primary[nrow(my_primary), "ID"]

  # Extract the prefix and numeric parts of the ID
  id_p <- substr(mystr, 1, 13)
  id_n <- as.numeric(substr(mystr, 14, 18)) + 1

  # Return the next numeric part and prefix part of the ID as a list
  return(list(id_n = id_n, id_p = id_p))
}



# 将一个数字转换为62进制表示
to_base62 <- function(num) {
  base62_chars <- c(0:9, letters, LETTERS)
  result <- ""
  repeat {
    remainder <- num %% 62
    result <- paste0(base62_chars[remainder + 1], result)
    num <- num %/% 62
    if (num == 0) break
  }
  return(result)
}

#' 根据指定参数生成ID序列
#'
#' 该函数根据指定的参数生成一系列ID。
#'
#' @param start_num 整数，表示ID序列的起始号码。
#' @param end_num 整数，表示ID序列的结束号码。
#' @param char 字符，放在每个ID的开头。
#' @param digit_length 整数，指定数字部分的长度，控制填充。
#' @param include_datetime 逻辑值，是否在ID中包含当前的年份、月份、日期、小时、分钟和秒。
#'
#' @return 一个包含所有生成ID的向量，从start_num到end_num。
#'
#' @examples
#' \dontrun{
#' start <- 123
#' end <- 135
#' character <- "A"
#' digit_length <- 5
#' result_ids <- generate_id(start, end, character, digit_length, include_datetime = TRUE)
#' for (result_id in result_ids) {
#'   print(paste("生成的ID:", result_id))
#' }
#' }
#'
#' @export
generate_id <- function(start_num = 1, end_num=10, char = "g", digit_length = 5, include_datetime = TRUE) {
  # 如果需要，生成日期时间字符串
  current_time <- ""
  if (include_datetime) {
    current_time <- format(Sys.time(), "%y")
    current_time <- paste0(current_time,
                           to_base62(as.numeric(format(Sys.time(), "%m"))),
                           to_base62(as.numeric(format(Sys.time(), "%d"))),
                           to_base62(as.numeric(format(Sys.time(), "%H"))),
                           to_base62(as.numeric(format(Sys.time(), "%M"))),
                           to_base62(as.numeric(format(Sys.time(), "%S"))))
  }

  # 生成ID序列
  ids <- vector(mode = "character", length = (end_num - start_num + 1))
  for (i in seq_len(end_num - start_num + 1)) {
    num_part <- sprintf(paste0("%0", digit_length, "d"), start_num + (i - 1))
    ids[i] <- paste0(char, current_time, num_part)
  }

  return(ids)
}

generate_stageid <- function(start_num=1, end_num, char="24GC", digit_length=4) {
  # Generate a series of IDs, each consisting of specified parameters
  ids <- c()  # Store the generated IDs
  # Iterate through the range of numbers to generate IDs
  for (num in start_num:end_num) {
    # Convert the number to a string with the specified number of digits
    num_str <- sprintf(paste0("%0", digit_length, "d"), num)
    # Construct the ID and add it to the vector
    ids <- c(ids, paste0(char, num_str))
  }

  return(ids)
}



