#' Write Data Frame to SQLite Database
#'
#' This function writes a data frame to a specified table in an SQLite database.
#'
#' @param df A data frame to be written to the SQLite database.
#' @param db_path A string specifying the path to the SQLite database file.
#' @param table_name A string specifying the name of the table to write the data to.
#' @param overwrite A logical value indicating whether to overwrite the table if it exists. Default is FALSE.
#' @param append A logical value indicating whether to append the data to the table if it exists. Default is FALSE.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
#' write_df_to_sqlite(df, "my_database.sqlite", "my_table", overwrite = TRUE)
#' }
write_df_to_sqlite <- function(df, db_path, table_name, overwrite = FALSE, append = FALSE) {
  # Load necessary packages
  library(DBI)
  library(RSQLite)

  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

  # Write the data frame to the specified table
  dbWriteTable(con, table_name, df, overwrite = overwrite, append = append, row.names = FALSE)

  # Disconnect from the database
  dbDisconnect(con)
}

# Example usage:
# df <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
# write_df_to_sqlite(df, "my_database.sqlite", "my_table", overwrite = TRUE)


#' Read Data Frame from SQLite Database
#'
#' This function reads data from a specified table in an SQLite database and returns it as a data frame.
#'
#' @param db_path A string specifying the path to the SQLite database file.
#' @param table_name A string specifying the name of the table to read the data from.
#'
#' @return A data frame containing the data read from the specified table.
#'
#' @examples
#' \dontrun{
#' df <- read_df_from_sqlite("my_database.sqlite", "my_table")
#' print(df)
#' }
read_df_from_sqlite <- function(db_path, table_name) {
  # Load necessary packages
  library(DBI)
  library(RSQLite)

  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

  # Read the data from the specified table
  df <- dbReadTable(con, table_name)

  # Disconnect from the database
  dbDisconnect(con)

  # Return the data frame
  return(df)
}



#' 从数据框更新SQLite数据库表
#'
#' 该函数用于从数据框更新SQLite数据库表。
#'
#' @param db_path 字符串，SQLite数据库的文件路径。
#' @param table_name 字符串，要更新的表名。
#' @param data_df 数据框，包含要更新的数据。数据框的第一列必须是fieldid。
#' @return 无返回值。如果更新成功，则提交事务；如果发生错误，则回滚事务。
#' @examples
#' \dontrun{
#' db_path <- "path/to/database.db"
#' table_name <- "my_table"
#' data_df <- data.frame(fieldid = 1:3, value = c("A", "B", "C"))
#' update_table_from_df(db_path, table_name, data_df)
#' }
update_table_from_df <- function(db_path, table_name, data_df) {
  # 连接到SQLite数据库
  conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

  # 开始一个事务
  dbBegin(conn)

  # 获取列名
  columns <- colnames(data_df)

  # 确保数据框的第一个列是ID字段
  if (columns[1] != "fieldid") {
    stop("数据框的第一个列必须是fieldid")
  }

  # 构造SET部分
  set_clause <- paste0(columns[-1], " = ?", collapse = ", ")

  # 创建SQL更新语句
  sql <- paste0("UPDATE ", table_name, " SET ", set_clause, " WHERE fieldid = ?")

  # 执行更新操作
  tryCatch({
    for (i in 1:nrow(data_df)) {
      # 提取字段ID和更新值
      row <- data_df[i, ]
      fieldid <- row[1]
      values <- row[-1]

      # 确保参数顺序与SQL占位符顺序一致
      params <- as.vector(unlist(c(values, fieldid)))

      # 执行SQL更新
      dbExecute(conn, sql, params = params)
    }
    # 提交事务
    dbCommit(conn)
  }, error = function(e) {
    # 出现错误时回滚事务
    dbRollback(conn)
    stop("更新操作失败: ", e$message)
  }, finally = {
    # 关闭连接
    dbDisconnect(conn)
  })
}



