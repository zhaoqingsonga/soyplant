

#' #' 生成性状表格
#' #'
#' #'返回openxlsx包createWorkbook()创建对象，一个表带验证功能的性状表
#' #' @param trait_col 数字，性状开始的列
#' #' @param validataion_number 数字，验证的行数
#' #' @return 返回openxlsx包createWorkbook()创建对象
#' #'
#' #'
#'
#' traitstable <- function(trait_col = 3,
#'                         validation_number = 100) {
#'   library(openxlsx)
#'   sj <- trait_col#性状开始列
#'   formated_rows <- validation_number#有效性验证行数
#'
#'   MSN <- "traits"
#'   traits <- soy_traits[order(soy_traits$order),]
#'   mysheetnames <- c(MSN)
#'
#'   hs1 <-
#'     createStyle(
#'       fgFill = "#DCE6F1",
#'       halign = "CENTER",
#'       textDecoration = "bold",
#'       border = "Bottom"
#'     )
#'   bodyStyle <-
#'     createStyle(border = "TopBottom", borderColour = "#4F81BD")
#'
#'   wb <- createWorkbook()
#'
#'   addWorksheet(wb, MSN, tabColour = "green")
#'
#'   number_of_traits <- nrow(traits)
#'   mytable <- MSN
#'   #表格中的列名
#'   conlns <-
#'     c(LETTERS,
#'       paste("A", LETTERS, sep = ""),
#'       paste("B", LETTERS, sep = ""))[1:number_of_traits]
#'   ##############add validational data
#'   for (i in 1:number_of_traits) {
#'     traiti <- as.vector(as.matrix(t(traits[i, 2:3])))
#'     leveli <- unlist(strsplit(as.character(traits$level_C[i]), "_"))
#'     length_leveli <- length(leveli) + 2
#'     #
#'     writeData(wb,
#'               mytable,
#'               traiti,
#'               startCol = sj + i,
#'               startRow = 1)
#'     SCol <- conlns[i]
#'
#'     #leveli中有中文时会有警告信息
#'     if (traits$field_type[i] == "C") {
#'       list_value<-paste(leveli,collapse = ",")
#'       list_value <- paste('"', list_value, '"', sep = "")
#'       #list_value<-'"Male,Female,Other"'
#'       dataValidation(
#'         wb,
#'         mytable,
#'         col = sj + i,
#'         rows = 3:(formated_rows + 2),
#'         type = "list",
#'         value = list_value,
#'         allowBlank = TRUE
#'       )
#'     }  else if (traits$field_type[i] == "D") {
#'       dataValidation(
#'         wb,
#'         mytable,
#'         col = sj + i,
#'         rows = 3:(formated_rows + 2),
#'         type = "date",
#'         operator = "greaterThanOrEqual",
#'         value = as.Date("2020-1-1")
#'       )
#'     }  else if (traits$field_type[i] == "N") {
#'       #以下这句有问题，不知道原因
#'       #conditionalFormatting(wb, mytable, cols = sj+i, rows = 1:500, type = "databar")
#'       dataValidation(
#'         wb,
#'         mytable,
#'         col = sj + i,
#'         rows = 3:(formated_rows + 2),
#'         type = "whole",
#'         operator = "between",
#'         value = as.numeric(leveli)
#'       )
#'
#'     }
#'
#'   }
#'   #########add style
#'   headerStyle <- hs1
#'   addStyle(
#'     wb,
#'     sheet = mytable,
#'     headerStyle,
#'     rows = 1:2,
#'     cols = 1:(16 + number_of_traits),
#'     gridExpand = TRUE
#'   )
#'   ## style for body
#'   ##设置内容主题的模板
#'
#'   addStyle(
#'     wb,
#'     sheet = mytable,
#'     bodyStyle,
#'     rows = 3:(formated_rows + 2),
#'     cols = 1:(16 + number_of_traits),
#'     gridExpand = TRUE
#'   )
#'   ####
#'   setColWidths(wb, mytable, cols = 1, widths = 10)
#'   setRowHeights(wb,
#'                 mytable,
#'                 rows = 1:(formated_rows + 2),
#'                 heights = 17.5)
#'
#'   return(wb)
#'
#' }
#'




#以下为原始，从level表中找校验项
#' 生成性状表格
#'
#'返回openxlsx包createWorkbook()创建对象，包括两个表，性状分类表和带验证功能的性状表
#' @param trait_col 数字，性状开始的列
#' @param validataion_number 数字，验证的行数
#' @return 返回openxlsx包createWorkbook()创建对象
#'
#
# traitstable <- function(trait_col = 3,
#                         validation_number = 100) {
#   library(openxlsx)
#   sj <- trait_col#性状开始列
#   formated_rows <- validation_number#有效性验证行数
#
#   MSN <- "traits"
#   traits <- soy_traits[order(soy_traits$order),]
#   mysheetnames <- c(MSN)
#
#   hs1 <-
#     createStyle(
#       fgFill = "#DCE6F1",
#       halign = "CENTER",
#       textDecoration = "bold",
#       border = "Bottom"
#     )
#   bodyStyle <-
#     createStyle(border = "TopBottom", borderColour = "#4F81BD")
#
#   wb <- createWorkbook()
#
#   addWorksheet(wb, MSN, tabColour = "green")
#   addWorksheet(wb, paste(MSN, "_level", sep = ""), tabColour = "blue",visible = FALSE)
#
#   number_of_traits <- nrow(traits)
#   mytable <- MSN
#   mytraits <- paste(mytable, "_level", sep = "")
#   #表格中的列名
#   conlns <-
#     c(LETTERS,
#       paste("A", LETTERS, sep = ""),
#       paste("B", LETTERS, sep = ""))[1:number_of_traits]
#   ##############add validational data
#   for (i in 1:number_of_traits) {
#     traiti <- as.vector(as.matrix(t(traits[i, 2:3])))
#     leveli <- unlist(strsplit(as.character(traits$level_C[i]), "_"))
#     length_leveli <- length(leveli) + 2
#     #
#     writeData(wb,
#               mytable,
#               traiti,
#               startCol = sj + i,
#               startRow = 1)
#     SCol <- conlns[i]
#     #
#     writeData(wb,
#               mytraits,
#               x = c(traiti, leveli),
#               startCol = SCol)
#     #leveli中有中文时会有警告信息
#     if (traits$field_type[i] == "C") {
#       list_value <-
#         paste(mytraits,
#               "!$",
#               SCol,
#               "$3:$",
#               SCol,
#               "$",
#               length_leveli + 1,
#               sep = "")
#       dataValidation(
#         wb,
#         mytable,
#         col = sj + i,
#         rows = 3:(formated_rows + 2),
#         type = "list",
#         value = list_value
#       )
#     }  else if (traits$field_type[i] == "D") {
#       dataValidation(
#         wb,
#         mytable,
#         col = sj + i,
#         rows = 3:(formated_rows + 2),
#         type = "date",
#         operator = "greaterThanOrEqual",
#         value = as.Date("2020-1-1")
#       )
#     }  else if (traits$field_type[i] == "N") {
#       #以下这句有问题，不知道原因
#       #conditionalFormatting(wb, mytable, cols = sj+i, rows = 1:500, type = "databar")
#       dataValidation(
#         wb,
#         mytable,
#         col = sj + i,
#         rows = 3:(formated_rows + 2),
#         type = "whole",
#         operator = "between",
#         value = as.numeric(leveli)
#       )
#
#   }
#
# }
# #########add style
# headerStyle <- hs1
# addStyle(
#   wb,
#   sheet = mytable,
#   headerStyle,
#   rows = 1:2,
#   cols = 1:(16 + number_of_traits),
#   gridExpand = TRUE
# )
# ## style for body
# ##设置内容主题的模板
#
# addStyle(
#   wb,
#   sheet = mytable,
#   bodyStyle,
#   rows = 3:(formated_rows + 2),
#   cols = 1:(16 + number_of_traits),
#   gridExpand = TRUE
# )
# ####
# setColWidths(wb, mytable, cols = 1, widths = 10)
# setRowHeights(wb,
#               mytable,
#               rows = 1:(formated_rows + 2),
#               heights = 17.5)
#
# return(wb)
#
# }


#' 生成性状表格
#'
#' 此函数用于创建一个包含特定格式和验证功能的性状表格工作簿，返回 `openxlsx` 包中 `createWorkbook()` 创建的对象，该工作簿包含一个表带验证功能的性状表。
#'
#' @param trait_col 数字，表示性状开始的列，默认为3，用于确定在表格中写入性状相关数据的起始列位置。
#' @param validataion_number 数字，表示验证的行数，默认为100，用于设定在表格中进行数据验证的行数范围。
#'
#' @return 返回 `openxlsx` 包中 `createWorkbook()` 创建的对象，即包含特定格式和验证功能的性状表格工作簿。
#'
#' @details
#' 函数内部主要执行以下操作：
#' 1. 首先加载 `openxlsx` 库，获取传入的性状开始列和验证行数参数，并定义一些常量，如工作表名称、样式等。
#' 2. 根据 `soy_traits` 数据（假设已存在）进行排序后获取性状数量，并确定表格中的列名。
#' 3. 循环遍历每个性状，将性状名称写入表格特定位置，并根据性状类型（字符型 `C`、日期型 `D`、数值型 `N`）进行不同的数据验证设置。
#'    - 对于字符型，将可能的取值拼接为字符串，设置为下拉列表形式的数据验证。
#'    - 对于日期型，设置为大于等于特定日期（`2020-01-01`）的数据验证。
#'    - 对于数值型，根据特定规则设置数据验证范围（当前代码中设置存在问题，需要进一步排查原因）。
#' 4. 为表格添加表头和内容的样式，设置列宽和行高，最后返回创建并设置好的工作簿对象。
#'
#' @examples
#' # 调用函数创建性状表格工作簿（使用默认参数）
#' wb <- traitstable()
#' # 可进一步对工作簿进行操作，如保存等
#' saveWorkbook(wb, "trait_table.xlsx")
#'
traitstable <- function(trait_col = 3,
                        validation_number = 100) {
  library(openxlsx)
  sj <- trait_col  # 性状开始列
  formated_rows <- validation_number  # 有效性验证行数

  MSN <- "traits"
  traits <- soy_traits[order(soy_traits$order), ]
  mysheetnames <- c(MSN)

  hs1 <-
    createStyle(
      fgFill = "#DCE6F1",
      halign = "CENTER",
      textDecoration = "bold",
      border = "Bottom"
    )
  bodyStyle <-
    createStyle(border = "TopBottom", borderColour = "#4F81BD")

  wb <- createWorkbook()

  addWorksheet(wb, MSN, tabColour = "green")

  number_of_traits <- nrow(traits)
  mytable <- MSN
  # 表格中的列名
  conlns <-
    c(LETTERS,
      paste("A", LETTERS, sep = ""),
      paste("B", LETTERS, sep = ""))[1:number_of_traits]
  ##############add validational data
  for (i in 1:number_of_traits) {
    traiti <- as.vector(as.matrix(t(traits[i, 2:3])))
    leveli <- unlist(strsplit(as.character(traits$level_C[i]), "_"))
    length_leveli <- length(leveli) + 2
    #
    writeData(wb,
              mytable,
              traiti,
              startCol = sj + i,
              startRow = 1)
    SCol <- conlns[i]

    # leveli中有中文时会有警告信息
    if (traits$field_type[i] == "C") {
      list_value <- paste(leveli, collapse = ",")
      list_value <- paste('"', list_value, '"', sep = "")
      # list_value<-'"Male,Female,Other"'
      dataValidation(
        wb,
        mytable,
        col = sj + i,
        rows = 3:(formated_rows + 2),
        type = "list",
        value = list_value,
        allowBlank = TRUE
      )
    }  else if (traits$field_type[i] == "D") {
      dataValidation(
        wb,
        mytable,
        col = sj + i,
        rows = 3:(formated_rows + 2),
        type = "date",
        operator = "greaterThanOrEqual",
        value = as.Date("2020-1-1")
      )
    }  else if (traits$field_type[i] == "N") {
      # 以下这句有问题，不知道原因
      # conditionalFormatting(wb, mytable, cols = sj+i, rows = 1:500, type = "databar")
      dataValidation(
        wb,
        mytable,
        col = sj + i,
        rows = 3:(formated_rows + 2),
        type = "whole",
        operator = "between",
        value = as.numeric(leveli)
      )

    }

  }
  #########add style
  headerStyle <- hs1
  addStyle(
    wb,
    sheet = mytable,
    headerStyle,
    rows = 1:2,
    cols = 1:(16 + number_of_traits),
    gridExpand = TRUE
  )
  ## style for body
  ## 设置内容主题的模板

  addStyle(
    wb,
    sheet = mytable,
    bodyStyle,
    rows = 3:(formated_rows + 2),
    cols = 1:(16 + number_of_traits),
    gridExpand = TRUE
  )
  ####
  setColWidths(wb, mytable, cols = 1, widths = 10)
  setRowHeights(wb,
                mytable,
                rows = 1:(formated_rows + 2),
                heights = 17.5)

  return(wb)

}




