

#' 生成性状表格
#'
#'返回openxlsx包createWorkbook()创建对象，包括两个表，性状分类表和带验证功能的性状表
#' @param trait_col 数字，性状开始的列
#' @param validataion_number 数字，验证的行数
#' @return 返回openxlsx包createWorkbook()创建对象

traitstable <- function(trait_col = 3,
                        validation_number = 100) {
  library(openxlsx)
  sj <- trait_col#性状开始列
  formated_rows <- validation_number#有效性验证行数

  MSN <- "traits"
  traits <- soy_traits[order(soy_traits$order),]
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
  addWorksheet(wb, paste(MSN, "_level", sep = ""), tabColour = "blue",visible = FALSE)

  number_of_traits <- nrow(traits)
  mytable <- MSN
  mytraits <- paste(mytable, "_level", sep = "")
  #表格中的列名
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
    #
    writeData(wb,
              mytraits,
              x = c(traiti, leveli),
              startCol = SCol)
    #leveli中有中文时会有警告信息
    if (traits$field_type[i] == "C") {
      list_value <-
        paste(mytraits,
              "!$",
              SCol,
              "$3:$",
              SCol,
              "$",
              length_leveli + 1,
              sep = "")
      dataValidation(
        wb,
        mytable,
        col = sj + i,
        rows = 3:(formated_rows + 2),
        type = "list",
        value = list_value
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
      #以下这句有问题，不知道原因
      #conditionalFormatting(wb, mytable, cols = sj+i, rows = 1:500, type = "databar")
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
##设置内容主题的模板

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
