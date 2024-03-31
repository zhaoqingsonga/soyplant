#' 保存数据表
#'
#' @param origin 数据框，保存生成的原始数据
#' @param planting 数据框，保存生成的种植数据
#' @param myview 数据框，保存调查表
#' @param filename 字符串，保存的文件名
#' @param overwrite 是否覆盖
#' @return 保存生成的workbook类型数据到excel中

savewb <-
  function(origin = NA,
           planting = NA,
           myview = NA,
           filename,
           overwrite = FALSE) {
    wb <-
      traitstable(trait_col = ncol(myview),
                  validation_number = nrow(myview))
    addWorksheet(wb, "planting", visible = TRUE)
    addWorksheet(wb, "origin", visible = FALSE)
    #wb中有
    #addWorksheet(wb, "traits", visible = TRUE)

    writeData(wb, "planting", planting, startRow = 2)
    writeData(wb, "origin", origin, startRow = 1)
    writeData(wb, "traits", myview, startRow = 2)
    #设置格式
    hs1 <-
      createStyle(
        fgFill = "lightblue",
        halign = "CENTER",
        textDecoration = "bold",
        border = "Bottom"
      )
    bodyStyle <-
      createStyle(border = "TopBottom", borderColour = "red")
    #########add style
    headerStyle <- hs1
    addStyle(
      wb,
      sheet = "planting",
      headerStyle,
      rows = 1:2,
      cols = 1:(9 + ncol(planting)),
      gridExpand = TRUE
    )
    ## style for body
    ##设置内容主题的模板

    addStyle(
      wb,
      sheet = "planting",
      bodyStyle,
      rows = 3:(nrow(planting) + 2),
      cols = 1:(9 + ncol(planting)),
      gridExpand = TRUE
    )
    ####
    setColWidths(wb, "planting", cols = 1, widths = 10)
    setRowHeights(wb,
                  "planting",
                  rows = 1:(nrow(planting) + 2),
                  heights = 17.5)
    #
    saveWorkbook(wb, overwrite = overwrite, filename)
    return("Ok!")
  }