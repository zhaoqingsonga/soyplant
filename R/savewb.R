#' 保存数据表
#'
#' @param origin 数据框，保存生成的原始数据
#' @param planting 数据框，保存生成的种植数据
#' @param myview 数据框，保存调查表
#' @param filename 字符串，保存的文件名
#' @param overwrite 是否覆盖
#' @return 保存生成的workbook类型数据到excel中

savewb <-
  function(origin = NULL,
           material = NULL,
           planting = NULL,
           myview = NULL,
           combi_matrix = NULL,
           filename,
           overwrite = FALSE) {

    wb <- traitstable(trait_col = ncol(myview), validation_number = nrow(myview))
    # 定义sheet参数列表，名称、tab颜色、写入起始行、内容
    sheet_info <- list(
      planting = list(tabColour = "darkred", data = planting, startRow = 2),
      material = list(tabColour = "red", data = material, startRow = 1),
      origin   = list(tabColour = "darkgreen", data = origin, startRow = 1),
      traits   = list(tabColour = NULL, data = myview, startRow = 2)
    )

    # 获取已存在的工作表名（兼容大小写问题，openxlsx工作表名大小写不敏感）
    existing_sheets <- tolower(names(wb))

    # 循环添加sheet及写入数据，避免已存在时重复添加
    for (s in names(sheet_info)) {
      if (!(tolower(s) %in% existing_sheets)) {
        addWorksheet(wb, s, visible = TRUE, tabColour = sheet_info[[s]]$tabColour)
      }
      writeData(wb, s, sheet_info[[s]]$data, startRow = sheet_info[[s]]$startRow)
    }
    # 组合矩阵表（如有），同样保证唯一名
    if (!is.null(combi_matrix)) {
      if (!("combi_matrix" %in% tolower(names(wb)))) {
        addWorksheet(wb, "combi_matrix", visible = TRUE, tabColour = "blue")
      }
      writeData(wb, "combi_matrix", combi_matrix, startRow = 1)
    }

    # 样式定义
    hs1 <- createStyle(
      fgFill = "lightblue",
      halign = "CENTER",
      textDecoration = "bold",
      border = "Bottom"
    )
    bodyStyle <- createStyle(border = "TopBottom", borderColour = "red")

    # 要批量添加样式的sheet
    format_sheets <- c("planting", "origin", "material")
    for (sheet in format_sheets) {
      # 列数统一用最大值，优化以防各表列数不同
      ncols <- if (!is.null(ncol(planting))) (9 + ncol(planting)) else 9
      nrows <- if (!is.null(nrow(planting))) (nrow(planting) + 2) else 2
      # 第一行做表头（planting行头2行，其余1行，实际根据现有代码也可合并简写如下）
      header_rows <- if (sheet == "planting") 1:2 else 1
      addStyle(
        wb,
        sheet = sheet,
        hs1,
        rows = header_rows,
        cols = 1:ncols,
        gridExpand = TRUE
      )
      # 内容区样式（planting从第3行，其余第2行）
      body_start <- if (sheet == "planting") 3 else 2
      addStyle(
        wb,
        sheet = sheet,
        bodyStyle,
        rows = body_start:nrows,
        cols = 1:ncols,
        gridExpand = TRUE
      )
      setColWidths(wb, sheet, cols = 1, widths = 10)
      setRowHeights(wb, sheet, rows = 1:nrows, heights = 17.5)
    }

    saveWorkbook(wb, overwrite = overwrite, filename)
    return("Ok!")
  }












