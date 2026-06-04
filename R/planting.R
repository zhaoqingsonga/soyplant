
#' 行数转行号
#'
#' @param hangshu 向量，各材料种植的行数
#' @return 返回各材料的行号
#' @example
#' number_to_linenumber(1:10)

rows_to_linenumber <- function(number = 1:10) {
  number <- as.numeric(number)
  if (any(is.na(number))) stop("'number' 必须是数值向量，当前包含 NA")
  end_number <- cumsum(number)
  start_number <- end_number - number + 1
  paste(start_number, end_number, sep = "-")
}


#' 添加编码重复对照（按间隔插入）
#'
#' @param my_primary 数据框，数据框中要包含id,stageid,name三列
#' @param ck 向量，对照名称,可一个对照或多个对照
#' @param interval 插入对照的材料间隔数
#' @param s_prefix 材料前缀
#' @param rp 种植材料重复数，第一重复顺序，其它重复随机，并排两列材料不相同
#' @return 返回插入对照和重复的数据框，给材料进行了编号
addrpckfixed <- function(my_primary,
                         ck = "JD12",
                         interval = 3,
                         s_prefix = "GC",
                         rp = 2,
                         digits = 3,
                         startN = 1,
                         first_as_ck = FALSE)

{
  mym <- my_primary#[c("id", "stageid", "name")]

  #加对照(如果有对照且interval>1)
  if (interval > 0 & length(ck) > 0) {
    patten <- rep(1:ceiling(nrow(mym) / interval),
                  each = interval,
                  length.out = nrow(mym))
    df_list <- split(mym, patten)
    df_list <- insert_ck_rows(df_list, ck, first_as_ck)
    df_list <- do.call(rbind, df_list)
    # insert_ck_rows 已正确设置 is_ck：原始行=0，对照行=1，无需再次赋值

    # 首行对照模式：末尾用对照闭合
    if (first_as_ck && nrow(df_list) > 0) {
      last_is_ck <- isTRUE(df_list$is_ck[nrow(df_list)] == 1)
      if (!last_is_ck) {
        ck_row <- df_list[1, , drop = FALSE]
        ck_row[] <- NA
        ck_row$is_ck <- 1L
        if ("name" %in% names(ck_row)) ck_row$name <- ck[1]
        df_list <- rbind(df_list, ck_row)
      }
    }

  } else{
    df_list <- mym
    df_list$is_ck <- 0L
  }

  # 确保 is_ck 列存在（当 ck 全为空字符串时 insert_ck_rows 不会生成此列）
  if (!"is_ck" %in% names(df_list)) {
    df_list$is_ck <- 0L
  }

  #第一重复加stageid,rp,code（code直接取stageid的数字部分）
  df_list$stageid <-
    generate_stageid(
      start_num = startN,
      end_num = nrow(df_list)+startN-1,
      char= s_prefix,
      digit_length = digits
    )
  df_list$code <- as.numeric(substring(df_list$stageid, nchar(s_prefix) + 1))
  df_list$rp <- 1#重复
  re_v = df_list
  rownames(re_v) <- NULL

  #如果rp>1增加重复(固定)
  if (rp > 1) {
    multirp <- list()
    for (rpi in 1:(rp - 1)) {
      df_list2 <- df_list
      rownames(df_list2) <- NULL
      # 非对照行整行替换（含code），保证code===stageid数字部分
      non_ck_rows <- which(!df_list$name %in% ck)
      df_list2[non_ck_rows, ] <- NA
      df_list2[non_ck_rows, ] <- df_list[sample(non_ck_rows), ]
      df_list2$rp <- rpi + 1
      multirp[[rpi]] <- df_list2
    }
    multirp <- do.call(rbind, multirp)
    re_v <- rbind(df_list, multirp)
    rownames(re_v) <- NULL
  }

  return(re_v)
}

#' 添加编码重复对照（对照随机）
#'
#'
#' @param my_plant 数据框，数据框中要包含id,stageid,name三列
#' @param ck 向量，对照名称
#' @param interval 插入对照的材料间隔数
#' @param s_prefix 材料前缀
#' @param rp 种植材料重复数，第一重复顺序，其它重复随机，并排两列材料不相同
#' @return 返回插入对照和重复的数据框，给材料进行了编号，对照随机

#ck随机,相邻两个重复，并排时不会相同
addrpck <- function(my_primary,
                    ck = c("冀豆12", "冀豆17"),
                    interval = 3,
                    s_prefix = "GC",
                    rp = 3,
                    digits = 3,
                    startN = 1,
                    first_as_ck = FALSE)

{
  mym <- my_primary#[c("id", "stageid", "name")]

  #加对照(如果有对照且interval>=1)
  if (interval > 0 & length(ck) > 0) {
    patten <- rep(1:ceiling(nrow(mym) / interval),
                  each = interval,
                  length.out = nrow(mym))
    df_list <- split(mym, patten)
    df_list <- insert_ck_rows(df_list, ck, first_as_ck)
    df_list <- do.call(rbind, df_list)
    # insert_ck_rows 已正确设置 is_ck：原始行=0，对照行=1，无需再次赋值

    # 首行对照模式：末尾用对照闭合
    if (first_as_ck && nrow(df_list) > 0) {
      last_is_ck <- isTRUE(df_list$is_ck[nrow(df_list)] == 1)
      if (!last_is_ck) {
        ck_row <- df_list[1, , drop = FALSE]
        ck_row[] <- NA
        ck_row$is_ck <- 1L
        if ("name" %in% names(ck_row)) ck_row$name <- ck[1]
        df_list <- rbind(df_list, ck_row)
      }
    }

  } else{
    df_list <- mym
    df_list$is_ck <- 0L
  }

  # 确保 is_ck 列存在（当 ck 全为空字符串时 insert_ck_rows 不会生成此列）
  if (!"is_ck" %in% names(df_list)) {
    df_list$is_ck <- 0L
  }

  #第一重复加stageid,rp,code（code直接取stageid的数字部分）
  df_list$stageid <-
    generate_stageid(
      start_num  = startN,
      end_num  = nrow(df_list)+startN-1,
      char = s_prefix,
      digit_length = digits
    )
  df_list$code <- as.numeric(substring(df_list$stageid, nchar(s_prefix) + 1))
  df_list$rp <- 1#重复
  re_v = df_list
  rownames(re_v) <- NULL

  #增加重复(随机)
  if (rp > 1) {
    multirp <- list()
    curr <- df_list#用于首个
    for (rpi in 1:(rp - 1)) {
      df_list2 <- df_list

      # 随机所有列（含code），保证code===stageid数字部分
      shuffled_idx <- sample(nrow(df_list))
      df_list2[] <- df_list[shuffled_idx, ]
      #直到全不一样，相邻两个重复相同位置不能有相同材料
      outj <- 0
      while (any(df_list2$name == curr$name)) {
        shuffled_idx <- sample(nrow(df_list))
        df_list2[] <- df_list[shuffled_idx, ]
        outj <- outj + 1
        if (outj >= 20) {
          break
        }
      }

      df_list2$rp <- rpi + 1
      multirp[[rpi]] <- df_list2
      curr <- df_list2
    }

    multirp <- do.call(rbind, multirp)
    re_v <- rbind(df_list, multirp)
    rownames(re_v) <- NULL

  }

  return(re_v)
}


addplace <- function(my_primary, place = c("石家庄", "德州")) {
  # 向量化版本：使用lapply + do.call(rbind)
  myd <- do.call(rbind, lapply(place, function(iname) {
    mat <- my_primary
    mat$place <- iname
    mat
  }))
  return(myd)
}

addtreatment <- function(my_primary, treatment = c("高密", "低密")) {
  # 向量化版本：使用lapply + do.call(rbind)
  myd <- do.call(rbind, lapply(treatment, function(iname) {
    mat <- my_primary
    mat$treatment <- iname
    mat
  }))
  return(myd)
}



addfieldid <- function(my_primary) {
  fieldid <-
    data.frame(fieldid = generate_id(start_num = 1,end_num  = nrow(my_primary),char = "f"))
  re_v <- cbind(fieldid, my_primary)
  return(re_v)
}

##
##考虑了不同地点是否重新编写FID
addplace_addfieldid_addrows <- function(my_primary, place = c("石家庄", "德州"), restartfid = FALSE, rows = 6) {
  # 确保 rows 为数值型，避免 rows_to_linenumber 中 cumsum/subtract 失败
  rows_num <- as.numeric(rows)
  if (is.na(rows_num)) stop("'rows' 参数必须是可以转换为数字的值，当前为: ", rows)

  if (restartfid) {
    # 不同地点重新编号：使用lapply收集，最后合并
    df <- do.call(rbind, lapply(place, function(iname) {
      mat <- my_primary
      mat$place <- iname
      mat$fieldid <- generate_id(start_num = 1, end_num = nrow(mat), char = "f")
      mat$rows <- rows_num
      mat$line_number <- rows_to_linenumber(mat$rows)
      Sys.sleep(1.1)
      mat
    }))
  } else {
    # 不同地点不重新编号
    df <- addplace(my_primary, place)
    df <- addfieldid(df)
    df$rows <- rows_num
    df$line_number <- rows_to_linenumber(df$rows)
  }

  return(df)
}


#' 确保数据框包含指定字段并调整字段顺序
#'
#' 该函数确保数据框中包含指定的必需字段。如果数据框中缺少某个必需字段，则自动添加并填充为 `NA`。函数还将必需字段调整至数据框的前面，其余字段保持在后面。
#'
#' @param df 数据框，需要检查并调整字段的顺序。
#' @param required_columns 字符串向量，包含所有需要的必需字段名。
#'
#' @return 返回调整后的数据框，其中必需字段在前，其他字段在后。
#'
#' @examples
#' # 定义数据框
#' df <- data.frame(name = c("A", "B"), stageid = c(1, 2))
#'
#' # 定义必需字段
#' required_columns <- c("id", "user", "stageid", "name", "ma", "pa", "mapa", "memo",
#'                       "stage", "next_stage", "f", "sele", "process", "path", "source")
#'
#' # 调用函数确保字段存在并重新排序
#' df <- ensure_and_reorder_columns(df, required_columns)
#'
#' # 查看结果
#' print(df)
#'
#' @export
ensure_and_reorder_columns <- function(df, required_columns) {
  for (col in required_columns) {
    if (!col %in% colnames(df)) {
      df[[col]] <- NA
    }
  }

  all_columns <- colnames(df)
  extra_columns <- setdiff(all_columns, required_columns)
  ordered_columns <- c(required_columns, extra_columns)

  df <- df[, ordered_columns]

  return(df)
}






#' 计划种植试验
#'
#' @param my_primary 数据框，数据框中需包含 id, stageid, name 三列（若无 stageid 列将自动补全）。
#' @param ck 对照名称。可以是：(1) 字符向量 → 所有地点统一对照；
#'   (2) 命名列表 `list("地点A" = "对照1", "地点B" = c("对照1","对照2"))` → 每地独立对照。
#' @param interval 插入对照的材料间隔数。
#' @param s_prefix stageid 前缀。可以是：(1) 字符串 → 统一前缀；
#'   (2) 命名列表 `list("地点A" = "SJZ", "地点B" = "DZ")` → 每地独立前缀。
#' @param rp 种植材料重复数，第一重复顺序，其它重复随机，并排两列材料不相同。
#' @param place 向量，试验地点（ck 为命名列表时自动使用 names(ck) 作为地点）。
#' @param treatment 向量，处理因素。
#' @param ckfixed 逻辑值，是否固定插入对照。
#' @param digits 整数，材料编号的位数。
#' @param rows 每块的行数。
#' @param restartfid 逻辑值，是否重新启动字段编号。
#' @param startN 整数，起始编号。
#' @param first_as_ck 逻辑值，首记录是否作为对照。
#'
#' @return 数据框，包含编号、对照插入、处理、地点等信息。
#'
#' @examples
#' \dontrun{
#' # 统一对照（原有方式）
#' planting(my_primary, ck = c("冀豆12", "冀豆17"))
#'
#' # 每地独立对照
#' planting(my_primary,
#'   ck = list("石家庄" = "冀豆12", "德州" = c("冀豆17", "鲁豆1号")))
#' }
planting <- function(
    my_primary,
    ck = c("冀豆12", "冀豆17"),
    interval = 3,
    s_prefix = "GC",
    rp = 2,
    treatment = c(""),
    place = c("石家庄", "德州"),
    ckfixed = TRUE,
    digits = 3,
    rows = 6,
    restartfid = FALSE,
    startN = 1,
    first_as_ck = FALSE
) {
  library(dplyr)

  # 如果没有 stageid 列，添加默认值为 NA
  if (!"stageid" %in% names(my_primary)) {
    my_primary <- my_primary |>
      mutate(stageid = NA)
  }

  # rows 防御性转换为数值型
  rows <- as.numeric(rows)

  # ── 判断是否 per-place ck（named list） ──
  per_place_ck <- is.list(ck) && !is.null(names(ck)) && length(ck) > 0
  # per-place s_prefix（命名列表）：不同地点可用不同前缀
  per_place_sp <- is.list(s_prefix) && !is.null(names(s_prefix))

  if (per_place_ck) {
    # ========================================================
    # 不同地点不同对照：每个地点独立插入对照 → 合并
    # ========================================================
    ck_places  <- names(ck)
    cur_startN <- startN
    result_parts <- list()

    # 固定随机种子：各地 2nd+ 重复的材料排列完全一致（便于备种）
    per_place_seed <- sample.int(1e9, 1)

    for (p in ck_places) {
      set.seed(per_place_seed)
      ck_p <- ck[[p]]
      sp   <- if (per_place_sp) s_prefix[[p]] else s_prefix
      part <- my_primary

      if (ckfixed) {
        part <- addrpckfixed(part, ck_p, interval, sp, rp, digits, cur_startN, first_as_ck)
      } else {
        part <- addrpck(part, ck_p, interval, sp, rp, digits, cur_startN, first_as_ck)
      }

      part <- addtreatment(part, treatment)
      part$place <- p
      # 推进起始编号：每地消耗的唯一 stageid 数 = 总行数 / 重复数
      cur_startN <- cur_startN + nrow(part) / rp
      result_parts[[p]] <- part
    }

    result <- do.call(rbind, result_parts)
    rownames(result) <- NULL

    # fieldid：按 restartfid 策略生成
    if (restartfid) {
      # 每地独立 fieldid（需延迟防碰撞，和 addplace_addfieldid_addrows 一致）
      place_groups <- split(result, factor(result$place, levels = unique(result$place)))
      result <- do.call(rbind, lapply(seq_along(place_groups), function(i) {
        grp <- place_groups[[i]]
        grp$fieldid <- generate_id(start_num = 1, end_num = nrow(grp), char = "f")
        rownames(grp) <- NULL
        if (i < length(place_groups)) Sys.sleep(1.1)
        grp
      }))
    } else {
      result <- addfieldid(result)
    }

    result$rows        <- rows
    result$line_number <- rows_to_linenumber(result$rows)

  } else {
    # ========================================================
    # 统一对照（原有逻辑，不变）
    # ========================================================
    result <- if (ckfixed) {
      my_primary |>
        addrpckfixed(ck, interval, s_prefix, rp, digits, startN, first_as_ck) |>
        addtreatment(treatment) |>
        addplace_addfieldid_addrows(place, restartfid, rows)
    } else {
      my_primary |>
        addrpck(ck, interval, s_prefix, rp, digits, startN, first_as_ck) |>
        addtreatment(treatment) |>
        addplace_addfieldid_addrows(place, restartfid, rows)
    }
  }

  # 对齐到field模式
  result <- align_to_field_schema(result, table_pattern = "planting")
}









#' 计划种植试验
#'
#'
#' @param my_primary 数据框，数据框中要包含id,stageid,name三列
#' @param ck 向量，对照名称
#' @param interval 插入对照的材料间隔数
#' @param s_prefix 材料前缀
#' @param rp 种植材料重复数，第一重复顺序，其它重复随机，并排两列材料不相同
#' @param place 向量，试验地点，可多个试验地点
#' @param treatment 向量，处理，可以多个水平
#' @param ckfixed 逻辑值，对照是否固定，是固定则按等材料数插入
#' @return 返回插入对照和重复的数据框，给材料进行了编号，对照随机

