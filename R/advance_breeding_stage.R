
#' 处理初级产比进入高级产比的数据
#'
#' 只有初级产比进入高级产比时，世代不增加
#'
#' @param candidates 数据框,要准备晋级的数据
#' @param start_num 整数，起始编号，默认为1
#' @return 数据框，处理后的高级产比数据
#' @export
advance_breeding_stage <- function(candidates, start_num = 1,
                                   selection_key="高级产比",
                                   target_key="国区省区"
) {
  # ignore.case = TRUE：兼容"预试"/"預試"/"预试阶段"等大小写/格式变体
  candidates <- subset(
    candidates,
    grepl(selection_key, candidates$next_stage, ignore.case = TRUE)
  )

  # 生成唯一ID
  candidates$id <- generate_id(start_num, end_num = nrow(candidates) + start_num - 1)

  # 记录来源名称和前fieldid,前stageid
  candidates$source <- candidates$name
  candidates$former_fieldid <- candidates$fieldid
  candidates$former_stageid <- candidates$stageid

  # 处理名称，小于9代时追加世代号
  candidates$name <- increment_generation_name(candidates$name, candidates$f)

  # 更新字段
  candidates$stage <- selection_key
  candidates$next_stage <- target_key
  candidates$f <- candidates$f + 1
  candidates$sele <- 5
  candidates$process <- paste(candidates$process, candidates$id, sep = "/")
  candidates$path <- paste(candidates$path, 0, sep = "-")

  # 移除行名
  rownames(candidates) <- NULL

  # 对齐到field模式
  candidates <- align_to_field_schema(candidates, table_pattern = "combination")
}


