#' 转换字段名为中文名
#'
#' 把数据框中的英文字段名转换为对应的中文名称。
#'
#' @param code 字符向量，要转换的字段名
#' @param toenglish 逻辑值，TRUE 返回中文名，FALSE 返回英文名
#'
#' @return 字符向量，与 `code` 等长的转换结果
#' @examples
#' transname(c("id", "name", "ma"))
#'
#' @export
transname <- function(code = c("fieldid", "id", "user", "stageid", "ma"), toenglish = TRUE) {
  # 使用命名向量：en2ch 的names是英文，values是中文
  en2ch <- c(
    "id" = "材料序号",
    "user" = "用户",
    "stageid" = "阶段名称",
    "name" = "名称",
    "f" = "世代",
    "ma" = "母本",
    "pa" = "父本",
    "mapa" = "亲本",
    "memo" = "备注",
    "stage" = "阶段",
    "next_stage" = "下阶段",
    "process" = "过程序号",
    "path" = "选育过程",
    "source" = "来源",
    "sele" = "选株数",
    "place" = "地点",
    "fieldid" = "田间序号",
    "code" = "代号"
  )
  # ch2en 的names是中文，values是英文
  ch2en <- names(en2ch)
  names(ch2en) <- en2ch

  if (toenglish) {
    # toenglish=TRUE: 输入英文字段名，输出中文字段名
    re_v <- en2ch[code]
  } else {
    # toenglish=FALSE: 输入中文字段名，输出英文字段名
    re_v <- ch2en[code]
  }
  return(re_v)
}
