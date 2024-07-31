#' 计算株行距函数
#'
#' 此函数用于计算在给定亩株数和行距的情况下的每米苗数和株距。
#'
#' @param mu_zhushu 每亩株数，默认为16000。
#' @param hangju 行距（米），默认为0.5。
#'
#' @return 返回一个列表，包含每米苗数和株距。
#' \describe{
#'   \item{mi_miaoshu}{每米苗数}
#'   \item{zhujv}{株距}
#' }
#' @examples
#' # 使用默认参数计算每米苗数和株距
#' result <- zhuhangjv()
#' print(result)
#'
#' # 使用自定义参数计算每米苗数和株距
#' result <- zhuhangjv(mu_zhushu = 18000, hangju = 0.6)
#' print(result)
#' @export
zhuhangjv <- function(mu_zhushu = 16000, hangju = 0.5) {
  # 计算单位平米的行长
  danwei_pingmi_hangchang <- 1 / hangju

  # 计算单位平米的粒数
  danwei_pingmi_lishu <- mu_zhushu / 666.6667

  # 计算米苗数
  mi_miaoshu <- danwei_pingmi_lishu / danwei_pingmi_hangchang

  # 计算株距
  zhujv <- danwei_pingmi_hangchang / danwei_pingmi_lishu

  return(list(mi_miaoshu = mi_miaoshu, zhujv = zhujv))
}
