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

#' 计算不同农业参数
#'
#' 该函数用于根据已知的亩密度、株距和行距计算缺失的农业参数，确保只提供一个缺失值，其余两个必须为已知值。支持计算亩密度、株距、行距及米株数等。
#'
#' @param 亩密度_万株 亩密度，单位：万株（可选）。
#' @param 行距_厘米 行距，单位：厘米（可选）。
#' @param 株距_厘米 株距，单位：厘米（可选）。
#'
#' @return 返回一个列表，包括以下字段：
#' \item{亩密度_万株}{计算出的亩密度，单位：万株。}
#' \item{行距_厘米}{计算出的行距，单位：厘米。}
#' \item{株距_厘米}{计算出的株距，单位：厘米。}
#' \item{米株数}{每米的株数。}
#' \item{亩用种量_万粒}{每亩用种量，单位：万粒，假设每株使用1.5粒种子。}
#'
#' @details
#' 该函数确保只有一个输入参数为空。如果三个参数中有两个以上为空，则会提示错误。计算公式如下：
#' \item{亩密度_万株 = 6666666 / (株距_厘米 * 行距_厘米) / 10000}{当亩密度为空时。}
#' \item{株距_厘米 = 6666666 / (亩密度_万株 * 10000 * 行距_厘米)}{当株距为空时。}
#' \item{行距_厘米 = 亩密度_万株 * 株距_厘米 / 6666666}{当行距为空时。}
#'
#' @examples
#' midu(亩密度_万株 = 30, 行距_厘米 = 20)
#' midu(株距_厘米 = 15, 行距_厘米 = 25)
#' midu(亩密度_万株 = 25, 株距_厘米 = 18)
#'
#' @export
midu <- function(亩密度_万株 = NULL, 行距_厘米 = NULL, 株距_厘米 = NULL) {
  # 确保只有一个参数为空
  if (sum(sapply(list(亩密度_万株, 株距_厘米, 行距_厘米), is.null)) != 1) {
    return("只有一个参数为空，其余两个必须提供非空值。")
  }

  # 计算空值的参数
  if (is.null(亩密度_万株)) {
    # 亩密度_万株 = 6666666 / 株距_厘米 * 行距_厘米
    亩密度_万株 <- 6666666 / (株距_厘米 * 行距_厘米)/10000
  } else if (is.null(株距_厘米)) {
    # 株距_厘米 = 6666666 / 亩密度_万株 * 行距_厘米
    株距_厘米 <- 6666666 / (亩密度_万株 *10000*行距_厘米)
  } else if (is.null(行距_厘米)) {
    # 行距_厘米 = 亩密度_万株 * 株距_厘米 / 6666666
    行距_厘米 <- 亩密度_万株 * 株距_厘米 / 6666666
  }

  # 计算 米株数
  米株数 <- 100 / 株距_厘米

  # 返回所有计算的结果
  return(list(亩密度_万株 = 亩密度_万株, 行距_厘米 = 行距_厘米, 株距_厘米 = 株距_厘米,  米株数 = 米株数, 亩用种量_万粒 = 亩密度_万株 * 1.5))
}
