#' @keywords internal
"_PACKAGE"

#' 大豆性状定义表
#'
#' 百奥云平台的性状定义原始表，每行一个性状，含性状编码、名称、缩写、
#' 类型、单位、取值范围等定义信息。由 `inst/extdata/temp_traits_baiaoyun.xlsx`
#' 转换而来。
#'
#' @format 一个数据框，88 行 13 列。
#' @seealso [soy_traits]
#' @name baiaoyun_traits
#' @docType data
#' @keywords datasets
NULL

#' 田间字段定义表
#'
#' 定义田间试验各阶段数据表的字段结构：字段名、顺序、所属表名与中文含义。
#' 包内多数函数（如 [align_to_field_schema()]）据此补齐与排序字段。
#'
#' @format 一个数据框，24 行 5 列。
#' @name field
#' @docType data
#' @keywords datasets
NULL

#' 杂交组合模板表
#'
#' 组合阶段的数据模板，定义组合记录所需的字段结构。
#'
#' @format 一个数据框。
#' @name mapa
#' @docType data
#' @keywords datasets
NULL

#' 组合阶段数据示例表
#'
#' 组合阶段记录的数据结构示例。
#'
#' @format 一个数据框。
#' @name my_combi
#' @docType data
#' @keywords datasets
NULL

#' 性状二维码数据表
#'
#' 性状定义按 `level_C` 拆分成多行后的明细表，每行对应性状的一个具体水平，
#' 并含 `level_code` 编码（由 `name_code` 与水平序号拼接而成）。
#'
#' @format 一个数据框，265 行 12 列。
#' @seealso [soy_traits]
#' @name qr_trait
#' @docType data
#' @keywords datasets
NULL

#' 大豆性状汇总表
#'
#' 包内使用的性状定义汇总表，每行一个性状，含中文名、英文缩写、
#' 水平取值、排序、字段类型（数值 N / 日期 D / 字符 C）等信息。
#'
#' @format 一个数据框，88 行 11 列。
#' @seealso [baiaoyun_traits] [qr_trait]
#' @name soy_traits
#' @docType data
#' @keywords datasets
NULL

#' 阶段编号前缀表
#'
#' 各育种阶段对应的编号前缀配置。
#'
#' @format 一个数据框。
#' @name stage_prefix
#' @docType data
#' @keywords datasets
NULL
