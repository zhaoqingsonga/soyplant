% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traits.R
\name{traitstable}
\alias{traitstable}
\title{#' 生成性状表格
#'
#'返回openxlsx包createWorkbook()创建对象，一个表带验证功能的性状表
#' @param trait_col 数字，性状开始的列
#' @param validataion_number 数字，验证的行数
#' @return 返回openxlsx包createWorkbook()创建对象
#'
#'}
\usage{
traitstable(trait_col = 3, validation_number = 100)
}
\arguments{
\item{trait_col}{数字，表示性状开始的列，默认为3，用于确定在表格中写入性状相关数据的起始列位置。}

\item{validataion_number}{数字，表示验证的行数，默认为100，用于设定在表格中进行数据验证的行数范围。}
}
\value{
返回openxlsx包createWorkbook()创建对象

生成性状表格

此函数用于创建一个包含特定格式和验证功能的性状表格工作簿，返回 `openxlsx` 包中 `createWorkbook()` 创建的对象，该工作簿包含一个表带验证功能的性状表。

返回 `openxlsx` 包中 `createWorkbook()` 创建的对象，即包含特定格式和验证功能的性状表格工作簿。
}
\description{

}
\details{

}
\examples{
# 调用函数创建性状表格工作簿（使用默认参数）
wb <- traitstable()
# 可进一步对工作簿进行操作，如保存等
saveWorkbook(wb, "trait_table.xlsx")

}
